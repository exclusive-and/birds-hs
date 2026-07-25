{-# Language NoMonoLocalBinds #-}

module Data.Graph where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.Bifunctor
import Data.Bounded
import Data.Foldable
import Data.Graph.Internal
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.Array
import Data.Set (Set)
import Data.Set qualified as Set
import Prelude
import Prettyprinter

-- | Sparse directed graphs.
data Graph a = Graph
  { edges :: Array [Vertex]   -- ^ Array of outgoing edges from each vertex.
  , nodes :: Array a          -- ^ Array of the original data or name of each vertex.
  }
  deriving (Eq, Show)

type Vertex = Int

-- | Map the vertices in a graph, without affecting its overall structure.
instance Functor Graph where
  fmap f Graph{edges, nodes} = Graph edges (fmap f nodes)

(!) :: Graph a -> Vertex -> a
(!) Graph{nodes} = indexArray nodes

-- | Create a new graph with only the raw internal structure of the input graph.
getStructure :: Graph a -> Graph Vertex
getStructure Graph{edges} =
  Graph edges (arrayFromList [0..length edges - 1])

-- | Construct a sparse graph from adjacency lists of numbered vertices.
unsafeMakeGraph :: [[Vertex]] -> Graph Vertex
unsafeMakeGraph adjs =
  let
    edges = arrayFromList adjs
    nodes = arrayFromList [0..length edges - 1]
  in
    Graph {edges, nodes}

-- | Construct a sparse graph from an adjacency map. Mainly intended for internal use.
fromMap :: forall a. (Ord a) => Map a [a] -> Graph a
fromMap adjacencyMap =
  let
    (nodes, edges) = unzip $ Map.elems $ identifyBfs go $ Map.keys adjacencyMap
  in
    Graph
      { edges = arrayFromList edges
      , nodes = arrayFromList nodes
      }
  where
    go x = Map.findWithDefault [] x adjacencyMap

fromKeyMap :: (Ord k) => Map k (a, [k]) -> Graph (k, a)
fromKeyMap keyed = runST do
  nodes <- newArray (length keyed) (error "Data.Graph.fromKeyMap: uninitialized node")
  edges <- newArray (length keyed) []
  let
    (_, indexMap) =
      Map.mapAccumWithKey
        (\v k (a, ks) -> (v + 1, (v, (a, k, ks))))
        0
        keyed
  forM_ indexMap \(v, (a, k, ks)) -> do
    writeArray nodes v (k, a)
    let
      f w = fst $
        Map.findWithDefault
          (error "Data.Graph.fromKeyMap: key does not exist")
          w
          indexMap
    writeArray edges v $ f <$> ks
  Graph <$> unsafeFreezeArray edges <*> unsafeFreezeArray nodes

toList :: Graph a -> [a]
toList = Data.Foldable.toList . nodes

-- | A function that rebuilds the internal structure of a graph.
newtype Rebuild = Rebuild {
    runRebuild ::
      forall s.
      Vertex
      -> [Vertex]
      -> ReaderT (MutableArray s [Vertex]) (ST s) ()
  }

-- | Modify the internal structure of a graph. Doesn't touch nodes at all.
rebuildG :: Rebuild -> Graph a -> Graph a
rebuildG rebuilder input = Graph rebuilt (nodes input)
  where
    rebuilt = createArray (length $ edges structure) []
      $ runReaderT
      $ forM_ (nodes structure) \v ->
          runRebuild rebuilder v (edges structure `indexArray` v)

    structure = getStructure input

-- | A strongly connected component. Within the same 'SCC', every vertex can always reach any
-- other vertex.
data SCC a b = Trivial b (a, Vertex) | Cycle b (NonEmpty (a, Vertex))
  deriving (Eq, Show)

instance Bifunctor SCC where
  bimap f g (Trivial b v) =
    Trivial (g b) (first f v)
  bimap f g (Cycle b vs) =
    Cycle (g b) (NonEmpty.map (first f) vs)

instance Functor (SCC a) where
  fmap = second

instance (Pretty a, Pretty b) => Pretty (SCC a b) where
  pretty (Trivial a v) =
    "Trivial" <+> pretty a <+> brackets (pretty v) 
  pretty (Cycle a vs) =
    "Cycle" <+> pretty a <+> pretty (NonEmpty.map fst vs)

-- | \(O(V + E)\). Map the vertices in a graph into a monoid, and contract along its edges
-- with @('<>')@. Outputs the strongly connected components of the graph because,
-- within the same 'SCC', the contraction is the same (up to shuffling) for all vertices.
-- The algorithm assumes that @('<>')@ is somewhat commutative, since the order of contractions
-- depends on the hidden structure of the input graph.
contractMap :: forall b a. Monoid b => (a -> b) -> Graph a -> [SCC a b]
contractMap f Graph{edges, nodes} =
  let
    -- See Note [contractMap attribution].
    (contracted, _stack, _depth) = runST do
      let size = length edges
      -- 1. Set up mutable arrays for preorders and partials.
      preorders <- newArray size (-1)
      partials <- newArray size mempty
      -- 2. Run a depth-first traversal and contraction on all vertices in the input.
      let initial = ([], [], 0)
          vertices = [0..size - 1]
      contractDfs initial vertices `runReaderT` (preorders, partials)
  in
    contracted
  where
    contractDfs = foldrM (whenInteresting contractAt)

    contractAt ::
      Vertex
      -> ([SCC a b], [Vertex], Int)
      -> ReaderT
          (MutableArray s Int, MutableArray s b)
          (ST s)
          ([SCC a b], [Vertex], Int)
    contractAt v (sccs, stack, depth) = do
      (preorders, partials) <- ask
      -- 1. Compute initial preorder number.
      writeArray preorders v depth
      -- 2. Recurse on adjacent vertices.
      let ws = edges `indexArray` v
      output <- contractDfs (sccs, v:stack, depth + 1) ws
      -- 3. Compute immediate and combined results.
      --    See Note [Recurse before calculating the immediate result].
      let y = f (nodes `indexArray` v)
      partials' <- traverse (readArray partials) ws
      let y' = mconcat (y:partials')
      writeArray partials v y'
      -- 4. Compute new preorder.
      preorders' <- traverse (readArray preorders) ws
      let n' = foldr min depth preorders'
      writeArray preorders v n'
      -- 5. Create a new SCC if one is detected.
      if n' == depth then pop [] v output else pure output

    pop scc v (sccs, [], _depth) = pure (sccs, [], 0)
    pop scc v (sccs, x:stack, depth) = do
      (preorders, partials) <- ask
      writeArray preorders x maxBound
      let depth' = depth - 1
      if v == x then do
        y <- partials `readArray` x
        -- Need to make sure that every vertex in the SCC gets the same final result!
        forM_ scc $ \(_, v) -> writeArray partials v y
        pure (createScc scc x y : sccs, stack, depth')
      else
        pop ((nodes `indexArray` x, x):scc) v (sccs, stack, depth')

    whenInteresting f v s = do
      (preorders, _) <- ask
      n <- preorders `readArray` v
      if n < 0 then f v s else pure s

    createScc [] x y | x `notElem` (edges `indexArray` x) =
      Trivial y (nodes `indexArray` x, x)
    createScc scc x y =
      Cycle y ((nodes `indexArray` x, x) :| scc)

{-
Note [contractMap attribution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'contractMap' is a variation of Tarjan's SCC-finding algorithm. Its implementation was derived
from the outline of the @Digraph@ algorithm written by DeRemer and Pennello in:

    DeRemer, F., and Pennello, T. "Efficient Computation of LALR(1) Look-Ahead Sets".
        ACM Transactions on Programming Languages and Systems, Vol 4, No 4 (1982), pp 615-649.
        https://doi.org/10.1145/69622.357187 (accessed on 2025-04-21).

Note [Recurse before calculating the immediate result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Contrary to the algorithm in the paper, my 'contractMap' implementation recurses /before/
computing the immediate result of a vertex. This change is important, because it
means 'contractMap' won't allow a vertex to contribute to the result of its own SCC twice.
-}

-- | Compute the strongly connected components of a graph.
sccs :: Graph a -> [SCC a (NonEmpty a)]
sccs input =
  flip map (sccs_ input) \case
    Trivial _ v   -> Trivial  (fst v :| []) v
    Cycle   _ vs  -> Cycle    (fst <$> vs)  vs

-- | Compute the strongly connected components of a graph.
sccs_ :: Graph a -> [SCC a ()]
sccs_ = contractMap (const ())

-- | Compute the reaching sets of a graph.
--
-- ==== __Examples__
--
-- Here's a simple example where we compute the reaching sets of a small graph:
--
-- >>> data ABC = A | B | C deriving (Eq, Ord, Show)
-- >>>
-- >>> reachingSets $ fromAdjacencies [(A, [B, C]), (B, [C]), (C, [])]
-- [Trivial (fromList [A,B,C]) 0,Trivial (fromList [B,C]) 1,Trivial (fromList [C]) 2]
reachingSets :: Ord a => Graph a -> [SCC a (Set a)]
reachingSets = contractMap Set.singleton

-- | Like 'reachingSets' with repetition.
--
-- ==== __Examples__
--
-- Here's what happens when we run this function on the same simple graph that we used
-- in the 'reachingSets' example:
--
-- >>> data ABC = A | B | C deriving (Eq, Ord, Show)
-- >>>
-- >>> reachingMulti $ fromAdjacencies [(A, [B, C]), (B, [C]), (C, [])]
-- [Trivial [A,B,C,C] 0,Trivial [B,C] 1,Trivial [C] 2]
--
-- Notice that multiple copies of @C@ show up? The intuitive reason is that the number of
-- copies of @C@ equals the total number of distinct paths through the graph that include @C@.
--
-- However, we don't see repetition in a cycle:
--
-- >>> reachingMulti $ fromAdjacencies [(A, [B]), (B, [A])]
-- [Cycle [B,A] (1 :| [0])]
--
-- This exception guarantees that we are indeed only counting /paths/; i.e. walks without
-- repeated vertices.
reachingMulti :: Graph a -> [SCC a [a]]
reachingMulti = contractMap (: [])

-- | Compute the /condensation/ of a graph.
--
-- The condensation of a graph is a new graph whose vertices each represent an entire 'SCC'
-- in the input graph.
condensation :: Monoid a => Graph a -> Graph (SCC a a)
condensation input@Graph{edges} =
  let
    sccs = zip [0..] $ contractMap id input
    nodes' = arrayFromList sccs
  in
    Graph (go nodes') (snd <$> nodes')
  where
    go nodes = runArray $ do
      -- 1. Build an association, for each input graph node, to the index of the node's
      --    SCC, as computed by 'contractMap'.
      labels <- newArray (length edges) (-1)
      traverse_ (uncurry labelVerts) nodes `runReaderT` labels
      labels' <- unsafeFreezeArray labels
      -- 2. Translate the structure of the input graph using the associations above.
      edges' <- newArray (length nodes) []
      traverse_ (uncurry rebuild) nodes `runReaderT` (labels', edges')
      pure edges'

    labelVerts n scc = do
      labels <- ask
      traverse_ (\v -> writeArray labels v n) $ sccVerts scc

    sccVerts (Trivial _ v ) = [snd v]
    sccVerts (Cycle   _ vs) = NonEmpty.toList $ NonEmpty.map snd vs

    rebuild n (Trivial _ v) = do
      (labels, edges') <- ask
      let ws  = edges `indexArray` snd v
          ws' = indexArray labels <$> ws
      writeArray edges' n ws'

    rebuild n (Cycle _ vs) = do
      (labels, edges') <- ask
      let ws  = concatMap (indexArray edges . snd) vs
          ws' = filter (/= n) $ indexArray labels <$> ws
      writeArray edges' n ws'
