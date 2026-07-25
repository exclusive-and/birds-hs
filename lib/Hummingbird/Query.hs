module Hummingbird.Query where

import Prelude

import Control.Concurrent
import Control.Monad
import Control.Monad.Primitive
import Data.ContentAddress
import Data.Dependent.HashMap (DHashMap)
import Data.Fetch
import Data.GADT.Compare
import Data.GADT.Show
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Primitive.MutVar
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Some
import Data.Text.Rope (Rope)
import Data.Typeable

import Prettyprinter

import Hummingbird.Codebase as Codebase
import Hummingbird.Codebase.Id
import Hummingbird.Error
import Hummingbird.Name as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var

data Query answer where
  -- |
  GetCodebase :: Query Codebase

  -- |
  GetAllKeys :: Query (Map Name Id)
  
  -- |
  LookupName ::
    !Name
    -> Query (Maybe (Hash, Surface.Term Var))

  -- |
  RenameExpr ::
    Surface.Term Name
    -> Query (CodePatch Renamed)

  -- |
  RenameDecls ::
    [Surface.Declaration Name]
    -> Query
        ( [Error]
        , [(Var, Surface.Term Var, FreeVars)]
        )

  -- |
  SerializeDecls ::
    [(Var, Surface.Term Var, FreeVars)]
    -> Query (CodePatch Renamed)

  -- |
  ParseRepl :: !Text -> Query (CodePatch Parsed)

  -- | Ingest a parsed surface-language declaration and try to intern
  -- its changes into the codebase.
  IngestDecl ::
    !Name.Module
    -> Surface.Declaration Name
    -> Query (Maybe [Error])

  -- | What is the raw text of this source file?
  FileText :: !FilePath -> Query Text

  -- | What is the rope representation of this source file?
  FileRope :: !FilePath -> Query Rope

  -- |
  ParsedFile ::
    !FilePath
    -> Query (Either [Error] (Surface.Module Name))

  -- | Ingest a source file and try to intern its changes into the codebase.
  IngestFile :: !FilePath -> Query (Maybe [Error])

  -- | Ingest a directory recursively.
  IngestDirectory :: !FilePath -> Query (Maybe [Error])

instance Eq (Query a) where
  (==) = defaultEq

instance GEq Query where
  GetCodebase         `geq` GetCodebase                   = Just Refl
  GetAllKeys          `geq` GetAllKeys                    = Just Refl
  LookupName x        `geq` LookupName y        | x == y  = Just Refl
  RenameExpr x        `geq` RenameExpr y        | x == y  = Just Refl
  RenameDecls x       `geq` RenameDecls y       | x == y  = Just Refl
  SerializeDecls x    `geq` SerializeDecls y    | x == y  = Just Refl
  IngestDecl nm1 decl1 `geq` IngestDecl nm2 decl2
    | nm1 == nm2, decl1 == decl2                          = Just Refl
  FileText x          `geq` FileText y          | x == y  = Just Refl
  FileRope x          `geq` FileRope y          | x == y  = Just Refl
  ParsedFile x        `geq` ParsedFile y        | x == y  = Just Refl
  IngestFile x        `geq` IngestFile y        | x == y  = Just Refl
  IngestDirectory x   `geq` IngestDirectory y   | x == y  = Just Refl
  _                   `geq` _                             = Nothing

instance GShow Query where
  gshowsPrec = defaultGshowsPrec

instance Hashable (Query a) where
  hashWithSalt = defaultHashWithSalt

  hash = \case
    GetCodebase         -> go  0 ()
    GetAllKeys          -> go  1 ()
    LookupName a        -> go  2 a
    RenameExpr a        -> go  5 a
    RenameDecls a       -> go  6 a
    SerializeDecls a    -> go  7 a
    ParseRepl a         -> go 10 a
    FileText a          -> go 50 a
    FileRope a          -> go 51 a
    IngestDecl a b      -> go 52 (a, b)
    ParsedFile a        -> go 53 a
    IngestFile a        -> go 54 a
    IngestDirectory a   -> go 55 a
    where
      go :: (Hashable b) => Int -> b -> Int
      go tag a = Data.Hashable.hash tag `hashWithSalt` a

instance Hashable (Some Query) where
  hash (Some query) = hash query
  hashWithSalt salt (Some query) = hashWithSalt salt query

deriving instance Show (Query a)
deriving instance Typeable (Query a)

{-# Specialize
    memoiseWithCycleDetection ::
      (Memoiseable Query)
      => MutVar RealWorld (DHashMap Query MemoEntry)
      -> MutVar RealWorld (HashMap ThreadId ThreadId)
      -> GenRules IO Query Query
      -> GenRules IO Query Query
    #-}
