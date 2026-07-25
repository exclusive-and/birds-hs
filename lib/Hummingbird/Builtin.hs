module Hummingbird.Builtin where

import Data.Binary
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as Map
import Prelude
import Prettyprinter

import Hummingbird.Name (Name)
import Hummingbird.Name qualified as Name

data Builtin
  = Cat
  | Apply
  | Dip
  | Swap
  | Dup
  | Drop
  | K
  | Cake
  | Placeholder
  deriving (Eq, Generic, Ord, Show)

instance Binary Builtin

instance Hashable Builtin

instance Pretty Builtin where
  pretty = \case
    Cat -> "cat"
    Apply -> "apply"
    Dip -> "dip"
    Swap -> "swap"
    Dup -> "dup"
    Drop -> "drop"
    K -> "k"
    Cake -> "cake"
    Placeholder -> "placeholder"

builtins :: Map Name Builtin
builtins = Map.fromList [
    (Name.Name "cat", Cat)
  , (Name.Name "apply", Apply)
  , (Name.Name "dip", Dip)
  , (Name.Name "swap", Swap)
  , (Name.Name "dup", Dup)
  , (Name.Name "drop", Drop)
  , (Name.Name "k", K)
  , (Name.Name "cake", Cake)
  , (Name.Name "placeholder", Placeholder)
  ]
