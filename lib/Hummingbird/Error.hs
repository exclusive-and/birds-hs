module Hummingbird.Error where

import Prelude

import Control.Exception (Exception)
import Control.Monad
import Control.Monad.Catch
import Data.ContentAddress
import Data.Hashable
import Data.Text qualified as Text

import Prettyprinter
import Prettyprinter.Render.Terminal
import Prettyprinter.Render.Text

import Hummingbird.Codebase.Hash
import Hummingbird.Codebase.Id
import Hummingbird.Name as Name
import Hummingbird.Surface qualified as Surface
import Hummingbird.Var (Var)

-- |
data Error
  = RawMessage !Text
  | CannotParse !FilePath !Text
  | Elaboration !Elaboration
  deriving
    ( Eq
    , Generic
    , Show
    )
  deriving anyclass (Hashable)

instance Exception Error
instance Exception [Error]

-- | Report all errors directly to the standard output.
reportAll :: [Error] -> IO ()
reportAll = print . vcat . map pretty

-- |
data Elaboration
  = NotInScope !Name
  | AmbiguousNames !Name !Var
  deriving
    ( Eq
    , Generic
    , Show
    )
  deriving anyclass (Hashable)

instance Pretty Error where
  pretty = \case
    RawMessage msg -> hang 2 $ vcat $ pretty <$> Text.lines msg
    CannotParse path msg ->
      hang 2 $ pretty msg
    Elaboration elab -> pretty elab

instance Pretty Elaboration where
  pretty = \case
    NotInScope name ->
      hang 2 $ "Not in scope:" <+> pretty name
    AmbiguousNames name other ->
      hang 2 $ "The name '" <> pretty name <> "' conflicts with an existing definition"
