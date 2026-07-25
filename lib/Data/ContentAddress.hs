module Data.ContentAddress where

import Prelude

import Control.Monad
import Crypto.Hash
import Crypto.Hash.Generic qualified
import Data.Array.Byte
import Data.Binary
import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Hashable
import Data.List (sortBy)
import Data.Text qualified as Text

import Prettyprinter

import Unsafe.Coerce (unsafeCoerce)

-- | SHA3-512 hash of some content-addressed data.
newtype Hash = Hash (Digest SHA3_512)
  deriving
    ( Eq
    , Ord
    , Show
    )
  deriving newtype (ByteArray.ByteArrayAccess)

instance Hashable Hash where
  salt `hashWithSalt` Hash digest =
    hashWithSalt
      salt
      (unsafeCoerce @_ @ByteArray digest)
    -- 'unsafeCoerce' is a necessary evil to make this function work. Digest and
    -- ByteArray are representationally equal under the hood; crypton hides
    -- the equality from us to protect the integrity of the individual hashes.

instance Binary Hash where
  get = do
    bs <- get @ByteString
    case digestFromByteString bs of
      Just digest -> pure (Hash digest)
      Nothing     -> fail "Hummingbird.Codebase.Hash.get: could not convert bytestring to hash"
  
  put (Hash digest) =
    put @ByteString $ ByteArray.convert digest

instance Pretty Hash where
  pretty (Hash a) =
    "#" <> pretty (Text.pack $ take 8 $ show a) <> "..."

-- | @'ContentAddress'@ is the class of content-addressable datatypes.
class ContentAddress a where
  contentHash :: a -> Hash
  contentHash =
    Hash . Crypto.Hash.Generic.hashWith SHA3_512
  
  default contentHash :: (Binary a) => a -> Hash

-- | Hashes are trivially content-addressable: they are their own addresses!
instance ContentAddress Hash where
  contentHash a = a
  {-# INLINE contentHash #-}

caSort :: (ContentAddress a) => [a] -> [a]
caSort = caSortBy id

caSortBy :: (ContentAddress x) => (a -> x) -> [a] -> [a]
caSortBy f = sortBy (compare `on` contentHash . f)
