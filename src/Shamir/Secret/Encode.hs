module Shamir.Secret.Encode where

import Shamir.Secret.N
import Shamir.Secret.Share
import Shamir.Secret.V.Internal

import Control.Monad
import Data.ByteString   (ByteString)
import Data.Vector       (Vector)
import Data.Word
import System.Random.MWC (GenIO, createSystemRandom, uniform)
import GHC.Prim          (Proxy#, proxy#)
import GHC.TypeLits

import qualified Data.ByteString as ByteString
import qualified Data.Vector     as Vector

-- | Encode a 'ByteString' into @n@ shares (two or more), any @k@ of
-- which are sufficient to decode it.
--
-- @
-- encode bytes = do
--   gen <- 'createSystemRandom'
--   'encodeWith' gen bytes
-- @
encode
  :: (KnownNat n, KnownNat k, 2 <= n, 2 <= k, k <= n)
  => ByteString
  -> IO (V n (Share k))
encode bytes = do
  gen <- createSystemRandom
  encodeWith gen bytes

-- | Like 'encode', but accepts a 'GenIO' rather than creating one using
-- 'createSystemRandom'.
encodeWith
  :: forall n k.
     (KnownNat n, KnownNat k, 2 <= n, 2 <= k, k <= n)
  => GenIO
  -> ByteString
  -> IO (V n (Share k))
encodeWith gen bytes = do
  coeffs <-
    Vector.replicateM
      (ByteString.length bytes)
      (map w2n <$> replicateM (k-1) (uniform gen))

  pure (fromList (encodeWith' n coeffs bytes))
 where
  n :: Integer
  n = natVal' (proxy# :: Proxy# n)

  k :: Int
  k = fromIntegral (natVal' (proxy# :: Proxy# k))

  w2n :: Word64 -> N
  w2n = fromIntegral

-- | @encodeWith' n cs bytes@ makes @n@ shares of @bytes@, given coefficients
-- to use for each index.
encodeWith' :: Integer -> Vector [N] -> ByteString -> [Share k]
encodeWith' n coeffs bytes =
  map (\i -> makeShare (fromInteger i) coeffs bytes) [1..n]
