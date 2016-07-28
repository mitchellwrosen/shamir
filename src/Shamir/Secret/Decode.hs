module Shamir.Secret.Decode where

import Shamir.Secret.N
import Shamir.Secret.Share
import Shamir.Secret.V

import qualified Shamir.Secret.N as N

import Control.Arrow   ((&&&))
import Data.ByteString (ByteString)
import Data.List
import Data.Word
import GHC.Prim        (Proxy#, proxy#)
import GHC.TypeLits

import qualified Data.ByteString as ByteString
import qualified Data.Vector     as Vector

-- | Decode a vector of @n@ 'Share's, only @k@ of which are necessary to
-- reconstruct the secret.
decode
  :: forall n k.
     (KnownNat n, KnownNat k, 2 <= k, k <= n)
  => V n (Share k)
  -> ByteString
decode (toList -> ss) =
  ByteString.pack
    (map
      (\i -> decodeByte (map (shareIndex &&& shareByte i) (take k ss)))
      [0 .. share_len-1])
 where
  share_len :: Int
  share_len = Vector.length (shareBytes (ss !! 0))

  k :: Int
  k = fromIntegral (natVal' (proxy# :: Proxy# k))

decodeByte :: [(N, N)] -> Word8
decodeByte ss = fromIntegral (N.toInteger (sum (map go ss)))
 where
  go :: (N, N) -> N
  go (x, y) = foldl' step y (map fst ss)
   where
    step :: N -> N -> N
    step acc x'
      | x == x'   = acc
      | otherwise = acc * (x' / (x' - x))
