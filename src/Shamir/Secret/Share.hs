module Shamir.Secret.Share where

import Shamir.Secret.N
import Shamir.Secret.Utils

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Vector     (Vector, (!))
import GHC.Generics    (Generic)
import GHC.TypeLits

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Vector            as Vector

-- | A @Share k@ needs at least @k@ shares to reproduce a secret.
data Share (k :: Nat) = Share
  { shareIndex :: N
  , shareBytes :: Vector N
  } deriving (Eq, Generic, NFData)

shareByte :: Int -> Share k -> N
shareByte i share = shareBytes share ! i

-- | @makeShare n coeffs bytes@ makes share number @n@ of @bytes@, given
-- coefficients @coeffs@.
--
-- The coefficients Vector and the ByteString are assumed to be the same
-- length.
makeShare :: N -> Vector [N] -> ByteString -> Share k
makeShare x css bytes = Share x share
 where
  share :: Vector N
  share = Vector.generate (ByteString.length bytes) go
   where
    go :: Int -> N
    go i = evalPolynomial (c:cs) x
     where
      c :: N
      c = fromIntegral (ByteString.unsafeIndex bytes i)

      cs :: [N]
      cs = css ! i
