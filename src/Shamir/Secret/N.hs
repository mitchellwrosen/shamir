module Shamir.Secret.N
  ( N
  , Shamir.Secret.N.toInteger
  ) where

import Data.Ratio

prime :: Integer
prime = 2683

newtype N = N { toInteger :: Integer }
  deriving (Eq, Show)

instance Num N where
  N x + N y = N ((x + y) `mod` prime)
  N x * N y = N ((x * y) `mod` prime)
  N x - N y = N ((x - y) `mod` prime)
  negate (N x) = N ((negate x) `mod` prime)
  abs x = x
  signum _ = 1
  fromInteger x = N (x `mod` prime)

instance Fractional N where
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)
  recip x = x ^ (prime - 2)
