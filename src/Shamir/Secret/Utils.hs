module Shamir.Secret.Utils where

-- | @evalPolynomial cs x@ evaluates the polynomial described by coefficients
-- @cs@ at value @x@.
--
-- >>> evalPolynomial [1,2,3] 4
-- 57
evalPolynomial :: Num a => [a] -> a -> a
evalPolynomial cs x = foldr (\c acc -> c + x*acc) 0 cs
