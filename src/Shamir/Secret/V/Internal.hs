module Shamir.Secret.V.Internal where

import GHC.Prim     (unsafeCoerce#)
import GHC.TypeLits

data V :: Nat -> * -> * where
  Nil   :: V 0 a
  (:::) :: a -> V n a -> V (n+1) a
infixr 5 :::

instance Show a => Show (V n a) where
  show = show . toList

toList :: V n a -> [a]
toList Nil = []
toList (x ::: xs) = x : toList xs

fromList :: [a] -> V n a
fromList []     = unsafeCoerce# Nil
fromList (x:xs) = unsafeCoerce# (x ::: fromList xs)
