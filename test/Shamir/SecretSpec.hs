module Shamir.SecretSpec where

import Shamir.Secret

import Data.ByteString (ByteString)
import Test.Hspec

import qualified Data.ByteString as ByteString

spec :: Spec
spec = it "encode/decode works" encodeDecode

encodeDecode :: IO ()
encodeDecode = do
  bytes <- ByteString.readFile "test/files/lorem-1K.txt"
  enc bytes >>= \case
    s1 ::: s2 ::: s3 ::: Nil -> do
      decode (s1 ::: s2 ::: Nil) `shouldBe` bytes
      decode (s2 ::: s3 ::: Nil) `shouldBe` bytes
      decode (s1 ::: s3 ::: Nil) `shouldBe` bytes
 where
  enc :: ByteString -> IO (V 3 (Share 2))
  enc = encode
