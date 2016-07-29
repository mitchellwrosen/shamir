{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.DeepSeq
import Control.Exception
import Data.Time.Clock
import Shamir.Secret (V(..))

import qualified Crypto.SecretSharing.Internal as SecretSharing
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Lazy          as LByteString
import qualified Shamir.Secret                 as Shamir

main :: IO ()
main = do
  bytes <- ByteString.readFile "test/files/lorem-1000K.txt"
  forceIO bytes

  let len = ByteString.length bytes

  putStrLn "----------"

  putStr "[shamir]         "
  (t0, s1 ::: s2 ::: _) <- timed (Shamir.encode bytes :: IO (V 3 (Shamir.Share 2)))
  results "encoded " len t0

  putStr "[shamir]         "
  (t1, _) <- timed (pure (Shamir.decode (s1 ::: s2 ::: Nil)))
  results "decoded " len t1

  putStrLn "----------"

  let bytesL = LByteString.fromStrict bytes
  forceIO bytesL

  putStr "[secret-sharing] "
  (t2, s3:s4:_) <- timed (SecretSharing.encode 2 3 bytesL)
  results "encoded " len t2

  putStr "[secret-sharing] "
  (t3, _) <- timed (pure (SecretSharing.decode [s3,s4]))
  results "encoded " len t3

  putStrLn "----------"

timed :: NFData a => IO a -> IO (NominalDiffTime, a)
timed act = do
  t0 <- getCurrentTime
  x <- act
  forceIO x
  t1 <- getCurrentTime
  pure (t1 `diffUTCTime` t0, x)

results :: String -> Int -> NominalDiffTime -> IO ()
results typ len t = do
  putStrLn $
    typ
    ++ show len
    ++ " bytes in "
    ++ show t
    ++ " seconds ("
    ++ show (round (fromIntegral len / t) :: Integer)
    ++ " bytes/second)"

forceIO :: NFData a => a -> IO ()
forceIO = evaluate . rnf

instance NFData SecretSharing.ByteShare
instance NFData SecretSharing.Share
