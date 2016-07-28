module Main where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-XBangPatterns"
  , "-XDataKinds"
  , "-XGADTs"
  , "-XKindSignatures"
  , "-XMagicHash"
  , "-XOverloadedStrings"
  , "-XScopedTypeVariables"
  , "-XTypeOperators"
  , "-XViewPatterns"
  , "src"
  ]
