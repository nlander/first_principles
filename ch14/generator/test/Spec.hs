module Main where

import Generator
import Test.QuickCheck

equalChances :: Gen Fool
equalChances = elements [Fulse, Frue]

skewedChances :: Gen Fool
skewedChances = frequency [ (2, return Fulse)
                          , (1, return Frue) ]

main :: IO ()
main = do
  sample equalChances
  sample skewedChances
