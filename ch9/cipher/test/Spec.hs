module Main where

import Cipher (caesar
             , unCaesar
             , vigenere
             , unVigenere)
import Test.QuickCheck

prop_caesar :: Property
prop_caesar =
  forAll (arbitrary :: Gen String)
    (\word -> word == (unCaesar . caesar) word)

prop_vigenere :: Property
prop_vigenere =
  forAll (arbitrary :: Gen String)
    (\word -> word == (unVigenere . vigenere) word)

main :: IO ()
main = do
  quickCheck prop_caesar
  quickCheck prop_vigenere
