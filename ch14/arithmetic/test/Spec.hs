module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List
import Arithmetic


main :: IO ()
main = do
--  putStrLn "half times two is id"
--  quickCheck prop_halfIdentity
--  putStrLn "sorted lists are ordered"
--  quickCheck prop_listOrdered
--  putStrLn "addition is associative"
--  quickCheck plusAssociative
--  putStrLn "addition is commutative"
--  quickCheck plusCommutative
--  putStrLn "multiplication is associative"
--  quickCheck multAssociative
--  putStrLn "multiplication is commutative"
--  quickCheck multCommutative
--  putStrLn "quotRemLaw for dividing by negative Integers"
--  quickCheck prop_quotRemLaw
--  putStrLn "quotRemLaw for dividing by positive Ints"
--  quickCheck prop_quotRemLaw'
--  putStrLn "divModLaw for dividing by negative Ints"
--  quickCheck prop_divModLaw
--  putStrLn "divModLaw for dividing by positive Integers"
--  quickCheck prop_divModLaw'
--  putStrLn "associativity of exponentiation"
--  quickCheck prop_powerAssociative
--  putStrLn "commutativity of exponentiation"
--  quickCheck prop_powerCommutative
--  putStrLn "reverse twice gets the same back"
--  quickCheck prop_listOrdered
--  putStrLn "$ is the same as function application"
--  quickCheck prop_funcApp
--  putStrLn "  . is the same as function composition"
--  quickCheck prop_funcComp
--  putStrLn "  flip foldr cons is list append"
--  quickCheck prop_foldConsIsAppend
--  putStrLn "  foldr append empty list is concat"
--  quickCheck prop_foldAppendIsConcat
--  putStrLn "  if n is between the list length and zero,"
--  putStrLn "  then the length of take n list is n."
--  putStrLn "  take large n is the id function."
--  putStrLn "  take negative n returns empty list."
--  quickCheck prop_lengthTake
  putStrLn "  read undoes show."
  quickCheck prop_stringRoundTrip

prop_powerAssociative :: Property
prop_powerAssociative =
  forAll (elements positiveInts)
  (\x -> forAll (elements positiveInts)
         (\y -> forAll (elements positiveInts)
                (\z -> powerAssociative x y z)))

prop_powerCommutative :: Property
prop_powerCommutative =
  forAll (elements positiveInts)
  (\x -> forAll (elements positiveInts)
         (\y -> powerCommutative x y))

prop_listOrdered :: Property
prop_listOrdered =
  forAll (arbitrary :: Gen [Int])
  (\l -> listOrdered $ sort l)

prop_halfIdentity :: Float -> Bool
prop_halfIdentity x = halfIdentity x == id x

positiveInts :: [Int]
positiveInts = [1..1000]

negativeInts :: [Int]
negativeInts = map negate positiveInts

positiveIntegers :: [Integer]
positiveIntegers = [1..1000]

negativeIntegers :: [Integer]
negativeIntegers = map negate positiveIntegers

prop_quotRemLaw :: Property
prop_quotRemLaw =
  forAll (arbitrary :: Gen Integer)
  (\x -> forAll (elements negativeIntegers)
         (\y -> quotRemLaw x y))

prop_quotRemLaw' :: Property
prop_quotRemLaw' =
  forAll (arbitrary :: Gen Int)
  (\x -> forAll (elements positiveInts)
         (\y -> quotRemLaw x y))

prop_divModLaw :: Property
prop_divModLaw =
  forAll (arbitrary :: Gen Int)
  (\x -> forAll (elements negativeInts)
         (\y -> divModLaw x y))

prop_divModLaw' :: Property
prop_divModLaw' =
  forAll (arbitrary :: Gen Integer)
  (\x -> forAll (elements positiveIntegers)
         (\y -> divModLaw x y))

halfIdentity :: Float -> Float
halfIdentity = (*2) . half

prop_funcApp :: Property
prop_funcApp =
  forAll (arbitrary :: Gen (Fun Int Int))
  (\(Fun _ f) -> forAll (arbitrary :: Gen Int)
                 (\a -> funcApp f a))

prop_funcComp :: Property
prop_funcComp =
  forAll (arbitrary :: Gen (Fun Char Float))
  (\(Fun _ f) -> forAll (arbitrary :: Gen (Fun Integer Char))
                 (\(Fun _ g) -> forAll (arbitrary :: Gen Integer)
                                (\x -> funcComp f g x)))

prop_foldConsIsAppend :: Property
prop_foldConsIsAppend =
  forAll (arbitrary :: Gen [Int])
  (\xs -> forAll (arbitrary :: Gen [Int])
          (\ys -> foldConsIsAppend xs ys))

prop_foldAppendIsConcat :: Property
prop_foldAppendIsConcat =
  forAll (arbitrary :: Gen [[Int]])
  (\lists -> foldAppendIsConcat lists)

prop_lengthTake :: Property
prop_lengthTake =
  forAll (arbitrary :: Gen Int)
  (\x -> forAll (arbitrary :: Gen [Char])
         (\chars -> lengthTake x chars))

prop_stringRoundTrip :: Property
prop_stringRoundTrip =
  forAll (arbitrary :: Gen Int)
  (\r -> stringRoundTrip r)

capitalizeWordIdempotence :: String -> Bool
capitalizeWordIdempotence x = capitalizeWord x
                              == twice capitalizeWord x
                              == fourTimes capitalizeWord x
