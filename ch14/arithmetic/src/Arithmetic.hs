module Arithmetic where

import Data.Char

half :: Float -> Float
half x = x / 2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go y (Nothing, t) = (Just y, t)
        go y (Just x,  t) = (Just y, x>=y)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative :: Int -> Int -> Bool
multCommutative x y =
  x * y == y * x

quotRemLaw :: Integral a => a ->  a -> Bool
quotRemLaw x y = (quot x y)*y + (rem x y) == x

divModLaw :: Integral a => a -> a ->  Bool
divModLaw x y = (div x y)*y + (mod x y) == x

powerAssociative :: Int -> Int -> Int -> Bool
powerAssociative x y z =
  (x ^ y) ^ z == x ^ (y ^ z)

powerCommutative :: Int -> Int -> Bool
powerCommutative x y =
  x ^ y == y ^ x

listReversal :: [Integer] -> Bool
listReversal xs = (reverse . reverse) xs == id xs

someFunc :: IO ()
someFunc = putStrLn "someFunc"

funcApp :: (Int -> Int) -> Int -> Bool
funcApp f a = (f $ a) == f a

funcComp :: (Char -> Float) -> (Integer -> Char) -> Integer -> Bool
funcComp f g x = (f . g) x == f (g x)

foldConsIsAppend :: [Int] -> [Int] -> Bool
foldConsIsAppend xs ys = (flip (foldr (:))) xs ys == xs ++ ys

foldAppendIsConcat :: [[Int]] -> Bool
foldAppendIsConcat lists = foldr (++) [] lists == concat lists

lengthTake :: Int -> [Char] -> Bool
lengthTake n xs = case length xs >= n && n >= 0 of
                    True  -> length (take n xs) == n
                    False -> case n >= 0 of
                      True  -> take n xs == id xs
                      False -> take n xs == []

stringRoundTrip :: (Eq a, Read a, Show a) => a -> Bool
stringRoundTrip r = (read (show r)) == r

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs
