module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    (-9) -> "negative nine"
    (-8) -> "negative eight"
    (-7) -> "negative seven"
    (-6) -> "negative six"
    (-5) -> "negative five"
    (-4) -> "negative four"
    (-3) -> "negative three"
    (-2) -> "negative two"
    (-1) -> "negative one"
    _ -> "not a digit"

digits :: Int -> [Int]
digits n = case n < 0 of
    True -> map negate . reverse . f $ (-n)
    False -> reverse . f $ n
  where
    f 0 = []
    f x = mod x 10 : f (div x 10)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n
