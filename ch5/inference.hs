--inference.hs
module Inference where

--f :: Num a => a -> a -> a
f x y = x + y + 3

--myConcat :: [Char] -> [Char] 
myConcat x = x ++ " yo"

--myMult :: Fractional x => x -> x
myMult x = (x / 3) * 5

--myTake :: Int -> [Char]
myTake x = take x "hey you"

--myCom :: Int -> Bool
myCom x = x > (length [1..10])

--myAlph :: Char -> Bool
myAlph x = x < 'z'
