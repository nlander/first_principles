{-

1. length :: Integral b => [a] -> b
- actually - length :: Foldable t => t a -> Int

2. a) 5
   b) 3
   c) 2
   d) 5
-}

--8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

--9
myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

--10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

--reading syntax
--1
x = (+)

fun xs = w `x` 1
    where w = length xs

id' = (\x -> x)

head' = (\(x : xs) -> x)

fst' (a, b) = a

{-function name to type

1. c) show :: Show a => a -> String

2. b) (==) :: Eq a => a -> a -> Bool

3. a) fst :: (a, b) -> a

4. d) (+) :: Num a => a -> a -> a
-}

type Name = String

data Pet = Cat | Dog Name deriving Show
