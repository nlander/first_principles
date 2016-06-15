{-# LANGUAGE NoMonomorphismRestriction #-}
{-
Intermission exercises

a) c) not :: Bool -> Bool
b) d) length :: [a] -> Int
c) b) concat :: [[a]] -> [a]
d) a) head :: [a] -> a
e) e) (<) :: Ord a => a -> a -> Bool

Intermission Currying
1. a) if f :: a -> a -> a -> a and x :: Char then
f x :: Char -> Char -> Char

2. d) if g :: a -> b -> c -> b then
g 0 'c' "woot" :: Char

3. d) if h :: (Num a, Num b) => a -> b -> b then
h 1.0 2 :: Num b => b

4. c) h 1 (5.5 :: Double) :: Double

5. a) If jackal :: (Ord a, Eq b) => a -> b -> a then
jackal "blah" "blahblah" :: String

6. e) jackal "keyboard" :: Eq b => b -> [Char]

7. d) if kessel :: (Ord a, Num b) => a -> b -> a then
kessel 1 2 :: (Num a, Ord a) => a

8. a) kessel 1 (2 :: Integer) :: (Num a, Ord a) => a

9. c) kessel (1 :: Integer) 2 :: Integer

Multiple choice
1. c)
2. a)
3. b)
4. c)

Determine the type
1.
-}
-- a :: Num a => a
a = (* 9) 6 
-- :: Num a => a
b = head [(0,"doge"),(1,"kitteh")] -- :: Num a => (a,[Char])
c = head [(0 :: Integer ,"doge"),(1,"kitteh")] -- :: (Integer,[Char])
d = if False then True else False -- :: Bool
e = length [1, 2, 3, 4, 5] -- :: Int
f = (length [1, 2, 3, 4]) > (length "TACOCAT") -- :: Bool

-- 2, 3, 4
x = 5
y = x + 5
w = y * 10 -- :: Num a => a
z y = y * 10 -- :: Num a => a -> a
f2 = 4 / y -- :: Fractional a => a

-- 5
x1 = "Julie"
y1 = " <3 "
z1 = "Haskell"
f3 = x1 ++ y1 ++ z1 -- :: [Char]

-- Does it compile?

-- 1
bigNum = (^) 5 $ 10
wahoo = bigNum + 10
bigNum2 = (^) 5
wahoo2 = bigNum2 $ 10

-- 2
x2 = print
y2 = print "woohoo!"
z2 = x2 "hello world"

-- 3

aa = (+)
bb = 5
cc = aa 10
dd = cc 200

-- 4

aa1 = 12 + bb1
bb1 = 10000 * 5

-- type variable or specific type constructor?

-- 2 f :: zed -> Zed -> Blah
-- fully polymorphic, concrete, concrete

-- 3 f :: Enum b => a -> b -> C
-- fully polymorphic, constrained polymorphic, concrete

-- 4 f :: f -> g -> C
-- fully polymorphic, fully polymorphic, concrete

-- Write a type signature
-- 1
functionH :: [a] -> a
functionH (x:_) = x

-- 2
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- 3
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
-- 1
i :: a -> a
i a = a

-- 2
ctype :: a -> b -> a
ctype a _ = a

-- 3 yes same as 2

-- 4
ctyp :: a -> b -> b
ctyp _ b = b

-- 5
r :: [a] -> [a]
r (_:xs) = xs

-- 6
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g = \x -> f (g x)

-- 7
a7 :: (a -> c) -> a -> a
a7 _ a = a

-- 8
a8 :: (a -> b) -> a -> b
a8 f a = f a
