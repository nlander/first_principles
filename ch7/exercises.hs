{-# LANGUAGE NoMonomorphismRestriction #-}
-- Intermission
--1.
mTh = \x -> \y -> \z -> x * y * z

--2. d)
mTh3 :: Num a => a -> a -> a
mTh3 = mTh 3

--3. a )
addOne = \n -> n + 1
--b)
addFive = \x y -> (if x > y then y else x) + 5
--c)
mflip f x y = f y x

--Intermission 2
--1.
k :: (a, b) -> a
k (x, y) = x
k1 :: Num a => a
k1 = k ((4 - 1), 10)
k2 :: String
k2 = k ("three", (1 + 2))
k3 :: Num a => a
k3 = k (3, True)

fTrip :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
fTrip (a, _, c) (d, _, f) = ((a, d), (c, f))

funcZ x =
   case x + 1 == 1 of
      True -> "AWESOME"
      False -> "wut"

pal xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs

--Intermission : case expressions
--1.
functionC x y =
  case greater of
    True -> x
    False -> y
  where greater = x > y
--2
ifEvenAdd2 n =
  case even n of
    True  -> (n+2)
    False -> n
--1 again
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- let's write code
tensDigit x = (snd . d) . (fst . d) $ x
   where d = flip divMod 10

hundredsDigit x = (snd . d) . (fst . d) . (fst . d) $ x
   where d = flip divMod 10

--2
foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
   True -> x
   False -> y

foldBool' x y b
   | b         = x
   | otherwise = y

--3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, b) = (f a, b)
