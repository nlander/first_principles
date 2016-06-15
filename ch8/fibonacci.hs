{-# LANGUAGE FlexibleInstances #-}

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = f 1 + f 2
   where f = \a -> fibonacci (x - a)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

data DividedResult a =
    Result a
  | DividedByZero
  deriving (Eq, Show)

instance Num a => Num (DividedResult a) where
  (+) (Result a) (Result b) = Result $ (+) a b
  (+) DividedByZero       _ = DividedByZero
  (+) _       DividedByZero = DividedByZero
  (*) (Result a) (Result b) = Result $ (*) a b
  (*) DividedByZero       _ = DividedByZero
  (*) _       DividedByZero = DividedByZero
  (-) (Result a) (Result b) = Result $ (-) a b
  (-) DividedByZero       _ = DividedByZero
  (-) _       DividedByZero = DividedByZero
  abs (Result a)            = Result $ abs a
  abs DividedByZero         = DividedByZero
  signum (Result a)         = Result $ signum a
  signum DividedByZero      = DividedByZero
  fromInteger a             = Result $ fromInteger a

instance Num a => Num (a, a)
    where
  (+) (a, b) (c, d) = (a + c, b + d)
  (-) (a, b) (c, d) = (a - c, b - d)
  (*) (a, b) (c, d) = (a * c, b * d)
  abs (a, b) = (abs a, abs b)
  signum (a, b)     = (signum a, signum b)
  fromInteger a     = (fromInteger a, fromInteger a)

dividedBy :: Integral a =>
        a -> a -> (DividedResult a, DividedResult a)
dividedBy _ 0 = (DividedByZero, DividedByZero)
dividedBy num denom = case sign num == sign denom of
   True  -> go (abs num) (abs denom) 0
   False -> negate . go (abs num) (abs denom) $ 0
  where go n   d        count
         | n < d     = (Result count,
                            Result  n)
         | otherwise = go (n - d) d (count + 1)
        sign n
         | n < 0     = (-1)
         | n > 0     = 1
         | otherwise = 0

--Recursion exercise 2.

sumTo :: (Eq a, Num a) => a -> a
sumTo = \a -> go a 1
  where go top count
         | top == count = top
         | otherwise    = count + go top (count + 1)

mult :: (Eq a, Num a, Ord a) => a -> a -> a
mult _ 0 = 0
mult a b = case b > 0 of
   True  -> a + mult a (b - 1)
   False -> c + mult c (d - 1)
     where c = negate a
           d = negate b
