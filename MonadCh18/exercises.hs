module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

--2
data PhhhbbtttEither b a =
    PhbtLeft a
  | PhbtRight b
  deriving (Eq, Show)

instance (Arbitrary b, Arbitrary a) =>
  Arbitrary (PhhhbbtttEither b a) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      elements [PhbtLeft a, PhbtRight b]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

instance Functor (PhhhbbtttEither b) where
  fmap _ (PhbtRight b) = PhbtRight b
  fmap f (PhbtLeft a)  = PhbtLeft $ f a

instance Applicative (PhhhbbtttEither b) where
  pure = PhbtLeft
  PhbtRight a <*> _ = PhbtRight a
  PhbtLeft f <*> x  = fmap f x

instance Monad (PhhhbbtttEither b) where
  return = pure
  PhbtRight a >>= _ = PhbtRight a
  PhbtLeft b  >>= f = f b

--3
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> x = fmap f x

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

--4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fromList <$> arbitrary

instance Eq a => EqProp (List a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = take' 3000 xs
          ys' = take' 3000 ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons h t) = Cons (f h) $ fmap f t

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f funcs) <*> vals = append
                            (fmap f vals)
                            (funcs <*> vals)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons a vals) >>= f = append
                        (f a)
                        (vals >>= f)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

take' :: Int -> List a -> List a
take' 0 _                     = Nil
take' _ Nil                   = Nil
take' n l@(Cons x xs) | n < 0 = take' (negate n) l
                      | n > 0 = Cons x $ take' (n - 1) xs


main = do
  let triggerNope = undefined :: Nope (Int, String, Int)
      triggerPhbt = undefined :: PhhhbbtttEither Char (Int, String, Int)
      triggerIden = undefined :: Identity (Int, String, Int)
      triggerList = undefined :: List (Int, String, Int)
  quickBatch $ functor triggerNope
  quickBatch $ applicative triggerNope
  quickBatch $ monad triggerNope
  quickBatch $ functor triggerPhbt
  quickBatch $ applicative triggerPhbt
  quickBatch $ monad triggerPhbt
  quickBatch $ functor triggerIden
  quickBatch $ applicative triggerIden
  quickBatch $ monad triggerIden
  quickBatch $ functor triggerList
  quickBatch $ applicative triggerList
  quickBatch $ monad triggerList
