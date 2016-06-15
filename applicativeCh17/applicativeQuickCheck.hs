import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

take' :: Int -> List a -> List a
take' 0 _                     = Nil
take' _ Nil                   = Nil
take' n l@(Cons x xs) | n < 0 = take' (negate n) l
                      | n > 0 = Cons x $ take' (n - 1) xs

repeat' :: a -> List a
repeat' x = Cons x $ repeat' x

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat'
  ZipList' (Cons f funcs) <*> ZipList' (Cons v vals) =
    let (ZipList' l) = ZipList' funcs <*> ZipList' vals
    in ZipList' $ Cons (f v) l
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' Nil <*> _ = ZipList' Nil

zipListTake :: Int -> ZipList a -> ZipList a
zipListTake x (ZipList xs) = ZipList (take x xs)

data Sum' a b =
    Firs a
  | Second b
  deriving (Eq, Show)

sumFromEither :: Either a b -> Sum' a b
sumFromEither (Left a) = Firs a
sumFromEither (Right b) = Second b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum' a b) where
  arbitrary = sumFromEither <$> arbitrary

instance (Eq a, Eq b) => EqProp (Sum' a b) where (=-=) = eq

data Validation e a =
    Error e
  | Succes a
  deriving (Eq, Show)

validationFromEither :: Either a b -> Validation a b
validationFromEither (Left a) = Error a
validationFromEither (Right b) = Succes b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = validationFromEither <$> arbitrary

instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

instance Functor (Sum' a) where
  fmap _ (Firs a) = Firs a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum' a) where
  pure = Second
  (Firs x) <*> _ = Firs x
  _ <*> Firs x   = Firs x
  (Second f) <*> x = fmap f x

instance Functor (Validation e) where
  fmap _ (Error e) = Error e
  fmap f (Succes a) = Succes $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Succes
  (Error e1) <*> (Error e2) = Error $ mappend e1 e2
  (Error e) <*> _           = Error e
  (Succes f) <*> x         = fmap f x

--1
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> x = f <$> x

--2
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

--3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two x f) <*> (Two y a) = Two (mappend x y) (f a)

--4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
                => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three x1 y1 f) <*> (Three x2 y2 a) = Three (mappend x1 x2)
                                              (mappend y1 y2)
                                              (f a)

--5
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
                => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' x f g) <*> (Three' y a b) = Three' (mappend x y) (f a) (g b)

--6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
                => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four x1 y1 z1 f) <*> (Four x2 y2 z2 a) =
    Four (mappend x1 x2) (mappend y1 y2) (mappend z1 z2) (f a)

--7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
                => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' x1 y1 z1 f) <*> (Four' x2 y2 z2 a) =
    Four' (mappend x1 x2) (mappend y1 y2) (mappend z1 z2) (f a)

--Combinations
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
