{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                       (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

--1
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= (return . Identity)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

--2
data Pair a = Pair a a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

--3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

--4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
                => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

--5
data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b)
                => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

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

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

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

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)


main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Char -> Bool)
  quickCheck (functorCompose' :: Identity Char
                              -> Fun Char Char
                              -> Fun Char Char
                              -> Bool)
  quickCheck (functorIdentity :: Pair Char -> Bool)
  quickCheck (functorCompose' :: Pair Char
                              -> Fun Char Char
                              -> Fun Char Char
                              -> Bool)
  quickCheck (functorIdentity :: Two Int Char -> Bool)
  quickCheck (functorCompose' :: Two Int Char
                              -> Fun Char Char
                              -> Fun Char Char
                              -> Bool)
  quickCheck (functorIdentity :: Three Int Integer Char -> Bool)
  quickCheck (functorCompose' :: Three Int Integer Char
                              -> Fun Char Char
                              -> Fun Char Char
                              -> Bool)
  quickCheck (functorIdentity :: Three' Int Char -> Bool)
  quickCheck (functorCompose' :: Three' Int Char
                              -> Fun Char Char
                              -> Fun Char Char
                              -> Bool)
  quickCheck (functorIdentity :: Four Char Int Integer Char -> Bool)
  quickCheck (functorCompose' :: Four Char Int Integer Char
                              -> Fun Char Char
                              -> Fun Char Char
                              -> Bool)
  quickCheck (functorIdentity :: Four' Int Char -> Bool)
  quickCheck (functorCompose' :: Four' Int Char
                              -> Fun Char Char
                              -> Fun Char Char
                              -> Bool)
