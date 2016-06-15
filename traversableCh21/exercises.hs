{-# LANGUAGE FlexibleContexts #-}
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--Identity
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldr f acc (Identity a) = f a acc

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

--Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Functor (Constant a) where
  fmap _ (Constant a) = (Constant a)

instance Foldable (Constant a) where
  foldr _ acc _ = acc

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

--Maybe
data Optional a =
    Nada
  | Yep a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(4, Yep <$> arbitrary), (1, return Nada)]

instance Eq a => EqProp (Optional a) where (=-=) = eq

instance Functor (Optional) where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable (Optional) where
  foldr _ acc Nada = acc
  foldr f acc (Yep a) = f a acc

instance Traversable (Optional) where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

--List
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil)
                       , (4, sized $ \n -> do
                               k <- choose (0, n)
                               traverse (\_ -> arbitrary) 
                                        (makeIntList k Nil))]
                where
                  makeIntList :: Int -> List Int -> List Int
                  makeIntList 0 list = list
                  makeIntList n list
                              | n < 0 = makeIntList (negate n) list
                              | n > 0 = makeIntList (n-1) $ Cons n list

instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable (List) where
  foldr _ acc Nil = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)

instance Traversable (List) where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

--Three
data Three a b c =
  Three a b c deriving (Eq, Show)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
           Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldr f acc (Three _ _ c) = f c acc

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

--Three'
data Three' a b =
  Three' a b b deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Foldable (Three' a) where
  foldr f acc (Three' a b1 b2) = f b1 $ f b2 acc

instance Traversable (Three' a) where
  traverse f (Three' a b1 b2) = Three' a <$> f b1 <*> f b2

--S (the one that'll SUCK!! lol :P )
data S n a = S (n a) a deriving (Eq, Show)

instance (Eq a, Eq (n a)) => EqProp (S n a) where (=-=) = eq

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
  foldr f acc (S n a) = foldr f (f a acc) n

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

--Tree
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Eq a => EqProp (Tree a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency [(1, return Empty)
                       , (1, Leaf <$> arbitrary)
                       , (4, do
                            k <- choose (0, 4)
                            traverse (\_ -> arbitrary) 
                                     (makeIntTree k
                                        (Node (Leaf 0) 9 (Leaf 0))))]
                where
                  makeIntTree :: Int -> Tree Int -> Tree Int
                  makeIntTree 0 tree = tree
                  makeIntTree n tree
                              | n < 0 = makeIntTree (negate n) tree
                              | n > 0 = makeIntTree (n-1) $
                                            Node tree n tree

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node tree1 a tree2) = Node (fmap f tree1)
                                     (f a)
                                     (fmap f tree2)

instance Foldable Tree where
  foldr _ acc Empty = acc
  foldr f acc (Leaf a) = f a acc
  foldr f acc (Node tree1 a tree2) = foldr f
                                       (f a (foldr f acc tree2))
                                       tree1

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node tree1 a tree2) = Node <$> traverse f tree1
                                         <*> f a
                                         <*> traverse f tree2

type TI = Tree

main = do
  let trigger = undefined :: TI (Int, Int, [Int])
  quickBatch (traversable trigger)
