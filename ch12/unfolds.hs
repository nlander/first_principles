import Data.List
import Data.Maybe

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n+x) xs)

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
  where go :: [a] -> [[a]] -> [a]
        go xs' [] = xs'
        go xs' (x:xs) = (go (xs' ++ x) xs)

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
    Nothing       -> []
    Just (a1, b1) -> a1 : myUnfoldr f b1

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\x -> Just (x, f x)) a

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
  Nothing         -> Leaf
  Just (a1,b1,a2) -> Node (unfold f a1) b1 (unfold f a2)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold numCase 0 where
  numCase x = case x == n of
                True  -> Nothing
                False -> Just (x+1,x,x+1)
