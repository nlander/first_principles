import Data.Monoid
--1
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

--2
product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

--3
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' val vals = getAny $ foldMap (Any . (==val)) vals

--4
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' vals
          | (not . null') vals = Just $ foldr min acc vals
          | otherwise         = Nothing
          where acc = head . toList' $ vals

--5
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' vals
          | (not . null') vals = Just $ foldr max acc vals
          | otherwise         = Nothing
          where acc = head . toList' $ vals

--6
null' :: (Foldable t) => t a -> Bool
null' vals = not . getAny $ foldMap (const (Any True)) vals

--7
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ x -> x + 1) 0

--8
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

--9
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldr mappend mempty

fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' = foldMap id

--10
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\val acc -> mappend (f val) acc) mempty
