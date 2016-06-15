import Data.Char

pullUppers :: String -> String
pullUppers []     = []
pullUppers (x:xs)
      | isUpper x = x : pullUppers xs
      | otherwise = pullUppers xs

capitalize ""     = ""
capitalize (x:xs) = toUpper x : xs

allCaps ""     = ""
allCaps (x:xs) = toUpper x : allCaps xs

capHead = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (b:bs) = b || myOr bs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElemRec :: Eq a => a -> [a] -> Bool
myElemRec _ [] = False
myElemRec a (x:xs) = a == x || myElemRec a xs

myElem :: Eq a => a -> [a] -> Bool
myElem a = any (== a)

myReverse :: [a] -> [a]
myReverse xs = go xs []
  where
    go [] xs = xs
    go (y:ys) xs = go ys (y:xs)

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = concat $ go f xs
  where
    go _ [] = []
    go f (x:xs) = f x : go f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

choose True  a _ = a
choose False _ b = b

myUltimumBy :: Bool -> (a -> a -> Ordering) -> [a] -> a
myUltimumBy _    _    (a:[])   = a
myUltimumBy crit comp (a:b:[]) = case comp a b of
                                    GT -> choose crit a b
                                    LT -> choose crit b a
                                    EQ -> a
myUltimumBy crit comp (a:b:xs) = case comp a b of
             GT -> myUltimumBy crit comp ((choose crit a b):xs)
             LT -> myUltimumBy crit comp ((choose crit b a):xs)
             EQ -> myUltimumBy crit comp (a:xs)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myUltimumBy True

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myUltimumBy False

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
