length' :: [a] -> Integer
length' [] = 0
length' (_:xs) = 1 + (fromIntegral . length $ xs)

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []    = []
myFilter pred (x:xs)
  | pred x                = x : filter pred xs
  | otherwise             = filter pred xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
