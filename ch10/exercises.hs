stops  = "pbtdkg"
vowels = "aeiou"

svs = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

pSyls = filter (\tup -> case tup of
                          ('p',_,_) -> True
                          (_,_,_)   -> False) svs

nouns = ["the pig", "the music", "your dad", "a spoon"]
verbs = ["plays", "eats", "loves", "modifies"]

sens = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

lettersToWordsRatio x =
  div (sum (map length (words x)))
      (length (words x))

lettersToWordsFraction x =
  (/) (fromIntegral . sum . map length . words $ x)
      (fromIntegral . length . words $ x)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> acc || f x) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = any (== a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> case f x of True -> x : acc; False -> acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\x acc -> case f x acc of GT -> x; LT -> acc; EQ -> acc) x (x:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\x acc -> case f x acc of LT -> x; GT -> acc; EQ -> acc) x (x:xs)
