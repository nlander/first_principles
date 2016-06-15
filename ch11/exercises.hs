import Data.Char

fexample :: Show a => (a, b) -> IO (a, b)
fexample t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf sub seq@(_:ts) = let l = length sub in
                           case sub == take l seq of
                             True  -> True
                             False -> isSubsequenceOf sub ts

capitalizeWords :: String -> [(String, String)]
capitalizeWords "" = []
capitalizeWords msg@(c:cs) = let word = takeWhile (/= ' ') msg in
    case c of
  ' ' -> capitalizeWords cs
  otherwise -> (word , capitalizeWord word) : capitalizeWords
                                         (dropWhile (/= ' ') msg)

capitalizeWords' :: String -> [(String, String)]
capitalizeWords' msg = reverse $ go [] $ words' msg
  where
    go acc [] = acc
    go acc (firstWord:restOfWords) =
         go ((firstWord, capitalizeWord firstWord) : acc) restOfWords

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = (toUpper c) : cs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph message = concat $ map capitalizeWord $ sentences' message

words' :: String -> [String]
words' "" = []
words' message = go [] "" (reverse message)
    where
      go listAcc wordAcc ""  = wordAcc : listAcc
      go listAcc wordAcc (x:xs) = case x of
        ' '       -> go (wordAcc : listAcc) "" xs
        otherwise -> go listAcc (x : wordAcc) xs

sentences' :: String -> [String]
sentences' "" = []
sentences' message = reverse $ go [] message
        where
  go acc ""  = acc
  go acc msg = go (punc : sen : acc) rem
    where
      punc = takeWhile (not . isLetter) $ dropWhile (/= '.') msg
      sen  = takeWhile (/= '.') msg
      rem  = dropWhile (not . isLetter) $ dropWhile (/= '.') msg
