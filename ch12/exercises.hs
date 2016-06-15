import Data.Char

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe text  = Just text

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = go 0 $ words text where
  go acc [] = acc
  go acc (_:[]) = acc
  go acc (word:wds) = let v = vowelStart $ head wds in
                                case notThe word of
                              Nothing -> go (acc + v) wds
                              Just _  -> go acc wds

vowelStart :: String -> Integer
vowelStart "" = 0
vowelStart (x:_) = if vowel x then 1 else 0

replaceThe :: String -> String
replaceThe text = unwords . reverse . go [] $ words text where
  go acc [] = acc
  go acc (word:wds) = case notThe word of
                      Nothing -> go ("a" : acc) wds
                      Just word -> go (word : acc) wds

countVowels :: String -> Integer
countVowels text = go 0 text where
  go acc "" = acc
  go acc (c:cs) = if   vowel c
                  then go (1+acc) cs
                  else go acc cs

countConsonants :: String -> Integer
countConsonants text = go 0 text where
  go acc "" = acc
  go acc (c:cs) = if   consonant c
                  then go (1+acc) cs
                  else go acc cs

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"
consonants = "bcdfghjklmnpqrstvwxyz"

vowel :: Char -> Bool
vowel c = (toLower c) `elem` vowels

consonant :: Char -> Bool
consonant c = (toLower c) `elem` consonants

mkWord :: String -> Maybe Word'
mkWord word = if countConsonants word <= countVowels word
              then Nothing
              else Just (Word' word)

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ makeNat x where
      makeNat 0 = Zero
      makeNat y = Succ $ makeNat $ y - 1
