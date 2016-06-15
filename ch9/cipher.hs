module Cipher where

import Data.Char

shift :: Int -> String -> String
shift enc = map $ encriptChr enc

encriptChr :: Int -> Char -> Char
encriptChr encrpt char
       | isUpper char = chr $ caseApply 65 encrpt char
       | isLower char = chr $ caseApply 97 encrpt char
       | otherwise    = char
  where
    caseApply caseAlt encription char =
      (+) caseAlt $ mod26Add encription $ ord char - caseAlt
    mod26Add x y = (x + y) `mod` 26

type Keyword = String

keywordShift :: Keyword -> String -> String
keywordShift key msg = zipWith encriptChr shifts msg
  where
    shifts     = map (getShift . toUpper) spacedKey
    spacedKey  = (putInSpaces "" msg (concat (repeat key)))
    getShift c = case isUpper c of
        True  -> ord c - ord 'A'
        False -> 0

keywordUnshift :: Keyword -> String -> String
keywordUnshift key msg = zipWith encriptChr shifts msg
  where
    shifts     = map (negate . getShift . toUpper) spacedKey
    spacedKey  = (putInSpaces "" msg (concat (repeat key)))
    getShift c = case isUpper c of
        True  -> ord c - ord 'A'
        False -> 0

putInSpaces :: String -> String -> String -> String
putInSpaces acc hasSpaces needsSpaces = reverse $
                      putInSpacesHelper acc hasSpaces needsSpaces

putInSpacesHelper acc [] _ = acc
putInSpacesHelper acc (c:hasSpaces) (d:needsSpaces) = case c of
  ' ' -> putInSpacesHelper (' ':acc) hasSpaces (d:needsSpaces)
  otherwise -> putInSpacesHelper (d:acc) hasSpaces needsSpaces

--ch11
vigenere  = keywordShift "ally"

unVigenere = keywordUnshift "ally"

caesar = shift 5

unCaesar = shift (-5)
