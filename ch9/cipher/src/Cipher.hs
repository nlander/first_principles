module Cipher
    ( cipher
    , caesar
    , unCaesar
    , vigenere
    , unVigenere
    ) where

import Data.Char
import Control.Monad (forever)
import System.Exit (exitSuccess)

data Cipher = Caesar | Vigenere
data EncodeingDirection = Encode | Decode
type Keyword = String

cipher :: IO ()
cipher = do
    putStrLn "Welcome to the cipher!"
    putStrLn "Would you like a caesar (c) or vigenere (v)?"
    forever $ do
      cipher_ <- getLine
      case cipher_ of
       ['c'] -> runCaesar
       ['v'] -> runVigenere
       _   -> putStrLn "Please enter c or v"
  where
    runCaesar = runCipher Caesar
    runVigenere = runCipher Vigenere

runCipher :: Cipher -> IO ()
runCipher c = do
    putStrLn "Hast thou a message to encode (e) or decode (d)?"
    forever $ do
      direction <- getLine
      case direction of
       ['e'] -> let f = encodingFunction c Encode in
                runEncoding f
       ['d'] -> let f = encodingFunction c Decode in
                runEncoding f
       _   -> putStrLn "Please enter e or d"
  where encodingFunction c e = case c of
          Caesar -> case e of
            Encode -> caesar
            Decode -> unCaesar
          Vigenere -> case e of
            Encode -> vigenere
            Decode -> unVigenere

runEncoding :: (String -> String) -> IO ()
runEncoding f = do
  putStrLn "Please enter the message:"
  message <- getLine
  putStrLn "Your transformed message is:"
  putStrLn $ f message
  exitSuccess

shift :: Int -> String -> String
shift enc = map $ encriptChr enc

encriptChr :: Int -> Char -> Char
encriptChr encrpt char
       | isAsciiUpper char = chr $ caseApply 65 encrpt char
       | isAsciiLower char = chr $ caseApply 97 encrpt char
       | otherwise    = char
  where
    caseApply caseAlt encription char =
      (+) caseAlt $ mod26Add encription $ ord char - caseAlt
    mod26Add x y = (x + y) `mod` 26

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
vigenere :: String -> String
vigenere  = keywordShift "ally"

unVigenere :: String -> String
unVigenere = keywordUnshift "ally"

caesar :: String -> String
caesar = shift 5

unCaesar :: String -> String
unCaesar = shift (-5)
