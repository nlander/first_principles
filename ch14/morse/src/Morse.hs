module Morse
    ( Morse
    , charToMorse
    , morseToChar
    , stringToMorse
    , letterToMorse
    , morseToLetter
    ) where

import qualified Data.Map as M

type Morse = String

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList list where
  list = zip (['a'..'z'] ++ ['0'..'9']) dotsAndDashes
  dotsAndDashes = [
        ".-"
      , "-..."
      , "-.-."
      , "-.."
      , "."
      , "..-."
      , "--."
      , "...."
      , ".."
      , ".---"
      , "-.-"
      , ".-.."
      , "--"
      , "-."
      , "---"
      , ".--."
      , "--.-"
      , ".-."
      , "..."
      , "-"
      , "..-"
      , "...-"
      , ".--"
      , "-..-"
      , "-.--"
      , "--.."
      , "-----"
      , ".----"
      , "..---"
      , "...--"
      , "....-"
      , "....."
      , "-...."
      , "--..."
      , "---.."
      , "----."
    ]
