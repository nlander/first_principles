--poemLines.hs
module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

breakOn :: Char -> String -> [String]
breakOn _ []   = []
breakOn c text = firstLine : breakOn c theRest 
  where
    firstLine = takeWhile (/= c) text
    theRest   = dropWhile (== c) $ dropWhile (/= c) text

myLines = breakOn '\n'

myWords = breakOn ' '

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO()
main =
  print $ "Are they equal? "
          ++ show (myLines sentences == shouldEqual)
