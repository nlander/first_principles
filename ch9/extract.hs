myWords :: [Char] -> [[Char]]
myWords "" = []
myWords words = wordFrom words : myWords theRest
    where
        wordFrom = takeWhile (/= ' ')
        theRest  = dropWhile (== ' ') $ dropWhile (/= ' ') words
