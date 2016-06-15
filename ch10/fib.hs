fibs    = takeWhile (<100) fiblist
  where
    fiblist = 1 : scanl (+) 1 fiblist

fibsN x = fibs !! x

facts   = take 20 factList
  where
    factList = scanl (*) 1 [1..]
