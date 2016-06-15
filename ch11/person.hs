module Jammin where

import Data.List

data Person =
  Person { name :: String
         , age  :: Int }
         deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Ord, Show)

data JamJars =
  Jam { kind :: Fruit
      , jars :: Int }
      deriving (Eq, Show)

instance Ord JamJars where
  compare (Jam k _) (Jam k' _) = compare k k'

row1 = [Jam Peach 3, Jam Plum 5]
row2 = [Jam Apple 7, Jam Plum 2, Jam Blackberry 8]
row3 = [Jam Peach 7, Jam Blackberry 6]
row4 = [Jam Plum 3, Jam Apple 2, Jam Peach 1]
row5 = [Jam Blackberry 1, Jam Apple 5]
row6 = [Jam Peach 4, Jam Apple 2, Jam Plum 7] 
allJam = [row1, row2, row3, row4, row5, row6]

totalJamJars :: [[JamJars]] -> Int
totalJamJars = sum . map jars . concat

mostRow :: [[JamJars]] -> [JamJars]
mostRow rows = rows !! index
  where
    index = snd . maximum . flip zip [0..] . map rowJars $ rows
    rowJars = sum . map jars

groupJam :: [[JamJars]] -> [[JamJars]]
groupJam = groupBy (\x y -> compare x y == EQ) . sortBy compare . concat
