myEnumFromTo :: (Enum a, Ord a) => a -> a -> [a]
myEnumFromTo a b = reverse $ accFunc a b []
  where
    accFunc x y [] = accFunc (succ x) y [x]
    accFunc x y ls
          | head ls == y = ls
          | y < x        = []
          | otherwise    = accFunc (succ x) y (x:ls)
