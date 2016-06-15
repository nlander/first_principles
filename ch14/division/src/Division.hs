module Division
    ( sayHello
    , dividedBy
    , multRec
    ) where

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n   d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

multRec :: (Ord a, Eq a, Num a) => a -> a -> a
multRec 0 b = 0
multRec a b = case a < 0 of
                True  -> negate $ m
                False -> m
              where m = b + multRec (abs a - 1) b
