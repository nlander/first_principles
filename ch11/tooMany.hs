{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

{-
instance TooMany Words where
  tooMany (Words "") = False
  tooMany (Words (c:cs)) = c > 'p'

instance TooMany CounTup where
  tooMany (Count (x, cs)) = tooMany x && tooMany cs
-}

instance TooMany String where
  tooMany "" = False
  tooMany (c:cs) = c > 'p'

instance TooMany (Int, String) where
  tooMany (x, chars) = tooMany x && tooMany chars

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany $ x + y

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany $ x - y

--newtype Words = Words [Char] deriving (Eq, Show)

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

--newtype CounTup = Count (Int, Words) deriving (Eq, Show)
