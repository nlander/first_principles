--dayOfWeek.hs
module DayOfWeek where

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Show)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _   = EQ

data Date =
  Date DayOfWeek Int

{-
instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False
-}

instance Eq Date where
  (==) (Date weekday monthNum)
       (Date weekday' monthNum') =
    weekday == weekday' && monthNum == monthNum'
