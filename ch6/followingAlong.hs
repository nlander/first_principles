divideThenAdd :: Fractional a => a -> a -> a
divideThenAdd x y = (x / y) + 1

subtractThenAdd :: Num a => a -> a -> a
subtractThenAdd x y = (x - y) + 1

data Mood = Blah

instance Show Mood where
  show _ = "Blah"
