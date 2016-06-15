returnLast' :: a -> b -> c -> d -> d
returnLast' _ _ _ d = d

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f a c = f a

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
              -> Employee
              -> Employee
              -> IO()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e'        = compare e e'

--intermission exercises
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

myAbs :: Integer -> Integer
myAbs x
   | x < 0     = (-x)
   | otherwise = x

bloodNa :: Integer -> String
bloodNa x
   | x < 135   = "too low"
   | x > 145   = "too high"
   | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
   | a^2 + b^2 == c^2 = "RIGHT ON"
   | otherwise        = "not right"

dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
   | x <= 0    = 0
   | x <= 1    = x * 15
   | x <= 2    = x * 12
   | x <= 4    = x * 8
   | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
--   | otherwise = 'F'
   | y >= 0.9  = 'A'
   | y >= 0.8  = 'B'
   | y >= 0.7  = 'C'
   | y >= 0.59 = 'D'
   | y <  0.59 = 'F'
   where y = x / 100

pal xs
    | xs == reverse xs = True
    | otherwise        = False

numbers x
    | x < 0   = -1
    | x == 0  = 0
    | x > 0   = 1
