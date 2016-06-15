import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\dbItem times -> case dbItem of
                       DbNumber _   -> times
                       DbString _   -> times
                       DbDate time  -> time : times) []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\dbItem ints  -> case dbItem of
                         DbNumber int -> int : ints
                         DbString _   -> ints
                         DbDate _     -> ints) []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = (fromIntegral $ sumDb xs) / (fromIntegral . length $ filterDbNumber xs)
