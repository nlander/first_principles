data PugType = PugData
data HuskyType a = HuskyData
data DogueDeBordeaux doge = DogueDeBordeaux doge

myPug = PugData :: PugType

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[[[Int]]]]]]
myOtherOtherHusky = HuskyData

myDoge :: DogueDeBordeaux Int
myDoge = DogueDeBordeaux 10

data Doggies a =
    Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
               Mini
             | Mazda
             | Tata
               deriving (Eq, Show)

data Airline =
         PapuAir
       | CatapultsR'Us
       | TakeYourChancesUnited
         deriving (Eq, Show)

type Size = Integer

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir

isCar :: Vehicle -> Bool
isCar = (\v -> case v of Plane _ _ -> False; Car _ _ -> True)

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

data Example0 =
  Example0 deriving (Eq, Show)

data Example1 =
  Example1 Int deriving (Eq, Show)

data Example2 =
  Example2 Int String deriving (Eq, Show)

data MyType = MyVal Int deriving (Eq, Show)

data Example = MakeExample Int deriving Show

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows  = Cows  Int deriving (Eq, Show)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43
