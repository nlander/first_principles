import Data.List (elemIndex)
import Control.Applicative

--1
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

--2
y2 :: Maybe Integer
y2 = lookup 3 $ zip [1,2,3] [4,5,6]

z2 :: Maybe Integer
z2 = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y2 <*> z2

--3
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3:: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x3 <*> y3

--4
xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x <*> y

--Identity
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  (Identity f) <*> (Identity a) = Identity $ f a

--Constant
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure b = Constant mempty
  (Constant a) <*> (Constant b) = Constant $ a `mappend` b

--Maybe
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
  then Nothing
  else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
   } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
  Cow <$> noEmpty name
      <*> noNegative age
      <*> noNegative weight

cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name age weight =
  liftA3 Cow (noEmpty name)
             (noNegative age)
             (noNegative weight)
