import Data.List
--exercises.hs

--intermission write eq instance for datatype.
--1
data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  TisAn x == TisAn y = x == y

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = a == c && b == d

--3
data StringOrInt =
    TisAnInt   Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) (TisAString xs) (TisAString ys) = xs == ys
  (==) _ _                    = False

--4
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x0 y0) (Pair x1 y1) = x0 == x1 && y0 == y1

--5
data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x0 y0) (Tuple x1 y1) = x0 == x1 && y0 == y1

--6
data Which a =
    ThisOne a
  | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _                     = False

--7
data EitherOr a b =
    Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _                   = False

-- Multiple choice
--1.c) The Eq class makes equality tests possible
--2.b) The typeclass Ord is a subclass of Eq.
--3.a) (>) :: Ord a => a -> a -> Bool
--4.c) x is a tuple
--5.a) Integral includes Int and Integer

--Does it typecheck?
--1.
data Person = Person Bool deriving Show

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

--2.
data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

--3.
--a) only inputs of type Mood are acceptable.
--b) It will give a type error because 9 is not a Mood
--c) It will complain because Mood doesn't derive Ord.

--4.
type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

--Given datatype declaration what can we do?
data Rocks =
  Rocks String deriving (Eq, Ord, Show)

data Yeah =
  Yeah Bool deriving (Eq, Ord, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Ord, Show)

--1
phew = Papu (Rocks "chases") (Yeah True)
--2
truth = Papu (Rocks "chomskydoz")
             (Yeah True)
--3
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
--4
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int
sigmund' :: Num a => a -> Int
sigmund' x = myX


jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

--Type-Kwon-Do

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = fromIntegral i + f a
