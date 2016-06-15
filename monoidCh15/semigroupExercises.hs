import Test.QuickCheck
import Data.Semigroup
import Data.List.NonEmpty as N

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = do
    head <- arbitrary
    rest <- arbitrary
    return (head :| rest)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

--1
data Trivial = Trivial deriving (Eq, Show)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Semigroup Trivial where
  _ <> _ = Trivial

--2
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    b <- arbitrary
    return (Identity b)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

type IdentityAssoc = Identity Trivial -> Identity Trivial -> Identity Trivial -> Bool

type IdentityAssoc' = Identity (NonEmpty Int) -> Identity (NonEmpty Int) -> Identity (NonEmpty Int) -> Bool

--3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two c d = Two (a <> c) (b <> d)

type TwoAssoc = Two Trivial (NonEmpty Char) -> Two Trivial (NonEmpty Char) -> Two Trivial (NonEmpty Char) -> Bool

--4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three d e f = Three (a <> d) (b <> e) (c <> f)

type ThreeAssoc = Three Trivial (NonEmpty Char) (NonEmpty Int) -> Three Trivial (NonEmpty Char) (NonEmpty Int) -> Three Trivial (NonEmpty Char) (NonEmpty Int) -> Bool

--5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four e f g h = Four (a <> e) (b <> f) (c <> g) (d <> h)

type FourAssoc = Four Trivial (NonEmpty Char) (NonEmpty Int) (NonEmpty Trivial) -> Four Trivial (NonEmpty Char) (NonEmpty Int) (NonEmpty Trivial) -> Four Trivial (NonEmpty Char) (NonEmpty Int) (NonEmpty Trivial) -> Bool

--6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = do
    bool <- arbitrary
    return (BoolConj bool)

instance Semigroup BoolConj where
  BoolConj False <> _ = BoolConj False
  _ <> BoolConj False = BoolConj False
  _ <> _              = BoolConj True

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

--7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = do
    bool <- arbitrary
    return (BoolDisj bool)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _              = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

--8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Fst a))
              , (1, return (Snd b))]

instance Semigroup (Or a b) where
  Snd x <> _ = Snd x
  _ <> Snd x = Snd x
  _ <> x     = x

type OrAssoc = Or Int Char -> Or Int Char -> Or Int Char -> Bool

--9
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g =
    Combine (\a -> f a <> g a)

--10
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

--11
data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Failure' a))
              , (1, return (Success' b))]

instance Semigroup (Validation a b) where
  Success' x <> _ = Success' x
  _ <> Success' x = Success' x
  _ <> x         = x

type ValidateAssoc = Validation Int Char -> Validation Int Char -> Validation Int Char -> Bool

--12
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    return (AccumulateRight a)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
    AccumulateRight (Success' x) <>
      AccumulateRight (Success' y) =
        AccumulateRight $ Success' (x <> y)
    AccumulateRight x <> AccumulateRight y =
      AccumulateRight $ x <> y

type AccRightAssoc = AccumulateRight Int (NonEmpty Char) -> AccumulateRight Int (NonEmpty Char) -> AccumulateRight Int (NonEmpty Char) -> Bool

--13
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) =>
  Arbitrary (AccumulateBoth a b) where
    arbitrary = do
      a <- arbitrary
      return (AccumulateBoth a)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
    AccumulateBoth (Success' x) <>
      AccumulateBoth (Success' y) =
        AccumulateBoth $ Success' (x <> y)
    AccumulateBoth (Failure' x) <>
      AccumulateBoth (Failure' y) =
        AccumulateBoth $ Failure' (x <> y)
    AccumulateBoth x <> AccumulateBoth y =
      AccumulateBoth $ x <> y

type AccBothAssoc = AccumulateBoth (NonEmpty Int) (NonEmpty Char) -> AccumulateBoth (NonEmpty Int) (NonEmpty Char) -> AccumulateBoth (NonEmpty Int) (NonEmpty Char) -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc')
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidateAssoc)
  quickCheck (semigroupAssoc :: AccRightAssoc)
  quickCheck (semigroupAssoc :: AccBothAssoc)
