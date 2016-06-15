import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

--1
data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend _ _ = Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

--2
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend (Identity x) (Identity y) = Identity $ x <> y

type IdentityAssoc = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool

--3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a b) (Two c d) = Two (a <> c) (b <> d)

type TwoAssoc = Two [Int] Trivial -> Two [Int] Trivial -> Two [Int] Trivial -> Bool

--4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  arbitrary = arbitrary >>= (return . BoolConj)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend (BoolConj True) x = x
  mappend x (BoolConj True) = x
  mappend _ _               = BoolConj False

type ConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

--5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = arbitrary >>= (return . BoolDisj)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend (BoolDisj False) x = x
  mappend x (BoolDisj False) = x
  mappend _ _               = BoolDisj True

type DisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

--6
newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\_ -> mempty)
  mappend (Combine f) (Combine g) =
    Combine (\x -> f x <> g x)

--7
newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Monoid (Comp a) where
  mempty = Comp id
  mappend (Comp f) (Comp g) = Comp (f . g)

--8
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend (Mem f) (Mem g) = Mem $ \s -> (fst (f s) <> fst (g s),
                                         snd (f (snd (g s))))

f' = Mem $ \s -> ("hi", s + 1)


main :: IO ()
main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
  quickCheck (monoidAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Identity [Int] -> Bool)
  quickCheck (monoidAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two [Int] Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Two [Int] Trivial -> Bool)
  quickCheck (monoidAssoc :: ConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidAssoc :: DisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
