import Data.Monoid
import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    b <- arbitrary
    frequency [ (1, return Nada)
              , (3, return (Only b))]

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada x = x
  mappend x Nada = x
  mappend (Only x) (Only y) = Only $ mappend x y

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    b <- arbitrary
    return (First' b)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) x = x
  mappend x (First' Nada) = x
  mappend x _ = x

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (a <> mempty) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (mempty <> a) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
