class Sumthin a where
  s :: a -> a

class Else g where
  e :: b -> f (g a b c)

class Biffy e where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d

class Impish v where
  impossibleKind :: v a -> v a

data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)
