data Silly a b c d = MkSilly a b c d deriving Show

data Product a b =
  a :&: b
  deriving (Eq, Show)

data List a = Nil | Cons a (List a)
