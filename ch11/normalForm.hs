data Fiction = Fictin deriving Show
data Nonfiction = Nonfictin deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String

data Author = Author (AuthorName, BookType)

{-
data Expr =
    Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

type Number = Int
type Add = (Expr, Expr)
type Minus = Expr
type Mult = (Expr, Expr)
type Divide = (Expr, Expr)

type Expr' =
  Either Number
    (Either Add
      (Either Minus
        (Either Mult Divide)))
-}

data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show
-}

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show
