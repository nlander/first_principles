data Validation err a =
    Failure err
  | Success a
  deriving (Eq, Show)
