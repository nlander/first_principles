type Name = String
type Age  = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Welcome to the person printer!"
  putStrLn "Please enter the person's name:"
  name <- getLine
  putStrLn "Please enter the person's age:"
  age_ <- getLine
  let age = read age_ :: Integer
  case mkPerson name age of
    Right person -> putStrLn ("Yay! Successfully\
                               \ got a person: "
                               ++ show person)
    Left error   -> putStrLn ("Oops, an error\
                              \ occurred: "
                               ++ show error)
