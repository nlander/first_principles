ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+2) else Nothing

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age =
  mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) =
  Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) =
  Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age > 0 of
  True  -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True  -> Right name
  False -> Left [NameEmpty]

safeTail        :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (x:xs) = Just xs
