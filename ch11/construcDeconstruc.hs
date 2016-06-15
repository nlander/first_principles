import Data.List

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

idInt :: Id Integer
idInt = MkId 10

idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

data Product a b =
  Product a b deriving (Eq, Show)

type Awesome = Bool
type ProductName = String

person :: Product ProductName Awesome
person = Product "Simon" True

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

{-
data Twitter =
  Twitter deriving (Eq, Show)

data AskFm =
  AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter
-}

data SocialNetwork =
    Twitter
  | AskFm
  deriving (Eq, Show)

type Twitter = String
type AskFm = String

twitter :: Sum Twitter AskFm
twitter = First "Twitter"

askfm :: Sum Twitter AskFm
askfm = First "Twitter"

data RecordProduct a b =
  RecordProduct { pfirst  :: a
                , psecond ::b }
                deriving (Eq, Show)

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42
                         , psecond = 0.00001 }

data OperatingSystem =
       GnuPlusLinux
     | OpenBSDPlusNevermindJustBSDStill
     | Mac
     | Windows
     deriving (Eq, Show)

data ProgrammingLanguage =
       Haskell
     | Agda
     | Idris
     | PureScript
     deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = nub [Programmer a b |
                   a <- allOperatingSystems
                 , b <- allLanguages]

data ThereYet =
  There Integer Float String Bool
  deriving (Eq, Show)

nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yusssss :: ThereYet
yusssss = notQuite False

newtype Name'   = Name' String deriving Show
newtype Acres   = Acres Int deriving Show

data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

data Farmer =
  Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
  FarmerRec { name       :: Name
            , acres      :: Acres
            , farmerType :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> True
  _           -> False

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)
