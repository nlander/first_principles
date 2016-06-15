import Data.Char

data DaPhone =
  DaPhone { keyOne :: String
          , keyTwo :: String
          , keyThree :: String
          , keyFour :: String
          , keyFive :: String
          , keySix :: String
          , keySeven :: String
          , keyEight :: String
          , keyNine :: String
          , keyStar :: String
          , keyZero :: String
          , keyHashtag :: String }
  deriving (Eq, Show)

phone :: DaPhone
phone = DaPhone
        { keyOne = "1"
        , keyTwo = "abc2"
        , keyThree = "def3"
        , keyFour = "ghi4"
        , keyFive = "jkl5"
        , keySix = "mno6"
        , keySeven = "pqrs7"
        , keyEight = "tuv8"
        , keyNine = "wxyz9"
        , keyStar = "*^"
        , keyZero = " +0"
        , keyHashtag = "#.," }

type Digit = Char

type Presses = Int

convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha.  Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

convoTaps :: [[(Digit, Presses)]]
convoTaps = map (cellPhonesDead phone) convo

popularLetterCost :: [(Char, Int)]
popularLetterCost = zip popLetters totalTaps where
  popLetters = map popularest convo
  totalTaps  = zipWith (*) letterCost letterCount
  letterCost = (map snd) . concat $ popRevTaps
  popRevTaps = map (phoneReverseTaps phone) popLetters
  letterCount = zipWith ($) (map occurs convo) popLetters

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (phoneReverseTaps phone)

remChar :: Char -> String -> String
remChar _ "" = ""
remChar x (c:cs) = if c == x
                 then remChar x cs
                 else c : remChar x cs

remSpaces :: String -> String
remSpaces = remChar ' '

coolestWord :: [String] -> String
coolestWord = coolestWord' . spacesConcat

coolestWord' :: String -> String
coolestWord' str = let wds = words str in
         snd . maximum . zip (map (wordOccurs wds) wds) $ wds

convoWords = words $ spacesConcat convo

spacesConcat :: [String] -> String
spacesConcat msgs = concat . reverse $ go msgs [] where
  go [] acc = acc
  go (x:xs) acc = go xs (x : " " : acc)

popularest :: String -> Char
popularest msg = let m = remSpaces msg in
           snd . maximum . zip (zip (map (occurs m) m) (map snd $ cellPhonesDead phone m)) $ m

msg4 = convo !! 3

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

freqs :: String -> [(Int, Char)]
freqs = map swap . cellPhonesDead phone

topFreqs :: [(Int, Char)] -> [Char]
topFreqs list = remDups . map snd $ go list [] where
  go []       acc = acc
  go l@(x:xs) acc = go xs (maximum l : acc)

charCost :: Char -> Presses
charCost = sum . map snd . reverseTaps

costliest :: [Char] -> Char
costliest chars = let cs = remDups chars
                      cost = map reverseTaps cs
                      tups = zip cost cs in
                  snd . maximum $ tups

remDups :: Eq a => [a] -> [a]
remDups list = reverse $ go list [] where
  go [] acc = acc
  go (x:xs) acc = if x `elem` acc
                then go xs acc
                else go xs (x:acc)

coolestLtr :: [String] -> Char
coolestLtr = popularest . concat

wordOccurs :: [String] -> String -> Int
wordOccurs ws wd = go wd ws 0 where
  go _ [] acc = acc
  go word (x:xs) acc
    | word == x = go word xs (acc + 1)
    | word /= x = go word xs acc

occurs :: String -> Char -> Int
occurs msg c = go c msg 0 where
  go _ "" acc = acc
  go char (x:xs) acc
    | char == x = go char xs (acc + 1)
    | char /= x = go char xs acc

reverseTaps :: Char -> [(Digit, Presses)]
reverseTaps = phoneReverseTaps phone

phoneReverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
phoneReverseTaps phone@(DaPhone one two three four five six seven eight nine star zero hash) c = reverse $ go phone c [] where
  go phone c acc
    | c `elem` one = ('1', indexOf c one + 1) : acc
    | c `elem` two = ('2', indexOf c two + 1) : acc
    | c `elem` three = ('3', indexOf c three + 1) : acc
    | c `elem` four = ('4', indexOf c four + 1) : acc
    | c `elem` five = ('5', indexOf c five + 1) : acc
    | c `elem` six = ('6', indexOf c six + 1) : acc
    | c `elem` seven = ('7', indexOf c seven + 1) : acc
    | c `elem` eight = ('8', indexOf c eight + 1) : acc
    | c `elem` nine = ('9', indexOf c nine + 1) : acc
    | c `elem` star = ('*', indexOf c star + 1) : acc
    | c `elem` zero = ('0', indexOf c zero + 1) : acc
    | c `elem` hash = ('#', indexOf c hash + 1) : acc
    | isUpper c = go phone (toLower c) (go phone '*' [] ++ acc)

  indexOf :: Char -> String -> Int
  indexOf c cs = head [x | x <- [0..4], cs !! x == c]

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum . map snd
