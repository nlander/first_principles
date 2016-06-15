--curryIsAwesome.hs
module CurryIsAwesome where

exclaim = (++ "!")

fifth xs = (!! 4) xs : []

lastPart = drop 9

thirdLetter :: String -> Char
thirdLetter = (!! 2)

letterIndex :: Int -> Char
letterIndex x = (!! x) $ exclaim cia

rvrs :: String -> String
rvrs xs = drop 9 xs ++ take 4 (drop 5 xs) ++ take 5 xs

cia = "Curry is awesome"

main :: IO()
main = print $ rvrs cia
