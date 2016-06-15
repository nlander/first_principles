--print5broken.hs
module Print5Broken where

printSecond :: IO()
printSecond = do
  putStrLn greeting

main :: IO()
main = do
  putStrLn greeting
  printSecond

--greeting :: String
greeting = "Yarrrrr"
