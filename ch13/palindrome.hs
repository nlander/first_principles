import Control.Monad
import System.Exit
import Data.List
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let letters = (map toLower) . removeSpaces $ line1
  case (letters == reverse letters) of
    True -> putStrLn "It's a palindrome!"
    False -> exitSuccess

rember :: Eq a => a -> [a] -> [a]
rember toBeRemoved removeFrom = if elem toBeRemoved removeFrom
                                then rember toBeRemoved $
                                 delete toBeRemoved removeFrom
                                else removeFrom

removeSpaces :: String -> String
removeSpaces = rember ' '
