{-# LANGUAGE NoMonomorphismRestriction #-}
module LearnParsers where

import Text.Trifecta
import Control.Applicative

stop :: Show a => Parser a
stop = unexpected "stop"

one = char '1' <* eof

one' :: Show a => Parser a
one' = one <* stop

oneTwo = char '1' *> char '2'

oneTwo' :: Show a => Parser a
oneTwo' = char '1' *> char '2' <* stop

testParse :: Show a => Parser a -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
