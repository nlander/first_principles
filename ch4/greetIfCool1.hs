--greetIfCool1.hs
module GreetIfcool1 where

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyyy. What's shakin'?"
  else
    putStrLn "pshhhh."
  where cool = coolness == "downright frosty yo"
