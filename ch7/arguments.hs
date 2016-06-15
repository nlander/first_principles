myNum :: Integer
myNum = 1

myVal f g h = myNum

bindExp :: Integer -> String
bindExp x = let x = 10; y = 5 in
            "the integer was: " ++ show x 
            ++ " and y was: " ++ show y
