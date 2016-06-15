-- Review of Types
--1. d)
--2. b)
--3. d)
--4. b)
--Reviewing currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"
frappe :: String -> String
frappe = flippy "haha"

mc91 n
     | n > 100  = n - 10
     | n <= 100 = mc91 . mc91 $ n + 11
