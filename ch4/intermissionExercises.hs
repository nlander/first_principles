{-
1. Mood is the type constructor

2. You can use the value Blah or the value Woot for a function requiring a value of type Mood.

3. Woot is a value, not a type, so it should not be included in the type signature.  The type signature should be :: Mood -> Mood.
-}

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah
