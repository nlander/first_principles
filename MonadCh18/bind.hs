import Control.Monad
import Control.Applicative

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)


bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name -> putStrLn ("y helo thar: " ++ name)
