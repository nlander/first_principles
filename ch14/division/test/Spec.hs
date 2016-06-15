import Test.Hspec
import Test.QuickCheck
import Division

main :: IO ()
main = quickCheck prop_additionGreater

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x
{-
main = hspec $ do
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
    it "5 times 7 is 35" $ do
      multRec 5 7 `shouldBe` 35
    it "-5 times 7 is -35" $ do
      multRec (-5) 7 `shouldBe` (-35)
    it "5 times -7 is -35" $ do
      multRec 5 (-7) `shouldBe` (-35)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
-}
