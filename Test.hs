import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Today

main :: IO ()
main = hspec $ do
    describe "Today" $ do
        it "get the right weekday from a timestamp" $ do
            getDayFromTimestamp 0 `shouldBe` Thursday
            getDayFromTimestamp 1466766758 `shouldBe` Friday
            getDayFromTimestamp 1466881750`shouldBe` Saturday
