import Test.Hspec
import Data.Maybe
import Models
import Tactics

main :: IO ()
main = hspec $ do
  describe "Strategy" $ do
    it "returns (1,1) as the first move" $ do
      firstMove 'x' `shouldBe` Move 1 1 'x'

    it "returns false if there is no winner" $ do
      winnerExists [Move 0 0 'x', Move 2 2 'o', Move 1 1 'x', Move 2 0 'o', Move 1 2 'x'] `shouldBe` False

    it "returns true if winner exists" $ do
      winnerExists [Move 0 0 'x', Move 1 0 'x', Move 2 0 'x', Move 0 2 'o', Move 2 2 'o'] `shouldBe` True
