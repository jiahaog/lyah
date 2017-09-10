import Control.Exception (evaluate)
import ReversePolishNotation (solveRPN)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main =
  hspec $ do
    describe "Prelude.head" $ do
      it "returns the first element of the list" $ do
        head [23 ..] `shouldBe` (23 :: Int)
      it "throws an exception with an empty list" $ do
        evaluate (head []) `shouldThrow` anyException
    describe "RPN.solveRPN" $ do
      it "returns the correct result for the example" $ do
        solveRPN "10 4 3 + 2 * -" `shouldBe` (-4)
      it "returns the correct result for floats" $ do
        solveRPN "10.1 4.2 +" `shouldBe` 14.3
      it "returns the correct result for division" $ do
        solveRPN "10 5 /" `shouldBe` 2
