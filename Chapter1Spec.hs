import Test.Hspec
import Chapter1

main :: IO ()
main = hspec $ do
  describe "Chapter1.3 product'" $ do
    it "returns the product' of list elements" $ do
      product' [1..5] `shouldBe` 120
  describe "Chapter1.4 qsort" $ do
    it "returns a list ordered descending" $ do
      qsort [1..5] `shouldBe` [5,4,3,2,1]
  describe "Chapter1.5 qsort'" $ do
    it "returns a list removed duplicated elements" $ do
      qsort' [2,2,3,1,1] `shouldBe` [1,2,3]
