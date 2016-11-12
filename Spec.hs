import Test.Hspec
import Chapter1
import Chapter2

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

  describe "Chapter2.3 n" $ do
    it "returns 2 without error" $ do
      (n) `shouldBe` 2
  describe "Chapter2.4 lasts" $ do
    it "returns the last element of a list" $ do
      last' [1..5] `shouldBe` 5
      last'' [1..5] `shouldBe` 5
      last''' [1..5] `shouldBe` 5
  describe "Chapter2.5 inits" $ do
    it "removes the last element of a list" $ do
      init' [1..5] `shouldBe` [1,2,3,4]
      init'' [1..5] `shouldBe` [1,2,3,4]
