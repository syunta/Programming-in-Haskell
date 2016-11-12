import Test.Hspec
import Chapter1
import Chapter2
import Chapter4

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

  describe "Chapter4.1 halve" $ do
    it "splits a list half-and-half as a tuple" $ do
      halve [1..6] `shouldBe` ([1,2,3], [4,5,6])
  describe "Chapter4.2 safetail" $ do
    it "returns the rest of list exepts the first" $ do
      safetail [1..5] `shouldBe` [2,3,4,5]
      safetail' [1..5] `shouldBe` [2,3,4,5]
      safetail'' [1..5] `shouldBe` [2,3,4,5]
    it "does not trow an exception if used with an empty list" $ do
      safetail ([] :: [Int]) `shouldBe` ([] :: [Int])
      safetail' ([] :: [Int]) `shouldBe` ([] :: [Int])
      safetail'' ([] :: [Int]) `shouldBe` ([] :: [Int])
  describe "Chapter4.3 <||>" $ do
    it "behaves as a logical disjunction" $ do
      let a = True || True
          b = True || False
          c = False || True
          d = False || False
      True <||> True `shouldBe` a
      True <||> False `shouldBe` b
      False <||> True `shouldBe` c
      False <||> False `shouldBe` d
      True <|||> True `shouldBe` a
      True <|||> False `shouldBe` b
      False <|||> True `shouldBe` c
      False <|||> False `shouldBe` d
      True <||||> True `shouldBe` a
      True <||||> False `shouldBe` b
      False <||||> True `shouldBe` c
      False <||||> False `shouldBe` d
      True <|||||> True `shouldBe` a
      True <|||||> False `shouldBe` b
      False <|||||> True `shouldBe` c
      False <|||||> False `shouldBe` d
  describe "Chapter4.4, 4.5 <&&>" $ do
    it "behaves as a logical conjunction" $ do
      let a = True && True
          b = True && False
          c = False && True
          d = False && False
      True <&&> True `shouldBe` a
      True <&&> False `shouldBe` b
      False <&&> True `shouldBe` c
      False <&&> False `shouldBe` d
      True <&&&> True `shouldBe` a
      True <&&&> False `shouldBe` b
      False <&&&> True `shouldBe` c
      False <&&&> False `shouldBe` d
  describe "Chapter4.6 mult" $ do
    it "returns a product of arguments" $ do
      mult 2 3 4 `shouldBe` 24
