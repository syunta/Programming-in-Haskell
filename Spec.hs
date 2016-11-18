import Test.Hspec
import Control.Exception (evaluate, catch)
import Chapter1
import Chapter2
import Chapter4
import Chapter5
import Chapter6
import Chapter7
import Chapter8

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

  describe "Chapter5.1 squaresuminhundred" $ do
    it "returns a sum of square from 1 to 100" $ do
      squaresuminhundred `shouldBe` sum (map (\x -> x^2) [1..100])
  describe "Chapter5.2 replicate'" $ do
    it "returns a list has same elements" $ do
      replicate' 3 True `shouldBe` [True, True, True]
  describe "Chapter5.3 pyths'" $ do
    it "generates a list of Pythagoras number" $ do
      pyths 10 `shouldBe` [(3,4,5), (4,3,5), (6,8,10), (8,6,10)]
  describe "Chapter5.4 perfects" $ do
    it "generates a list of perfect number that equals sum of factors except itself" $ do
      perfects 500 `shouldBe` [6,28,496]
  describe "Chapter5.5 comprehension has two generaters" $ do
    it "can express double comprehension has one generater" $ do
      comprehension_has_two_generaters `shouldBe` double_comprehension_has_one_generater
  describe "Chapter5.6 positions" $ do
    it "returns all indexes as a list refer the target values by the key" $ do
      positions False [True, False, True, False] `shouldBe` [1,3]
  describe "Chapter5.7 scalarproduct" $ do
    it "returns scalar product by arg lists" $ do
      scalarproduct [1,2,3] [4,5,6] `shouldBe` 32
  describe "Chapter5.8 crack" $ do
    it "can crack Caesar cipher contains upper case" $ do
      crack "KDVNHOO lv IXQ !" `shouldBe` "HASKELL is FUN !"

  describe "Chapter6.1 <^>" $ do
    it "is a operator of power" $ do
      2 <^> 5 `shouldBe` 2^5
  describe "Chapter6.3 and'" $ do
    it "checks a list whether its all elements are True" $ do
      and' [True, True, True] `shouldBe` True
      and' [True, False, True] `shouldBe` False
  describe "Chapter6.3 concat'" $ do
    it "takes a list in lists and conjoins a list of elements" $ do
      concat' [[1],[2],[3]] `shouldBe` [1,2,3]
  describe "Chapter6.3 replicate'" $ do
    it "returns a list has same elements" $ do
      replicate'' 3 5 `shouldBe` [5, 5, 5]
  describe "Chapter6.3 <!!>" $ do
    it "returns an element targeted index" $ do
      [1,2,3,4] <!!> 2 `shouldBe` 3
  describe "Chapter6.3 elem'" $ do
    it "checks a list whether contains a target element" $ do
      elem' 4 [1,2,3,4,5] `shouldBe` True
      elem' 7 [1,2,3,4,5] `shouldBe` False
  describe "Chapter6.4 merge" $ do
    it "takes two sorted lists and conjoins a sorted list" $ do
      merge [2,5,6] [1,3,4] `shouldBe` [1,2,3,4,5,6]
  describe "Chapter6.5 msort" $ do
    it "behaves like a merge sort" $ do
      msort [2,5,1,6,3,4] `shouldBe` [1,2,3,4,5,6]
  describe "Chapter6.6 sum'" $ do
    it "ruterns sum of list elements" $ do
      sum' [1,2,3] `shouldBe` 6
  describe "Chapter6.6 take'" $ do
    it "returns a list contains targeted number elements" $ do
      take' 3 [1,2,3,4,5] `shouldBe` [1,2,3]
  describe "Chapter6.6 last''''" $ do
    it "returns the last element of a list" $ do
      last'''' [1,2,3,4] `shouldBe` 4

  describe "Chapter7.1 mapfilter" $ do
    it "ruturns a list applied filter and map" $ do
      mapfilter (\x -> x * x) odd [1,2,3,4,5] `shouldBe` [1,9,25]
  describe "Chapter7.2 all'" $ do
    it "checks a list of all elements with predicate" $ do
      all' odd [1,2,3,4,5] `shouldBe` False
      all' odd [1,3,5] `shouldBe` True
  describe "Chapter7.2 any'" $ do
    it "checks a list whether contains an element satisfied predicate" $ do
      any' even [1,2,3,4,5] `shouldBe` True
      any' even [1,3,5] `shouldBe` False
  describe "Chapter7.2 takeWhile'" $ do
    it "takes a list of elements until fails checking predicate" $ do
      takeWhile  even [2,4,5,6] `shouldBe` [2,4]
      takeWhile' even [2,4,5,6] `shouldBe` [2,4]
  describe "Chapter7.2 dropWhile'" $ do
    it "drops a list of elements until fails checking predicate" $ do
      dropWhile  even [2,4,5,6] `shouldBe` [5,6]
      dropWhile' even [2,4,5,6] `shouldBe` [5,6]
  describe "Chapter7.3 map'" $ do
    it "maps a list of elements by argument function" $ do
      map' (*2) [1,2,3] `shouldBe` [2,4,6]
  describe "Chapter7.3 filter'" $ do
    it "takes a list of elements sutisfied checking predicate" $ do
      filter' even [1,2,3,4] `shouldBe` [2,4]
  describe "Chapter7.4 dec2int" $ do
    it "convert decimal notation to integer" $ do
      dec2int [2,3,4,5] `shouldBe` 2345
  describe "Chapter7.6 curry'" $ do
    it "takes a function taking a tuple and returns a curry function" $ do
      let test_add (x,y) = x + y
          curry_add = curry' test_add
      curry_add 3 7 `shouldBe` 10
      (curry_add 3) 7 `shouldBe` 10
  describe "Chapter7.6 uncurry'" $ do
    it "takes a curry function and returns a function taking a tuple" $ do
      (uncurry' (+)) (3,7) `shouldBe` 10
  describe "Chapter7.7 chop8'" $ do
    it "splits a bit list each 8 bit" $ do
      chop8' [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0] `shouldBe` [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0],[1,1,0,0,0,1,1,0]]
  describe "Chapter7.7 map''" $ do
    it "maps a list of elements by argument function" $ do
      map'' (*2) [1,2,3] `shouldBe` [2,4,6]
  describe "Chapter7.7 iterate'" $ do
    it "generates an infinite list" $ do
      take 4 (iterate' (*3) 1) `shouldBe` [1,3,9,27]
  describe "Chapter7.8 parity" $ do
    it "appends 0 to bits if that has odd ones" $ do
      parity [0,1,0,0] `shouldBe` [1,0,1,0,0]
    it "appends 0 to bits if that has even ones" $ do
      parity [1,0,1,0] `shouldBe` [0,1,0,1,0]
  describe "Chapter7.8 unparity" $ do
    it "removes a head of parity bit if bits is valid" $ do
      unparity [1,0,1,0,0] `shouldBe` [0,1,0,0]
    it "throws an exception if bits is invalid" $ do
      evaluate (unparity [1,1,1,0,0]) `shouldThrow` anyException
  describe "Chapter7.8 transmit" $ do
    it "encodes string to bits with parity bit and decodes bits to string without parity bit" $ do
      transmit "succeed" `shouldBe` "succeed"

  describe "Chapter8.1 int'" $ do
    it "parses integer" $ do
      parse int "-123d" `shouldBe` [(-123,"d")]
      parse int "123" `shouldBe` [(123,"")]
      parse int "adb" `shouldBe` []
  describe "Chapter8.2 comment" $ do
    it "parses comment of haskell" $ do
      parse comment "--abc\n" `shouldBe` [((),"")]
      parse comment "--abc\ndef" `shouldBe` [((),"def")]
