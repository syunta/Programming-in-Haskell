import Test.Hspec
import Chapter10

main :: IO ()
main = hspec $ do
  describe "Chapter10.1 mult" $ do
    it "calculates multiplication" $ do
      mult (Succ (Succ (Succ Zero))) (Succ (Succ Zero)) `shouldBe` (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))

  let t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))
  describe "Chapter10.2 occurs" $ do
    it "checks a tree whether contains a target element" $ do
      occurs 1 t `shouldBe` True
      occurs 3 t `shouldBe` True
      occurs 4 t `shouldBe` True
      occurs 5 t `shouldBe` True
      occurs 6 t `shouldBe` True
      occurs 7 t `shouldBe` True
      occurs 9 t `shouldBe` True
      occurs 2 t `shouldBe` False
  describe "Chapter10.3 countleaves" $ do
    it "counts leaves of tree" $ do
      countleaves t `shouldBe` 4
  describe "Chapter10.3 balanced" $ do
    it "checks a tree whether it is balanced" $ do
      balanced t `shouldBe` True
      balanced (Node (Node (Leaf 1) 2 (Node (Leaf 4) 3 (Leaf 5))) 6 (Leaf 8)) `shouldBe` False
  describe "Chapter10.4 balance" $ do
    it "generate a balanced tree from a list of interger" $ do
      balance [1,3,4,5,6,7,9] `shouldBe` t
      balanced (balance [1,3,4,5,6,7,9]) `shouldBe` True
  describe "Chapter10.5 eval'" $ do
    let t1 = [('A', True), ('B', True)]
        t2 = [('A', True), ('B', False)]
        t3 = [('A', False), ('B', True)]
        t4 = [('A', False), ('B', False)]
    it "supports disjunction" $ do
      eval t1 (Or (Var 'A') (Var 'B')) `shouldBe` True
      eval t2 (Or (Var 'A') (Var 'B')) `shouldBe` True
      eval t3 (Or (Var 'A') (Var 'B')) `shouldBe` True
      eval t4 (Or (Var 'A') (Var 'B')) `shouldBe` False
    it "supports equivalence" $ do
      eval t1 (IFF (Var 'A') (Var 'B')) `shouldBe` True
      eval t2 (IFF (Var 'A') (Var 'B')) `shouldBe` False
      eval t3 (IFF (Var 'A') (Var 'B')) `shouldBe` False
      eval t4 (IFF (Var 'A') (Var 'B')) `shouldBe` True
  describe "Chapter10.6 parse prop" $ do
    it "parses string to proposition expression" $ do
      let testTaut = isTaut . fst . head . (parse prop)
      testTaut "A ^ ¬ B" `shouldBe` False
      testTaut "A => (A ^ B)" `shouldBe` False
      testTaut "(A ^ B) => A" `shouldBe` True
      testTaut "(A ^ (A => B)) => B" `shouldBe` True
