module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List (foldl', sort)

import MyLib

main :: IO ()
main = defaultMain tests

tests :: TestTree
<<<<<<< HEAD
tests = testGroup "ZipLong Tests"
  [ testCase "zipLong [1,2,3] \"abc\" = [(1,'a'),(2,'b'),(3,'c')]" $ zipLong [1,2,3] "abc" @?= [(1,'a'),(2,'b'),(3,'c')]
  , testCase "zipLong [1,2] \"abcd\" = [(1,'a'),(2,'b'),(1,'c'),(2,'d')]" $ zipLong [1,2] "abcd" @?= [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
  , testCase "zipLong [] \"abcd\" = []" $ zipLong ([] :: [Int]) "abcd" @?= []
=======
tests = testGroup "All Tests"
  [ testGroup "Unit Tests"
    [ testCase "3*5 == 15" $ 3*5 @?= 15
    , testCase "2*2 == 4" $ 4 @=? 2*2
    , testCase "rev []" $ rev [] @?= ([] :: [Int])
    , testCase "rev [1,2,3]" $
      rev [1,2,3] @?= [3,2,1]
    ]
  , testProperty "reverse works" $ prop_rev
  , testProperty "strange opaque value" prop_opaque
  , testProperty "all numbers do not divide 10000" $
    prop_div10000
  , treeTests
  ]

treeTests :: TestTree
treeTests = testGroup "Tree Tests"
  [ traversalTests
  , insertTests
  ]

traversalTests :: TestTree
traversalTests = testGroup "traversal"
  [ testCase "empty" $ traversal empty @?= ([] :: [Int])
  , testCase "single elt" $ traversal (Node Nothing 1 Nothing) @?= [1]
  , testCase "three elts" $
    traversal (Node (Just $ leaf 1) 2 (Just $ leaf 3)) @?= [1,2,3]
  ]

isCorrect :: Ord a => Tree a -> Bool
isCorrect Empty = True
isCorrect (Node ml v mr) = maybe True isCorrect ml &&
  maybe True isCorrect mr &&
  all (<v) (maybe [] traversal ml) &&
  all (>=v) (maybe [] traversal mr)

-- Поменять генератор двоичных деревьев поиска так,
-- чтобы задавался диапазон элементов дерева и
-- в левом и в правом поддереве генерировались только
-- нужные элементы (чтобы BST было корректным)
arbitraryTree :: Size -> Gen (Tree Int)
arbitraryTree 0 = pure empty
arbitraryTree size = do
  leftSize <- Size <$> Gen.int (Range.linear 0 $ unSize size - 1)
  let rightSize = size - leftSize - 1
  l <- if leftSize == 0
       then pure Nothing
       else Just <$> arbitraryTree leftSize
  v <- Gen.int $ Range.linear 0 10000
  r <- if rightSize == 0
       then pure Nothing
       else Just <$> arbitraryTree rightSize
  pure $ Node l v r

treeGen :: Gen (Tree Int)
treeGen = Gen.sized arbitraryTree

prop_bst :: Property
prop_bst = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 30) $ Gen.int (Range.linear 0 1000)
  let t = foldl' (\tree elt -> insert elt tree) empty xs
  assert $ isCorrect t

prop_inserts :: Property
prop_inserts = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 30) $ Gen.int (Range.linear 0 1000)
  let t = foldl' (\tree elt -> insert elt tree) empty xs
  sort xs === traversal t

insertTests :: TestTree
insertTests = testGroup "insert"
  [ testProperty "BST is correct" prop_bst
  , testProperty "Insert really inserts" prop_inserts
  , testCase "1" $ isCorrect (leaf 1) @? "leaf 1"
  , testCase "3" $
    isCorrect (Node (Just $ leaf 1) 2 (Just $ leaf 2)) @? "tree 3"
>>>>>>> 3cd79a35a4f2a607de6b4254b5857271f3d4c26d
  ]
