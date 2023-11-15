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
main = defaultMain insertTests

tests :: TestTree
tests = testGroup "ZipLong Tests"
  [ testCase "zipLong [1,2,3] \"abc\" = [(1,'a'),(2,'b'),(3,'c')]" $ zipLong [1,2,3] "abc" @?= [(1,'a'),(2,'b'),(3,'c')]
  , testCase "zipLong [1,2] \"abcd\" = [(1,'a'),(2,'b'),(1,'c'),(2,'d')]" $ zipLong [1,2] "abcd" @?= [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
  , testCase "zipLong [] \"abcd\" = []" $ zipLong ([] :: [Int]) "abcd" @?= []
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
arbitraryTree :: Int -> Int -> Gen (Tree Int)
arbitraryTree low high
  | low > high = pure Empty
  | otherwise = do
    v <- Gen.int (Range.linear low high)
    l <- arbitraryTree low (v - 1)
    r <- arbitraryTree (v + 1) high
    pure $ Node (Just l) v (Just r)

treeGen :: Int -> Int -> Gen (Tree Int)
treeGen minVal maxVal = arbitraryTree minVal maxVal

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
  
prop_isCorrect :: Property
prop_isCorrect = property $ do
  tree <- forAll (treeGen 0 10000)
  assert $ isCorrect tree
  
prop_rotateLeft :: Property
prop_rotateLeft = property $ do
  tree <- forAll (treeGen 0 10000)
  assert $ isCorrect (rotateLeft tree)

prop_rotateRight :: Property
prop_rotateRight = property $ do
  tree <- forAll (treeGen 0 10000)
  assert $ isCorrect (rotateRight tree)

insertTests :: TestTree
insertTests = testGroup "insert"
  [ testProperty "BST is correct" prop_bst
  , testProperty "Insert really inserts" prop_inserts
  , testCase "1" $ isCorrect (leaf 1) @? "leaf 1"
  , testCase "3" $
    isCorrect (Node (Just $ leaf 1) 2 (Just $ leaf 2)) @? "tree 3"
  ,	testProperty "Is the tree correct?" prop_isCorrect
  , testProperty "Is the rotating left correct?" prop_rotateLeft
  , testProperty "Is the rotating right correct?" prop_rotateRight
  ]
