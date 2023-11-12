module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import MyLib

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ZipLong Tests"
  [ testCase "zipLong [1,2,3] \"abc\" = [(1,'a'),(2,'b'),(3,'c')]" $ zipLong [1,2,3] "abc" @?= [(1,'a'),(2,'b'),(3,'c')]
  , testCase "zipLong [1,2] \"abcd\" = [(1,'a'),(2,'b'),(1,'c'),(2,'d')]" $ zipLong [1,2] "abcd" @?= [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
  , testCase "zipLong [] \"abcd\" = []" $ zipLong ([] :: [Int]) "abcd" @?= []
  ]
