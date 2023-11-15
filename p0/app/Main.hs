module Main where

import qualified MyLib

main :: IO ()
main = do
  putStrLn $ show $ MyLib.rotateRight treeExample
  
treeExample :: MyLib.Tree Int
treeExample =
  MyLib.Node
    { MyLib.left = Just (MyLib.Node (Just (MyLib.Node Nothing 2 Nothing)) 1 (Just (MyLib.Node Nothing 3 Nothing)))
    , MyLib.value = 4
    , MyLib.right = Just (MyLib.Node Nothing 5 Nothing)
    }
 