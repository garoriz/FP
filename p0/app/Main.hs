module Main where

import qualified MyLib (someFunc, zipLong)

main :: IO ()
main = do
  putStrLn $ show $ MyLib.zipLong [1,2,3] "abc"
