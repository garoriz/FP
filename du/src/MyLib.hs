{-# LANGUAGE FlexibleContexts #-}

module MyLib (someFunc) where


import Control.Monad.IO.Class
import Control.Monad.State
import System.IO
import System.Environment
import System.Directory


someFunc :: IO ()
someFunc = do
  args <- getArgs 
  dir <- getLine
  directory <- listDirectory dir
  putStrLn $ "Arguments: " ++ show args ++ " Directory: " ++ show directory