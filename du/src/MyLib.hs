{-# LANGUAGE FlexibleContexts #-}

module MyLib (someFunc) where


import Control.Monad.IO.Class
import Control.Monad.State
import System.IO
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except

data Config = Config
  { level :: Int 
  , humanReadable :: Bool 
  , verbose :: Bool 
  , rootDir :: FilePath
  } deriving Show

getDirectorySize :: Int -> FilePath -> IO Integer
getDirectorySize 0 dir = do
    contents <- listDirectory dir
    sizes <- forM contents $ \name -> do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
            then getDirectorySize 0 path
            else getFileSize path
    return $ sum sizes
getDirectorySize n dir = do
    contents <- listDirectory dir
    sizes <- forM contents $ \name -> do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
            then getDirectorySize (n - 1) path
            else return 0
    return $ sum sizes

printDirectorySize :: Int -> Bool -> FilePath -> IO ()
printDirectorySize n humanReadable dir = do
    size <- getDirectorySize n dir
    let readableSize = formatSize humanReadable size
    if (n == -1) 
      then putStrLn $ "Level isn't specified"
      else putStrLn $ "Total size of " ++ dir ++ " at level " ++ show n ++ ": " ++ readableSize

formatSize :: Bool -> Integer -> String
formatSize True size
  | size < 1024 = show size ++ "B"
  | size < 1024^2 = show (size `div` 1024) ++ "K"
  | size < 1024^3 = show (size `div` (1024^2)) ++ "M"
  | otherwise = show (size `div` (1024^3)) ++ "G"
formatSize False size = show size

someFunc :: IO ()
someFunc = do
	args <- liftIO getArgs 
	let (config, rootDir) = parseArgs args
	printDirectorySize (level config) (humanReadable config) rootDir
  --directory <- listDirectory (head args)
  --fileSize <- getFileSize ("D:\\kNN")
  --putStrLn $ show fileSize
  
parseArgs :: [String] -> (Config, FilePath)
parseArgs args =
  let config = Config
        { level = if "-s" `elem` args then 0 else maybe (-1) read (getOption "-d" args)
        , humanReadable = "-h" `elem` args
        , verbose = "-v" `elem` args
        , rootDir = last args
        }
  in (config, rootDir config)
  where
    getOption :: String -> [String] -> Maybe String
    getOption option (opt : value : rest)
      | opt == option = Just value
      | otherwise = getOption option rest
    getOption _ _ = Nothing