module MyLib (someFunc, zipLong) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

zipLong firstList secondList = zip firstList secondList
