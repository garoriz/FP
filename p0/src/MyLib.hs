module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs -- [x]

-- Напишите тесты к функции и функцию
--
-- Работает как zip, но если один список
-- длиннее, циклически переиспользует второй
--
-- > zipLong [1,2,3] "abc"
-- [(1,'a'),(2,'b'),(3,'c')]
--
-- > zipLong [1,2] "abcd"
-- [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
--
-- > zipLong [] "abcd"
-- []
zipLong :: [a] -> [b] -> [(a,b)]
zipLong [] bs = []
zipLong as [] = []
zipLong as bs
   | length as > length bs = firstZip ++ (zipLongL (drop firstZipSize as) bs)
   | length as < length bs = firstZip ++ (zipLongR as (drop firstZipSize bs))
   | otherwise = firstZip
  where
    firstZip = zip as bs
    firstZipSize = length firstZip
	

zipLongL [] bs = []
zipLongL as bs = firstZip ++ (zipLongL (drop firstZipSize as) bs)
  where
    firstZip = zip as bs
    firstZipSize = length firstZip

zipLongR as [] = []
zipLongR as bs = firstZip ++ (zipLongR as (drop firstZipSize bs))
  where
    firstZip = zip as bs
    firstZipSize = length firstZip
