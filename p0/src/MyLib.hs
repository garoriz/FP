module MyLib where

import Data.Maybe

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
<<<<<<< HEAD
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
=======
zipLong as bs = error "Implement me!"


-- Binary Search Tree
--
-- left < root <= right
data Tree a
  = Empty
  | Node
    { left :: Maybe (Tree a)
    , value :: a
    , right :: Maybe (Tree a)
    }
  deriving (Eq,Show,Read)

empty :: Tree a
empty = Empty

leaf :: a -> Tree a
leaf a = Node Nothing a Nothing

traversal :: Tree a -> [a]
traversal Empty = []
traversal (Node ml v mr)
  = maybe [] traversal ml ++ [v] ++ maybe [] traversal mr

insert :: Ord a => a -> Tree a -> Tree a
insert v Empty = leaf v
insert v t@(Node ml root mr)
  | v < root  = t{ left = Just $ maybe (leaf v) (insert v) ml }
  | otherwise = t{ right= Just $ maybe (leaf v) (insert v) mr }

-- Напишите тесты-свойства к функциям и сами функции
-- левого и правого поворота деревьев
-- (см. https://en.wikipedia.org/wiki/Red%E2%80%93black_tree)
rotateLeft :: Tree a -> Tree a
rotateLeft t = t

rotateRight :: Tree a -> Tree a
rotateRight t = t
>>>>>>> 3cd79a35a4f2a607de6b4254b5857271f3d4c26d
