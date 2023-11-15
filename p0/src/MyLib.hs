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

makeNode :: Maybe (Tree a) -> a -> Maybe (Tree a) -> Tree a
makeNode l v r = Node l v r

-- Напишите тесты-свойства к функциям и сами функции
-- левого и правого поворота деревьев
-- (см. https://en.wikipedia.org/wiki/Red%E2%80%93black_tree)
rotateLeft :: Tree a -> Tree a
rotateLeft (Node (Just l) x (Just (Node rl rv rr))) =
  makeNode (Just (makeNode (Just l) x rl)) rv rr
rotateLeft t = t

rotateRight :: Tree a -> Tree a
rotateRight (Node (Just (Node ll lv lr)) x (Just r)) =
  makeNode ll lv (Just (makeNode lr x (Just r)))
rotateRight t = t

