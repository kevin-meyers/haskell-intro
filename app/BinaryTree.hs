module BinaryTree
  ( treeInsert
  , treeElem
  , toTree
  ) where

data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)
  deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x > a = Node a left (treeInsert x right)
  | otherwise = Node a (treeInsert x left) right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | otherwise = treeElem x right

toTree :: (Ord a) => [a] -> Tree a
toTree = foldr treeInsert EmptyTree
