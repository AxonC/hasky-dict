module Dictionary where

import Prelude hiding (lookup)

data BST = InternalNode Int String BST BST
                | Leaf


lookup :: Int -> BST -> Maybe String
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode key item leftChild rightChild)
  | soughtKey < key = lookup soughtKey leftChild
  | soughtKey > key = lookup soughtKey rightChild
  | otherwise = Just item