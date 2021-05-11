module BST where

import Prelude hiding (lookup)

-- k = key, v = value
data BST k v = InternalNode k v (BST k v) (BST k v)
                | Leaf 
                deriving Show 

-- K must implement a sub-class which has a comparison operator for key comparison.
insert :: (Ord k) => k -> v -> BST k v -> BST k v
insert key value Leaf = InternalNode key value Leaf Leaf
insert key value (InternalNode currentKey currentValue leftChild rightChild)
  -- handle creating a new node with given key and value.
  | key == currentKey = InternalNode currentKey value leftChild rightChild
  -- left child traversal
  | key < currentKey =  InternalNode currentKey value (insert key value leftChild) rightChild 
  -- right child traversal
  | key > currentKey = InternalNode currentKey value leftChild (insert key value rightChild) 

lookup :: (Ord k) => k -> BST k v -> Maybe v 
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode key item leftChild rightChild)
  | soughtKey < key = lookup soughtKey leftChild
  | soughtKey > key = lookup soughtKey rightChild
  | otherwise = Just item -- Just represents optional value outlined by Maybe modifier.

remove :: (Ord k) => k -> BST k v -> BST k v
remove keyToRemove Leaf = Leaf
remove keyToRemove node@(InternalNode key item leftChild rightChild)
  | keyToRemove < key = InternalNode key item (remove keyToRemove leftChild) rightChild
  | keyToRemove > key = InternalNode key item leftChild (remove keyToRemove rightChild)
  | otherwise = removeNode node

removeNode :: (Ord k) => BST k v -> BST k v
removeNode (InternalNode _ _ Leaf rightChild) = rightChild
removeNode (InternalNode _ _ leftChild Leaf) = leftChild
removeNode node@(InternalNode _ _ leftChild rightChild) =
  let (InternalNode minKey minItem minLeftChild minRightChild) = findMinimumNode rightChild in
    InternalNode minKey minItem minLeftChild (remove minKey minRightChild)

findMinimumNode :: BST k v -> BST k v
findMinimumNode node@(InternalNode key _ Leaf _) = node
findMinimumNode node@(InternalNode key _ leftChild _) = findMinimumNode leftChild
