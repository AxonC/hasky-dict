module BST where

import Prelude hiding (lookup)

data BST = InternalNode Int String BST BST
                | Leaf

insert :: Int -> String -> BST -> BST
insert key value Leaf = InternalNode key value Leaf Leaf
insert key value (InternalNode currentKey currentValue leftChild rightChild)
  -- handle creating a new node with given key and value.
  | key == currentKey = InternalNode currentKey value leftChild rightChild
  -- left child traversal
  | key < currentKey =  InternalNode currentKey value (insert key value leftChild) rightChild 
  -- right child traversal
  | key > currentKey = InternalNode currentKey value leftChild (insert key value rightChild) 

lookup :: Int -> BST -> Maybe String
lookup soughtKey Leaf = Nothing
lookup soughtKey (InternalNode key item leftChild rightChild)
  | soughtKey < key = lookup soughtKey leftChild
  | soughtKey > key = lookup soughtKey rightChild
  | otherwise = Just item -- Just represents optional value outlined by Maybe modifier.

remove :: Int -> BST -> BST
remove keyToRemove Leaf = Leaf
remove keyToRemove node@(InternalNode key item leftChild rightChild)
  | keyToRemove < key = InternalNode key item (remove keyToRemove leftChild) rightChild
  | keyToRemove > key = InternalNode key item leftChild (remove keyToRemove rightChild)
  | otherwise = removeNode node

removeNode :: BST -> BST
removeNode (InternalNode _ _ Leaf rightChild) = rightChild
removeNode (InternalNode _ _ leftChild Leaf) = leftChild
removeNode node@(InternalNode _ _ leftChild rightChild) =
  let (InternalNode minKey minItem minLeftChild minRightChild) = findMinimumNode rightChild in
    InternalNode minKey minItem minLeftChild (remove minKey minRightChild)

findMinimumNode :: BST -> BST
findMinimumNode node@(InternalNode key _ Leaf _) = node
findMinimumNode node@(InternalNode key _ leftChild _) = findMinimumNode leftChild

isLeaf :: BST -> Bool
isLeaf Leaf = True
isLeaf InternalNode {} = False

exampleTree :: BST
exampleTree = InternalNode 10 "New Key" (InternalNode 2 "Bad Key" (InternalNode 1 "Key 1" Leaf Leaf) Leaf) Leaf

main :: IO ()
main = do
  print (BST.isLeaf Leaf)
  let tree2 = BST.remove 1 exampleTree
  print (BST.lookup 1 tree2)
  let tree3 = BST.insert 5 "Test Key 2" tree2
  print (BST.lookup 5 tree3)
  let tree4 = BST.insert 4 "Test Key 3" tree3
  print (BST.lookup 4 tree4)
  let tree5 = BST.insert 4 "Test Key 6" tree4
  print (BST.lookup 4 tree5)