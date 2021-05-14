module BSTSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck
import Test.QuickCheck.Function

import Data.Function ( on )
import Data.List

import BST
import Numeric.Natural

-- key generated must be comparable as per the BST interface, so K must derive Ord
instance (Ord k, Arbitrary k, Arbitrary i) => Arbitrary (BST k i) where
    arbitrary = sized $ \n -> do -- sized stops a recursive infinite loop of children.
        testing <- listOf arbitrary -- produce list of arbitrary values which are ordered
        -- uncurry applies a given function to a list of pairs. flip reverses the argument for the purpose of the foldl function.
        return (foldl (flip $ uncurry BST.insert) Leaf testing) -- applying the insert function to each element of the list of pairs

-- prop_lookup_tree :: Int -> BST Int Int -> Bool
-- prop_lookup_tree passedKey tree@Leaf = Nothing == BST.lookup passedKey tree
-- prop_lookup_tree passedKey tree@(InternalNode _ value _ _) = Just value == BST.lookup passedKey tree

populateBSTFromPairs :: (Ord k) => [(k, v)] -> BST k v
-- fold R to respect behavior of nubBy going from L -> R
-- populateBSTFromPairs = foldl (flip $ uncurry BST.insert) Leaf  -- applying the insert function to each element of the list of pairs
populateBSTFromPairs = foldr (\(key, value) tree -> BST.insert key value tree) Leaf


test_insert_with_same_key :: Assertion
test_insert_with_same_key =
    let key = 10 in
    let newValue = 13 in
    let tree = InternalNode key 11 Leaf Leaf in
        assertEqual "" (Just newValue) (BST.lookup key (BST.insert key newValue tree))

test_insert_with_same_key_at_deeper_level :: Assertion
test_insert_with_same_key_at_deeper_level =
    let key = 10 in
    let newValue = 13 in
    let tree = InternalNode (key -1) 11 Leaf (InternalNode key 12 Leaf Leaf) in
        assertEqual "" (Just newValue) (BST.lookup key (BST.insert key newValue tree))

test_insert_into_leaf :: Assertion
test_insert_into_leaf =
    let key = 1 in
    let value = 1 in
    let tree = Leaf in
        assertEqual "" (Just value) (BST.lookup key (BST.insert key value Leaf))

test_insert_into_leaf_with_less_key :: Assertion
test_insert_into_leaf_with_less_key =
    let key = 2 in
    let value = 1 in
    let tree = InternalNode key value Leaf Leaf in
        assertEqual "" (Just value) (BST.lookup key (BST.insert (key - 1) (value - 1) tree))

test_insert_into_leaf_with_greater_key :: Assertion
test_insert_into_leaf_with_greater_key =
    let key = 2 in
    let value = 1 in
    let tree = InternalNode key value Leaf Leaf in
        assertEqual "" (Just value) (BST.lookup key (BST.insert (key + 1) (value + 1) tree))

test_insert_new_node_increments_size_by_one :: Assertion 
test_insert_new_node_increments_size_by_one =
    let key = 2 in
    let value = 1 in
    let tree = InternalNode key value Leaf Leaf in
        assertEqual "" (2 :: Int) (BST.size (BST.insert (key + 1) value tree))
 

test_insert_into_existing_node_does_not_increment_size :: Assertion 
test_insert_into_existing_node_does_not_increment_size = 
    let key = 2 in
    let value = 1 in
    let tree = InternalNode key value Leaf Leaf in
        assertEqual "" (1 :: Int) (BST.size (BST.insert key value tree))

prop_insert :: Int -> Int -> BST Int Int -> Bool
prop_insert passedKey passedValue tree = Just passedValue == BST.lookup passedKey (BST.insert passedKey passedValue tree)

prop_test_removal :: Int -> Int -> BST Int Int -> Bool
prop_test_removal keyToRemove value tree = do 
    let modifiedTree = BST.insert keyToRemove value tree
    Nothing == BST.lookup keyToRemove (BST.remove keyToRemove modifiedTree)

prop_test_remove_node :: BST Int Int -> Bool
prop_test_remove_node Leaf = True -- removal of leaf has no effect.
prop_test_remove_node tree@(InternalNode key value _ _) = do 
    let modifiedTree = BST.insert key value tree
    Nothing == BST.lookup key (BST.removeNode modifiedTree)

prop_test_remove_if :: Fun Int Bool -> [(Int, Int)] -> Bool
prop_test_remove_if (Fn predicate) pairs = 
    let distinctPairs = nubBy ((==) `on` fst) pairs in
    let appliedPairs = filter (not . (\(x, _) -> predicate x)) distinctPairs in -- we are interested in the pairs which remain after the predicate has been applied
    let sortedPairs = sortOn fst appliedPairs in -- sort the pairs in the order expected from
        sortedPairs == BST.to_list (BST.removeIf predicate (populateBSTFromPairs pairs))

prop_test_to_list :: [(Int, Int)] -> Bool
-- REFERENCE: https://stackoverflow.com/questions/48254995/remove-duplicates-of-pairs-in-a-list - sorting list
prop_test_to_list pairs = let distinctPairs = nubBy ((==) `on` fst) pairs in
    let sortedPairs = sortOn fst distinctPairs in
        sortedPairs == BST.to_list (populateBSTFromPairs pairs)
-- END OF REFERENCE

prop_test_equality :: [(Int, Int)] -> Bool
prop_test_equality pairs = populateBSTFromPairs pairs == populateBSTFromPairs pairs

prop_test_inequality :: Int -> Int -> [(Int, Int)] -> Bool 
prop_test_inequality extraKey extraValue pairs = do
    let tree = populateBSTFromPairs pairs in
        -- inequality check needs to not insert an existing key.
     BST.lookup extraKey tree /= Nothing || populateBSTFromPairs pairs /= BST.insert extraKey extraValue (populateBSTFromPairs pairs) 

prop_size :: [(Int, Int)] -> Bool
prop_size pairs = do
    let tree = populateBSTFromPairs pairs in
        let expectedSize = length (nubBy ((==) `on` fst) pairs) in
            expectedSize == BST.size tree

bstTests :: TestTree
bstTests = testGroup "BST Test Suite" [
    testCase "test insert with same key" test_insert_with_same_key,
    testCase "test deeper insert with same key" test_insert_with_same_key_at_deeper_level,
    testCase "test insert into leaf" test_insert_into_leaf,
    testCase "test insert into leaf with key less than previous" test_insert_into_leaf_with_less_key,
    testCase "test insert into tree with key greater than previous" test_insert_into_leaf_with_greater_key,
    testCase "test insert into tree increments size by one" test_insert_new_node_increments_size_by_one,
    testCase "test insert into existing node does not increment size" test_insert_into_existing_node_does_not_increment_size,
    testProperty "test insert" prop_insert,
    testProperty "test tree can have nodes removed" prop_test_removal,
    testProperty "test internal remove node" prop_test_remove_node,
    testProperty "test to list function" prop_test_to_list,
    testProperty "test remove if function" prop_test_remove_if,
    testProperty "test equality" prop_test_equality,
    testProperty "test inequality" prop_test_inequality,
    testProperty "test size" prop_size
    ]
