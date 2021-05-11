module BSTSpec where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import BST

-- key generated must be comparable as per the BST interface, so K must derive Ord
instance (Ord k, Arbitrary k, Arbitrary i) => Arbitrary (BST k i) where
    arbitrary = sized $ \n -> do -- sized stops a recursive infinite loop of children.
        testing <- listOf arbitrary -- produce list of arbitrary values which are ordered
        -- uncurry applies a given function to a list of pairs. flip reverses the argument for the purpose of the foldl function.
        return (foldl (flip $ uncurry BST.insert) Leaf testing) -- applying the insert function to each element of the list of pairs

-- prop_lookup_tree :: Int -> BST Int Int -> Bool
-- prop_lookup_tree passedKey tree@Leaf = Nothing == BST.lookup passedKey tree
-- prop_lookup_tree passedKey tree@(InternalNode _ value _ _) = Just value == BST.lookup passedKey tree

prop_insert :: Int -> Int -> BST Int Int -> Bool
prop_insert passedKey passedValue tree = Just passedValue == BST.lookup passedKey (BST.insert passedKey passedValue tree)

prop_test_removal :: Int -> BST Int Int -> Bool
prop_test_removal keyToRemove tree = Nothing == BST.lookup keyToRemove (BST.remove keyToRemove tree)

prop_test_remove_node :: BST Int Int -> Bool
prop_test_remove_node Leaf = True -- removal of leaf has no effect.
prop_test_remove_node tree@(InternalNode key _ _ _) = Nothing == BST.lookup key (BST.removeNode tree)


bstTests :: TestTree
bstTests = testGroup "BST Test Suite" [
    testProperty "test insert" prop_insert,
    testProperty "test tree can have nodes removed" prop_test_removal,
    testProperty "test internal remove node" prop_test_remove_node
    ]
