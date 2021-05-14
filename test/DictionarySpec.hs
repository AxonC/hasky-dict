{-# LANGUAGE TypeSynonymInstances #-} -- Required to allow for Dictionary to be used as an arbitrary instance.

module DictionarySpec where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Data.Function

import Data.List

import Dictionary

instance (Ord k, Arbitrary k, Arbitrary i) => Arbitrary (Dictionary k i) where
    arbitrary = sized $ \n -> do -- sized stops a recursive infinite loop of children.
        testing <- listOf arbitrary -- produce list of arbitrary values which are ordered
        -- uncurry applies a given function to a list of pairs. flip reverses the argument for the purpose of the foldl function.
        return (foldl (flip $ uncurry Dictionary.insert) emptyDictionary testing) -- applying the insert function to each element of the list of pairs

populateDictionaryFromPairs :: (Ord k) => [(k, v)] -> Dictionary k v
populateDictionaryFromPairs = foldr (\(key, value) tree -> Dictionary.insert key value tree) emptyDictionary 

prop_dictionary_insert :: String -> String -> Dictionary String String -> Bool
prop_dictionary_insert passedKey passedValue dict = Just passedValue == Dictionary.lookup passedKey (Dictionary.insert passedKey passedValue dict)

prop_dictionary_remove :: String -> String -> Dictionary String String -> Bool 
prop_dictionary_remove passedKey passedValue dict = do
    let modifiedTree = Dictionary.insert passedKey passedValue dict
    Nothing == Dictionary.lookup passedKey (Dictionary.remove passedKey modifiedTree)

prop_dictionary_remove_if :: Fun String Bool -> [(String, String)] -> Bool
prop_dictionary_remove_if (Fn predicate) pairs = 
    let distinctPairs = nubBy ((==) `on` fst) pairs in
    let appliedPairs = filter (not . (\(x, _) -> predicate x)) distinctPairs in -- we are interested in the pairs which remain after the predicate has been applied
    let sortedPairs = sortOn fst appliedPairs in -- sort the pairs in the order expected from
        sortedPairs == Dictionary.to_list (Dictionary.removeIf predicate (populateDictionaryFromPairs pairs))

prop_test_to_list :: [(String, String)] -> Bool
-- REFERENCE: https://stackoverflow.com/questions/48254995/remove-duplicates-of-pairs-in-a-list - sorting list
prop_test_to_list pairs = let distinctPairs = nubBy ((==) `on` fst) pairs in
    let sortedPairs = sortOn fst distinctPairs in
        sortedPairs == Dictionary.to_list (populateDictionaryFromPairs pairs)
-- END OF REFERENCE


dictionaryTests :: TestTree 
dictionaryTests = testGroup "Dictionary Type Tests" [
        testProperty "test insert for dictionary" prop_dictionary_insert,
        testProperty "test remove from dictionary" prop_dictionary_remove,
        testProperty "test remove with predicate from dictionary" prop_dictionary_remove_if,
        testProperty "test dictionary to list" prop_test_to_list
        ]
