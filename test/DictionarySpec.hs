{-# LANGUAGE TypeSynonymInstances #-}

module DictionarySpec where
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Dictionary

instance (Ord k, Arbitrary k, Arbitrary i) => Arbitrary (Dictionary k i) where
    arbitrary = sized $ \n -> do -- sized stops a recursive infinite loop of children.
        testing <- listOf arbitrary -- produce list of arbitrary values which are ordered
        -- uncurry applies a given function to a list of pairs. flip reverses the argument for the purpose of the foldl function.
        return (foldl (flip $ uncurry Dictionary.insert) emptyDictionary testing) -- applying the insert function to each element of the list of pairs

prop_dictionary_insert :: String -> String -> Dictionary String String -> Bool
prop_dictionary_insert passedKey passedValue dict = Just passedValue == Dictionary.lookup passedKey (Dictionary.insert passedKey passedValue dict)


dictionaryTests :: TestTree 
dictionaryTests = testGroup "Dictionary Type Tests" [
        testProperty "test insert for dictionary" prop_dictionary_insert
        ]
