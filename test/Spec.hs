import BSTSpec (bstTests)
import DictionarySpec (dictionaryTests)

import Test.Tasty

tests :: TestTree
tests = testGroup "Entire Test Suite" [bstTests, dictionaryTests]

main :: IO ()
main = defaultMain tests