module Lib where

import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- data BST item = Leaf
--                 | InternalNode Int item (BST item) (BST item)
