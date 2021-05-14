module Dictionary (lookup, insert, remove, removeIf, to_list, Dictionary, emptyDictionary) where
import Prelude hiding (lookup)
import BST 

type Dictionary = BST

emptyDictionary :: Dictionary k i
emptyDictionary = Leaf