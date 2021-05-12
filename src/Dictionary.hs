module Dictionary (lookup, insert, remove, Dictionary, emptyDictionary) where
import Prelude hiding (lookup)
import BST 

-- new type instead
-- newtype - behaves like a static datatype, proxy functions still required
type Dictionary = BST

emptyDictionary :: Dictionary k i
emptyDictionary = Leaf