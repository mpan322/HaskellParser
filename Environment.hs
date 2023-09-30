module Environment where
import Types
import Control.Monad.State

data BTree a = Leaf | Node a (BTree a) (BTree a)

instance Functor BTree where
    -- fmap :: (a -> b) -> BTree a -> BTree b
    fmap f (Node v tl tr) = Node (f v) (fmap f tl) (fmap f tr)
    fmap _ Leaf = Leaf

add :: Ord a => BTree a -> a -> BTree a
add Leaf v = Node v Leaf Leaf
add (Node e tl tr) v = 
    case compare v e of
        LT  -> Node e (add tl v) tr
        _   -> Node e tl (add tr v)

set :: Ord a => BTree a -> a -> BTree a
set Leaf v = Node v Leaf Leaf
set (Node e tl tr) v = 
    case compare v e of
        LT -> Node e (set tl v) tr
        GT -> Node e tl (set tr v)
        EQ -> Node v tl tr


type Variable = (String, Data) -- name, value
type Function = (String, [String], [Cons], [BTree Elem]) -- name, params, commands, closure bindings 
data Elem = V Variable | F Function 

getName :: Elem -> String
getName (V (name, _)) = name
getName (F (name, _, _, _)) = name

instance Eq Elem where
    (==) e1 e2 = (getName e1) == (getName e2)

instance Ord Elem where
    compare e1 e2 = compare (getName e1) (getName e2) 

type Env = State [BTree Elem]


findBTree :: BTree Elem -> String -> Maybe Elem
findBTree Leaf _ = Nothing
findBTree (Node elem tl tr) name =
    case compare name (getName elem) of
        LT -> findBTree tl name
        GT -> findBTree tr name
        EQ -> Just elem

find :: Env a -> String -> Env (Maybe Elem)
find envs name = do
    env <- get envs
    let mElem = findBTree env name
    case mElem of
        Nothing -> find envs name
        _       -> return mElem  