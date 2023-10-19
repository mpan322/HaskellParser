module Types where 

-- top level constructs
data Cons = None 
    | Quit
    | Last
    | Access Int
    | Print Eval
    | Eval Eval
    | For Cons Eval Cons [Cons]
    | While Eval [Cons]
    | IfElIf Eval [Cons] Eval [Cons]
    | VarDecl String Eval -- name + value
    deriving Show

-- things which evaluate to a value
data Eval = Val Data
    | Add Eval Eval
    | Mul Eval Eval
    | Sub Eval Eval
    | Var String -- variable name
    | FunCall String [Eval] -- function name + params
    | FunDecl String [String] [Cons] -- function name + param names + internals
    deriving Show

-- data
data Data = Nil 
    | Int Int
    | Real Double
    | String String
    | Bool Bool
    | List [Eval]
    | Tuple [Eval] 
    | Function [String] [Cons] Frame 
    | Error String
    deriving Show

data Variable = V (String, Data) deriving Show -- name, value
getName :: Variable -> String
getName (V pair) = fst pair

makeVar :: String -> Data -> Variable
makeVar name value = V (name, value) 

getValue :: Variable -> Data
getValue (V pair) = snd pair


data BTree a = Leaf | Node a (BTree a) (BTree a) deriving Show
type Frame = BTree Variable

instance Functor BTree where
    -- fmap :: (a -> b) -> BTree a -> BTree b
    fmap f (Node v tl tr) = Node (f v) (fmap f tl) (fmap f tr)
    fmap _ Leaf = Leaf
