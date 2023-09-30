module Types where 

-- top level constructs
data Cons = Print Eval
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
data Data = Int Int
    | Real Double
    | String String
    | Bool Bool
    | List [Eval]
    | Tuple [Eval] 
    deriving Show
