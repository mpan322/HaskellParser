module Runners where

import Types
import Environment
import REPLState
import Control.Applicative


-- binary :: Monad m =>  BinaryOp -> Eval -> Eval -> Env m Data
-- binary f a b = do
--     va <- eval a
--     vb <- eval b 
--     return (f va vb)



type BinaryOp = Data -> Data -> Data
addF :: BinaryOp
addF (Int a) (Int b) = Int (a + b) 
mulF :: BinaryOp
mulF (Int a) (Int b) = Int (a * b)
subF :: BinaryOp
subF (Int a) (Int b) = Int (a - b)



-- badd = binary addF
-- bmul = binary mulF
-- bsub = binary subF
-- bdiv = binary (/)
-- bpow = binary (^)
-- bmod = binary (%)

-- process :: REPLState IO -> Cons -> REPLState IO
-- process state None = do
--     print "Error, unable to process command"
-- process state (Print str) = do
--     print str
-- process state (VarDecl name value) = do
--     process state value
--     let var = makeVar name val
--     return $ do
--         add var
--     print "OK"
-- process state (Eval ev) = do
--     print $ eval ev
    -- env = runZipper (eval ev) env

-- process state Last = do
    -- process state (Access $ len - 1)
    -- where
    --     len = stackLen $ hist state
-- process state (Access n) = do
--     let out = runStack (do
--         hist state
--         len <- stackLeng
--         (c:cs) <- loopN (len - n) pop
--         loopN (len - n) (push (c:cs))
--         return c
--         ) []
--     case out of
--         Nothing -> do
--             print "Error, no command exists"
--         Just co -> do
--             process state co


-- eval :: (Monad m) => Eval -> Env m Data
-- eval (Add a b) = badd a b
-- eval (Sub a b) = bsub a b
-- eval (Mul a b) = bmul a b
-- eval (Var str) = do
--     elem <- find str
--     case elem of
--         Nothing -> return $ Error "Error no variable exists."
--         Just ou -> return $ getValue ou

