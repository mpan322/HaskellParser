module REPLState where

import Environment
import Zipper
import Types

type History = Zipper Cons Cons
data REPLState m = REPL { env :: Env m Data, hist :: History }

initEnv :: Monad m => Env m Data
initEnv = do 
    -- addFrame
    return Nil

initHist :: History
initHist = return None

initState :: REPLState IO
initState = REPL initEnv initHist
