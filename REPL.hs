module REPL where

import Environment
import Parser
import Parsers
import Types
import Runners
import REPLState

-- repl :: REPLState IO -> IO ()
-- repl state = do
--     putStr (show (fst $ runZipper help (None, [], [])) ++ " > ")
--     input <- getLine
--     case run program input of
--         Nothing         -> do
--             putStrLn "Parse error"
--             repl state
--         Just (Quit, "") -> do
--             print "Exiting repl"
--         Just (cmd, "")  -> do
--             print $ "Running Command: " ++ (show cmd)
--             process state cmd
--             repl state

--     where 
--         help = hist state >>= \_ -> lenZip


-- start :: IO ()
-- start = do
--     print "Starting REPL..."
--     repl initState