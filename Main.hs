module Main where
import Parsers
import Parser hiding(get, put)
import Environment
import REPL

import Zipper
import Control.Monad.Trans
import Control.Monad
import Debug.Trace
import Types
import System.IO.Unsafe
import Control.Monad.Trans.State.Lazy

main :: IO ()
main = do
    print "Testing String"
--     print $ run string "\"HELLO\""

--     print "Testing Integer"
--     print $ run integer "1101010"
--     print $ run integer "1"
--     print $ run integer "-12021"
--     print $ run integer "a"
--     print $ run integer "aaaaaa"
--     print $ run integer ""

--     print "Testing Naturals"
--     print $ run natural "121121"
--     print $ run natural "1"
--     print $ run natural "-121121"
--     print $ run natural "a"
--     print $ run natural "aaaaa"
--     print $ run natural ""

--     print "Testing Reals"
--     print $ run real "1.1"
--     print $ run real "1.0000"
--     print $ run real "1111"
--     print $ run real "1."

--     print "Testing Lists"
--     print $ run pList "[1, 2, 3, 4]"
--     print $ run pList "[1, 2, 3, 4,]"
--     print $ run pList "[1]"
--     print $ run pList "[]"
--     print $ run pList "[1, 2, 3 4,]"

--     print "Testing Tuple"
--     print $ run pTuple "(1, 2, 3, 4 * 5)"

--     print "Testing Function Decl"
--     print $ run pFunDecl "fn main(a, b, c, s) { print(a); x = a + b; }"
--     print $ run pFunDecl "fn main() {}"

--     print "Testing Function Call"
--     print $ run pFunCall "main(a + 2 * 5, b, c, s)"
--     print $ run pFunCall "main()"

testZipper :: ZipperT Int IO ()
testZipper = do
    put (1, [], [])
    liftIO $ putStrLn "asaaaaa"
    return ()


-- testEnv :: IO ()
-- testEnv = do 
--     let env = do
--         addFrame
--         add $ makeVar "a" (Int 7)
--         addFrame
--         add $ makeVar "b" (Int 1)
--         addFrame
--         find "a"
--     print $ runZipper env (Leaf,[],[])

-- -- testStack :: IO ()
-- -- testStack = do
-- --     let s = do
-- --         push 1
-- --         push 2
-- --         pop
-- --         push 3
-- --         peek
-- --         return 2
-- --     print $ runStack s []
