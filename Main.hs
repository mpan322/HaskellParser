module Main where
import Parsers
import Parser
import Environment

main :: IO ()
main = do

    print "Testing String"
    print $ run string "\"HELLO\""

    print "Testing Integer"
    print $ run integer "1101010"
    print $ run integer "1"
    print $ run integer "-12021"
    print $ run integer "a"
    print $ run integer "aaaaaa"
    print $ run integer ""

    print "Testing Naturals"
    print $ run natural "121121"
    print $ run natural "1"
    print $ run natural "-121121"
    print $ run natural "a"
    print $ run natural "aaaaa"
    print $ run natural ""

    print "Testing Reals"
    print $ run real "1.1"
    print $ run real "1.0000"
    print $ run real "1111"
    print $ run real "1."

    print "Testing Lists"
    print $ run pList "[1, 2, 3, 4]"
    print $ run pList "[1, 2, 3, 4,]"
    print $ run pList "[1]"
    print $ run pList "[]"
    print $ run pList "[1, 2, 3 4,]"

    print "Testing Tuple"
    print $ run pTuple "(1, 2, 3, 4 * 5)"

    print "Testing Function Decl"
    print $ run pFunDecl "fn main(a, b, c, s) { print(a); x = a + b; }"
    print $ run pFunDecl "fn main() {}"

    print "Testing Function Call"
    print $ run pFunCall "main(a + 2 * 5, b, c, s)"
    print $ run pFunCall "main()"


newtype EnvT m a = EnvT { runEnvT :: m (Env a) }

instance Functor (EnvT m) where
    fmap env = do

instance Monad (EnvT m) where
    return = pure

    (>>=) EnvT m a -> (a -> EnvT m b) -> EnvT m b 
    (>>=) env f = EnvT $ do 
        value <- runEnvT env
        



testEnv :: Env [Frame]
testEnv = do
    addFrame
    Environment.get