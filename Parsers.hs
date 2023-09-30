module Parsers where
import Parser
import Data.Char
import Control.Applicative
import Types

-- takes the next character from the input stream
failure :: Parser a
failure = empty

nextChar :: Parser Char
nextChar = P $ \s ->
    case s of
        []     -> Nothing
        (x:xs) -> Just $ (x, xs) 

sat :: (Char -> Bool) -> Parser Char
sat f = do 
    v <- nextChar
    if f v then
        return v
    else 
        failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isLower

letter :: Parser Char
letter = sat isAlpha

alphaNum :: Parser Char
alphaNum = sat isAlphaNum 

char :: Char -> Parser Char
char c = sat (== c)


-- helping parsers for language constructs

str :: String -> Parser String
str [] = return []
str (x:xs) = do 
    char x
    str xs
    return (x:xs)

ident :: Parser String
ident = do 
    x <- lower
    xs <- many alphaNum
    return (x:xs)

nat :: Parser Int
nat = do 
    numStr <- some digit
    return (read numStr)

int :: Parser Int
int = do
    char '-'
    n <- nat
    return (-1 * n)
    ||| 
    nat


realH :: Parser Double
realH = do
    n <- int
    char '.'
    d <- nat
    return $ read ((show n) ++ "." ++ (show d))

space ::  Parser ()
space = do
    many $ sat isSpace
    return ()

-- terminals

token :: Parser a -> Parser a
token p = do 
    space
    v <- p
    space 
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

string :: Parser String
string = token $ do
    char '"'
    s <- many $ sat ( /= '"')
    char '"'
    return s 

real :: Parser Double
real = token realH


symbol :: String -> Parser String
symbol xs = token (str xs)

var :: Parser String
var = symbol "var"

symPrint :: Parser String
symPrint = symbol "print"


false :: Parser Bool
false = do
    symbol "false"
    return False

true :: Parser Bool
true = do
    symbol "true"
    return True

wrapped :: String -> String -> Parser a -> Parser a
wrapped open close parser = do
    symbol open
    v <- parser
    symbol close
    return v

paren :: Parser a -> Parser a
paren p = wrapped "(" ")" p

bracket ::  Parser a -> Parser a
bracket p = wrapped "{" "}" p

-- rules


program :: Parser Cons
program = do
    s <- statement
    symbol ";"
    return s

statement :: Parser Cons
statement = do
    name <- identifier
    symbol "="
    e <- expr
    return (VarDecl name e)
    |||
    do 
        symPrint
        e <- paren expr
        return (Print e)

expr :: Parser Eval 
expr = do
    t <- term
    symbol "+"
    e <- expr
    return (Add t e)
    |||
    term

term :: Parser Eval
term = do
    f <- factor
    symbol "*"
    t <- term
    return (Mul f t)
    |||
    factor

factor :: Parser Eval
factor = do
    d <- dataParser
    return (Val d)
    |||
    do 
        i <- identifier
        return (Var i)
    |||
    do 
        e <- paren expr
        return e


dataParser :: Parser Data
dataParser = do
    i <- integer
    return (Int i)
    |||
    do 
        s <- string
        return (String s)
    |||
    do 
        r <- real
        return (Real r)
    |||
    do 
        t <- true
        return (Bool t)
    |||
    do 
        f <- false
        return (Bool f)
    |||
    do 
        ls <- pList
        return (List ls)
    |||
    do 
        ts <- pTuple
        return (Tuple ts)
    


    

pTuple :: Parser [Eval]
pTuple = paren $ pCommaed expr

pList :: Parser [Eval]
pList = wrapped "[" "]" (pCommaed expr)

-- parser for comma seperated list
pCommaed :: Parser a -> Parser [a]
pCommaed p = do
    vs <- many $ do 
        v <- p
        symbol ","
        return v
    do 
        v <- p
        return (vs ++ [v])
        |||
        return vs

pFunDecl :: Parser (String, [String], [Cons])
pFunDecl = do
    symbol "fn"
    name <- identifier
    params <- paren $ pCommaed identifier
    commands <- bracket $ many program
    return (name, params, commands)

pFunCall :: Parser (String, [Eval])
pFunCall = do
    name <- identifier
    params <- paren $ pCommaed expr
    return (name, params)

pWhile :: Parser (Eval, [Cons])
pWhile = do
    symbol "while"
    e <- paren expr
    commands <- bracket $ many program
    return (e, commands) 

pFor :: Parser (Cons, Eval, Cons, [Cons])
pFor = do
    symbol "for"
    (dec, cond, inc) <- paren $ do
        dec <- program
        symbol ";"
        cond <- expr
        symbol ";"
        inc <- program
        return (dec, cond, inc)
    commands <- bracket $ many program
    return (dec, cond, inc, commands)

