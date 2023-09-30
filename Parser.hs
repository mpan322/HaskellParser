module Parser where
import Control.Monad.State
import Control.Monad
import Control.Applicative

data Parser a = P (String -> Maybe (a, String))

run :: Parser a -> String -> Maybe (a, String)
run (P f) s = f s

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f p = do 
        a <- p
        return (f a)

instance Applicative Parser where 
    -- pure :: a -> Parser s a
    pure v = P $ \s -> Just $ (v, s)

    -- (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    (<*>) pf pa = do
        f <- pf
        a <- pa
        return (f a)



instance Monad Parser where
    return = pure

    -- (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    (>>=) p f = P $ \s -> 
        case run p s of
            Just (a, s')    -> run (f a) s'
            Nothing         -> Nothing

instance MonadPlus Parser where
    -- mzero :: Parser a
    mzero = P $ \s -> Nothing

    -- mplus :: Parser a -> Parser a -> Parser a
    mplus p q = P $ \s -> 
        case (run p s) of
            Nothing -> run q s
            Just va -> Just va




instance Alternative Parser where 
    empty = mzero

    -- (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) = mplus

    -- many :: Parser a -> Parser [a]
    many p = some p ||| return []

    some p = do
        v <- p
        vs <- many p
        return (v:vs)
 
get :: Parser String
get = P $ \s -> Just $ (s, s)

put :: String -> Parser ()
put s = P $ \_ -> Just $ ((), s)

(|||) :: Parser a -> Parser a -> Parser a
(|||) = (<|>)