module Environment where
import Types
import Control.Monad
import Control.Applicative

data BTree a = Leaf | Node a (BTree a) (BTree a) deriving Show

instance Functor BTree where
    -- fmap :: (a -> b) -> BTree a -> BTree b
    fmap f (Node v tl tr) = Node (f v) (fmap f tl) (fmap f tr)
    fmap _ Leaf = Leaf

addBTree :: Ord a => BTree a -> a -> Maybe (BTree a)
addBTree Leaf v = Just $ Node v Leaf Leaf
addBTree (Node e tl tr) v = 
    case compare v e of
        LT  -> do
            res <- addBTree tl v 
            return $ Node e res tr
        GT  -> do 
            res <- addBTree tr v
            return $ Node e tl res
        EQ -> Nothing


set :: Ord a => BTree a -> a -> BTree a
set Leaf v = Node v Leaf Leaf
set (Node e tl tr) v = 
    case compare v e of
        LT -> Node e (set tl v) tr
        GT -> Node e tl (set tr v)
        EQ -> Node v tl tr


type Variable = (String, Data) -- name, value
type Function = (String, [String], [Cons], [BTree Elem]) -- name, params, commands, closure bindings 
data Elem = V Variable | F Function deriving Show

getName :: Elem -> String
getName (V (name, _)) = name
getName (F (name, _, _, _)) = name

instance Eq Elem where
    (==) e1 e2 = (getName e1) == (getName e2)

instance Ord Elem where
    compare e1 e2 = compare (getName e1) (getName e2) 

type Frame = BTree Elem
type Env = Stack Frame 

newtype Stack s b = Stack { runStack :: [s] -> (b, [s]) }
instance Functor (Stack s) where
    fmap f stack = do
        a <- stack
        return (f a)
        

instance Applicative (Stack s) where
    pure v = Stack $ \s -> (v, [])

    -- (<*>) Stack s (a -> b) -> Stack s a -> Stack s b
    (<*>) stackf stacka = do
        f <- stackf
        a <- stacka
        return (f a)

instance Monad (Stack s) where
    return = pure 

    -- (>>=) Stack s a -> (a -> Stack s b) -> Stack s b 
    (>>=) stack f = Stack $ \s -> 
        let (v, s') = runStack stack s in
        runStack (f v) s'


alt :: Stack s (Maybe a) -> Stack s (Maybe a) -> Stack s (Maybe a) 
alt s1 s2 = do
    o <- s1
    case o of
        Nothing -> s2
        output  -> return output

push :: s -> Stack s ()
push e = Stack $ \ss -> ((), e:ss)

mPush :: Maybe s -> Stack s ()
mPush mtop = do 
    case mtop of
        Nothing  -> return ()
        Just top -> push top
    
pop :: Stack s (Maybe s)
pop = Stack $ \ss -> 
    case ss of
        []      -> (Nothing, ss)
        (s:rs)  -> (Just s, rs)

get :: Stack s [s] 
get = Stack $ \s -> (s, s)

peek :: Stack s (Maybe s)
peek = Stack $ \ss -> 
    case ss of
        []      -> (Nothing, ss)
        (s:rs)  -> (Just s, s:rs)

findBTree :: BTree Elem -> String -> Maybe Elem
findBTree Leaf _ = Nothing
findBTree (Node elem tl tr) name =
    case compare name (getName elem) of
        LT -> findBTree tl name
        GT -> findBTree tr name
        EQ -> Just elem

addFrame :: Env ()
addFrame = do
    push Leaf
    return ()

find :: String -> Env (Maybe Elem)
find name = do
    mtop <- pop
    case mtop of
        Nothing  -> return Nothing -- implies reached top of stack
        Just top -> do
            out <- do -- search this frame, if it fails move to the next
                return $ findBTree top name
                `alt`
                find name
            push top -- put back the top of the stack
            return out


add :: Elem -> Env (Maybe ())
add elem = do
    mtop <- pop
    case mtop of
        Nothing -> return Nothing
        Just top -> do
            case addBTree top elem of
                Nothing -> do 
                    out <- add elem
                    push top
                    return out
                output  -> do
                    push top
                    return $ Just ()


            
