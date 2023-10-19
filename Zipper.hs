module Zipper where
import Control.Monad.Trans.State.Lazy

-- ZIPPER --

newtype Zipper s a = Zipper { runZipper :: ZipperType s -> (a, ZipperType s) }
type ZipperType s = (s, [s], [s]) 
type ZipperT s m a = StateT (ZipperType s) m a



instance Functor (Zipper s) where
    fmap f zip = do
        a <- zip
        return (f a)

instance Applicative (Zipper s) where
    pure = return

    (<*>) zf za = do
        f <- zf
        a <- za
        return (f a)

instance Monad (Zipper s) where
    return v = Zipper $ \s -> (v, s)

    (>>=) zip f = Zipper $ \s -> 
        let (a, newS) = runZipper zip s in
        runZipper (f a) newS


-- -- STACK --
-- newtype Stack s a = Stack { runStack :: [s] -> (a, [s]) }
-- instance Functor (Stack s) where
--     fmap f stack = do
--         a <- stack
--         return (f a)
        

-- instance Applicative (Stack s) where
--     pure v = Stack $ \s -> (v, s)

--     -- (<*>) Stack s (a -> b) -> Stack s a -> Stack s b
--     (<*>) stackf stacka = do
--         f <- stackf
--         a <- stacka
--         return (f a)

-- instance Monad (Stack s) where
--     return = pure 

--     -- (>>=) Stack s a -> (a -> Stack s b) -> Stack s b 
--     (>>=) stack f = Stack $ \s -> 
--         let (v, s') = runStack stack s in
--         runStack (f v) s'


-- move the zipper left and right
left :: Zipper s (Maybe s)
left = Zipper $ \(s, m, rs) -> 
    case m of
        (l:ls)  -> (Just l, (l, ls, s:rs))
        []      -> (Nothing, (s, [], rs))

right :: Zipper s (Maybe s)
right = Zipper $ \(s, ls, m) -> 
    case m of
        (r:rs)  -> (Just r, (r, s:ls, rs))
        []      -> (Nothing, (s, ls, []))

focus :: Zipper s s
focus = Zipper $ \(s, ls, rs) -> (s, (s, ls, rs))





leftT :: Monad m => ZipperT s m (Maybe s)
leftT = do 
    s <- get
    case s of
        (s, l:ls, rs) -> do
            modify $ \q -> (l, ls, s:rs)
            return (Just l) 
        (s, [], rs) -> do
            return Nothing

rightT :: Monad m => ZipperT s m (Maybe s)
rightT = do 
    s <- get
    case s of
        (s, ls, r:rs) -> do
            modify $ \q -> (r, s:ls, rs)
            return (Just r) 
        (s, [], rs) -> do
            return Nothing

focusT :: Monad m => ZipperT s m s
focusT = do
    (s, _, _) <- get
    return s
    

isLeftMostH :: ZipperType s -> Bool
isLeftMostH (_, [], _) = True
isLeftMostH _          = False

isRightMostH :: ZipperType s -> Bool
isRightMostH (_, _, []) = True
isRightMostH _          = False


isLeftMost :: Zipper s Bool
isLeftMost = do
    s <- zget
    return (isLeftMostH s)

isRightMost :: Zipper s Bool
isRightMost = do
    s <- zget
    return (isRightMostH s)

isLeftMostT :: Monad m => ZipperT s m Bool
isLeftMostT = do
    s <- get
    return (isLeftMostH s)

isRightMostT :: Monad m => ZipperT s m Bool
isRightMostT = do
    s <- get
    return (isRightMostH s)


loops :: Zipper s (Maybe a) -> Zipper s [a]
loops zip = do
    z <- zip
    case z of
        Nothing -> return []
        Just v  -> do
            zs <- loops zip
            return (v:zs)

loopsT :: Monad m => ZipperT s m (Maybe a) -> ZipperT s m [a]
loopsT zip = do
    z <- zip
    case z of
        Nothing -> return []
        Just v  -> do
            zs <- loopsT zip
            return (v:zs)


zget :: Zipper s (ZipperType s)
zget = Zipper $ \s -> (s, s)

setLeft :: Zipper s [s]
setLeft = loops left 

setRight :: Zipper s [s]
setRight = loops right 

setLeftT :: Monad m => ZipperT s m [s]
setLeftT = loopsT leftT

setRightT :: Monad m => ZipperT s m [s]
setRightT = loopsT rightT 


insertLeft :: s -> Zipper s ()
insertLeft s = Zipper $ \(v, ls, rs) -> ((), (v, s:ls, rs))

insertRight :: s -> Zipper s ()
insertRight s = Zipper $ \(v, ls, rs) -> ((), (v, ls, s:rs))

replace :: s -> Zipper s s
replace new = Zipper $ \(old, ls, rs) -> (old, (new, ls, rs))

pushLeft :: s -> Zipper s ()
pushLeft s = do
    f <- replace s
    insertLeft f

pushRight :: s -> Zipper s ()
pushRight s = do
    f <- replace s
    insertRight f


replaceT :: Monad m => s -> ZipperT s m s 
replaceT new = do
    f <- focusT
    modify $ \(old, ls, rs) -> (new, ls, rs)
    return f


lenZip :: Zipper s Int
lenZip = do
    (c, ls, rs) <- zget
    return $ (length ls) + (length rs) + 1



-- isEmpty :: Stack s Bool
-- isEmpty = do
--     len <- stackLeng
--     return $ len == 0


-- -- repeat until the stack is empty
-- loop :: Stack s a -> Stack s [a]
-- loop stack = do
--     e <- isEmpty
--     if e then do
--         return []
--     else do
--         o <- stack
--         os <- loop stack
--         return (o:os)

-- -- repeat n times
-- loopN :: Int -> Stack s a -> Stack s [a]
-- loopN n stack = do
--     e <- isEmpty
--     if n == 0 || e then do
--         return []
--     else do
--         o <- stack
--         os <- loopN (n - 1) stack
--         return (o:os)


-- alt :: Stack s (Maybe a) -> Stack s (Maybe a) -> Stack s (Maybe a) 
-- alt s1 s2 = do
--     o <- s1
--     case o of
--         Nothing -> s2
--         output  -> return output

alt :: Zipper s (Maybe a) -> Zipper s (Maybe a) -> Zipper s (Maybe a)
alt z1 z2 = do
    o <- z1
    case o of
        Nothing -> z2
        output  -> return output

-- push :: s -> Stack s ()
-- push e = Stack $ \ss -> ((), e:ss)

-- mPush :: Maybe s -> Stack s ()
-- mPush mtop = do 
--     case mtop of
--         Nothing  -> return ()
--         Just top -> push top
    
-- pop :: Stack s (Maybe s)
-- pop = Stack $ \ss -> 
--     case ss of
--         []      -> (Nothing, ss)
--         (s:rs)  -> (Just s, rs)

-- get :: Stack s [s] 
-- get = Stack $ \s -> (s, s)

-- peek :: Stack s (Maybe s)
-- peek = Stack $ \ss -> 
--     case ss of
--         []      -> (Nothing, ss)
--         (s:rs)  -> (Just s, s:rs)

-- stackLeng :: Stack s Int
-- stackLeng = do
--     s <- get
--     return $ length s


-- stackLen :: Stack s a -> Int
-- stackLen stack = fst $ runStack (do 
--     stack
--     stackLeng) []