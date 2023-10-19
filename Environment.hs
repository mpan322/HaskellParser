module Environment where
import Types
import Control.Monad
import Control.Applicative
import Zipper
import Control.Monad.Trans.State.Lazy



instance Eq Variable where
    (==) e1 e2 = (getName e1) == (getName e2)

instance Ord Variable where
    compare e1 e2 = compare (getName e1) (getName e2) 

type Env m a = ZipperT Frame m a

-- initEnv :: Env m a
-- initEnv = state (Leaf, [], [])

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

findBTree :: BTree Variable -> String -> Maybe Variable
findBTree Leaf _ = Nothing
findBTree (Node elem tl tr) name =
    case compare name (getName elem) of
        LT -> findBTree tl name
        GT -> findBTree tr name
        EQ -> Just elem

-- addFrame :: Env m ()
-- addFrame = do
--     pushRight Leaf

-- find :: String -> Env Maybe Variable
-- find name = do 
--     out <- do 
--         f <- focus
--         return $ findBTree f name
--         `alt`
--         do
--             right
--             b <- isRightMost
--             if not b then do
--                 find name
--             else 
--                 return Nothing
--     setLeft
--     return out

-- add :: Variable -> Env Maybe ()
-- add elem = lift $ do
--     f <- focus
--     let out = addBTree f elem
--     case out of
--         Nothing -> return Nothing
--         Just ne -> do
--             replace ne
--             return $ Just ()
            
