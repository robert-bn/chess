{-#Language FlexibleContexts#-}
module Main where
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State 
import Control.Monad (join)
import Control.Monad.Trans.Maybe
import Board
import ProductEnum ()
import Move
import Data.Matrix (fromList)
import Text.Read (readMaybe)

-- testEnum :: (Enum a, Bounded a, Eq a, Bounded b, Enum b, Eq b) => (a,b) -> Bool
-- testEnum (x,y) = toEnum (fromEnum (x,y)) == (x,y)

-- printBoard :: Board -> String
-- printBoard :: Matrix BoardSquare -> [[String]]

printBoard :: Board -> IO ()
printBoard = mapM_ putStrLn . showBoard

parseQuery :: String -> Maybe BoardPosition
parseQuery (':':'q':' ':rest) = parsePosition rest
parseQuery _ = Nothing

query :: Board -> BoardPosition -> IO ()
query b p = do
    putStrLn ("Moves at " ++ pShowPosition p ++ ":")
    mapM_ (putStrLn . ("  " ++) . pShowMove) (validMoves p b)

liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe Nothing  = mzero
liftMaybe (Just x) = return x

getMoveOrQuery :: IO (Either Move BoardPosition)
getMoveOrQuery = do
    liftIO (putStr "Move> ")
    moveOrQueryStr <- liftIO getLine
    case (parseMove moveOrQueryStr, parseQuery moveOrQueryStr) of
         (Just x, Nothing) -> return (Left x)
         (Nothing, Just y) -> return (Right y)

-- should also be monadmaybe for handling failed parts - no probably not
interactiveSession :: (MonadIO m, MonadState Board m, MonadPlus m) => m () 
interactiveSession = do
    board <- get
    liftIO (printBoard board)
    -- Could use an Either here with either function
    myMoveOrQuery <- liftIO getMoveOrQuery
    either (handleMove board) (liftIO . query board) myMoveOrQuery
    interactiveSession
    where
        handleMove b m = modify (move m)
   
main :: IO ()
main = do
    runStateT interactiveSession initialBoard
    return ()

testMove = Move {moveToTuple = (Pos {toTuple = (N4,N2)},Pos {toTuple = (N4,N4)})}

unsafeMove :: String -> Move
unsafeMove x = case parseMove x of
                    (Just x) -> x
