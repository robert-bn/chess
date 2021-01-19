{-#Language FlexibleContexts, RankNTypes#-}
module Move where
import Data.Maybe (fromMaybe, isJust, maybeToList, catMaybes)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Applicative
import Text.Read (readMaybe)
import Board (BoardPosition, Board, occupied, canTake, bimap', safePred, safeSucc, getSquare, fromTuple, squareContents, clearSquare, parsePosition, shiftPiece, boardMap, pShowPosition)
import Piece
import Data.Matrix ( mapPos )
import Test.QuickCheck

newtype Move   = Move { moveToTuple :: (BoardPosition, BoardPosition) }
    deriving (Eq, Show)
data Environment = Env { getBoard :: Board, getColour :: Colour}
type Mover a = ReaderT Environment (State BoardPosition) a
type Motion = BoardPosition -> BoardPosition
  
evalMover :: Environment -> BoardPosition -> Mover a -> a
evalMover env p m = evalState (runReaderT m env) p

makeMove :: BoardPosition -> BoardPosition -> Move
makeMove x y = Move (x,y)

moveToWhere   :: Move -> BoardPosition
moveToWhere = fst . moveToTuple

moveFromWhere :: Move -> BoardPosition
moveFromWhere = snd . moveToTuple
-- Motions in the board.
-- On fail (i.e. if they attempt to move to a square
-- outside the confines of the board) they return False.
-- Otherwise, update the position and return the 
-- motion :: (BoardPosition -> p (Maybe BoardPosition) (Maybe BoardPosition)) -> Mover Bool
motion :: (BoardPosition -> Maybe BoardPosition) -> Mover Bool
motion f = do
    p <- get
    case (f p) of
         Nothing -> return False
         Just p' -> do { put p'; return True }
--     

north, northEast, east, southEast, south, southWest, west, northWest :: Mover Bool
north      = motion $ sequenceA . bimap' return   safeSucc
northEast  = motion $ sequenceA . bimap' safePred safeSucc
east       = motion $ sequenceA . bimap' safePred return      
southEast  = motion $ sequenceA . bimap' safePred safePred
south      = motion $ sequenceA . bimap' return   safePred
southWest  = motion $ sequenceA . bimap' safeSucc safePred
west       = motion $ sequenceA . bimap' safeSucc return      
northWest  = motion $ sequenceA . bimap' safeSucc safeSucc

andMove :: Alternative f => [Mover Bool] -> Mover (f BoardPosition)
andMove []     = maybeHere canMoveOrTakeS
andMove (x:xs) = do
    continue <- x
    if continue then andMove xs
                else return empty
    
moverMap :: (Colour -> Board -> BoardPosition -> a) -> Mover a
moverMap f = do
    env <- ask
    let c = getColour env
    let b = getBoard  env
    p <- get
    return (f c b p)

satisfy :: (Colour -> Board -> BoardPosition -> Bool) -> Mover Bool
satisfy = moverMap
    
maybeHere :: Alternative f => Mover Bool -> Mover (f BoardPosition)
maybeHere m = do
    b <- m
    p <- get
    return (p <$ (guard b))

occupiedS :: Mover Bool
occupiedS = satisfy (const occupied)

unoccupiedS :: Mover Bool
unoccupiedS = fmap not occupiedS

-- When square is empty, it returns False
canTakeS :: Mover Bool
canTakeS = satisfy canTake

canMoveOrTakeS :: Mover Bool
canMoveOrTakeS = do
    bCanTake    <- canTakeS;
    bUnoccupied <- unoccupiedS;
    case (bUnoccupied,bCanTake) of
         (False,True)  -> return True
         (True,_)      -> return True
         (False,False) -> return False

-- Move along a motion a single time and return its position,
-- but only if we can take an opposing piece
justOnceTake :: Alternative f => Mover a -> Mover (f BoardPosition)
justOnceTake m = do
    m -- do the movement
    maybeHere canTakeS

-- Move along a motion a single time and return its position,
-- but only if the new position is unoccupied
justOnceNoTake :: Alternative f => Mover a -> Mover (f BoardPosition)
justOnceNoTake m = do
    m -- do the movement
    maybeHere unoccupiedS
    
inLineUntilTake :: Mover Bool -> Mover [BoardPosition]
inLineUntilTake m = do
    bSuccess  <- m -- do the movement
    p         <- get
    bCanTake  <- canTakeS
    bOccupied <- occupiedS
    rest <- inLineUntilTake m
    case (bSuccess,bOccupied,bCanTake) of
         (True,True,True)  -> return [p]
         (True,False,_)    -> return (p:rest)
         _ -> return []
    
okayCondition :: Bool -> Bool -> Bool
okayCondition bUnoccupied bCanTake = 
    case (bUnoccupied,bCanTake) of
         (False,True)  -> True
         (True,_)      -> True
         (False,False) -> False

inLineUntilTake' :: Mover a -> Mover [BoardPosition]
inLineUntilTake' m = fmap catMaybes $ whileM unoccupiedS (do {
        m;
        maybeHere canMoveOrTakeS
    })
 

-- Move across the board in a direction (no taking)
motionNoTake :: Motion -> Mover (Maybe BoardPosition)
motionNoTake next =
    do  
        modify next
        maybeHere unoccupiedS
    

justOnce :: Mover (Maybe a) -> Mover [a]
justOnce m  = fmap maybeToList m

choice :: Foldable t => t (Mover [a]) -> Mover [a]
choice = foldl choice' (return [])
    where
        choice' m m' = do
            p <- get
            xs <- m
            put p
            xs' <- m'
            return (xs ++ xs')

validMoves :: BoardPosition -> Board -> [Move]
validMoves p b = maybe mempty findMoves (squareContents (getSquare b p))
    where
        findMoves q@(_,c) = let
                                b'       = clearSquare b p
                                env      = Env { getBoard  = b
                                               , getColour = c
                                               }
                                validTos = evalMover env p (validMovesS q)
                            in
                                map (makeMove p) validTos


validMovesS :: (Piece, Colour) -> Mover [BoardPosition]
validMovesS (Pawn, White) = choice [ justOnceNoTake north           -- | Pawns can move forward one, or take diagonally.
                                   , justOnceTake   northWest       -- | In addition, they have two additional special moves
                                   , justOnceTake   northEast       -- | which can be used at certain times, which are.
                                   ]                                -- |    1. En passant  (see https://en.wikipedia.org/wiki/En_passant)
                                                                    -- |    2. Move foward 2 squares if they have not yet moved in a game
validMovesS (Pawn, Black) = choice [ justOnceNoTake south
                                   , justOnceTake   southWest
                                   , justOnceTake   southEast
                                   ]
                                   
validMovesS (Queen, _)    = choice [ inLineUntilTake north          -- 1  | With the queen in the centre, each move
                                   , inLineUntilTake east           -- 3  | directions, then repeated
                                   , inLineUntilTake northEast      -- 2  | it can take is in each of these 
                                   , inLineUntilTake southEast      -- 4  | 
                                   , inLineUntilTake south          -- 5  |        8 1 2
                                   , inLineUntilTake southWest      -- 6  |        7 Q 3
                                   , inLineUntilTake west           -- 7  |        6 5 4
                                   , inLineUntilTake northWest      -- 8  |
                                   ]

validMovesS (Rook, _)     = choice [ inLineUntilTake north          -- 1  | With the rook in the centre, each move
                                   , inLineUntilTake east           -- 2  | directions, then repeated
                                   , inLineUntilTake south          -- 3  |          1 
                                   , inLineUntilTake west           -- 4  |        4 R 2 
                                   ]                                --    |          3
                                                                    --    | In addition, there is one special move that rooks can take, which is castling. If the rook in question and the king have not been moved in the game, and there are no pieces in the way, then it's possible to swap the rook and the knight. There are some additional requirements, see: https://en.wikipedia.org/wiki/Castling

validMovesS (Bishop, _)   = choice [ inLineUntilTake northEast      -- 1  | With the bishop in the centre, each move
                                   , inLineUntilTake southEast      -- 2  | directions, then repeated
                                   , inLineUntilTake southWest      -- 3  |        4   1
                                   , inLineUntilTake northWest      -- 4  |          B   
                                   ]                                --    |        3   2

validMovesS (King, _)     = choice [ justOnceTake north             -- 1  | With the king in the centre, each move
                                   , justOnceTake northEast         -- 2  | it can take is in each of these 
                                   , justOnceTake east              -- 3  | directions, but only move one in each direction
                                   , justOnceTake southEast         -- 4  | 
                                   , justOnceTake south             -- 5  |        8 1 2
                                   , justOnceTake southWest         -- 6  |        7 K 3
                                   , justOnceTake west              -- 7  |        6 5 4
                                   , justOnceTake northWest         -- 8  |
                                   ]

validMovesS (Knight, _)   = choice [ andMove [ north , north , west  ] -- 1  | With the knight in the centre, each move
                                   , andMove [ north , north , east  ] -- 2  | it can take is to each of these squares,
                                   , andMove [ east  , east  , north ] -- 3  | and it can jump over any pieces in the way
                                   , andMove [ east  , east  , south ] -- 4  |          
                                   , andMove [ south , south , east  ] -- 5  |           # 1 # 2 #
                                   , andMove [ south , south , west  ] -- 6  |           8 # # # 3
                                   , andMove [ west  , west  , south ] -- 7  |           # # K # #
                                   , andMove [ west  , west  , north ] -- 8  |           7 # # # 4
                                   ]                                   --    |           # 6 # 5 #
                                                                    
isValid :: Board -> Move -> Bool
isValid b m@(Move (from, _)) = m `elem` (validMoves from b)

                                                        
parseMove :: String -> Maybe Move
parseMove [x1,y1,' ',x2,y2] = do
    from <- parsePosition [x1,y1]
    to   <- parsePosition [x2,y2]
    guard (from /= to)
    return (makeMove from to)
parseMove _ = Nothing

pShowMove :: Move -> String
pShowMove m@(Move (f,t)) = pShowPosition f ++ "→" ++ pShowPosition t

move :: Move -> Board -> Board
move m = flip boardMap <*> shiftPiece (moveToTuple m)
