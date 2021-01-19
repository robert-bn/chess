{-#Language GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts#-}
module Board where
import Data.Matrix ( Matrix, (!), fromList, mapPos, toLists )
import Data.Traversable
import Piece ( prettyPiece, CPiece, colourIs, Colour )
import ProductEnum()  -- Needed for Enum instances for product
import Control.Monad (guard, join)
import Control.Applicative (liftA2)
import Data.Maybe ( isNothing, isJust )
import Data.Char
import Data.Maybe (fromMaybe)
import Test.QuickCheck

newtype BoardSquare        = Square { squareContents :: Maybe CPiece }
    deriving (Show, Eq, Enum)
    
newtype Board              = Board { asMatrix :: Matrix BoardSquare }
    deriving (Show, Eq)

data N8 = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8
    deriving (Show, Eq, Enum, Bounded, Ord)

asDigit :: N8 -> Char
asDigit x = show x !! 1

asAlpha :: N8 -> Char
asAlpha N1 = 'a'
asAlpha N2 = 'b'
asAlpha N3 = 'c'
asAlpha N4 = 'd'
asAlpha N5 = 'e'
asAlpha N6 = 'f'
asAlpha N7 = 'g'
asAlpha N8 = 'h'


type BoardPosition = BoardPosition' N8
    
newtype BoardPosition' a = Pos { toTuple :: (a,a) }
    deriving (Eq, Enum, Bounded, Show)
    
bimap' :: (a -> b) -> (a -> b) -> BoardPosition' a -> BoardPosition' b
bimap' f g (Pos (x,y)) = Pos (f x, g y)

xCoord :: BoardPosition' a -> a
xCoord = fst . toTuple

yCoord :: BoardPosition' a -> a
yCoord = snd . toTuple

instance Functor BoardPosition' where
    fmap f (Pos (x,y)) = Pos ((f x), (f y))
    
instance Foldable BoardPosition' where
    foldMap f (Pos (x,y)) = f x <> f y

instance Traversable BoardPosition' where
    traverse f (Pos (x,y)) = liftA2 (curry Pos) (f x) (f y)
                   
isEmpty :: BoardSquare -> Bool
isEmpty = isNothing . squareContents

fromTuple :: (N8,N8) -> BoardPosition
fromTuple = Pos

emptySquare :: BoardSquare
emptySquare = Square Nothing

safePred :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safePred x | x /= minBound = return (pred x)
           | otherwise     = Nothing

safeSucc :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc x | x /= maxBound = return (succ x)
           | otherwise     = Nothing

-- Attempts to get a board square given a board position, on failure
-- (i.e. because position not on board) return Nothing.
getSquare :: Board -> BoardPosition -> BoardSquare
getSquare b p = (asMatrix b) ! (asIndex p)
    where
        asIndex :: BoardPosition -> (Int,Int)
        asIndex (Pos (x,y)) = (8 - fromEnum y, 1 + fromEnum x) -- Matrices in haskell have indices starting at 1

boardMap :: (BoardPosition -> BoardSquare -> BoardSquare) -> Board -> Board
boardMap f b = Board (mapPos (f . fromIndex) (asMatrix b))
    where
        fromIndex :: (Int,Int) -> BoardPosition
        fromIndex (x,y) = Pos (toEnum (y - 1), toEnum (8 - x))

{-
assertEquivalence :: Bool
assertEquivalence = all (\p -> p == fromIndex (asIndex p)) [minBound..maxBound]
-}

-- Clear a square from the board
clearSquare :: Board -> BoardPosition -> Board
clearSquare b p = boardMap remove b
    where
        remove p' s | p == p'   = emptySquare
                    | otherwise = s
                    
shiftPiece :: (BoardPosition, BoardPosition) -> Board -> BoardPosition -> BoardSquare -> BoardSquare
shiftPiece (f,t) b' p s | p == t     = getSquare b' f  -- replace square piece is moving to with old square
                        | p == f     = emptySquare     -- clear square piece is moving from
                        | otherwise  = s               -- otherwise leave unchanged

-- Checks if a piece of a certain colour can take a piece at a square
-- This ONLY checks that the colours are different, it doesn't take
-- anything else into account.
-- When square is empty, it returns False
canTake :: Colour -> Board -> BoardPosition -> Bool
canTake c b p = fromMaybe False $ do
    piece  <- squareContents (getSquare b p)
    return (colourIs (/= c) piece)

unoccupied :: Board -> BoardPosition -> Bool
unoccupied b p = isEmpty (getSquare b p)


occupied :: Board -> BoardPosition -> Bool
occupied b = not . unoccupied b
                                                    
initialBoard :: Board
initialBoard = Board . fromList 8 8 $ map toEnum [  9,11,10, 8, 7,10,11, 9
                                                 , 12,12,12,12,12,12,12,12
                                                 ,  0, 0, 0, 6, 0, 0, 0, 0
                                                 ,  0, 0, 0, 0, 0, 0, 0, 0
                                                 ,  0, 0, 0, 0, 0, 0, 0, 0
                                                 ,  0, 0, 0, 0, 0, 0, 0, 0
                                                 ,  6, 6, 6, 6, 6, 6, 6, 6
                                                 ,  3, 5, 4, 2, 1, 4, 5, 3
                                                 ]         


        

-- TODO: Refactor this to use boardMap instead of mapPos
showBoard :: Board -> [String]
showBoard = (["  A  B  C  D  E  F  G  H"] ++)
                . zipWith (++) (map show [8,7..1])
                . map ((++ bgClear) . concat)
                . toLists 
                . mapPos colourSquare
                . fmap (maybe " " prettyPiece . squareContents)
                . asMatrix
    where   
        colourSquare :: (Int, Int) -> String -> String
        colourSquare (x,y) c | even (x+y) = bgBlack ++ " " ++ c ++ " "
                             | otherwise  = bgRed   ++ " " ++ c ++ " "

        bgBlack, bgRed, bgClear :: String
        bgBlack = "\x1b[100m" 
        bgRed   = "\x1b[41m"  
        bgClear = "\x1b[49m"


parsePosition :: String -> Maybe BoardPosition
parsePosition [x,y] = do
    let alphaRange = foldMap (`zip` [minBound..maxBound]) [['A'..'H'], ['a'..'h']]
    let digitRange = zip ['1'..'8'] [minBound..maxBound]
    -- Make sure input is valid
    x'  <- lookup x alphaRange
    y'  <- lookup y digitRange
    return (fromTuple (x',y'))
    
parsePosition _ = Nothing

pShowPosition :: BoardPosition -> String
pShowPosition (Pos (x,y)) = [asAlpha x,asDigit y]
    

    
