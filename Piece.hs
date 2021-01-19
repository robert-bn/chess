module Piece where
import Data.Char (chr)
import ProductEnum ()

data Colour = White | Black
    deriving (Eq, Enum, Show, Read, Bounded)

data Piece = King
           | Queen
           | Rook
           | Bishop
           | Knight
           | Pawn
    deriving (Eq, Show, Read, Enum, Bounded)

type CPiece = (Piece, Colour)

colourIs :: (Colour -> a) -> CPiece -> a
colourIs f (_,c) = f c

prettyPiece :: CPiece -> String
prettyPiece (p,c) = colour c . chr $ (fromEnum p + 0x265A)
    where
        colour :: Colour -> Char -> String
        colour Black c = fgBlack ++ c : fgClear
        colour White c = fgWhite ++ c : fgClear

        fgBlack, fgWhite :: String
        fgBlack = "\x1b[30m"  
        fgWhite = "\x1b[37m"  
        fgClear = "\x1b[39m"
