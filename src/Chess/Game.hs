module Chess.Game (Square(..), Coord, Element, Move(..), ChessGame, makeInitialGame)  where

import qualified Data.Array as A
import Data.Array ((//), (!))
import Chess.Piece

data Square = Square Piece | Empty deriving (Show, Eq)

type Coord = (Int, Int)
type Element = (Coord, Square)

-- Move piece fromCoord toCoord
data Move = Move Piece Coord Coord deriving (Show, Eq)

-- A board is a 2D array of squares.  Black starts at the top (first row) and
-- White starts at the bottom.
type Board = A.Array Coord Square

emptyBoard :: Board
emptyBoard = A.listArray ((1, 1), (8, 8)) (repeat Empty)

-- Set a board square to a new value.
setBoardSquare :: Board -> Coord -> Piece -> Board
setBoardSquare board coord piece = board // [(coord, Square piece)]

setBoardSquares :: Board -> [(Coord, Piece)] -> Board
setBoardSquares board assignments =
  board // [(coord, Square piece) | (coord, piece) <- assignments]

boardRow :: Board -> Int -> [Element]
boardRow board rowNum =
  [((rowNum, colNum), board!(rowNum, colNum)) | colNum <- [1..8]]

data ChessGame = ChessGame {

  board :: Board,

  -- Whose turn is next
  nextPlayer :: PieceColor,

  -- Moves played so far, with most recent move at the head of the list
  lastMoves :: [Move]
}

instance Show ChessGame where
    show game = foldl1 (.) (map showRow [1..8]) rowSeparator
        where rowSeparator = "+---+---+---+---+---+---+---+---+\n"
              showRow r    =
                  showString rowSeparator . showChar '|' .
                  foldl1 (.) (map (showSquare.snd) (boardRow (board game) r)) .
                  showChar '\n'
              showSquare (Square piece) =
                showChar ' ' . showChar (pieceToChar piece) . showString " |"
              showSquare Empty = showString "   |"

initialPiecePositions = [
  -- black pieces
  ((1,1), Piece Black Rook),
  ((1,2), Piece Black Knight),
  ((1,3), Piece Black Bishop),
  ((1,4), Piece Black Queen),
  ((1,5), Piece Black King),
  ((1,6), Piece Black Bishop),
  ((1,7), Piece Black Knight),
  ((1,8), Piece Black Rook),

  -- black  pawns
  ((2,1), Piece Black Pawn),
  ((2,2), Piece Black Pawn),
  ((2,3), Piece Black Pawn),
  ((2,4), Piece Black Pawn),
  ((2,5), Piece Black Pawn),
  ((2,6), Piece Black Pawn),
  ((2,7), Piece Black Pawn),
  ((2,8), Piece Black Pawn),

  -- white  pawns
  ((7,1), Piece White Pawn),
  ((7,2), Piece White Pawn),
  ((7,3), Piece White Pawn),
  ((7,4), Piece White Pawn),
  ((7,5), Piece White Pawn),
  ((7,6), Piece White Pawn),
  ((7,7), Piece White Pawn),
  ((7,8), Piece White Pawn),

  -- white  pieces
  ((8,1), Piece White Rook),
  ((8,2), Piece White Knight),
  ((8,3), Piece White Bishop),
  ((8,4), Piece White Queen),
  ((8,5), Piece White King),
  ((8,6), Piece White Bishop),
  ((8,7), Piece White Knight),
  ((8,8), Piece White Rook)
  ]

makeInitialGame :: PieceColor -> ChessGame
makeInitialGame startingPlayer =
  ChessGame {
    board = setBoardSquares emptyBoard initialPiecePositions,
    nextPlayer = startingPlayer,
    lastMoves = []
  }
