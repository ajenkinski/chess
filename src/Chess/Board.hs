-- | Defines a Board datatype representing a chess board

module Chess.Board (
  Square(..),
  Coord,
  Element,
  Board,
  emptyBoard,
  initialBoard,
  listBoard,
  startRow,
  setBoardSquare,
  setBoardSquares,
  clearBoardSquare,
  boardRow,
  boardSquare,
  getPiece,
  isInBounds,
  movePiece,
  isSquareEmpty
) where

import qualified Data.Array as A
import Data.Array ((//), (!))
import Chess.Piece

-- | Represents the contents of one square of a chess board
data Square = Square Piece | Empty deriving (Show, Eq)

-- | (Row, Column) coordinate of a chess board square.  Range is from 1 to 8.
type Coord = (Int, Int)

-- | Represents a particular square of a chess board
type Element = (Coord, Square)

-- | A board is a 2D array of squares.  Black starts at the top (first row) and
-- White starts at the bottom.
newtype Board = Board (A.Array Coord Square) deriving (Eq)

instance Show Board where
  show board = showChar '\n' . showBoard board $ ""
    where rowSeparator = "+---+---+---+---+---+---+---+---+\n"
          showSquare (Square piece) =
            showChar ' ' . showChar (pieceToChar piece) . showString " |"
          showSquare Empty = showString "   |"
          showRow board r =
              showString rowSeparator . showChar '|' .
              foldl1 (.) (map (showSquare.snd) (boardRow board r)) .
              showChar '\n'
          showBoard board = foldl1 (.) (map (showRow board) [1..8]) . showString rowSeparator

-- | An empty chess board
emptyBoard :: Board
emptyBoard = Board $ A.listArray ((1, 1), (8, 8)) (repeat Empty)

-- | A chess board with pieces setup in the standard initial game positions
initialBoard :: Board
initialBoard = Board $ A.listArray ((1, 1), (8, 8)) squares
  where officerRow color = map (Square . Piece color) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
        pawnRow color = replicate 8 (Square (Piece color Pawn))
        squares = officerRow Black ++
                  pawnRow Black ++
                  replicate 32 Empty ++
                  pawnRow White ++
                  officerRow White

listBoard :: [Square] -> Board
listBoard squares = Board $ A.listArray ((1, 1), (8, 8)) squares

-- | Returns the row number at which a chess piece starts the game
startRow :: Piece -> Int
startRow piece =
  case piece of
    Piece Black Pawn -> 2
    Piece White Pawn -> 7
    Piece Black _ -> 1
    Piece White _ -> 8

-- | Place a piece on a board square
setBoardSquare :: Board -> Coord -> Piece -> Board
setBoardSquare (Board board) coord piece = Board $ board // [(coord, Square piece)]

-- | Place pieces on multiple board squares
setBoardSquares :: Board -> [(Coord, Piece)] -> Board
setBoardSquares (Board board) assignments =
  Board $ board // [(coord, Square piece) | (coord, piece) <- assignments]

-- | Set a board square to Empty
clearBoardSquare :: Board -> Coord -> Board
clearBoardSquare (Board board) coord = Board $ board // [(coord, Empty)]

-- | Return the contents of a row of a board
boardRow :: Board -> Int -> [Element]
boardRow (Board board) rowNum =
  [((rowNum, colNum), board!(rowNum, colNum)) | colNum <- [1..8]]

-- | Return the contents of a square of a board
boardSquare :: Board -> Coord -> Square
boardSquare (Board board) coord = board ! coord

isSquareEmpty :: Board -> Coord -> Bool
isSquareEmpty board coord = boardSquare board coord == Empty

-- | Get the piece at a board square, if there is a piece on that square
getPiece :: Board -> Coord -> Maybe Piece
getPiece board coord =
  case boardSquare board coord of
    Empty -> Nothing
    Square piece -> Just piece

-- | True if a coordinate is in bounds of the board
isInBounds :: Coord -> Bool
isInBounds (row, col) = row > 0 && row < 9 && col > 0 && col < 9

-- | If from and to are valid coordinates, and the from square is not empty,
-- return a new board with piece moved to the 'to' square, else return Nothing
movePiece :: Coord -> Coord -> Board -> Maybe Board
movePiece from to _ | not (isInBounds from && isInBounds to) = Nothing
movePiece from to board = do
  fromPiece <- getPiece board from
  return (setBoardSquare (clearBoardSquare board from) to fromPiece)
