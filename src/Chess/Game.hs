module Chess.Game (
  Square(..),
  Coord,
  Element,
  Move(..),
  ChessGame,
  makeInitialGame,
  canCastleQueenSide,
  canCastleKingSide)  where

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

startRow :: Piece -> Int
startRow piece =
  case piece of
    Piece Black Pawn -> 2
    Piece White Pawn -> 7
    Piece Black _ -> 1
    Piece White _ -> 8

-- Set a board square to a new value.
setBoardSquare :: Board -> Coord -> Piece -> Board
setBoardSquare board coord piece = board // [(coord, Square piece)]

setBoardSquares :: Board -> [(Coord, Piece)] -> Board
setBoardSquares board assignments =
  board // [(coord, Square piece) | (coord, piece) <- assignments]

boardRow :: Board -> Int -> [Element]
boardRow board rowNum =
  [((rowNum, colNum), board!(rowNum, colNum)) | colNum <- [1..8]]

boardSquare :: Board -> Coord -> Square
boardSquare = (!)


data ChessGame = ChessGame {

  board :: Board,

  -- Whose turn is next
  nextPlayer :: PieceColor,

  -- Moves played so far, with most recent move at the head of the list
  lastMoves :: [Move]
}

instance Show ChessGame where
    show game = showBoard (board game) (showString "Turn: " (show (nextPlayer game)))
        where rowSeparator = "+---+---+---+---+---+---+---+---+\n"
              showSquare (Square piece) =
                showChar ' ' . showChar (pieceToChar piece) . showString " |"
              showSquare Empty = showString "   |"
              showRow board r =
                  showString rowSeparator . showChar '|' .
                  foldl1 (.) (map (showSquare.snd) (boardRow board r)) .
                  showChar '\n'
              showBoard board = foldl1 (.) (map (showRow board) [1..8]) . showString rowSeparator

squareAt :: ChessGame -> Coord -> Square
squareAt = boardSquare . board

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

canCastle :: Int -> PieceColor -> ChessGame -> Bool
canCastle rookColumn color game =
  let king = Piece color King
      rook = Piece color Rook
      kingStart = (startRow king, 5)
      rookStart = (startRow rook, rookColumn)
      moveMatches piece1 from1 (Move piece2 from2 _) = (piece1, from1) == (piece2, from2)
  in squareAt game kingStart == Square king &&
     squareAt game rookStart == Square rook &&
     not (any (moveMatches king kingStart) (lastMoves game)) &&
     not (any (moveMatches rook rookStart) (lastMoves game))

canCastleQueenSide :: PieceColor -> ChessGame -> Bool
canCastleQueenSide = canCastle 1

canCastleKingSide :: PieceColor -> ChessGame -> Bool
canCastleKingSide = canCastle 8
