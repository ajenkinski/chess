{- |
This module defines a GameState data type which encapsulates the state of a
chess game.
-}

module Chess.Game (
  Move(..),
  CastlingType(..),
  GameState,
  squareAt,
  nextPlayer,
  lastMoves,
  initialGame,
  canCastle,
  enPassantTarget,
  halfMoveClock,
  displayAsGrid)  where

import qualified Data.Array as A
import Data.Array ((//), (!))
import Chess.Piece
import Chess.Board

-- | The type of castle move
data CastlingType = KingSide | QueenSide deriving (Eq, Show)

-- | Represents a move of a piece from one square to another.
-- Move piece fromCoord toCoord
data Move = Movement Piece Coord Coord
          | DoublePawnMove Piece Coord Coord
          | Capture Piece Coord Coord
          | EnPassant Piece Coord Coord
          | Promotion Piece Coord Coord PieceType
          | Castling PieceColor CastlingType
          deriving (Show, Eq)

-- | Returns a new board with a move applied to it, or Nothing if the move is
-- not valid.  This function does not check whether the move is legal,
-- it just checks that the 'from' and 'to' coordinates are in bounds, and that
-- there is actually a piece on the 'from' square.
applyMoveToBoard :: Board -> Move -> Maybe Board
applyMoveToBoard board (Movement _ from to) = movePiece from to board
applyMoveToBoard board (DoublePawnMove _ from to) = movePiece from to board
applyMoveToBoard board (Capture _ from to) = movePiece from to board
applyMoveToBoard board (EnPassant _ from@(x1, y1) to@(x2, y2)) =
  movePiece from to (clearBoardSquare board (x1, y2))
applyMoveToBoard board (Promotion (Piece color _) from to newType) = do
  board' <- movePiece from to board
  return (setBoardSquare board' to (Piece color newType))
applyMoveToBoard board (Castling color QueenSide) = do
  let row = startRow (Piece color King)
  board' <- movePiece (row, 5) (row, 2) board
  movePiece (row, 1) (row, 3) board'
applyMoveToBoard board (Castling color KingSide) = do
  let row = startRow (Piece color King)
  board' <- movePiece (row, 5) (row, 7) board
  movePiece (row, 8) (row, 6) board'


-- | Represents the state of a chess game.  This representation contains enough
-- information to know what moves are legal at any given point.
data GameState = GameState {
  -- | The chess board
  board :: Board,

  -- | Whose turn is next
  nextPlayer :: PieceColor,

  -- | Moves played so far, with most recent move at the head of the list
  lastMoves :: [Move],

  -- | En passant target square, or Nothing if there's no en passant target
  -- square. If a pawn has just made a two-square move, this is the position
  -- "behind" the pawn. This is recorded regardless of whether there is a pawn
  -- in position to make an en passant capture.
  enPassantTarget :: Maybe Coord,

  -- | Castling moves possible for white
  whiteCastlingTypes :: [CastlingType],

  -- | Castling moves possible for black
  blackCastlingTypes :: [CastlingType],

  -- | Number of half moves since the last pawn move or capture
  halfMoveClock :: Int
} deriving (Eq, Show)

-- | Displays a chess game as an ascii grid
displayAsGrid :: GameState -> String
displayAsGrid game = showBoard (board game) ""
  where rowSeparator = "+---+---+---+---+---+---+---+---+\n"
        showSquare (Square piece) =
          showChar ' ' . showChar (pieceToChar piece) . showString " |"
        showSquare Empty = showString "   |"
        showRow board r =
            showString rowSeparator . showChar '|' .
            foldl1 (.) (map (showSquare.snd) (boardRow board r)) .
            showChar '\n'
        showBoard board = foldl1 (.) (map (showRow board) [1..8]) . showString rowSeparator

-- | Return the contents of a square on the chess board
squareAt :: GameState -> Coord -> Square
squareAt = boardSquare . board

-- | Return a GameState with pieces setup in starting positions, and the
-- nextPlayer set to the given starting player color
initialGame :: PieceColor -> GameState
initialGame startingPlayer =
  GameState {
    board = initialBoard,
    nextPlayer = startingPlayer,
    lastMoves = [],
    enPassantTarget = Nothing,
    whiteCastlingTypes = [QueenSide, KingSide],
    blackCastlingTypes = [QueenSide, KingSide],
    halfMoveClock = 0
  }

-- | Return true if king of the indicated color can still castle with the rook
-- in the given side.
canCastle :: GameState -> PieceColor -> CastlingType -> Bool
canCastle game color castleType =
  case color of
    Black -> castleType `elem` blackCastlingTypes game
    White -> castleType `elem` whiteCastlingTypes game
