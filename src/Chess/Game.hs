{- |
This module defines a GameState data type which encapsulates the state of a
chess game.
-}

module Chess.Game (
  Move(..),
  CastlingType(..),
  GameState(..),
  initialGame,
  makeGame,
  canCastle,
  module Chess.Board,
  module Chess.Piece
  )  where

import qualified Data.Array as A
import Data.Array ((//), (!))
import Chess.Piece
import Chess.Board
import Data.Maybe

-- | The type of castle move
data CastlingType = KingSide | QueenSide deriving (Eq, Show)

-- | Represents a move of a piece from one square to another.
-- All constructors other than 'Castling' have a pair of Coords giving the
-- start and end squares
data Move
  -- | A move which doesn't fit in any of the other constructors
  = Movement Piece Coord Coord
  -- | A 2 square pawn move from the pawn's starting row
  | DoublePawnMove Piece Coord Coord
  -- | A capturing move other than an en passant or promotion move
  | Capture Piece Coord Coord
  -- | An en passant capture move.  Start and end coords are the for the
  -- capturing pawn
  | EnPassant Piece Coord Coord
  -- | A pawn promotion move
  | Promotion Piece Coord Coord PieceType
  -- | A castle move
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
  gameBoard :: Board,

  -- | Whose turn is next
  currentPlayer :: PieceColor,

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

-- | Return a GameState with pieces setup in starting positions, and the
-- currentPlayer set to the given starting player color
initialGame :: PieceColor -> GameState
initialGame startingPlayer =
  GameState {
    gameBoard = initialBoard,
    currentPlayer = startingPlayer,
    lastMoves = [],
    enPassantTarget = Nothing,
    whiteCastlingTypes = [QueenSide, KingSide],
    blackCastlingTypes = [QueenSide, KingSide],
    halfMoveClock = 0
  }

makeGame :: Board -> PieceColor -> [Move] -> Maybe Coord -> [CastlingType] -> [CastlingType] -> Int -> GameState
makeGame board player history enPassant whiteCastling blackCastling halfMoves =
  GameState {
    gameBoard = board,
    currentPlayer = player,
    lastMoves = history,
    enPassantTarget = enPassant,
    whiteCastlingTypes = whiteCastling,
    blackCastlingTypes = blackCastling,
    halfMoveClock = halfMoves
  }

-- | Return true if king of the indicated color can still castle with the rook
-- in the given side.
canCastle :: GameState -> PieceColor -> CastlingType -> Bool
canCastle game color castleType =
  case color of
    Black -> castleType `elem` blackCastlingTypes game
    White -> castleType `elem` whiteCastlingTypes game

-- | Return true if the given square contains an opponent piece of the current
-- player other than a King
hasKillablePiece :: GameState -> Coord -> Bool
hasKillablePiece game coord =
  case getPiece (gameBoard game) coord of
    Just (Piece color ptype) -> color == opponent (currentPlayer game) && ptype /= King
    Nothing -> False

allLegalMoves :: GameState -> [Move]
allLegalMoves game = []

-- | Get all possible moves for the pawn at at a coordinate
allPawnMoves :: GameState -> Coord -> [Move]
allPawnMoves game from@(fromRow, fromCol) =
  movement ++ enPassant ++ capture ++ promotion ++ capturePromotion
  where board = gameBoard game
        piece@(Piece color Pawn) = fromJust $ getPiece board from
        direction = if color == Black then 1 else -1
        toRow = fromRow + direction
        moveCoord = (fromRow + direction, fromCol)
        captureCoords = [(fromRow + direction, fromCol - 1),
                         (fromRow + direction, fromCol + 1)]
        canPromote = if color == Black then toRow == 8 else toRow == 1
        movement = [Movement piece from moveCoord |
                    isInBounds moveCoord && isSquareEmpty board moveCoord]
        enPassant = [EnPassant piece from to | to <- captureCoords,
                     enPassantTarget game == Just to]
        capture = [Capture piece from to | to <- captureCoords,
                   isInBounds to && hasKillablePiece game to]
        promotion = [Promotion piece from moveCoord newType |
                     newType <- [Queen, Bishop, Rook, Knight],
                     canPromote && isSquareEmpty board moveCoord]
        capturePromotion = [Promotion piece from to newType |
                            to <- captureCoords,
                            newType <- [Queen, Bishop, Rook, Knight],
                            canPromote && hasKillablePiece game to]
