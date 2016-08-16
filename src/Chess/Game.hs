{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
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
  makeMove,
  allLegalMoves,
  moveToStdStr,
  isLegalMove,
  inCheck,
  module Chess.Board,
  module Chess.Piece
  )  where

import qualified Data.Array as A
import Data.Array ((//), (!))
import Data.List (delete, unfoldr)
import Data.Maybe (mapMaybe, fromJust)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.Char as Char

import Chess.Piece
import Chess.Board


-- | The type of castle move
data CastlingType = KingSide | QueenSide deriving (Eq, Show, Generic, NFData)

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
  deriving (Show, Eq, Generic, NFData)

moveToStdStr :: Move -> String
moveToStdStr move =
  case move of
    Movement _ from to -> moveStr from to
    DoublePawnMove _ from to -> moveStr from to
    Capture _ from to -> moveStr from to
    EnPassant _ from to -> moveStr from to
    Promotion _ from to toType -> moveStr from to ++ [pieceToChar (Piece White toType)]
    Castling color QueenSide ->
      let row = startRow (Piece color King)
      in moveStr (row, 5) (row, 3)
    Castling color KingSide ->
      let row = startRow (Piece color King)
      in moveStr (row, 5) (row, 7)
  where
    moveStr (fromRow, fromCol) (toRow, toCol) =
      [colChar fromCol, rowChar fromRow, colChar toCol, rowChar toRow]
    rowChar row = head (show (9 - row))
    colChar col = Char.chr (Char.ord 'a' + (col - 1))

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
  board' <- movePiece (row, 5) (row, 3) board
  movePiece (row, 1) (row, 4) board'
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
} deriving (Eq, Show, Generic, NFData)

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

-- | Return a new GameState with move applied. Returns Nothing if there is no piece on the start square, or
-- either start or destination squares are out of bounds.  Otherwise, does not check the legality of the move.
makeMove :: GameState -> Move -> Maybe GameState
makeMove game move =
  do newBoard <- applyMoveToBoard (gameBoard game) move
     return GameState {
               gameBoard = newBoard,
               currentPlayer = opponent (currentPlayer game),
               lastMoves = move : lastMoves game,
               enPassantTarget = enPassant,
               whiteCastlingTypes = whiteCastling,
               blackCastlingTypes = blackCastling,
               halfMoveClock = halfMoves
            }
  where
    enPassant =
      case move of
        DoublePawnMove (Piece Black Pawn) (fromRow, fromCol) _ -> Just (fromRow + 1, fromCol)
        DoublePawnMove (Piece White Pawn) (fromRow, fromCol) _ -> Just (fromRow - 1, fromCol)
        _ -> Nothing
    updateCastling castling =
      let rookRow = startRow (Piece (currentPlayer game) Rook) in
      case move of
        Castling _ _ -> []
        Movement (Piece _ King) _ _ -> []
        Movement (Piece _ Rook) (fromRow, fromCol) _ | fromRow == rookRow && fromCol == 1 ->
          delete QueenSide castling
        Movement (Piece _ Rook) (fromRow, fromCol) _ | fromRow == rookRow && fromCol == 8 ->
          delete KingSide castling
        Capture (Piece _ Rook) (fromRow, fromCol) _ | fromRow == rookRow && fromCol == 1 ->
          delete QueenSide castling
        Capture (Piece _ Rook) (fromRow, fromCol) _ | fromRow == rookRow && fromCol == 8 ->
          delete KingSide castling
        _ -> castling
    whiteCastling =
      case currentPlayer game of
        White -> updateCastling (whiteCastlingTypes game)
        Black -> whiteCastlingTypes game
    blackCastling =
      case currentPlayer game of
        Black -> updateCastling (blackCastlingTypes game)
        White -> blackCastlingTypes game
    halfMoves =
      case move of
        Movement (Piece _ ptype) _ _ | ptype /= Pawn -> halfMoveClock game + 1
        Castling _ _ -> halfMoveClock game + 1
        _ -> 0


-- | Return true if king of the indicated color can still castle with the rook
-- in the given side.
canCastle :: GameState -> PieceColor -> CastlingType -> Bool
canCastle game color castleType =
  let rookColumn = case castleType of { QueenSide -> 1; KingSide -> 8 }
      rookRow = case color of { Black -> 1; White -> 8 }
      rookPresent = getPiece (gameBoard game) (rookRow, rookColumn) == Just (Piece color Rook)
  in
  case color of
    Black -> castleType `elem` blackCastlingTypes game && rookPresent
    White -> castleType `elem` whiteCastlingTypes game && rookPresent

-- | Return true if the given square contains an opponent piece of the current
-- player other than a King
hasOpponentPiece :: GameState -> Coord -> Bool
hasOpponentPiece game coord =
  case getPiece (gameBoard game) coord of
    Just (Piece color _) -> color == opponent (currentPlayer game)
    Nothing -> False


isKingCaptureMove :: GameState -> Move -> Bool
isKingCaptureMove game move =
  let enemy = opponent (currentPlayer game)
      board = gameBoard game
  in
  case move of
    Capture _ _ to -> getPiece board to == Just (Piece enemy King)
    Promotion _ _ to _ -> getPiece board to == Just (Piece enemy King)
    _ -> False

-- | Returns True if the given player is in check
inCheck :: GameState -> PieceColor -> Bool
inCheck game player =
  let game' = game { currentPlayer = opponent player }
  in any (isKingCaptureMove game') (allPotentialMoves game')

-- | Returns true if the current player would be in check after making move
inCheckAfterMove :: GameState -> Move -> Bool
inCheckAfterMove game move = inCheck (fromJust $ makeMove game move) (currentPlayer game)

-- | Returns true if move is a castling move which moves through check
movesThroughCheck :: GameState -> Move -> Bool
movesThroughCheck game move =
  case move of
    Castling color castlingType ->
      let kingRow = startRow (Piece color King)
          squaretoCheck = case castlingType of
            QueenSide -> (kingRow, 4)
            KingSide -> (kingRow, 6)
      in inCheck game color || inCheckAfterMove game (Movement (Piece color King) (kingRow, 5) squaretoCheck)
    _ -> False

isLegalMove :: GameState -> Move -> Bool
isLegalMove game move = not (isKingCaptureMove game move || inCheckAfterMove game move || movesThroughCheck game move)

allLegalMoves :: GameState -> [Move]
allLegalMoves game =
  [move | move <- allPotentialMoves game, isLegalMove game move]


{- |
Returns a superset of all the legal moves on the board for the current player. The returned moves include the following
illegal moves:
1. Moves that end up with the player in check
2. Castling moves where the king moves through check
3. Moves that capture the king

Also, this doesn't take into account rules for calling a draw.
-}
allPotentialMoves :: GameState -> [Move]
allPotentialMoves game = concatMap movesForSquare (boardElements (gameBoard game))
  where
    movesForSquare (coord, square) =
      case square of
        Empty -> []
        Square (Piece color _) | color /= currentPlayer game -> []
        Square (Piece _ ptype) ->
          case ptype of
            Pawn -> allPawnMoves game coord
            Rook -> allRookMoves game coord
            Knight -> allKnightMoves game coord
            Bishop -> allBishopMoves game coord
            King -> allKingMoves game coord
            Queen -> allQueenMoves game coord

sumSquares :: Coord -> Coord -> Coord
sumSquares (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

-- | Return move of a piece by the amount specified by the direction argument.
-- Only makes the move if the destination square is in bounds, and is Empty
-- or contains a capture-able opponent piee.  Meant to be used to advance any
-- piece other than pawns by one unit of movment.
maybeMoveInDirection :: GameState -> Piece -> Coord -> Coord -> Maybe Move
maybeMoveInDirection game piece from direction =
  let to = sumSquares from direction in
  if not (isInBounds to) then Nothing
  else
    case boardSquare (gameBoard game) to of
      Empty -> Just (Movement piece from to)
      Square _ | hasOpponentPiece game to -> Just (Capture piece from to)
      _ -> Nothing

-- | Repeatedly applies maybeMoveInDirection until encountering the board edge, a capture, or a
-- friendly piece, and returns the list of moves along the way.
movesInDirection :: GameState -> Piece -> Coord -> Coord -> [Move]
movesInDirection game piece from direction =
  unfoldr (\dir' -> do move <- maybeMoveInDirection game piece from dir'
                       case move of
                         Capture{} -> Just (move, (9, 9))
                         _ -> Just (move, sumSquares dir' direction))
          direction

-- | Return true if the 'count' squares from 'start' in 'direction' are empty
squaresAreEmpty :: GameState -> Coord -> Coord -> Int -> Bool
squaresAreEmpty game start direction count =
  let squares = take count (iterate (sumSquares direction) start)
  in all (isSquareEmpty (gameBoard game)) squares


-- | Get all possible moves for the pawn at at a coordinate
allPawnMoves :: GameState -> Coord -> [Move]
allPawnMoves game from@(fromRow, fromCol) =
  movement ++ doubleMove ++ enPassant ++ capture ++ promotion ++ capturePromotion
  where board = gameBoard game
        piece@(Piece color Pawn) = fromJust $ getPiece board from
        direction = if color == Black then 1 else -1
        moveCoord = sumSquares from (direction, 0)
        doubleMoveCoord = sumSquares from (2 * direction, 0)
        captureCoords = [sumSquares from (direction, -1),
                         sumSquares from (direction, 1)]
        canPromote = if color == Black then fromRow == 7 else fromRow == 2
        movement = [Movement piece from moveCoord |
                    not canPromote && isInBounds moveCoord && isSquareEmpty board moveCoord]
        doubleMove = [DoublePawnMove piece from doubleMoveCoord |
                      fromRow == startRow piece && isSquareEmpty board moveCoord && isSquareEmpty board doubleMoveCoord]
        enPassant = [EnPassant piece from to | to <- captureCoords,
                     enPassantTarget game == Just to]
        capture = [Capture piece from to | to <- captureCoords,
                   not canPromote && isInBounds to && hasOpponentPiece game to]
        promotion = [Promotion piece from moveCoord newType |
                     newType <- [Queen, Bishop, Rook, Knight],
                     canPromote && isSquareEmpty board moveCoord]
        capturePromotion = [Promotion piece from to newType |
                            to <- captureCoords,
                            newType <- [Queen, Bishop, Rook, Knight],
                            canPromote && isInBounds to && hasOpponentPiece game to]

allRookMoves :: GameState -> Coord -> [Move]
allRookMoves game from =
  let directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]
      piece@(Piece _ Rook) = fromJust $ getPiece (gameBoard game) from
  in concatMap (movesInDirection game piece from) directions

allBishopMoves :: GameState -> Coord -> [Move]
allBishopMoves game from =
  let directions = [(1,1), (1, -1), (-1, 1), (-1, -1)]
      piece@(Piece _ Bishop) = fromJust $ getPiece (gameBoard game) from
  in concatMap (movesInDirection game piece from) directions

allQueenMoves :: GameState -> Coord -> [Move]
allQueenMoves game from =
  let directions = [(1,1), (1, -1), (-1, 1), (-1, -1), (1, 0), (-1, 0), (0, 1), (0, -1)]
      piece@(Piece _ Queen) = fromJust $ getPiece (gameBoard game) from
  in concatMap (movesInDirection game piece from) directions

allKnightMoves :: GameState -> Coord -> [Move]
allKnightMoves game from =
  let directions = [(2, -1), (2, 1), (-2, -1), (-2, 1), (1, 2), (-1, 2), (1, -2), (-1, -2)]
      piece@(Piece _ Knight) = fromJust $ getPiece (gameBoard game) from
  in mapMaybe (maybeMoveInDirection game piece from) directions

allKingMoves :: GameState -> Coord -> [Move]
allKingMoves game from =
  let directions = [(1,1), (1, -1), (-1, 1), (-1, -1), (1, 0), (-1, 0), (0, 1), (0, -1)]
      piece@(Piece color King) = fromJust $ getPiece (gameBoard game) from
      normalMoves = mapMaybe (maybeMoveInDirection game piece from) directions
      queenSideCastle = [Castling color QueenSide |
        canCastle game color QueenSide,
        squaresAreEmpty game (sumSquares from (0, -1)) (0, -1) 3]
      kingSideCastle = [Castling color KingSide |
        canCastle game color KingSide,
        squaresAreEmpty game (sumSquares from (0, 1)) (0, 1) 2]
  in normalMoves ++ queenSideCastle ++ kingSideCastle
