{- |
This module defines a chess piece data type, and associated functions
-}

module Chess.Piece (
  PieceType(..),
  PieceColor(..),
  Piece(..),
  opponent,
  pieceToChar) where

import qualified Data.Char

-- | Represents the type of a chess piece
data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
  deriving (Show, Eq)

-- | Color of a chess piece
data PieceColor = Black | White deriving (Show, Eq)

-- | A chess piece
data Piece = Piece PieceColor PieceType deriving (Show, Eq)


-- | The opposing piece color
opponent :: PieceColor -> PieceColor
opponent Black = White
opponent White = Black

-- | Returns the 1-character code of a chess piece, as defined in the FEN
-- representation of a chess game.
pieceToChar :: Piece -> Char
pieceToChar (Piece color ptype) =
  let typeChar = pieceTypeChar ptype in
  case color of
    Black -> typeChar
    White -> Data.Char.toUpper typeChar

-- private module functions

pieceTypeChar :: PieceType -> Char
pieceTypeChar ptype =
  case ptype of
    Pawn -> 'p'
    Rook -> 'r'
    Knight -> 'n'
    Bishop -> 'b'
    King -> 'k'
    Queen -> 'q'
