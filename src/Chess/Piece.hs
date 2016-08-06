module Chess.Piece (
  PieceType(..),
  PieceColor(..),
  Piece(..),
  opponent,
  pieceToChar) where

import qualified Data.Char
data PieceType = Pawn | Rook | Knight | Bishop | King | Queen
  deriving (Show, Eq)

data PieceColor = Black | White deriving (Show, Eq)

data Piece = Piece PieceColor PieceType deriving (Show, Eq)

opponent :: PieceColor -> PieceColor
opponent Black = White
opponent White = Black

pieceTypeChar :: PieceType -> Char
pieceTypeChar ptype =
  case ptype of
    Pawn -> 'p'
    Rook -> 'r'
    Knight -> 'n'
    Bishop -> 'b'
    King -> 'k'
    Queen -> 'q'

pieceToChar :: Piece -> Char
pieceToChar (Piece color ptype) =
  let typeChar = pieceTypeChar ptype in
  case color of
    Black -> typeChar
    White -> Data.Char.toUpper typeChar
