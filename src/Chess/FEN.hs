-- | Module to parse and generate FEN chess game strings. see
-- https://chessprogramming.wikispaces.com/Forsyth-Edwards+Notation for
-- FEN syntax.

module Chess.FEN (
  parseFEN
) where

import qualified Data.Text as T
import qualified Data.Char as Char
import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Maybe
import Chess.Game

-- | Parse a FEN game representation into a GameState
parseFEN :: String -> GameState
parseFEN fen =
  let [pieceS, turnS, castlingS, enPassantS, halfMoveS, fullMoveS] = words fen
      rankStrings = splitOn "/" pieceS
      elements = concatMap parseRank rankStrings
      board = listBoard elements
      player = if turnS == "b" then Black else White
      (blackCastling, whiteCastling) = parseCastling castlingS
      enPassant = parseEnPassant enPassantS
      halfMoves = read halfMoveS
  in makeGame board player [] enPassant whiteCastling blackCastling halfMoves
  where parseRank = concatMap parseRankChar
        parseRankChar char
          | Char.isAlpha char = [Square (Data.Maybe.fromJust (charToPiece char))]
          | Char.isDigit char = replicate (Char.digitToInt char) Empty
          | otherwise = error "Unexpected character in FEN pieces string"

        parseCastling castlingString =
          if castlingString == "-" then ([], [])
          else
            let (blackString, whiteString) = List.partition Char.isLower castlingString
            in (map parseCastlingChar blackString, map parseCastlingChar whiteString)
        parseCastlingChar char =
          case Char.toLower char of
            'k' -> KingSide
            'q' -> QueenSide

        parseEnPassant enPassantString =
          case enPassantString of
            ['-'] -> Nothing
            [fileChar, rankChar] ->
              let column = Char.ord fileChar - Char.ord 'a' + 1
                  row = 9 - Char.digitToInt rankChar
              in Just (row, column)
