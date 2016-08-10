module Main where

import Chess.Piece
import Chess.Game
import Chess.Board

main = do
  print Pawn
  print (Piece White Rook)
  print [Square (Piece White King), Square (Piece Black Queen), Empty]
  let game = initialGame White
  print game
  print (canCastle game White QueenSide)
  print (canCastle game Black KingSide)
