module Main where

import Chess.Piece
import Chess.Game

main = do
  print Pawn
  print (Piece White Rook)
  print [Square (Piece White King), Square (Piece Black Queen), Empty]
  print (makeInitialGame White)
