module Main where

import Data.Maybe (fromJust)
import qualified System.IO as SIO
import System.Environment (getArgs)
import Control.Monad (when)
import System.Exit (die)

import Chess.Game
import Chess.FEN
import Chess.Perft
import Control.Applicative

main = do
  args <- getArgs
  when (length args /= 3) $ 
    die "Usage: perft (perft|divide) <depth> <FEN>"
  let [command, depthStr, fen] = args
  let depth = read depthStr :: Int
  case command of
    "perft" -> printPerfList depth fen
    "divide" -> printDivide depth fen
