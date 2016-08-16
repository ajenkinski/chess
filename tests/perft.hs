module Main where

import Data.Tree (Tree, unfoldTree, levels, subForest, rootLabel)
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad (mapM_, foldM_, forM_)
import qualified Control.Parallel.Strategies as PS
import qualified System.IO as SIO

import Chess.Game
import Chess.FEN

treeStrategy :: PS.Strategy (Tree GameState)
treeStrategy = PS.rseq

listStrategy :: PS.Strategy [GameState]
listStrategy = PS.parListChunk 50 PS.rdeepseq

children :: GameState -> [GameState]
children game = mapMaybe (makeMove game) (allLegalMoves game)

gameTree :: GameState -> Tree GameState
gameTree = unfoldTree (\game -> (game, children game))

perftList :: Int -> GameState -> [Int]
perftList depth start =
  let tree = gameTree start
  in take depth [length level | level <- tail (levels tree)]

perft :: Int -> GameState -> Int
perft depth start = length (levels (gameTree start) !! depth)

divide :: Int -> GameState -> [(Move, Int)]
divide depth start =
  let subtrees = subForest (gameTree start)
  in [((head.lastMoves.rootLabel) tree, length (levels tree !! (depth - 1))) | tree <- subtrees]

printPerfList depth start =
  forM_ (zip [1..] (perftList depth start)) $ \(n, levelSize) -> do
    putStr ("Level " ++ show n ++ ": ")
    print levelSize

printDivide depth start = do
  let counts = divide depth start
  forM_ counts $ \(move, count) ->
    putStrLn (moveToStdStr move ++ " " ++ show count)
  putStrLn ("Moves: " ++ show (length counts))


main = do
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  let depth = 6
  let start1 = parseFEN "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
  let start2 = fromJust $ makeMove start1 (Movement (Piece White Rook) (8, 1) (8, 2))
  let start3 = fromJust $ makeMove start2 (Capture (Piece Black Pawn) (6, 8) (7, 7))
  let start4 = fromJust $ makeMove start3 (Movement (Piece White Pawn) (7, 1) (6, 1))
  printPerfList 4 start1
  -- let start2 = fromJust $ makeMove start1 (DoublePawnMove (Piece White Pawn) (7,1) (5,1))
  -- let start3 = fromJust $ makeMove start2 (Movement (Piece Black Pawn) (2,4) (3,4))
  -- let start4 = fromJust $ makeMove start3 (Movement (Piece White Pawn) (5,1) (4,1))
  --
  -- print start4
  -- forM_ (allLegalMoves start3) $ \move -> putStrLn (moveToStdStr move)
