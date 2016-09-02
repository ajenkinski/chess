{-# LANGUAGE FlexibleInstances #-}
module Chess.Perft (
  GameLike,
  perftList,
  perft,
  divide,
  printPerfList,
  printDivide
) where

import Data.Tree (Tree, unfoldTree, levels, subForest, rootLabel)
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad (mapM_, foldM_, forM_)
import Chess.Game
import Chess.FEN

children :: GameState -> [GameState]
children game = mapMaybe (makeMove game) (allLegalMoves game)

gameTree :: GameState -> Tree GameState
gameTree = unfoldTree (\game -> (game, children game))

-- Allow functions to accept a GameState or String interchangeably
class GameLike a where
  gameState :: a -> GameState

instance GameLike GameState where
  gameState = id

instance GameLike [Char] where
  gameState = parseFEN

perftList :: GameLike a => Int -> a -> [Int]
perftList depth start =
  let tree = gameTree (gameState start)
  in take depth [length level | level <- tail (levels tree)]

perft :: GameLike a => Int -> a -> Int
perft depth start = length (levels (gameTree (gameState start)) !! depth)

divide :: GameLike a => Int -> a -> [(Move, Int)]
divide depth start =
  let subtrees = subForest (gameTree (gameState start))
  in [((head.lastMoves.rootLabel) tree, length (levels tree !! (depth - 1))) | tree <- subtrees]

printPerfList :: GameLike a => Int -> a -> IO ()
printPerfList depth start =
  forM_ (zip [1..] (perftList depth (gameState start))) $ \(n, levelSize) -> do
    putStr ("Level " ++ show n ++ ": ")
    print levelSize

printDivide :: GameLike a => Int -> a -> IO ()
printDivide depth start = do
  let counts = divide depth (gameState start)
  forM_ counts $ \(move, count) ->
    putStrLn (moveToStdStr move ++ " " ++ show count)
  putStrLn ("Moves: " ++ show (length counts))


