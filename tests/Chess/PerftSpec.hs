module Chess.PerftSpec (spec) where

import Test.Hspec
import Data.Tree (Tree, unfoldTree, levels, subForest, rootLabel)
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad (forM_, mapM_)

import Chess.Game
import Chess.FEN
import Chess.PerftSuite


children :: GameState -> [GameState]
children game = mapMaybe (makeMove game) (allLegalMoves game)

gameTree :: GameState -> Tree GameState
gameTree = unfoldTree (\game -> (game, children game))

perftList :: Int -> GameState -> [Int]
perftList depth start =
  let tree = gameTree start
  in take depth [length level | level <- tail (levels tree)]

testPerft maxDepth (fen, expectedDepthCounts) =
  describe ("Perft test for \"" ++ fen ++ "\"") $ do
    let start = parseFEN fen
    let depthCounts = perftList maxDepth start
    forM_ (zip3 [1..] depthCounts expectedDepthCounts) $ \(depth, actualCount, expectedCount) ->
      it ("Count for depth " ++ show depth ++ " should be " ++ show expectedCount) $
        actualCount `shouldBe` expectedCount

spec :: Spec
spec = mapM_ (testPerft 5) perftSuite
