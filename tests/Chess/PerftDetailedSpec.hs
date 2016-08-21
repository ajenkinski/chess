{- hspec test suite which runs perft tests -}
module Chess.PerftDetailedSpec (spec) where

import Test.Hspec
import Data.List (foldl1')
import Data.Tree (Tree, unfoldTree, levels, subForest, rootLabel)
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad (forM_, mapM_)

import Chess.Game
import Chess.FEN
import Chess.PerftSuiteDetailed


children :: GameState -> [GameState]
children game = mapMaybe (makeMove game) (allLegalMoves game)

gameTree :: GameState -> Tree GameState
gameTree = unfoldTree (\game -> (game, children game))

countBy :: (a -> Bool) -> [a] -> Int
countBy pred xs = length (filter pred xs)

perftList :: Int -> GameState -> [DepthInfo]
perftList depth start = map depthStats treeLevels
  where
    treeLevels = take depth (tail (levels (gameTree start)))
    depthStats level = foldl1' addDepthStats (map gameDepthStats level)
    gameDepthStats game =
      DI { numNodes = 1,
           numCaptures = if lastMoveCapture game then 1 else 0,
           numEnPassants = if lastMoveEnPassant game then 1 else 0,
           numCastles = if lastMoveCastle game then 1 else 0,
           numPromotions = if lastMovePromotion game then 1 else 0,
           numChecks = if inCheck game then 1 else 0,
           numCheckMates = if inCheckMate game then 1 else 0
         }
    addDepthStats (DI x1 x2 x3 x4 x5 x6 x7) (DI y1 y2 y3 y4 y5 y6 y7) =
      DI (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
    lastMoveCapture game = 
      case head (lastMoves game) of
        Capture{} -> True
        EnPassant{} -> True
        Promotion _ _ _ _ True -> True
        _ -> False
    lastMoveEnPassant game = 
      case head (lastMoves game) of
        EnPassant{} -> True
        _ -> False
    lastMoveCastle game = 
      case head (lastMoves game) of
        Castling{} -> True
        _ -> False
    lastMovePromotion game = 
      case head (lastMoves game) of
        Promotion{} -> True
        _ -> False

testPerft :: Int -> TestCase -> Spec
testPerft maxDepth (fen, expectedDepthInfos) =
  -- filter out expected counts greater than 100 million because my engine is too slow
  describe ("Detailed Perft test for \"" ++ fen ++ "\"") $ do
    let start = parseFEN fen
    let depthInfos = perftList maxDepth start
    let filteredExpectedInfos = takeWhile ((<= 100000000) . numNodes) expectedDepthInfos
    forM_ (zip3 [1..] depthInfos filteredExpectedInfos) $ \(depth, actualInfo, expectedInfo) -> 
      it ("Counts for depth " ++ show depth ++ " match " ++ show expectedInfo) $ do
        numNodes expectedInfo `shouldBe` numNodes actualInfo
        numCaptures expectedInfo `shouldBe` numCaptures actualInfo
        numEnPassants expectedInfo `shouldBe` numEnPassants actualInfo
        numCastles expectedInfo `shouldBe` numCastles actualInfo
        numPromotions expectedInfo `shouldBe` numPromotions actualInfo
        numChecks expectedInfo `shouldBe` numChecks actualInfo
        numCheckMates expectedInfo `shouldBe` numCheckMates actualInfo

spec :: Spec
spec = mapM_ (testPerft 4) testCases

