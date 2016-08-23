{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Chess.PerftSuiteDetailed where
{- ^
This gives test cases, similar to PerftSuite, but in addition to 
specifying expected node counts for each depth, it also specifies other
expected statistics for each search depth, such as number of captures,
en passants, castles, etc.
-}

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data DepthInfo = DI {
  numNodes      :: Int,
  numCaptures   :: Int,
  numEnPassants :: Int,
  numCastles    :: Int,
  numPromotions :: Int,
  numChecks     :: Int,
  numCheckMates :: Int
  } deriving (Eq, Show, Generic, NFData)

type TestCase = (String, [DepthInfo])

testCases :: [TestCase]
testCases = [
  ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
   [DI 20 0 0 0 0 0 0,
    DI 400 0 0 0 0 0 0,
    DI 8902 34 0 0 0 12 0,
    DI 197281 1576 0 0 0 469 8,
    DI 4865609 82719 258 0 0 27351 347,
    DI 119060324 2812008 5248 0 0 809099 10828]),
  ("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
   [DI 48 8 0 2 0 0 0,
    DI 2039 351 1 91 0 3 0,
    DI 97862 17102 45 3162 0 993 1,
    DI 4085603 757163 1929 128013 15172 25523 43,
    DI 193690690 35043416 73365 4993637 8392 3309887 30171]),
  ("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
   [DI 14 1 0 0 0 2 0,
    DI 191 14 0 0 0 10 0,
    DI 2812 209 2 0 0 267 0,
    DI 43238 3348 123 0 0 1680 17,
    DI 674624 52051 1165 0 0 52950 0,
    DI 11030083 940350 33325 0 7552 452473 2733,
    DI 178633661 14519036 294874 0 140024 12797406 87]),
  ("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
   [DI 6 0 0 0 0 0 0,
    DI 264 87 0 6 48 10 0,
    DI 9467 1021 4 0 120 38 22,
    DI 422333 131393 0 7795 60032 15492 5,
    DI 15833292 2046173 6512 0 329464 200568 50562,
    DI 706045033 210369132 212 10882006 81102984 26973664 81076])
  ]

