module Chess.FENSpec (spec) where

import Test.Hspec
import Control.Monad (mapM_)
import Chess.FEN
import Chess.Game

allPieceTypes = [Rook, Knight, Bishop, Queen, King, Pawn]
[br, bn, bb, bq, bk, bp] = map (Square . Piece Black) allPieceTypes
[wr, wn, wb, wq, wk, wp] = map (Square . Piece White) allPieceTypes
em = Empty

parseTestCases = [
  ("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
   GameState {
      gameBoard = listBoard [br, bn, bb, bq, bk, bb, bn, br,
                             bp, bp, bp, bp, bp, bp, bp, bp,
                             em, em, em, em, em, em, em, em,
                             em, em, em, em, em, em, em, em,
                             em, em, em, em, em, em, em, em,
                             em, em, em, em, em, em, em, em,
                             wp, wp, wp, wp, wp, wp, wp, wp,
                             wr, wn, wb, wq, wk, wb, wn, wr],
      currentPlayer = White,
      lastMoves = [],
      enPassantTarget = Nothing,
      whiteCastlingTypes = [KingSide, QueenSide],
      blackCastlingTypes = [KingSide, QueenSide],
      halfMoveClock = 0
   }
  ),
  ("rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2",
   GameState {
      gameBoard = listBoard [br, bn, bb, bq, bk, bb, bn, br,
                             bp, bp, em, bp, bp, bp, bp, bp,
                             em, em, em, em, em, em, em, em,
                             em, em, bp, em, em, em, em, em,
                             em, em, em, em, wp, em, em, em,
                             em, em, em, em, em, em, em, em,
                             wp, wp, wp, wp, em, wp, wp, wp,
                             wr, wn, wb, wq, wk, wb, wn, wr],
      currentPlayer = White,
      lastMoves = [],
      enPassantTarget = Just (3, 3),
      whiteCastlingTypes = [KingSide, QueenSide],
      blackCastlingTypes = [KingSide, QueenSide],
      halfMoveClock = 0
   }
  )
 ]

testParseFEN (fen, expected) =
  it ("Parses fen string \"" ++ fen ++ "\"") $ do
    let actual = parseFEN fen
    actual `shouldBe` expected

spec :: Spec
spec = do
  describe "parseFEN" $ do
    mapM_ testParseFEN parseTestCases
