module Reversi.Heuristics.Learn where

import Prelude

import Data.Array (foldRecM, (!!), (..))
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Reversi.Com (diskCount, miniMax)
import Reversi.Game (Strategy, silent)
import Reversi.Heuristics.Eval (EvalNN, evalBoard, learnGameEvalNN, randEvalNN, saveEvalNN)
import Reversi.System (availablePositions, countDisks, initialBoard, nextBoards)
import Reversi.Util (maximumIs, minimumIs, randArr)

main :: Effect Unit
main = do
  initEvalNN <- liftEffect $ randEvalNN
  saveEvalNN "0" initEvalNN
  -- evalNNM <- liftEffect $ loadEvalNN "100" $ initEvalNN
  launchAff_ do
    learned <- foldRecM (step) initEvalNN $ 0 .. 100
    liftEffect $ saveEvalNN "100" learned

step :: EvalNN -> Int -> Aff EvalNN
step evalNN i = do
  log $ "Step: " <> show i
  last /\ history <- silent (com true evalNN) (com false evalNN) initialBoard
  let
    b /\ w = countDisks last
    isWinB = b > w
  pure $ learnGameEvalNN 0.01 evalNN history isWinB

com :: Boolean -> EvalNN -> Strategy
com c evalNN board = do
  let
    bc /\ wc = countDisks board
    turn = bc + wc
    avs = availablePositions board c
    nb = nextBoards board c
  let
    points = map (miniMax (if turn < 56 then evalBoard evalNN else diskCount) nextBoards (not c) 1) nb
    is = (if c then maximumIs (toNumber turn * 0.01) else minimumIs (toNumber turn * 0.01)) points
  i <- liftEffect $ fromMaybe 1 <$> randArr is
  pure $ fromMaybe { h: 0, w: 0 } $ avs !! i
