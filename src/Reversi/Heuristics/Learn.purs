module Reversi.Heuristics.Learn where

import Prelude

import Data.Array (foldRecM, (!!), (..))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random, randomRange)
import Partial.Unsafe (unsafePartial)
import Reversi.Com (diskCount, miniMax)
import Reversi.Game (Strategy, silent)
import Reversi.Heuristics.Eval (EvalNN, evalBoard, learnGameEvalNN, loadEvalNN, randEvalNN, saveEvalNN)
import Reversi.System (availablePositions, countDisks, initialBoard, nextBoards)
import Reversi.Util (maximumIs, minimumIs, randArr)

initGen :: Int
initGen = 20000

steps :: Int
steps = 1000000

main :: Effect Unit
main = do
  initEvalNN <- liftEffect $ randEvalNN
  evalNNM <- liftEffect $ loadEvalNN (show initGen) $ initEvalNN
  launchAff_ do
    learned <- foldRecM step (unsafePartial $ fromJust evalNNM) $ initGen .. (initGen + steps - 1)
    liftEffect $ saveEvalNN (show $ initGen + steps) learned

step :: EvalNN -> Int -> Aff EvalNN
step evalNN i = do
  log $ "Step: " <> show i
  if i `mod` 1000 == 0 then liftEffect $ saveEvalNN (show i) evalNN
  else pure unit
  last /\ history <- silent (com true evalNN) (com false evalNN) initialBoard
  let
    b /\ w = countDisks last
    isWinB
      | b > w = Just true
      | b < w = Just false
      | otherwise = Nothing
    Tuple newNN diff = learnGameEvalNN 0.001 evalNN history isWinB
  log $ "Diff: " <> show diff
  pure newNN

com :: Boolean -> EvalNN -> Strategy
com c evalNN board = do
  r <- liftEffect $ randomRange 0.0 1.0
  let
    bc /\ wc = countDisks board
    turn = bc + wc
    isRandom = r < 0.005
    avs = availablePositions board c
    nb = nextBoards board c
    points = map (miniMax (if turn < 61 then evalBoard evalNN else diskCount) nextBoards (not c) 1) nb
    is = (if c then maximumIs else minimumIs) (if isRandom then 1.0 else 0.0001) points
  i <- liftEffect $ fromMaybe 1 <$> randArr is
  pure $ fromMaybe { h: 0, w: 0 } $ avs !! i
