module Reversi.Heuristics.Learn where

import Prelude

import Data.Array (foldRecM, take, (!!), (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomRange)
import Partial.Unsafe (unsafePartial)
import Reversi.Com (diskCount, miniMax)
import Reversi.Game (Strategy, silent)
import Reversi.Heuristics.Eval (EvalNN, evalBoard, learnGameEvalNN, loadEvalNN, saveEvalNN)
import Reversi.System (availablePositions, countDisks, initialBoard, nextBoards)
import Reversi.Util (maximumIs, minimumIs, randArr)

initGen :: Int
initGen = 100

steps :: Int
steps = 1000000

main :: Effect Unit
main = do
  -- evalNN <- liftEffect $ randEvalNN
  evalNN <- liftEffect $ unsafePartial $ loadEvalNN (show initGen)
  launchAff_ do
    learned <- foldRecM step evalNN $ initGen .. (initGen + steps - 1)
    liftEffect $ saveEvalNN (show $ initGen + steps) $ learned

step :: EvalNN -> Int -> Aff EvalNN
step evalNN i = do
  log $ "Step: " <> show i
  if i `mod` 100 == 0 then liftEffect $ saveEvalNN (show i) evalNN
  else pure unit
  last /\ history <- silent (com true evalNN) (com false evalNN) initialBoard
  let
    b /\ w = countDisks last
    isWinB
      | b > w = Just true
      | b < w = Just false
      | otherwise = Nothing
    Tuple newNN diff = learnGameEvalNN 0.05 evalNN (take 58 history) isWinB
  log $ "Diff: " <> show diff
  pure newNN

com :: Boolean -> EvalNN -> Strategy
com c evalNN board = do
  r <- liftEffect $ randomRange 0.0 1.0
  let
    bc /\ wc = countDisks board
    turn = bc + wc
    isRandom = r < 0.02
    avs = availablePositions board c
    nb = nextBoards board c
  if isRandom then liftEffect $ fromMaybe { h: 0, w: 0 } <$> randArr avs
  else do
    let
      points = map (if turn < 57 then miniMax (evalBoard evalNN) nextBoards (not c) 2 else miniMax diskCount nextBoards (not c) 10) nb
      is = (if c then maximumIs 0.0001 else minimumIs 0.0001) points
    i <- liftEffect $ fromMaybe 0 <$> randArr is
    pure $ fromMaybe { h: 0, w: 0 } $ avs !! i
