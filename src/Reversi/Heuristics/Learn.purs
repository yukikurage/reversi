module Reversi.Heuristics.Learn where

import Prelude

import Data.Array (foldRecM, take, (!!), (..))
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Random (randomRange)
import Reversi.Com (diskCount, miniMax)
import Reversi.Game (Strategy, silent)
import Reversi.Heuristics.Eval (EvalNN, evalBoard, learnGameEvalNN, loadEvalNN, randEvalNN, saveEvalNN)
import Reversi.System (availablePositions, countDisks, initialBoard, nextBoards)
import Reversi.Util (maximumIs, minimumIs, randArr)

initGen :: Int
initGen = 67400

steps :: Int
steps = 1000000

main :: Effect Unit
main = do
  initEvalNN <- liftEffect $ randEvalNN
  initEvalNN2 <- liftEffect $ randEvalNN
  evalNNM /\ evalNNM2 <- liftEffect $ loadEvalNN (show initGen) initEvalNN initEvalNN2
  launchAff_ do
    learned <- foldRecM step (evalNNM /\ evalNNM2) $ initGen .. (initGen + steps - 1)
    liftEffect $ uncurry (saveEvalNN (show $ initGen + steps)) $ learned

step :: (EvalNN /\ EvalNN) -> Int -> Aff (Tuple EvalNN EvalNN)
step (evalNN1 /\ evalNN2) i = do
  log $ "Step: " <> show i
  if i `mod` 100 == 0 then liftEffect $ saveEvalNN (show i) evalNN1 evalNN2
  else pure unit
  last /\ history <- silent (com true evalNN1 evalNN2) (com false evalNN1 evalNN2) initialBoard
  let
    b /\ w = countDisks last
    isWinB
      | b > w = Just true
      | b < w = Just false
      | otherwise = Nothing
    Tuple newNN diff = learnGameEvalNN 0.05 evalNN1 evalNN2 (take 58 history) isWinB
  log $ "Diff: " <> show diff
  pure newNN

com :: Boolean -> EvalNN -> EvalNN -> Strategy
com c evalNN evalNN2 board = do
  r <- liftEffect $ randomRange 0.0 1.0
  let
    bc /\ wc = countDisks board
    turn = bc + wc
    isRandom = r < 0.02
    avs = availablePositions board c
    nb = nextBoards board c
  if isRandom then liftEffect $ fromMaybe { h: 0, w: 0 } <$> randArr avs
  else do
    Milliseconds st <- liftEffect $ unInstant <$> now
    let
      go :: Int -> Effect (Array Number)
      go n = do
        let
          p = map (miniMax (evalBoard evalNN evalNN2) nextBoards (not c) n) nb
        Milliseconds et <- liftEffect $ unInstant <$> now
        if et - st < 50.0 && n < 20 then
          go (n + 1)
        else
          pure p
    points <- if turn < 57 then (liftEffect $ go 1) else pure $ map (miniMax diskCount nextBoards (not c) 10) nb
    let
      is = (if c then maximumIs 0.0001 else minimumIs 0.0001) points
    i <- liftEffect $ fromMaybe 0 <$> randArr is
    pure $ fromMaybe { h: 0, w: 0 } $ avs !! i
