module Reversi.Console where

{-

Reversi using the console.

-}

import Prelude

import Data.Array (catMaybes, length, zipWith, (!!))
import Data.DateTime.Instant (unInstant)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Random (randomInt)
import Node.Process (exit)
import Reversi.Com (diskCount, miniMax)
import Reversi.Game (Strategy, console)
import Reversi.Heuristics.Eval (EvalNN, evalBoard, loadEvalNN, randEvalNN)
import Reversi.System (availablePositions, boardToString, countDisks, indexToString, initialBoard, nextBoards, putDisk, stringToIndex)
import Reversi.Util (maximumIs, minimumIs, randArr)
import Stdin (questionValid)

{-
type Strategy =
  Board -- Current board
  -> Aff (Int /\ Int) -- (h /\ w), the position to put a disk
-}

{-
type Player = Boolean -- True: Black, False: White
  -> { strategy :: Strategy
  , turnCallback :: Board -> Aff Unit
  , invalidCallback :: Board -> Aff Unit
  , skipCallback :: Board -> Aff Unit
  }
-}

main :: Effect Unit
main = launchAff_ do
  initEvalNN <- liftEffect $ randEvalNN
  initEvalNN2 <- liftEffect $ randEvalNN
  evalNN1 /\ evalNN2 <- liftEffect $ loadEvalNN "68500" initEvalNN initEvalNN2
  lastBoard /\ _ <- console (evalCom true evalNN1 evalNN2) (manual evalNN1 evalNN2) initialBoard
  log $ "Game finished. Final board: " <> "\n" <> boardToString lastBoard
  let
    b /\ w = countDisks lastBoard
  log $ "Black: " <> show b <> ", White: " <> show w
  log $ "Winner: " <> if b > w then "Black" else "White"
  liftEffect $ exit 0

manual :: EvalNN -> EvalNN -> Strategy
manual evalNN evalNN2 b = do
  log $ "Strength: " <> show (evalBoard evalNN evalNN2 b)
  log "Please input the position to put a disk."
  pos <- questionValid "> " stringToIndex $ log "Invalid input."
  pure pos

randomCom :: Boolean -> Strategy
randomCom c board = liftEffect do
  let
    avs = availablePositions board c
    len = length avs
  r <- randomInt 0 (len - 1)
  pure $ fromMaybe { h: 0, w: 0 } $ avs !! r

diskCountCom :: Boolean -> Int -> Strategy
diskCountCom c depth board = do
  let
    avs = availablePositions board c
    nb = nextBoards board c
    points = map (miniMax diskCount nextBoards (not c) depth) nb
    is = (if c then maximumIs 0.001 else minimumIs 0.001) points
  i <- liftEffect $ fromMaybe 1 <$> randArr is
  pure $ fromMaybe { h: 0, w: 0 } $ avs !! i

evalCom :: Boolean -> EvalNN -> EvalNN -> Strategy
evalCom c evalNN evalNN2 board = do
  log $ "Strength: " <> show (evalBoard evalNN evalNN2 board)
  let
    bc /\ wc = countDisks board
    turn = bc + wc
    avs = availablePositions board c
    nb = catMaybes do
      p <- avs
      pure $ putDisk p c board
  Milliseconds st <- liftEffect $ unInstant <$> now
  let
    go :: Int -> Effect (Array Number)
    go n = do
      let
        p = map (miniMax (evalBoard evalNN evalNN2) nextBoards (not c) n) nb
      Milliseconds et <- liftEffect $ unInstant <$> now
      if et - st < 500.0 && n < 20 then
        go (n + 1)
      else
        pure p
  points <- if turn < 57 then (liftEffect $ go 2) else pure $ map (miniMax diskCount nextBoards (not c) 10) nb
  log $ joinWith "\n" $ zipWith (\pos point -> indexToString pos <> " " <> show point) avs points
  let
    is = (if c then maximumIs 0.0001 else minimumIs 0.0001) points
  i <- liftEffect $ fromMaybe 0 <$> randArr is
  pure $ fromMaybe { h: 0, w: 0 } $ avs !! i
