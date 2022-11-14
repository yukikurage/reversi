module Reversi.Console where

{-

Reversi using the console.

-}

import Prelude

import Data.Array (length, (!!))
import Data.DateTime.Instant (unInstant)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Random (randomInt)
import Reversi.Com (diskCount, miniMax)
import Reversi.Game (Strategy, console)
import Reversi.Heuristics.Eval (EvalNN, evalBoard, loadEvalNN, randEvalNN)
import Reversi.System (availablePositions, boardToString, countDisks, initialBoard, nextBoards, stringToIndex)
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
  evalNNM <- liftEffect $ loadEvalNN "100" $ initEvalNN
  case evalNNM of
    Just evalNN -> do
      lastBoard /\ _ <- console (evalCom true evalNN) (diskCountCom false 3) initialBoard
      log $ "Game finished. Final board: " <> "\n" <> boardToString lastBoard
      let
        b /\ w = countDisks lastBoard
      log $ "Black: " <> show b <> ", White: " <> show w
      log $ "Winner: " <> if b > w then "Black" else "White"
    Nothing -> pure unit

manual :: Strategy
manual _ = do
  log "Please input the position to put a disk."
  pos <- questionValid "h: " stringToIndex $ log "Invalid input."
  pure pos

randomCom :: Boolean -> Strategy
randomCom c board = liftEffect do
  log "Please input the position to put a disk."
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
    bc /\ wc = countDisks board
    turn = bc + wc
    points = map (miniMax diskCount nextBoards (not c) depth) nb
    is = (if c then maximumIs (toNumber turn * 0.02) else minimumIs (toNumber turn * 0.02)) points
  i <- liftEffect $ fromMaybe 1 <$> randArr is
  pure $ fromMaybe { h: 0, w: 0 } $ avs !! i

evalCom :: Boolean -> EvalNN -> Strategy
evalCom c evalNN board = do
  let
    bc /\ wc = countDisks board
    turn = bc + wc
    avs = availablePositions board c
    nb = nextBoards board c
  Milliseconds st <- liftEffect $ unInstant <$> now
  let
    go :: Int -> Effect (Array Number)
    go n = do
      let
        p = map (miniMax (if turn < 56 then evalBoard evalNN else diskCount) nextBoards (not c) n) nb
      Milliseconds et <- liftEffect $ unInstant <$> now
      if et - st < 500.0 && n < 20 then
        go (n + 1)
      else
        pure p
  points <- liftEffect $ go 2
  let
    is = (if c then maximumIs (toNumber turn * 0.01) else minimumIs (toNumber turn * 0.01)) points
  i <- liftEffect $ fromMaybe 1 <$> randArr is
  pure $ fromMaybe { h: 0, w: 0 } $ avs !! i
