module Reversi.Console where

{-

Reversi using the console.

-}

import Prelude

import Data.Array (length, (!!))
import Data.DateTime.Instant (unInstant)
import Data.Int (fromString, toNumber)
import Data.Maybe (fromMaybe, fromMaybe')
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Random (randomInt)
import Reversi.Com (diskCount, miniMax)
import Reversi.Game (Player, gameStart)
import Reversi.Heuristics.Eval (Params, evalBoard, initParams, readFromFile)
import Reversi.System (availablePositions, boardToString, countDisks, initialBoard, nextBoards)
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

gen ∷ Int
gen = 100

main :: Effect Unit
main = launchAff_ do
  params <- liftEffect $ fromMaybe' (\_ -> initParams) <$> readFromFile ("gen/" <> show gen) "0.json"
  params60 <- liftEffect $ fromMaybe' (\_ -> initParams) <$> readFromFile ("gen/60") "0.json"
  lastBoard <- gameStart (evalInitCom params) (evalInitCom params60) initialBoard
  log $ "Game finished. Final board: " <> "\n" <> boardToString lastBoard
  let
    b /\ w = countDisks lastBoard
  log $ "Black: " <> show b <> ", White: " <> show w
  log $ "Winner: " <> if b > w then "Black" else "White"

manual :: Player
manual = \c ->
  { strategy: \_ -> do
      log "Please input the position to put a disk."
      h <- questionValid "h: " fromString $ log "Invalid input."
      w <- questionValid "w: " fromString $ log "Invalid input."
      pure $ h /\ w
  , turnCallback: \board -> do
      log $ boardToString board
      log $ "Your turn. (" <> (if c then "Black" else "White") <> ")"
  , invalidCallback: \_ -> do
      log "Invalid position."
  , skipCallback: \_ -> do
      log "You cannot put a disk. Skip your turn."
  }

randomCom :: Player
randomCom = \c ->
  { strategy: \board -> liftEffect do
      log "Please input the position to put a disk."
      let
        avs = availablePositions board c
        len = length avs
      r <- randomInt 0 (len - 1)
      pure $ fromMaybe (-1 /\ -1) $ avs !! r
  , turnCallback: \board -> do
      log $ boardToString board
      log $ "Com turn. (" <> (if c then "Black" else "White") <> ")"
  , invalidCallback: \_ -> do
      log "Invalid position."
  , skipCallback: \_ -> do
      log "You cannot put a disk. Skip your turn."
  }

diskCountCom :: Int -> Player
diskCountCom depth = \c ->
  { strategy: \board -> do
      let
        avs = availablePositions board c
        nb = nextBoards board c
        bc /\ wc = countDisks board
        turn = bc + wc
        points = map (miniMax diskCount nextBoards (not c) depth) nb
        is = (if c then maximumIs (toNumber turn * 0.02) else minimumIs (toNumber turn * 0.02)) points
      i <- liftEffect $ fromMaybe 1 <$> randArr is
      pure $ fromMaybe (-1 /\ -1) $ avs !! i
  , turnCallback: \board -> do
      log $ boardToString board
      log $ "Com turn. (" <> (if c then "Black" else "White") <> ")"
  , invalidCallback: \_ -> do
      log "Invalid position."
  , skipCallback: \_ -> do
      log "You cannot put a disk. Skip your turn."
  }

evalInitCom :: Params -> Player
evalInitCom params = \c ->
  { strategy: \board -> do
      let
        bc /\ wc = countDisks board
        turn = bc + wc
        avs = availablePositions board c
        nb = nextBoards board c
      Milliseconds st <- liftEffect $ unInstant <$> now
      let
        go :: Int -> Effect (Array Number)
        go n = do
          log $ show n
          let
            p = map (miniMax (if turn < 56 then evalBoard params else diskCount) nextBoards (not c) n) nb
          Milliseconds et <- liftEffect $ unInstant <$> now
          if et - st < 500.0 && n < 20 then
            go (n + 1)
          else
            pure p
      points <- liftEffect $ go 2
      let
        is = (if c then maximumIs (toNumber turn * 0.01) else minimumIs (toNumber turn * 0.01)) points
      i <- liftEffect $ fromMaybe 1 <$> randArr is
      pure $ fromMaybe (-1 /\ -1) $ avs !! i
  , turnCallback: \board -> do
      log $ boardToString board
      log $ "Com turn. (" <> (if c then "Black" else "White") <> ")"
  , invalidCallback: \_ -> do
      log "Invalid position."
  , skipCallback: \_ -> do
      log "You cannot put a disk. Skip your turn."
  }
