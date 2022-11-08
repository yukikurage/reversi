module Reversi.Console where

{-

Reversi using the console.

-}

import Prelude

import Data.Array (length, (!!))
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Reversi.Com (diskCount, miniMax)
import Reversi.Game (Player)
import Reversi.System (availablePositions, boardToString, nextBoards)
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

diskCountCom :: Player
diskCountCom = \c ->
  { strategy: \board ->
      let
        avs = availablePositions board c
        nb = nextBoards board c
        points = map (miniMax (\b -> diskCount b c) nextBoards (not c) 3) nb
        maxI /\ _ = foldlWithIndex (\i (accI /\ accP) p -> if p > accP then (i /\ p) else (accI /\ accP)) (-1 /\ -infinity) points
      in
        pure $ fromMaybe (-1 /\ -1) $ avs !! maxI
  , turnCallback: \board -> do
      log $ boardToString board
      log $ "Com turn. (" <> (if c then "Black" else "White") <> ")"
  , invalidCallback: \_ -> do
      log "Invalid position."
  , skipCallback: \_ -> do
      log "You cannot put a disk. Skip your turn."
  }
