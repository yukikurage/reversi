module Reversi.Console where

import Prelude

import Data.Int (fromString)
import Data.Tuple.Nested ((/\))
import Effect.Class.Console (log)
import Reversi.Game (Player)
import Reversi.System (boardToString)
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
