module Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Reversi.Console (manual)
import Reversi.Game (gameStart)
import Reversi.System (boardToString, countDisks, initialBoard)

main :: Effect Unit
main = launchAff_ do
  lastBoard <- gameStart manual manual initialBoard
  log $ "Game finished. Final board: " <> "\n" <> boardToString lastBoard
  let
    b /\ w = countDisks lastBoard
  log $ "Black: " <> show b <> ", White: " <> show w
  log $ "Winner: " <> if b > w then "Black" else "White"
