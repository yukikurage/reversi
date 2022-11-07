module Reversi.Game where

{-

Effective implementation

-}

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Reversi.System (Board, availablePositions, isGameOver, putDisk)

type Strategy =
  Board -- Current board
  -> Aff (Int /\ Int) -- (h /\ w), the position to put a disk

type Player =
  Boolean -- True: Black, False: White
  -> { strategy :: Strategy
     , turnCallback :: Board -> Aff Unit
     , invalidCallback :: Board -> Aff Unit
     , skipCallback :: Board -> Aff Unit
     }

gameStart :: Player -> Player -> Board -> Aff Board
gameStart mkBlack mkWhite initBoard = do
  let
    black = mkBlack true
    white = mkWhite false

    loop :: Boolean -> Board -> Aff Board
    loop _ board | isGameOver board = pure board
    loop player board | availablePositions board player == [] = do
      (if player then black else white).skipCallback board
      (if player then white else black).turnCallback board
      loop (not player) board
    loop player board = do
      let
        pl = if player then black else white
        op = if player then white else black
      h /\ w <- pl.strategy board
      let
        boardM = putDisk h w player board
      case boardM of
        Nothing -> do
          pl.invalidCallback board
          loop player board
        Just newB -> do
          op.turnCallback newB
          loop (not player) newB

  black.turnCallback initBoard
  loop true initBoard
