module Reversi.Game where

{-

Effective implementation

-}

import Prelude

import Control.Monad.ST.Global (toEffect)
import Data.Array.ST as AST
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Reversi.System (Board, Pos, availablePositions, boardToString, indexToString, isGameOver, putDisk)

type Strategy = Board -> Aff Pos

printPlayer :: Boolean -> String
printPlayer true = " ● "
printPlayer false = " ○ "

console :: Strategy -> Strategy -> Board -> Aff (Board /\ Array Board)
console black white initBoard = do
  arrST <- liftEffect $ toEffect AST.new
  _ <- liftEffect $ toEffect $ AST.push initBoard arrST
  let
    loop :: Boolean -> Board -> Aff Board
    loop _ board | isGameOver board = pure board
    loop player board | availablePositions board player == [] = do
      log $ "Skipped: " <> printPlayer player
      loop (not player) board
    loop player board = do
      log $ "Next: " <> printPlayer player
      log $ boardToString board
      let
        pl = if player then black else white
      pos <- pl board
      let
        boardM = putDisk pos player board
      case boardM of
        Nothing -> do
          log $ "Invalid position: " <> indexToString pos
          loop player board
        Just newB -> do
          _ <- liftEffect $ toEffect $ AST.push newB arrST
          log $ "Put: " <> printPlayer player <> " → " <> indexToString pos <> "."
          loop (not player) newB

  last <- loop true initBoard
  history <- liftEffect $ toEffect $ AST.freeze arrST
  pure $ last /\ history

silent :: Strategy -> Strategy -> Board -> Aff (Board /\ Array Board)
silent black white initBoard = do
  arrST <- liftEffect $ toEffect AST.new
  _ <- liftEffect $ toEffect $ AST.push initBoard arrST
  let
    loop :: Boolean -> Board -> Aff Board
    loop _ board | isGameOver board = pure board
    loop player board | availablePositions board player == [] = do
      loop (not player) board
    loop player board = do
      let
        pl = if player then black else white
      pos <- pl board
      let
        boardM = putDisk pos player board
      case boardM of
        Nothing -> do
          loop player board
        Just newB -> do
          _ <- liftEffect $ toEffect $ AST.push newB arrST
          loop (not player) newB

  last <- loop true initBoard
  history <- liftEffect $ toEffect $ AST.freeze arrST
  pure $ last /\ history
