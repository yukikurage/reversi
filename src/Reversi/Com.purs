module Reversi.Com where

{-

Computer strategies

-}

import Prelude

import Data.Array (foldl, null)
import Data.Int (toNumber)
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))
import Reversi.Heuristics.NN (sigmoid)
import Reversi.System (Board, countDisks, isGameOver, nextBoards)

-- | miniMax
-- | 現在から最善手を打った時の最終的な盤面のスコアを返す
-- | evalF : 評価関数
-- |   evalF が大きいほうが true にとって有利
-- | getNext : 次の盤面を得る関数
-- | player : 次に駒をおくプレイヤー
-- | target : 評価する盤面の対象
miniMax :: (Board -> Number) -> Boolean -> Int -> Board -> Number
miniMax evalF _ 0 target = evalF target
miniMax _ _ _ target | isGameOver target = greater target
miniMax evalF player depth target =
  let
    nexts = nextBoards target player
  in
    if null nexts then
      miniMax evalF (not player) depth target
    else if player then
      foldl (\acc x -> max (miniMax evalF (not player) (depth - 1) x) acc) (-infinity) nexts
    else
      foldl (\acc x -> min (miniMax evalF (not player) (depth - 1) x) acc) infinity nexts

alphaBeta :: (Board -> Number) -> Boolean -> Int -> Number -> Number -> Board -> Number
alphaBeta evalF _ 0 _ _ target = evalF target
alphaBeta _ _ _ _ _ target | isGameOver target = greater target
alphaBeta evalF player depth alpha beta target =
  let
    nexts = nextBoards target player
  in
    if null nexts then
      alphaBeta evalF (not player) depth alpha beta target
    else if player then
      foldl (\acc x -> if acc > beta then acc else max (alphaBeta evalF (not player) (depth - 1) acc beta x) acc) (-infinity) nexts
    else
      foldl (\acc x -> if acc < alpha then acc else min (alphaBeta evalF (not player) (depth - 1) alpha acc x) acc) infinity nexts

diskCount :: Board -> Number
diskCount b =
  let
    bl /\ wh = countDisks b
  in
    sigmoid $ toNumber $ bl - wh

greater :: Board -> Number
greater b =
  let
    bl /\ wh = countDisks b
  in
    if bl > wh then 1.0 else if bl < wh then 0.0 else 0.5
