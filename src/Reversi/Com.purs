module Reversi.Com where

{-

Computer strategies

-}

import Prelude

import Data.Array (foldl, null)
import Data.Int (toNumber)
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Reversi.System (Board, boardToString, countDisks)
import Unsafe.Coerce (unsafeCoerce)

-- | miniMax
-- | 現在から最善手を打った時の最終的な盤面のスコアを返す
-- | evalF : 評価関数
-- |   evalF が大きいほうが true にとって有利
-- | getNext : 次の盤面を得る関数
-- | player : 次に駒をおくプレイヤー
-- | target : 評価する盤面の対象
miniMax :: forall a. (a -> Number) -> (a -> Boolean -> Array a) -> Boolean -> Int -> a -> Number
miniMax evalF _ _ 0 target = evalF target
miniMax evalF getNext player depth target =
  let
    nexts = getNext target player
  in
    if null nexts then
      let
        nextsOp = getNext target $ not player
        _ = map (boardToString <<< unsafeCoerce) nextsOp
      in
        if null nextsOp then
          evalF target
        else if player then
          foldl (\acc x -> min (miniMax evalF getNext player (depth - 1) x) acc) infinity nextsOp
        else
          foldl (\acc x -> max (miniMax evalF getNext player (depth - 1) x) acc) (-infinity) nextsOp

    else if player then
      foldl (\acc x -> max (miniMax evalF getNext (not player) (depth - 1) x) acc) (-infinity) nexts
    else
      foldl (\acc x -> min (miniMax evalF getNext (not player) (depth - 1) x) acc) infinity nexts

diskCount :: Board -> Number
diskCount b =
  let
    bl /\ wh = countDisks b
  in
    toNumber $ bl - wh
