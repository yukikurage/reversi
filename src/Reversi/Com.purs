module Reversi.Com where

{-

Computer strategies

-}

import Prelude

import Data.Array (null)
import Data.Foldable (maximum, minimum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))
import Reversi.System (Board, countDisks)

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
      evalF target
    else
      let
        nexts' = map (miniMax evalF getNext (not player) (depth - 1)) nexts
      in
        if player then
          fromMaybe (-infinity) $ maximum nexts'
        else
          fromMaybe infinity $ minimum nexts'

diskCount :: Board -> Number
diskCount b =
  let
    bl /\ wh = countDisks b
  in
    toNumber $ bl - wh
