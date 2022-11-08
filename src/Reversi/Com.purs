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
-- | 最善手を探す
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
        nexts' = map (\x -> miniMax evalF getNext (not player) (depth - 1) x) nexts
      in
        if player then
          fromMaybe (-infinity) $ maximum nexts'
        else
          fromMaybe infinity $ minimum nexts'

diskCount :: Board -> Boolean -> Number
diskCount b c =
  let
    bl /\ wh = countDisks b
  in
    toNumber if c then bl - wh else wh - bl
