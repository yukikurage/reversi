module Reversi.Util where

import Prelude

import Data.Array (filter, length, null, (!!), (..))
import Data.Foldable (maximum, minimum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (infinity)
import Effect (Effect)
import Effect.Random (randomInt)

-- | 評価の差が最大から num 以下のものの index を返す
maximumIs :: Number -> Array Number -> Array Int
maximumIs _ arr | null arr = []
maximumIs x arr =
  let
    max = maximum arr
  in
    filter
      ( \i -> fromMaybe false do
          v <- arr !! i
          m <- max
          pure $ v >= m - x
      ) $ 0 .. (length arr - 1)

-- | 評価の差が最小から num 以下のものの index を返す
minimumIs :: Number -> Array Number -> Array Int
minimumIs _ arr | null arr = []
minimumIs x arr =
  let
    min = minimum arr
  in
    filter
      ( \i -> fromMaybe false do
          v <- arr !! i
          m <- min
          pure $ v <= m + x
      ) $ 0 .. (length arr - 1)

maximumI :: Array Number -> Int
maximumI arr = _.index $ foldlWithIndex (\index acc value -> if value > acc.value then { value, index } else acc) { value: -infinity, index: -1 } arr

minimumI :: Array Number -> Int
minimumI arr = _.index $ foldlWithIndex (\index acc value -> if value < acc.value then { value, index } else acc) { value: infinity, index: -1 } arr

randArr :: forall a. Array a -> Effect (Maybe a)
randArr arr = do
  let
    l = length arr
  if l == 0 then
    pure Nothing
  else do
    i <- randomInt 0 (l - 1)
    pure $ arr !! i
