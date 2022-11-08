module Reversi.Util where

import Prelude

import Data.Array (filter, length, null, (!!), (..))
import Data.Foldable (maximum, minimum)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (randomInt)

maximumIs :: Array Number -> Array Int
maximumIs arr | null arr = []
maximumIs arr =
  let
    max = maximum arr
  in
    filter (\i -> arr !! i == max) $ 0 .. (length arr - 1)

minimumIs :: Array Number -> Array Int
minimumIs arr | null arr = []
minimumIs arr =
  let
    min = minimum arr
  in
    filter (\i -> arr !! i == min) $ 0 .. (length arr - 1)

randArr :: forall a. Array a -> Effect (Maybe a)
randArr arr = do
  let
    l = length arr
  if l == 0 then
    pure Nothing
  else do
    i <- randomInt 0 (l - 1)
    pure $ arr !! i
