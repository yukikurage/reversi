module Reversi.Util where

import Prelude

import Data.FoldableWithIndex (foldlWithIndex)
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))

maximumI :: Array Number -> Int
maximumI arr =
  let
    maxI /\ _ = foldlWithIndex (\i (maxI /\ max) x -> if x > max then i /\ x else maxI /\ max) ((-1) /\ -infinity) arr
  in
    maxI

minimumI :: Array Number -> Int
minimumI arr =
  let
    minI /\ _ = foldlWithIndex (\i (minI /\ min) x -> if x < min then i /\ x else minI /\ min) ((-1) /\ infinity) arr
  in
    minI