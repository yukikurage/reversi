module Reversi.Heuristics.Learn where

import Prelude

import Data.Array ((!!), (..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (fromMaybe)
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))
import Effect (Effect, forE)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Reversi.Com (miniMax)
import Reversi.Game (Player, gameStart)
import Reversi.Heuristics.Eval (evalBoard, initParams)
import Reversi.System (availablePositions, boardToString, countDisks, initialBoard, nextBoards)

main :: Effect Unit
main = launchAff_ do
  for_ (0 .. 999) \i -> do
    _ <- gameStart (evalInitPlayer 0) (evalInitPlayer 0) initialBoard
    log $ "Game finished." <> show i

evalInitPlayer :: Int -> Player
evalInitPlayer depth = \c ->
  let
    ip = initParams 8 8
  in
    { strategy: \board ->
        let
          avs = availablePositions board c
          nb = nextBoards board c
          points = map (miniMax (evalBoard ip c) nextBoards (not c) depth) nb
          maxI /\ _ = foldlWithIndex (\i (accI /\ accP) p -> if p > accP then (i /\ p) else (accI /\ accP)) (-1 /\ -infinity) points
        in
          pure $ fromMaybe (-1 /\ -1) $ avs !! maxI
    , turnCallback: \_ -> pure unit
    , invalidCallback: \_ -> pure unit
    , skipCallback: \_ -> pure unit
    }
