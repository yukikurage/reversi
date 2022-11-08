module Test.Main where

import Prelude

import Data.Array ((..))
import Data.Maybe (fromMaybe')
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class.Console (log)
import Reversi.Heuristics.Eval (indexP, initParams, readFromFile)

main :: Effect Unit
main = do
  params <- fromMaybe' (\_ -> initParams) <$> readFromFile ("gen/" <> show 55) "0.json"
  for_ (0 .. 7) \i -> do
    for_ (0 .. 7) \j -> do
      log $ (show $ indexP params i j) <> " "
    log "\n"
