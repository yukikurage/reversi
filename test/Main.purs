module Test.Main where

import Prelude

import Data.Int (toNumber)
import Data.Reflectable (class Reflectable)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Reversi.Heuristics.NN (Matrix, NN, Vector, layerComb, layerRelu, mGenerate, nnPure, runNN, vGenerate, (>|>))

testNN :: forall o. Reflectable o Int => NN o o Number
testNN = nnPure >|> layerRelu

testMatrix :: Matrix 2 3 Number
testMatrix = mGenerate \i j -> toNumber (i + j)

testNN2 :: NN 3 2 Number
testNN2 = nnPure >|> layerRelu >|> layerComb testMatrix

main :: Effect Unit
main = do
  let
    vInput :: Vector 10 Number
    vInput = vGenerate \i -> toNumber i - 5.0
    vOutput = runNN testNN vInput
  logShow vOutput
  let
    vInput2 :: Vector 3 Number
    vInput2 = vGenerate \i -> toNumber i - 1.0
    vOutput2 = runNN testNN2 vInput2
  logShow vOutput2
