module Test.Main where

import Prelude

import Data.Array (foldRecM, (..))
import Data.Number (pow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Random (randomRange)
import Reversi.Heuristics.NN (Matrix, Vector, learnNN, mRandom, nnMatrix, nnSigmoid, vDiff2, vSingleton, vToA, (>|>))
import Reversi.System (initialBoard)

testFunc :: Vector 1 Number -> Vector 1 Number
testFunc = vToA >>> (\x -> x `pow` 2.0 + 10.0) >>> vSingleton

main :: Effect Unit
main = do
  initMatrix1 :: Matrix 6 2 Number <- mRandom
  initMatrix2 :: Matrix 6 7 Number <- mRandom
  initMatrix3 :: Matrix 1 7 Number <- mRandom
  let
    initNN = nnMatrix initMatrix1 >|> nnSigmoid >|> nnMatrix initMatrix2 >|> nnSigmoid >|> nnMatrix initMatrix3
    step prevNN stepNum = do
      randInput <- vSingleton <$> randomRange (-2.0) 2.0
      let
        expected = testFunc randInput
        Tuple newNN output = learnNN 0.01 prevNN randInput expected
      when (stepNum `mod` 100 == 0) do
        -- log $ "Step " <> show stepNum
        -- log $ "Input: " <> show randInput
        -- log $ "Output: " <> show output
        -- log $ "Expected: " <> show expected
        log $ "Diff: " <> show (vDiff2 output expected)
      pure newNN
  void $ foldRecM step initNN $ 0 .. 10000