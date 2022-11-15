module Test.Main where

import Prelude

import Data.Array (catMaybes, foldRecM, zipWith, (..))
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Random (randomRange)
import Reversi.Com (diskCount, miniMax)
import Reversi.Heuristics.NN (Matrix, Vector, learnNN, mRandom, nnMatrix, nnSigmoid, vDiff2, vSingleton, vToA, (>|>))
import Reversi.System (availablePositions, boardToString, indexToString, initialBoard, nextBoards, putDisk)
import Reversi.Util (maximumIs, minimumIs)

testFunc :: Vector 1 Number -> Vector 1 Number
testFunc = vToA >>> (\x -> x `pow` 2.0 + 10.0) >>> vSingleton

{-
  A B C D E F G H
1 ● ● ● ○ ● ● 　　
2 ○ ● ○ ○ ○ ○ ○ ○
3 ○ ○ ● ○ ○ ○ ○ ○
4 ○ ● ○ ● ○ ○ ○ ○
5 ○ ● ● ○ ○ ● ○ ○
6 ○　  ● ○ ● ● ● ○
7 ○ 　 ● ● ● ○ ● ●
8 ○ ○ ● ● ● ● ● ●
-}

b = Just true
w = Just false
e = Nothing

testBoard =
  [ [ b, b, b, w, b, b, e, e ]
  , [ w, b, w, w, w, w, w, w ]
  , [ w, w, b, w, w, w, w, w ]
  , [ w, b, w, b, w, w, w, w ]
  , [ w, b, b, w, w, b, w, w ]
  , [ w, e, b, w, b, b, b, w ]
  , [ w, e, b, b, b, w, b, b ]
  , [ w, w, b, b, b, b, b, b ]
  ]

main :: Effect Unit
main = do
  log $ boardToString testBoard
  let
    avs = availablePositions testBoard true
    nb = catMaybes do
      p <- avs
      pure $ putDisk p true testBoard
    points = map (miniMax diskCount nextBoards (not true) 10) nb
    is = (if true then maximumIs 0.001 else minimumIs 0.001) points
  log $ joinWith "\n" $ zipWith (\pos point -> indexToString pos <> " " <> show point) avs points
-- initMatrix1 :: Matrix 6 2 Number <- mRandom
-- initMatrix2 :: Matrix 6 7 Number <- mRandom
-- initMatrix3 :: Matrix 1 7 Number <- mRandom
-- let
--   initNN = nnMatrix initMatrix1 >|> nnSigmoid >|> nnMatrix initMatrix2 >|> nnSigmoid >|> nnMatrix initMatrix3
--   step prevNN stepNum = do
--     randInput <- vSingleton <$> randomRange (-2.0) 2.0
--     let
--       expected = testFunc randInput
--       Tuple newNN output = learnNN 0.01 prevNN randInput expected
--     when (stepNum `mod` 100 == 0) do
--       -- log $ "Step " <> show stepNum
--       -- log $ "Input: " <> show randInput
--       -- log $ "Output: " <> show output
--       -- log $ "Expected: " <> show expected
--       log $ "Diff: " <> show (vDiff2 output expected)
--     pure newNN
-- void $ foldRecM step initNN $ 0 .. 10000
