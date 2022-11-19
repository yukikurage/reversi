module Test.Main where

import Prelude

import Control.Apply (lift2)
import Data.Array (foldRecM, (..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number (pow)
import Data.Reflectable (class Reflectable)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Random (randomRange)
import Partial.Unsafe (unsafePartial)
import Reversi.Heuristics.NN (type (>|>), Matrix, NNFunction(..), NNMatrix(..), Vector, mRandom, readCSV, teach, vSingleton, vToA, writeCSV, (>|>))

testFunc :: Vector 1 Number -> Vector 1 Number
testFunc = vToA >>> (\x -> x * 2.0 + 10.0) >>> vSingleton

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

b :: Maybe Boolean
b = Just true

w :: Maybe Boolean
w = Just false

e :: forall a117. Maybe a117
e = Nothing

testBoard :: Array (Array (Maybe Boolean))
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

vDiff2 :: forall i. Reflectable i Int => Vector i Number -> Vector i Number -> Number
vDiff2 v1 v2 = foldl (\acc x -> acc + x * x) 0.0 (lift2 (-) v1 v2)

main :: Effect Unit
main = do
  -- log $ boardToString testBoard
  -- let
  --   avs = availablePositions testBoard true
  --   nb = catMaybes do
  --     p <- avs
  --     pure $ putDisk p true testBoard
  --   points = map (miniMax diskCount nextBoards (not true) 10) nb
  --   is = (if true then maximumIs 0.001 else minimumIs 0.001) points
  -- log $ joinWith "\n" $ zipWith (\pos point -> indexToString pos <> " " <> show point) avs points

  initMatrix1 :: Matrix 4 2 Number <- mRandom
  initMatrix3 :: Matrix 1 5 Number <- mRandom
  let
    initNN :: NNMatrix 1 2 4 >|> NNFunction 4 4 >|> NNMatrix 4 5 1
    initNN = NNMatrix initMatrix1 >|> NNSigmoid >|> NNMatrix initMatrix3
    -- initNN = fst $ unsafePartial $ fromJust $ readCSV [ "-1.231177506053002", "1.004135933239787", "1.7074635593527552", "1.3219870641275338", "-0.8620685115451133", "0.867490713798025", "-1.4021524507875254", "1.1240538535991857", "sigmoid", "4.562738875322424", "3.729775824387187", "4.449298898806501", "1.525035894625125", "1.825657051523913" ]
    step prevNN stepNum = do
      randInput <- vSingleton <$> randomRange (-2.0) 2.0
      let
        expected = testFunc randInput
        Tuple newNN output = teach 0.04 prevNN randInput expected
      when (stepNum `mod` 100 == 0) do
        -- log $ "Step " <> show stepNum
        -- log $ "Input: " <> show randInput
        -- log $ "Output: " <> show output
        -- log $ "Expected: " <> show expected
        log $ "Diff: " <> show (vDiff2 output expected)
      pure newNN
  nnFinal <- foldRecM step initNN $ 0 .. 10000
  log $ "Final NN: " <> show (writeCSV nnFinal)
