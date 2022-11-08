module Reversi.Heuristics.Eval where

import Prelude

import Data.Array (index, mapWithIndex, replicate, zipWithA)
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (randomBool, randomRange)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (mkdir, readTextFile, writeTextFile)
import Node.Path (concat)
import Reversi.System (Board, countDisks)
import Simple.JSON (readJSON_, writeJSON)

-- | ax + b
type CellParams =
  { a :: Number
  , b :: Number
  }

initCellParams :: CellParams
initCellParams =
  { a: 0.0
  , b: 1.0
  }

randCellParams :: Effect CellParams
randCellParams = do
  a <- randomRange (-0.1) 0.1 -- 傾きはゆるくする
  b <- randomRange (-1.0) 1.0
  pure { a, b }

crossCellParams :: CellParams -> CellParams -> Effect CellParams
crossCellParams p1 p2 = do
  af <- randomBool
  bf <- randomBool
  pure
    { a: if af then p1.a else p2.a
    , b: if bf then p1.b else p2.b
    }

mutateCellParams :: CellParams -> Effect CellParams
mutateCellParams p = do
  af <- randomBool
  bf <- randomBool
  a <- if af then randomRange (-0.1) 0.1 else pure p.a
  b <- if bf then randomRange (-1.0) 1.0 else pure p.b
  pure { a, b }

evalCell :: CellParams -> Int -> Boolean -> Maybe Boolean -> Number
evalCell { a, b } turn player cell =
  let
    cellP = case cell of
      Nothing -> 0.0
      Just c -> if c == player then 1.0 else -1.0
  in
    cellP * (a * toNumber turn + b)

-- | 8 × 8
type Params = Array (Array CellParams)

initParams :: Int -> Int -> Params
initParams h w = replicate h (replicate w initCellParams)

randParams :: Int -> Int -> Effect Params
randParams h w = replicateA h (replicateA w randCellParams)

crossParams :: Params -> Params -> Effect Params
crossParams p1 p2 = do
  let
    crossRow :: Array CellParams -> Array CellParams -> Effect (Array CellParams)
    crossRow r1 r2 = zipWithA crossCellParams r1 r2
  zipWithA crossRow p1 p2

mutateParams :: Params -> Effect Params
mutateParams p = for p \row -> for row mutateCellParams

paramsToString :: Params -> String
paramsToString = writeJSON

writeToFile :: String -> String -> Params -> Effect Unit
writeToFile path name params = do
  let
    content = paramsToString params
    fileName = concat [ path, name ]
  mkdir path
  writeTextFile UTF8 fileName content

readFromFile :: String -> String -> Effect (Maybe Params)
readFromFile path name = do
  let
    fileName = concat [ path, name ]
  content <- readTextFile UTF8 fileName
  pure $ readJSON_ content

stringToParams :: String -> Maybe Params
stringToParams = readJSON_

indexB :: Params -> Int -> Int -> Maybe CellParams
indexB board h w = do
  row <- index board h
  index row w

evalBoard :: Params -> Boolean -> Board -> Number
evalBoard params player board =
  let
    bl /\ wh = countDisks board
    turn = bl + wh
    cellParams h w = fromMaybe initCellParams $ indexB params h w
    evalCell' h w = evalCell (cellParams h w) turn player
  in
    sum $ mapWithIndex (\h row -> sum $ mapWithIndex (\w cell -> evalCell' h w cell) row) board
