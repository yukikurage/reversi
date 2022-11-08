module Reversi.Heuristics.Eval where

import Prelude

import Data.Array (index, mapWithIndex, replicate, zipWithA, (..))
import Data.Foldable (sum)
import Data.Functor (mapFlipped)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (randomBool, randomRange)
import Node.Encoding (Encoding(..))
import Node.FS.Perms (all, mkPerms)
import Node.FS.Sync (mkdir', readTextFile, writeTextFile)
import Node.Path (concat)
import Partial.Unsafe (unsafeCrashWith)
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

evalCell :: CellParams -> Int -> Maybe Boolean -> Number
evalCell { a, b } turn cell =
  let
    cellP = case cell of
      Nothing -> 0.0
      Just c -> if c then 1.0 else -1.0
  in
    cellP * (a * toNumber turn + b)

-- | 10 個
type Params = Array (Array CellParams)

initParams :: Params
initParams = mapFlipped (0 .. 3) \i -> replicate (i + 1) initCellParams

randParams :: Effect Params
randParams = for (0 .. 3) \i -> replicateA (i + 1) randCellParams

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
  mkdir' path { recursive: true, mode: mkPerms all all all }
  writeTextFile UTF8 fileName content

readFromFile :: String -> String -> Effect (Maybe Params)
readFromFile path name = do
  let
    fileName = concat [ path, name ]
  content <- readTextFile UTF8 fileName
  pure $ readJSON_ content

stringToParams :: String -> Maybe Params
stringToParams = readJSON_

indexP :: Params -> Int -> Int -> Maybe CellParams
indexP params h w = do
  let
    h' = if h >= 4 then h - 4 else 3 - h
    w' = if w >= 4 then w - 4 else 3 - w
    h'' /\ w'' = if h' > w' then h' /\ w' else w' /\ h'
  row <- index params h''
  index row w''

-- | 最後 12 手は最大化するように打つ
greedyNum :: Int
greedyNum = 12

evalBoard :: Params -> Board -> Number
evalBoard params board =
  let
    bl /\ wh = countDisks board
    turn = bl + wh
    cellParams h w = fromMaybe' (\_ -> unsafeCrashWith "Error") $ indexP params h w
    evalCell' h w = evalCell (cellParams h w) turn
  in
    sum $ mapWithIndex (\h row -> sum $ mapWithIndex (\w cell -> evalCell' h w cell) row) board
