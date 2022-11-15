module Reversi.Heuristics.Eval where

import Prelude

import Data.Array (catMaybes, foldl, (..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Partial.Unsafe (unsafePartial)
import Reversi.Heuristics.NN (Matrix, NN, Vector, learnNN, mRandom, nnLoadString, nnMatrix, nnRelu, nnSigmoid, nnStack, nnStackCopy, nnSum, nnToString, runNN, vAppend, vFromArray, vSingleton, vToA, (>|>))
import Reversi.System (Board, flipAll, indexB)

type EvalNN = NN 144 1 Number

boardToInput :: Board -> Vector 144 Number
boardToInput board = unsafePartial $ fromJust do
  let
    cellToNum = case _ of
      Nothing -> 0.0
      Just true -> 1.0
      Just false -> -1.0
  tl :: Vector 16 Number <- vFromArray $ catMaybes do
    h <- 0 .. 3
    w <- 0 .. 3
    pure $ cellToNum <$> indexB board { h, w }
  tr :: Vector 16 Number <- vFromArray $ catMaybes do
    w <- 7 .. 4
    h <- 0 .. 3
    pure $ cellToNum <$> indexB board { h, w }
  br :: Vector 16 Number <- vFromArray $ catMaybes do
    h <- 7 .. 4
    w <- 7 .. 4
    pure $ cellToNum <$> indexB board { h, w }
  bl :: Vector 16 Number <- vFromArray $ catMaybes do
    w <- 0 .. 3
    h <- 7 .. 4
    pure $ cellToNum <$> indexB board { h, w }
  t :: Vector 16 Number <- vFromArray $ catMaybes do
    h <- 0 .. 3
    w <- 2 .. 5
    pure $ cellToNum <$> indexB board { h, w }
  r :: Vector 16 Number <- vFromArray $ catMaybes do
    w <- 7 .. 4
    h <- 2 .. 5
    pure $ cellToNum <$> indexB board { h, w }
  b :: Vector 16 Number <- vFromArray $ catMaybes do
    h <- 7 .. 4
    w <- 5 .. 2
    pure $ cellToNum <$> indexB board { h, w }
  l :: Vector 16 Number <- vFromArray $ catMaybes do
    w <- 0 .. 3
    h <- 5 .. 2
    pure $ cellToNum <$> indexB board { h, w }
  c :: Vector 16 Number <- vFromArray $ catMaybes do
    h <- 2 .. 5
    w <- 2 .. 5
    pure $ cellToNum <$> indexB board { h, w }
  pure $
    tl
      `vAppend` tr
      `vAppend` br
      `vAppend` bl
      `vAppend` t
      `vAppend` r
      `vAppend` b
      `vAppend` l
      `vAppend` c

randEvalNN :: Effect EvalNN
randEvalNN = do
  matrix1 :: Matrix 8 17 Number <- mRandom
  matrix2 :: Matrix 8 9 Number <- mRandom
  matrix3 :: Matrix 1 9 Number <- mRandom
  let
    nnPart = nnMatrix matrix1 >|> nnRelu >|> nnMatrix matrix2 >|> nnRelu >|> nnMatrix matrix3 >|> nnSigmoid
    nnFour = nnStackCopy (nnStackCopy nnPart) >|> nnSum
    nnStacked = nnFour `nnStack` nnFour `nnStack` nnPart
  matrix4 :: Matrix 1 4 Number <- mRandom
  pure $ nnStacked >|> nnMatrix matrix4 >|> nnSigmoid

evalBoard :: EvalNN -> Board -> Number
evalBoard nn board = vToA $ runNN nn $ boardToInput board

learnEvalNN :: Number -> EvalNN -> Board -> Maybe Boolean -> Tuple EvalNN Number
learnEvalNN learningRate nn board teach =
  let
    t = case teach of
      Just true -> 1.0
      Just false -> 0.0
      Nothing -> 0.5
    Tuple newNN output = learnNN learningRate nn (boardToInput board) $ vSingleton t
    d = (vToA output - t) * (vToA output - t)
  in
    Tuple newNN d

learnGameEvalNN :: Number -> EvalNN -> Array Board -> Maybe Boolean -> Tuple EvalNN Number
learnGameEvalNN learningRate nn history teach =
  let
    nn' = foldl
      ( \(Tuple acc d) b ->
          let
            Tuple newNN dAdd = learnEvalNN learningRate acc b teach
          in
            Tuple newNN (d + dAdd)
      )
      (Tuple nn 0.0)
      history
  in
    foldl
      ( \(Tuple acc d) b ->
          let
            Tuple newNN dAdd = learnEvalNN learningRate acc (flipAll b) (map not teach)
          in
            Tuple newNN (d + dAdd)
      )
      nn'
      history

saveEvalNN :: String -> EvalNN -> Effect Unit
saveEvalNN filename evalNN = do
  writeTextFile UTF8 ("nn/" <> filename) $ nnToString evalNN

loadEvalNN :: String -> EvalNN -> Effect (Maybe EvalNN)
loadEvalNN filename evalNN = do
  str <- readTextFile UTF8 ("nn/" <> filename)
  pure $ nnLoadString str evalNN
