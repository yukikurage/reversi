module Reversi.Heuristics.Eval where

import Prelude

import Data.Array (catMaybes, foldl, (..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Partial.Unsafe (unsafePartial)
import Reversi.Heuristics.NN (type (>-<), type (>|>), NNFunction(..), NNId(..), NNMatrix(..), NNSumCopy(..), Vector, forwardPropagation, mRandom, readCSV, sigmoid, teach, vAppend, vFromArray, vSingleton, vToA, writeCSV, (>-<), (>|>))
import Reversi.System (Board, countDisks, flipAll, indexB)

type EvalNNDiag = NNMatrix 16 17 8 >|> NNFunction 8 8 >|> NNMatrix 8 9 4
type EvalNNTBRL = NNMatrix 16 17 8 >|> NNFunction 8 8 >|> NNMatrix 8 9 4
type EvalNNCenter = NNMatrix 16 17 8 >|> NNFunction 8 8 >|> NNMatrix 8 9 4
type EvalNNBoard = (NNSumCopy (NNSumCopy (EvalNNDiag)) >-< NNSumCopy (NNSumCopy (EvalNNTBRL)) >-< EvalNNCenter) >|> NNMatrix 12 13 1 >|> NNFunction 1 1
type EvalNN = (EvalNNBoard >-< EvalNNBoard >-< NNId 1) >|> NNMatrix 3 4 1 >|> NNFunction 1 1

boardToInput :: Board -> Vector 289 Number
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
  let
    res = tl
      `vAppend` tr
      `vAppend` br
      `vAppend` bl
      `vAppend` t
      `vAppend` r
      `vAppend` b
      `vAppend` l
      `vAppend` c
    b /\ w = countDisks board
    turn = b + w
  pure $ res `vAppend` res `vAppend` vSingleton (sigmoid $ toNumber turn)

randEvalNN :: Effect EvalNN
randEvalNN = do
  matrixDiag1 <- mRandom
  matrixDiag2 <- mRandom

  matrixTBRL1 <- mRandom
  matrixTBRL2 <- mRandom

  matrixCenter1 <- mRandom
  matrixCenter2 <- mRandom

  matrixBoard1 <- mRandom

  matrix1 <- mRandom

  let
    evalNNDiag = NNMatrix matrixDiag1 >|> NNRelu >|> NNMatrix matrixDiag2
    evalNNTBRL = NNMatrix matrixTBRL1 >|> NNRelu >|> NNMatrix matrixTBRL2
    evalNNCenter = NNMatrix matrixCenter1 >|> NNRelu >|> NNMatrix matrixCenter2
    evalNNBoard = (NNSumCopy (NNSumCopy (evalNNDiag)) >-< NNSumCopy (NNSumCopy (evalNNTBRL)) >-< evalNNCenter) >|> NNMatrix matrixBoard1 >|> NNRelu
    evalNN = (evalNNBoard >-< evalNNBoard >-< NNId) >|> NNMatrix matrix1 >|> NNSigmoid
  pure evalNN

evalBoard :: EvalNN -> Board -> Number
evalBoard nn board = vToA $ forwardPropagation nn $ boardToInput board

learnEvalNN :: Number -> EvalNN -> Board -> Maybe Boolean -> EvalNN /\ Number
learnEvalNN learningRate nn board expected =
  let
    t = case expected of
      Just true -> 1.0
      Just false -> 0.0
      Nothing -> 0.5
    Tuple newNN output = teach learningRate nn (boardToInput board) $ vSingleton t
    d = (vToA output - t) * (vToA output - t)
  in
    newNN /\ d

learnGameEvalNN :: Number -> EvalNN -> Array Board -> Maybe Boolean -> EvalNN /\ Number
learnGameEvalNN learningRate nn history teach =
  let
    nnAndDiff = foldl
      ( \(acc /\ d) b ->
          let
            newNN /\ dAdd = learnEvalNN learningRate acc b teach
          in
            newNN /\ (d + dAdd)
      )
      (nn /\ 0.0)
      history
  in
    foldl
      ( \(acc /\ d) b ->
          let
            newNN /\ dAdd = learnEvalNN learningRate acc (flipAll b) (map not teach)
          in
            newNN /\ (d + dAdd)
      )
      nnAndDiff
      history

saveEvalNN :: String -> EvalNN -> Effect Unit
saveEvalNN filename evalNN = do
  writeTextFile UTF8 ("nnCSV/" <> filename) $ joinWith "," $ writeCSV (evalNN :: EvalNN)

loadEvalNN :: String -> Effect EvalNN
loadEvalNN filename = do
  str <- readTextFile UTF8 ("nnCSV/" <> filename)
  pure $ fst $ unsafePartial $ fromJust $ readCSV $ split (Pattern ",") str
