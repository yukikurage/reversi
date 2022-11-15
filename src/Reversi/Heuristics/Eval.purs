module Reversi.Heuristics.Eval where

import Prelude

import Data.Array (catMaybes, foldl, (..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, writeTextFile)
import Partial.Unsafe (unsafePartial)
import Reversi.Heuristics.NN (Matrix, NN, Vector, learnNN, mRandom, nnLoadString, nnMatrix, nnRelu, nnSigmoid, nnStack, nnStackCopy, nnSum, nnToString, runNN, vAppend, vFromArray, vSingleton, vToA, (>|>))
import Reversi.System (Board, countDisks, flipAll, indexB)

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
    nnPart =
      nnMatrix matrix1
        >|> nnRelu
        >|> nnMatrix matrix2
        >|> nnRelu
        >|> nnMatrix matrix3
        >|> nnSigmoid
    nnFour =
      nnStackCopy (nnStackCopy nnPart)
        >|> nnSum
    nnStacked = nnFour `nnStack` nnFour `nnStack` nnPart
  matrix4 :: Matrix 1 4 Number <- mRandom
  pure $ nnStacked >|> nnMatrix matrix4 >|> nnSigmoid

evalBoard :: EvalNN -> EvalNN -> Board -> Number
evalBoard nn1 nn2 board =
  let
    b /\ w = countDisks board
    turn = b + w
  in
    vToA $ runNN (if turn < 44 then nn1 else nn2) $ boardToInput board

learnEvalNN :: Number -> EvalNN -> EvalNN -> Board -> Maybe Boolean -> Tuple (EvalNN /\ EvalNN) Number
learnEvalNN learningRate nn1 nn2 board teach =
  let
    b /\ w = countDisks board
    turn = b + w
    t = case teach of
      Just true -> 1.0
      Just false -> 0.0
      Nothing -> 0.5
    Tuple newNN output = learnNN learningRate (if turn < 44 then nn1 else nn2) (boardToInput board) $ vSingleton t
    d = (vToA output - t) * (vToA output - t)
  in
    Tuple (if turn < 44 then Tuple newNN nn2 else Tuple nn1 newNN) d

learnGameEvalNN :: Number -> EvalNN -> EvalNN -> Array Board -> Maybe Boolean -> Tuple (EvalNN /\ EvalNN) Number
learnGameEvalNN learningRate nn1 nn2 history teach =
  let
    nn' = foldl
      ( \(Tuple (Tuple acc1 acc2) d) b ->
          let
            Tuple newNN dAdd = learnEvalNN learningRate acc1 acc2 b teach
          in
            Tuple newNN (d + dAdd)
      )
      (Tuple (Tuple nn1 nn2) 0.0)
      history
  in
    foldl
      ( \(Tuple (Tuple acc1 acc2) d) b ->
          let
            Tuple newNN dAdd = learnEvalNN learningRate acc1 acc2 (flipAll b) (map not teach)
          in
            Tuple newNN (d + dAdd)
      )
      nn'
      history

saveEvalNN :: String -> EvalNN -> EvalNN -> Effect Unit
saveEvalNN filename evalNN evalNN2 = do
  writeTextFile UTF8 ("nn/" <> filename) $ nnToString evalNN
  writeTextFile UTF8 ("nn2/" <> filename) $ nnToString evalNN2

loadEvalNN :: String -> EvalNN ->EvalNN -> Effect (Tuple EvalNN EvalNN)
loadEvalNN filename evalNN evalNN2 = do
  str <- readTextFile UTF8 ("nn/" <> filename)
  str2 <- readTextFile UTF8 ("nn2/" <> filename)
  pure $ Tuple (unsafePartial $ fromJust $ nnLoadString str evalNN) (unsafePartial $ fromJust $nnLoadString str2 evalNN2)
