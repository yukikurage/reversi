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
import Reversi.Heuristics.NN (type (>|>), NNFunction(..), NNMatrix(..), Vector, forwardPropagation, mRandom, readCSV, teach, vCons, vFromArray, vSingleton, vToA, writeCSV, (>|>))
import Reversi.System (Board, countDisks, flipAll, indexB)

type EvalNN = NNMatrix 65 66 12
  >|> NNFunction 12 12
  >|> NNMatrix 12 13 12
  >|> NNFunction 12 12
  >|> NNMatrix 12 13 1
  >|> NNFunction 1 1

boardToInput :: Board -> Vector 65 Number
boardToInput board = unsafePartial $ fromJust do
  let
    cellToNum = case _ of
      Nothing -> 0.5
      Just true -> 1.0
      Just false -> 0.0
    b /\ w = countDisks board
    turn = b + w
  bs <- vFromArray $ catMaybes do
    h <- 0 .. 7
    w <- 0 .. 7
    pure $ cellToNum <$> indexB board { h, w }
  pure $ vCons (toNumber turn) bs

randEvalNN :: Effect EvalNN
randEvalNN = do
  matrix1 <- mRandom
  matrix2 <- mRandom
  matrix3 <- mRandom
  pure $ NNMatrix matrix1
    >|> NNRelu
    >|> NNMatrix matrix2
    >|> NNSigmoid
    >|> NNMatrix matrix3
    >|> NNSigmoid

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
