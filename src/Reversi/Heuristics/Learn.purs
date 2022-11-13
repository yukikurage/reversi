module Reversi.Heuristics.Learn where

import Prelude

import Control.Alternative (guard)
import Control.Monad.ST.Global (toEffect)
import Data.Array (foldM, length, replicate, sortBy, take, zip, (!!), (..))
import Data.Array.ST as AST
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe, fromMaybe')
import Data.Traversable (for)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Reversi.Com (miniMax)
import Reversi.Game (Strategy, silent)
import Reversi.Heuristics.Eval (Params, crossParams, evalBoard, initParams, mutateParams, randParams, readFromFile, writeToFile)
import Reversi.System (availablePositions, countDisks, initialBoard, nextBoards)
import Reversi.Util (maximumIs, minimumIs, randArr)

main :: Effect Unit
main = launchAff_ do
  learn 10 410 100000

learn :: Int -> Int -> Int -> Aff Unit
learn saveMod initGen step = do
  newParams <- liftEffect $ for (0 .. 4) \j -> do
    mp <- readFromFile ("gen/" <> show initGen) (show j <> ".json")
    pure $ fromMaybe' (\_ -> initParams) mp
  let
    f :: Array Params -> Int -> Aff (Array Params)
    f selected i = do
      log $ "generation: " <> show i
      params <- liftEffect $ genNext selected
      rs <- ranking params
      let
        select = take 5 rs
      when (i `mod` saveMod == 0) do
        forWithIndex_ select \j p -> liftEffect do
          writeToFile ("gen/" <> show i) (show j <> ".json") p
        log $ "saved"
      log $ "generation: " <> show i <> " finished"
      pure select
  finalSelect <- foldM f newParams ((initGen + 1) .. (initGen + step))
  forWithIndex_ finalSelect \j p -> liftEffect do
    writeToFile ("gen/" <> show (initGen + step)) (show j <> ".json") p
  log $ "saved"

-- | choose top 5
-- | mutate 1
-- | cross 14
-- | copy 5
genNext :: Array Params -> Effect (Array Params)
genNext tops = do
  mutated <- for (0 .. 0) \_ -> do
    i <- randomInt 0 4
    let
      p = fromMaybe' (\_ -> initParams) $ tops !! i
    mutateParams p
  crossed <- for (0 .. 13) \_ -> do
    i <- randomInt 0 4
    j <- randomInt 0 4
    let
      p1 = fromMaybe' (\_ -> initParams) $ tops !! i
      p2 = fromMaybe' (\_ -> initParams) $ tops !! j
    crossParams p1 p2
  pure $ take 5 tops <> crossed <> mutated

genNew :: Int -> Effect (Array Params)
genNew n = replicateA n randParams

ranking :: Array Params -> Aff (Array Params)
ranking params = do
  let
    l = length params
    sets = do
      i <- 0 .. (l - 1)
      j <- 0 .. (l - 1)
      guard $ i /= j
      pure $ i /\ j
  scoreTable <- for sets \(i /\ j) -> do
    let
      p1 = fromMaybe' (\_ -> initParams) $ params !! i
      p2 = fromMaybe' (\_ -> initParams) $ params !! j
    board <- silent (evalPlayer true p1) (evalPlayer false p2) initialBoard
    let
      c1 /\ c2 = countDisks board
      score = if c1 > c2 then 1 else if c1 < c2 then -1 else 0
    log $ "Game finished. " <> show i <> " vs " <> show j <> " : " <> show score
    pure $ (i /\ j) /\ score
  scoresST <- liftEffect $ toEffect $ AST.thaw $ replicate l 0
  for_ scoreTable \((i /\ j) /\ score) -> liftEffect $ toEffect do
    void $ AST.modify i (_ + score) scoresST
    void $ AST.modify j (_ - score) scoresST
  scores <- liftEffect $ toEffect $ AST.freeze scoresST
  let
    scoresWithIndex = zip scores $ 0 .. (l - 1)
    sorted = sortBy (\(a /\ _) (b /\ _) -> compare b a) scoresWithIndex
    rankIndex = map snd sorted
  log $ "Ranking: " <> show rankIndex
  pure $ map (\i -> fromMaybe' (\_ -> initParams) $ params !! i) rankIndex

evalPlayer :: Boolean -> Params -> Strategy
evalPlayer c params board = do
  let
    evalF = evalBoard params
    avs = availablePositions board c
    nb = nextBoards board c
    bc /\ wc = countDisks board
    turn = bc + wc
    points = map (miniMax evalF nextBoards (not c) 1) nb
    is = (if c then maximumIs (toNumber turn * 0.01) else minimumIs (toNumber turn * 0.01)) points
  i <- liftEffect $ fromMaybe 1 <$> randArr is
  pure $ fromMaybe { h: 0, w: 0 } $ avs !! i
