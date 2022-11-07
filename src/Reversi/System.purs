module Reversi.System where

{-

Definitions of pure functions.

-}

import Prelude

import Control.Alternative (guard)
import Data.Array (fold, foldl, index, last, length, mapWithIndex, takeWhile, updateAt, (..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple.Nested (type (/\), (/\))

-- | Cell
-- | Nothing -> Empty
-- | Just True -> Black
-- | Just False -> White
type Cell = Maybe Boolean

-- | Board
-- | 8x8
type Board = Array (Array Cell)

sizeH :: Int
sizeH = 8

sizeW :: Int
sizeW = 8

indexB :: Board -> Int -> Int -> Maybe Cell
indexB board h w = do
  row <- index board h
  index row w

updateAtB :: Int -> Int -> Cell -> Board -> Maybe Board
updateAtB h w cell board = do
  row <- index board h
  newRow <- updateAt w cell row
  updateAt h newRow board

updateAtIndicesB :: Array ((Int /\ Int) /\ Cell) -> Board -> Maybe Board
updateAtIndicesB indices board = foldl
  ( \macc ((h /\ w) /\ c) -> do
      acc <- macc
      updateAtB h w c acc
  )
  (Just board)
  indices

t :: Cell
t = Just true

f :: Cell
f = Just false

e :: Cell
e = Nothing

-- | Initial Board
initialBoard :: Board
initialBoard =
  [ [ e, e, e, e, e, e, e, e ]
  , [ e, e, e, e, e, e, e, e ]
  , [ e, e, e, e, e, e, e, e ]
  , [ e, e, e, t, f, e, e, e ]
  , [ e, e, e, f, t, e, e, e ]
  , [ e, e, e, e, e, e, e, e ]
  , [ e, e, e, e, e, e, e, e ]
  , [ e, e, e, e, e, e, e, e ]
  ]

-- | Cell -> String
cellToString :: Cell -> String
cellToString c = case c of
  Just true -> "● "
  Just false -> "○ "
  Nothing -> "　"

boardToString :: Board -> String
boardToString b =
  let
    header = "  " <> joinWith " " (map show $ 0 .. (sizeW - 1))
    rowStr i row = show i <> " " <> fold (map cellToString row)
  in
    header <> "\n" <>
      (joinWith "\n" $ mapWithIndex rowStr b)

-- | Put disk on board and compute next board
-- | h, w, color, board -> next board
-- | If the position is invalid, return Nothing
putDisk :: Int -> Int -> Boolean -> Board -> Maybe Board
putDisk h w c b =
  let
    -- | Check if the position is empty
    isEmpty :: Boolean
    isEmpty = indexB b h w == Just Nothing

    -- | Directions to compute
    directions = do
      dh <- [ -1, 0, 1 ]
      dw <- [ -1, 0, 1 ]
      guard $ not $ dh == 0 && dw == 0
      pure $ dh /\ dw

    -- | Compute update indices of the board
    updates :: (Int /\ Int) -> Array (Int /\ Int)
    updates (dirH /\ dirW) =
      let
        connectedOps = takeWhile (\(th /\ tw) -> indexB b th tw == Just (Just $ not c)) do
          i <- 1 .. (max sizeH sizeW - 1)
          pure $ (h + dirH * i) /\ (w + dirW * i)
        isPinched = length connectedOps > 0 && Just (Just c) == do
          lH /\ lW <- last connectedOps
          indexB b (lH + dirH) (lW + dirW)
      in
        if isPinched then connectedOps else []

    -- | Update indices
    updateIndices :: Array (Int /\ Int)
    updateIndices = fold <<< map updates $ directions
  in
    if isEmpty && length updateIndices /= 0 then updateAtIndicesB (map (_ /\ Just c) updateIndices <> [ (h /\ w) /\ Just c ]) b else Nothing

-- | Count disks
countDisks :: Board -> Int /\ Int
countDisks b =
  let
    count :: Cell -> Int
    count c = sum $ map (sum <<< map (\cell -> if cell == c then 1 else 0)) b
  in
    count (Just true) /\ count (Just false)

-- | Enumerate all possible positions
availablePositions :: Board -> Boolean -> Array (Int /\ Int)
availablePositions b c = do
  h <- 0 .. (sizeH - 1)
  w <- 0 .. (sizeW - 1)
  guard $ putDisk h w c b /= Nothing
  pure $ h /\ w

-- | Check if the game is over
isGameOver :: Board -> Boolean
isGameOver b = availablePositions b true == [] && availablePositions b false == []
