module Reversi.System where

{-

Definitions of pure functions.

-}

import Prelude

import Control.Alternative (guard)
import Control.Monad.ST (run)
import Data.Array (catMaybes, concat, drop, filter, fold, foldl, index, last, length, mapWithIndex, nub, replicate, reverse, take, takeWhile, updateAt, (!!), (..))
import Data.Array.ST as AST
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (for_, sum)
import Data.Int (pow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (for)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)

-- | Cell
-- | Nothing -> Empty
-- | Just true -> Black
-- | Just false -> White
type Cell = Maybe Boolean

size :: Int
size = 8

type BRow = Array Cell

encodeBR :: BRow -> Int
encodeBR row =
  let
    f acc a = acc * 3 + case a of
      Nothing -> 0
      Just true -> 1
      Just false -> 2
  in
    foldl f 0 row

decodeBR :: Int -> BRow
decodeBR n =
  let
    f :: Int → Cell
    f p = case n `div` pow 3 p `mod` 3 of
      0 -> Nothing
      1 -> Just true
      2 -> Just false
      _ -> unsafeCrashWith "decodeBR: impossible"
  in
    map f $ (size - 1) .. 0

canPutBRow :: Int -> Boolean -> BRow -> Boolean
canPutBRow i disk row =
  let
    rs = takeWhile (\c -> eq c $ Just $ not disk) $ drop (i + 1) row
    ls = takeWhile (\c -> eq c $ Just $ not disk) $ reverse $ take i row
    rPinched = (row !! (i + length rs + 1) == Just (Just disk)) && length rs > 0
    lPinched = (row !! (i - length ls - 1) == Just (Just disk)) && length ls > 0
  in
    (rPinched || lPinched) && (row !! i == Just Nothing)

availablePositionsBRowEncoded :: Array (Array Int /\ Array Int)
availablePositionsBRowEncoded =
  let
    f encoded =
      let
        aps disk = filter (\i -> canPutBRow i disk $ decodeBR encoded) $ 0 .. (size - 1)
      in
        aps true /\ aps false
  in
    map f $ 0 .. (pow 3 size - 1)

availablePositionsBRow :: BRow -> Boolean -> Array Int
availablePositionsBRow row disk =
  let
    row8 = row <> (replicate (size - length row) Nothing)
  in
    fromMaybe [] $ map (if disk then fst else snd) $ index availablePositionsBRowEncoded $ encodeBR row8

-- | Board
-- | 8x8
type Board = Array BRow

type Pos = { h :: Int, w :: Int }

isInBoard :: Pos -> Boolean
isInBoard { h, w } = h >= 0 && h < size && w >= 0 && w < size

mulPos :: Pos -> Int -> Pos
mulPos { h, w } n = { h: h * n, w: w * n }

infix 5 mulPos as *<

mulPosFlipped :: Int -> Pos -> Pos
mulPosFlipped n { h, w } = { h: h * n, w: w * n }

addPos :: Pos -> Pos -> Pos
addPos { h: h1, w: w1 } { h: h2, w: w2 } = { h: h1 + h2, w: w1 + w2 }

infixl 4 addPos as >+<

infix 5 mulPosFlipped as >*

indexB :: Board -> Pos -> Maybe Cell
indexB board { h, w } = do
  row <- index board h
  index row w

rowsB :: Array (Array Pos)
rowsB = map (\h -> map (\w -> { h, w }) $ 0 .. (size - 1)) $ 0 .. (size - 1)

colsB :: Array (Array Pos)
colsB = map (\w -> map (\h -> { h, w }) $ 0 .. (size - 1)) $ 0 .. (size - 1)

diagsB :: Array (Array Pos)
diagsB =
  let
    dirPlus = { h: 1, w: 1 }
    dirMinus = { h: 1, w: -1 }

    getDiag :: Pos -> Pos -> Array Pos
    getDiag pos dir = filter isInBoard $ (map (\i -> pos >+< dir *< i) $ (-size + 1) .. (size - 1))
    firstRow = map (\w -> { h: 0, w }) $ 0 .. (size - 1)
    lastRowWithoutEdge = map (\w -> { h: size - 1, w }) $ 1 .. (size - 2)
    samplePoses = firstRow <> lastRowWithoutEdge
  in
    map (\pos -> getDiag pos dirPlus) samplePoses <> map (\pos -> getDiag pos dirMinus) samplePoses

linesB :: Array (Array Pos)
linesB = rowsB <> colsB <> diagsB

-- | Enumerate all possible positions
availablePositions :: Board -> Boolean -> Array Pos
availablePositions b disk = nub $ concat do
  line <- linesB
  let
    lineDisks = map (\p -> join $ indexB b p) line
    available = availablePositionsBRow lineDisks disk
  pure $ filter isInBoard $ catMaybes $ map (index line) available

-- | Return next boards
-- | Sorted by same order as availablePositions
nextBoards :: Board -> Boolean -> Array Board
nextBoards b disk = catMaybes do
  p <- availablePositions b disk
  pure $ putDisk p disk b

-- | Check if the game is over
isGameOver :: Board -> Boolean
isGameOver b = availablePositions b true == [] && availablePositions b false == []

updateAtB :: Pos -> Cell -> Board -> Board
updateAtB { h, w } cell board = fromMaybe board do
  row <- index board h
  newRow <- updateAt w cell row
  updateAt h newRow board

updateAtIndicesB :: Array (Pos /\ Cell) -> Board -> Board
updateAtIndicesB indices board = run do
  bst <- for board AST.thaw
  for_ indices \({ h, w } /\ cell) -> case index bst h of
    Nothing -> pure unit
    Just row -> void $ AST.poke w cell row
  for bst AST.freeze

-- | Initial Board
initialBoard :: Board
initialBoard =
  let
    t = Just true
    f = Just false
    e = Nothing
  in
    [ [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, f, t, e, e, e ]
    , [ e, e, e, t, f, e, e, e ]
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
    header = "  " <> joinWith " " [ "A", "B", "C", "D", "E", "F", "G", "H" ]
    rowStr i row = show (i + 1) <> " " <> fold (map cellToString row)
  in
    header <> "\n" <>
      (joinWith "\n" $ mapWithIndex rowStr b)

stringToIndex :: String -> Maybe Pos
stringToIndex s = case take 2 $ toCharArray s of
  [ hs, ws ] -> do
    let
      h = toCharCode hs - toCharCode 'A'
      w = toCharCode ws - toCharCode '1'
    guard $ h >= 0 && h < size && w >= 0 && w < size
    pure { h, w }
  _ -> Nothing

indexToString :: Pos -> String
indexToString { h, w } = fromCharArray $ catMaybes [ fromCharCode $ toCharCode 'A' + h, fromCharCode $ toCharCode '1' + w ]

-- | Put disk on board and compute next board
-- | h, w, color, board -> next board
-- | If the position is invalid, return Nothing
putDisk :: Pos -> Boolean -> Board -> Maybe Board
putDisk pos c b =
  let
    -- | Check if the position is empty
    isEmpty :: Boolean
    isEmpty = indexB b pos == Just Nothing

    -- | Directions to compute
    directions = do
      dh <- [ -1, 0, 1 ]
      dw <- [ -1, 0, 1 ]
      guard $ not $ dh == 0 && dw == 0
      pure $ { h: dh, w: dw }

    -- | Compute update indices of the board
    updates :: Pos -> Array Pos
    updates dir =
      let
        connectedOps = takeWhile (\p -> indexB b p == Just (Just $ not c)) do
          i <- 1 .. (size - 1)
          pure $ pos >+< dir *< i
        isPinched = length connectedOps > 0 && Just (Just c) == do
          lp <- last connectedOps
          indexB b $ lp >+< dir
      in
        if isPinched then connectedOps else []

    -- | Update indices
    updateIndices :: Array Pos
    updateIndices = fold <<< map updates $ directions
  in
    if isEmpty && length updateIndices /= 0 then Just $ updateAtIndicesB (map (_ /\ Just c) updateIndices <> [ pos /\ Just c ]) b else Nothing

-- | Count disks
countDisks :: Board -> Int /\ Int
countDisks b =
  let
    count :: Cell -> Int
    count c = sum $ map (sum <<< map (\cell -> if cell == c then 1 else 0)) b
  in
    count (Just true) /\ count (Just false)
