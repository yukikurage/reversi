module Reversi.Heuristics.NN where

import Prelude

import Control.Apply (lift2)
import Data.Array (drop, foldM, length, range, snoc, splitAt, take, uncons, zipWith, (!!), (:))
import Data.Array.Partial (tail)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, sum)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number (exp, fromString)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Random (randomRange)
import Partial.Unsafe (unsafePartial)
import Prim.Int (class Add)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Type.Proxy (Proxy(..))

newtype Vector :: Int -> Type -> Type
newtype Vector s a = Vector (Array a)

derive newtype instance Eq a => Eq (Vector s a)
derive newtype instance Ord a => Ord (Vector s a)
derive newtype instance Show a => Show (Vector s a)
derive newtype instance Functor (Vector s)
instance Reflectable s Int => Apply (Vector s) where
  apply f x = vZipWith ($) f x

instance Reflectable s Int => Applicative (Vector s) where
  pure a = vGenerate \_ -> a

derive newtype instance Foldable (Vector s)
derive newtype instance Traversable (Vector s)
derive newtype instance ReadForeign a => ReadForeign (Vector s a)
derive newtype instance WriteForeign a => WriteForeign (Vector s a)

instance (Reflectable s Int, Semigroup a) => Semigroup (Vector s a) where
  append = vZipWith append

instance (Reflectable s Int, Monoid a) => Monoid (Vector s a) where
  mempty = pure mempty

vSingleton :: forall a. a -> Vector 1 a
vSingleton a = Vector [ a ]

vToArray :: forall s a. Vector s a -> Array a
vToArray (Vector a) = a

vFromArray :: forall s a. Reflectable s Int => Array a -> Maybe (Vector s a)
vFromArray a =
  if length a == reflectType (Proxy :: Proxy s) then Just (Vector a)
  else Nothing

vToA :: forall a. Vector 1 a -> a
vToA = unsafePartial case _ of
  Vector [ a ] -> a

vGenerate :: forall s a. Reflectable s Int => (Int -> a) -> Vector s a
vGenerate f =
  let
    s = reflectType (Proxy :: Proxy s)
  in
    Vector $ map f $ range 0 $ s - 1

vGenerateA :: forall s m a. Reflectable s Int => Applicative m => (Int -> m a) -> m (Vector s a)
vGenerateA f =
  let
    s = reflectType (Proxy :: Proxy s)
  in
    Vector <$> traverse f (range 0 $ s - 1)

vReplicateA :: forall s m a. Reflectable s Int => Applicative m => m a -> m (Vector s a)
vReplicateA = vGenerateA <<< const

vZipWith :: forall s a b c. Reflectable s Int => (a -> b -> c) -> Vector s a -> Vector s b -> Vector s c
vZipWith f (Vector a) (Vector b) = Vector $ zipWith f a b

vCons :: forall s s' a. Reflectable s Int => Add s 1 s' => a -> Vector s a -> Vector s' a
vCons a (Vector v) = Vector $ a : v

vTail :: forall s s' a. Reflectable s Int => Add s' 1 s => Vector s a -> Vector s' a
vTail (Vector v) = Vector $ unsafePartial $ tail v

vIndex :: forall s a. Reflectable s Int => Vector s a -> Int -> Maybe a
vIndex (Vector v) i = v !! i

vAppend :: forall s s' s'' a. Reflectable s Int => Add s s' s'' => Vector s a -> Vector s' a -> Vector s'' a
vAppend (Vector a) (Vector b) = Vector $ a <> b

vDivide :: forall s s' s'' a. Reflectable s Int => Add s s' s'' => Vector s'' a -> Tuple (Vector s a) (Vector s' a)
vDivide (Vector v) =
  let
    s = reflectType (Proxy :: Proxy s)
  in
    Tuple (Vector $ take s v) (Vector $ drop s v)

newtype Matrix :: Int -> Int -> Type -> Type
newtype Matrix h w a = Matrix (Array (Array a))

derive newtype instance Eq a => Eq (Matrix h w a)
derive newtype instance Ord a => Ord (Matrix h w a)
derive newtype instance Show a => Show (Matrix h w a)
instance Functor (Matrix h w) where
  map f (Matrix a) = Matrix $ map (map f) a

instance (Reflectable h Int, Reflectable w Int) => Apply (Matrix h w) where
  apply f x = mZipWith ($) f x

instance (Reflectable h Int, Reflectable w Int) => Applicative (Matrix h w) where
  pure a = mGenerate \_ _ -> a

instance Foldable (Matrix h w) where
  foldr f z (Matrix a) = foldr (flip $ foldr f) z a
  foldl f z (Matrix a) = foldl (foldl f) z a
  foldMap f (Matrix a) = foldMap (foldMap f) a

instance Traversable (Matrix h w) where
  traverse f (Matrix a) = Matrix <$> traverse (traverse f) a
  sequence (Matrix a) = Matrix <$> traverse sequence a

derive newtype instance ReadForeign a => ReadForeign (Matrix h w a)
derive newtype instance WriteForeign a => WriteForeign (Matrix h w a)

instance (Reflectable h Int, Reflectable w Int, Semigroup a) => Semigroup (Matrix h w a) where
  append = mZipWith append

instance (Reflectable h Int, Reflectable w Int, Monoid a) => Monoid (Matrix h w a) where
  mempty = pure mempty

mIndex :: forall h w a. Reflectable h Int => Reflectable w Int => Matrix h w a -> Int -> Int -> Maybe a
mIndex (Matrix m) i j = m !! i >>= (_ !! j)

mGenerate :: forall h w a. Reflectable h Int => Reflectable w Int => (Int -> Int -> a) -> Matrix h w a
mGenerate f =
  let
    h = reflectType (Proxy :: Proxy h)
    w = reflectType (Proxy :: Proxy w)
    rowFn i = map (f i) $ range 0 $ w - 1
  in
    Matrix $ map rowFn $ range 0 $ h - 1

mGenerateA :: forall h w m a. Reflectable h Int => Reflectable w Int => Applicative m => (Int -> Int -> m a) -> m (Matrix h w a)
mGenerateA f =
  let
    h = reflectType (Proxy :: Proxy h)
    w = reflectType (Proxy :: Proxy w)
    rowFn i = traverse (f i) $ range 0 $ w - 1
  in
    Matrix <$> traverse rowFn (range 0 $ h - 1)

mReplicateA :: forall h w m a. Reflectable h Int => Reflectable w Int => Applicative m => m a -> m (Matrix h w a)
mReplicateA = mGenerateA <<< const <<< const

mZipWith :: forall h w a b c. Reflectable h Int => Reflectable w Int => (a -> b -> c) -> Matrix h w a -> Matrix h w b -> Matrix h w c
mZipWith f (Matrix a) (Matrix b) = Matrix $ zipWith (zipWith f) a b

-- -- | Matrix & Vector multiplication
mulMV :: forall h w a. Reflectable h Int => Reflectable w Int => Semiring a => Matrix h w a -> Vector w a -> Vector h a
mulMV (Matrix m) (Vector v) = Vector $ mapFlipped m \row -> sum $ zipWith (*) row v

transpose :: forall h w a. Reflectable h Int => Reflectable w Int => Matrix h w a -> Matrix w h a
transpose m = mGenerate \i j -> unsafePartial $ fromJust $ mIndex m j i

outer :: forall h w a b c. Reflectable h Int => Reflectable w Int => (a -> b -> c) -> Vector h a -> Vector w b -> Matrix h w c
outer f (Vector as) (Vector bs) = Matrix $ mapFlipped as \a -> map (f a) bs

relu :: forall a. Ord a => Ring a => a -> a
relu x = max zero x

relu' :: forall a. Ord a => Ring a => a -> a
relu' x = if x > zero then one else zero

sigmoid :: Number -> Number
sigmoid x = one / (one + exp (-x))

sigmoid' :: Number -> Number
sigmoid' x = sigmoid x * (one - sigmoid x)

type DiffNumber = Number

class NN nn i o | nn -> i o where
  forwardPropagation :: nn -> Vector i Number -> Vector o Number
  backPropagation :: Number -> nn -> Vector i Number -> Vector o DiffNumber -> nn /\ Vector i DiffNumber

teach :: forall nn i o. NN nn i o => Reflectable i Int => Reflectable o Int => Number -> nn -> Vector i Number -> Vector o Number -> nn /\ Vector o Number
teach rate nn input outputExpected =
  let
    outputActual = forwardPropagation nn input
    diff = lift2 (-) outputActual outputExpected
    newNN /\ _ = backPropagation rate nn input diff
  in
    newNN /\ outputActual

class WriteCSV a where
  writeCSV :: a -> Array String

instance WriteCSV Number where
  writeCSV = pure <<< show

instance WriteCSV a => WriteCSV (Array a) where
  writeCSV = join <<< map writeCSV

instance WriteCSV a => WriteCSV (Vector n a) where
  writeCSV (Vector arr) = writeCSV arr

instance WriteCSV a => WriteCSV (Matrix h w a) where
  writeCSV (Matrix arr) = join $ map writeCSV arr

class ReadCSV a where
  readCSV :: Array String -> Maybe (a /\ Array String)

instance ReadCSV Number where
  readCSV arr = do
    { head, tail } <- uncons arr
    num <- fromString head
    pure $ num /\ tail

instance (ReadCSV a, Reflectable n Int) => ReadCSV (Vector n a) where
  readCSV arr = do
    let
      len = reflectType (Proxy :: Proxy n)
      { before, after } = splitAt len arr
    v <-
      foldM
        ( \{ acc, rest } _ -> do
            val /\ rest' <- readCSV rest
            pure { acc: acc `snoc` val, rest: rest' }
        )
        { acc: [], rest: before } $ range 0 $ len - 1
    pure $ Vector v.acc /\ after

instance (ReadCSV a, Reflectable h Int, Reflectable w Int) => ReadCSV (Matrix h w a) where
  readCSV arr = do
    let
      h = reflectType (Proxy :: Proxy h)
      w = reflectType (Proxy :: Proxy w)

      { before, after } = splitAt (h * w) arr

      readRow :: Array String -> Maybe (Array a /\ Array String)
      readRow arr2 = do
        let
          { before, after } = splitAt w arr2
        v <-
          foldM
            ( \{ acc, rest } _ -> do
                val /\ rest' <- readCSV rest
                pure { acc: acc `snoc` val, rest: rest' }
            )
            { acc: [], rest: before } $ range 0 $ w - 1
        pure $ v.acc /\ after
    m <-
      foldM
        ( \{ acc, rest } _ -> do
            row /\ rest' <- readRow rest
            pure { acc: acc `snoc` row, rest: rest' }
        )
        { acc: [], rest: before } $ range 0 $ h - 1
    pure $ Matrix m.acc /\ after

data NNAppend nnLeft nnRight = NNAppend nnLeft nnRight

nnAppend :: forall nnLeft nnRight i m o. NN nnLeft i m => NN nnRight m o => NNAppend nnLeft nnRight -> Vector i Number -> Vector o Number
nnAppend (NNAppend nnLeft nnRight) v = forwardPropagation nnRight $ forwardPropagation nnLeft v

infixl 4 NNAppend as >|>

infixl 4 type NNAppend as >|>

derive instance (Eq nnLeft, Eq nnRight) => Eq (NNAppend nnLeft nnRight)
derive instance (Ord nnLeft, Ord nnRight) => Ord (NNAppend nnLeft nnRight)
instance (WriteCSV nnLeft, WriteCSV nnRight) => WriteCSV (NNAppend nnLeft nnRight) where
  writeCSV (NNAppend nnLeft nnRight) = writeCSV nnLeft <> writeCSV nnRight

instance (ReadCSV nnLeft, ReadCSV nnRight) => ReadCSV (NNAppend nnLeft nnRight) where
  readCSV arr = do
    nnLeft /\ arr' <- readCSV arr
    nnRight /\ arr'' <- readCSV arr'
    pure $ NNAppend nnLeft nnRight /\ arr''

instance (NN nnLeft i m, NN nnRight m o) => NN (NNAppend nnLeft nnRight) i o where
  forwardPropagation (NNAppend nnLeft nnRight) input =
    forwardPropagation nnRight $ forwardPropagation nnLeft input

  backPropagation rate (NNAppend nnLeft nnRight) inputLeft outputDiffRight =
    let
      inputRight = forwardPropagation nnLeft inputLeft
      newNNR /\ outputDiffLeft = backPropagation rate nnRight inputRight outputDiffRight
      newNNL /\ inputDiffLeft = backPropagation rate nnLeft inputLeft outputDiffLeft
    in
      NNAppend newNNL newNNR /\ inputDiffLeft

newtype NNMatrix :: Int -> Int -> Int -> Type
newtype NNMatrix i i_plus_one o = NNMatrix (Matrix o i_plus_one Number)

derive newtype instance Eq (NNMatrix i i_plus_one o)
derive newtype instance Ord (NNMatrix i i_plus_one o)
derive newtype instance WriteCSV (NNMatrix i i_plus_one o)
derive newtype instance (Reflectable i_plus_one Int, Reflectable o Int) => ReadCSV (NNMatrix i i_plus_one o)

instance (Reflectable i Int, Reflectable i_plus_one Int, Reflectable o Int, Add i 1 i_plus_one) => NN (NNMatrix i i_plus_one o) i o where
  forwardPropagation (NNMatrix m) input = mulMV m $ vCons 1.0 input

  backPropagation rate (NNMatrix m) input outputDiff =
    let
      inputDiff = vTail $ mulMV (transpose m) outputDiff
      input' = vCons 1.0 input
      m' = lift2 (\x y -> x - rate * y) m $ outer (*) outputDiff input'
    in
      NNMatrix m' /\ inputDiff

data NNFunction :: Int -> Int -> Type
data NNFunction i o = NNRelu | NNSigmoid

derive instance Eq a => Eq (NNFunction i o)

instance WriteCSV (NNFunction i o) where
  writeCSV NNRelu = [ "relu" ]
  writeCSV NNSigmoid = [ "sigmoid" ]

instance (Reflectable i Int, Reflectable o Int) => ReadCSV (NNFunction i o) where
  readCSV arr = do
    { head, tail } <- uncons arr
    case head of
      "relu" -> pure $ NNRelu /\ tail
      "sigmoid" -> pure $ NNSigmoid /\ tail
      _ -> Nothing

instance Reflectable i Int => NN (NNFunction i i) i i where
  forwardPropagation NNRelu input = map relu input
  forwardPropagation NNSigmoid input = map sigmoid input

  backPropagation _ NNRelu input outputDiff =
    let
      inputDiff = lift2 (*) outputDiff $ map relu' input
    in
      NNRelu /\ inputDiff
  backPropagation _ NNSigmoid input outputDiff =
    let
      inputDiff = lift2 (*) outputDiff $ map sigmoid' input
    in
      NNSigmoid /\ inputDiff

data NNStack nn1 nn2 = NNStack nn1 nn2

nnStack :: forall nn1 nn2 i1 o1 i2 o2 i o. Add i1 i2 i => Add o1 o2 o => NN nn1 i1 o1 => NN nn2 i2 o2 => nn1 -> nn2 -> NNStack nn1 nn2
nnStack nn1 nn2 = NNStack nn1 nn2

infixl 4 NNStack as >-<

derive instance (Eq nn1, Eq nn2) => Eq (NNStack nn1 nn2)

instance (WriteCSV nn1, WriteCSV nn2) => WriteCSV (NNStack nn1 nn2) where
  writeCSV (NNStack nn1 nn2) = writeCSV nn1 <> writeCSV nn2

instance (ReadCSV nn1, ReadCSV nn2) => ReadCSV (NNStack nn1 nn2) where
  readCSV arr = do
    nn1 /\ arr' <- readCSV arr
    nn2 /\ arr'' <- readCSV arr'
    pure $ NNStack nn1 nn2 /\ arr''

instance (NN nn1 i1 o1, NN nn2 i2 o2, Add i1 i2 i, Add o1 o2 o, Reflectable i1 Int, Reflectable i2 Int, Reflectable i Int, Reflectable o1 Int, Reflectable o2 Int, Reflectable o Int) => NN (NNStack nn1 nn2) i o where
  forwardPropagation (NNStack nn1 nn2) input =
    let
      input1 /\ input2 = vDivide input
      output1 = forwardPropagation nn1 input1
      output2 = forwardPropagation nn2 input2
    in
      vAppend output1 output2

  backPropagation rate (NNStack nn1 nn2) input outputDiff =
    let
      input1 /\ input2 = vDivide input
      outputDiff1 /\ outputDiff2 = vDivide outputDiff
      newNN1 /\ inputDiff1 = backPropagation rate nn1 input1 outputDiff1
      newNN2 /\ inputDiff2 = backPropagation rate nn2 input2 outputDiff2
    in
      NNStack newNN1 newNN2 /\ vAppend inputDiff1 inputDiff2

newtype NNCopy nn = NNCopy nn

nnCopy :: forall nn i o. NN nn i o => nn -> NNCopy nn
nnCopy nn = NNCopy nn

infixl 4 NNCopy as >||<

derive instance Eq nn => Eq (NNCopy nn)

derive newtype instance WriteCSV nn => WriteCSV (NNCopy nn)
derive newtype instance ReadCSV nn => ReadCSV (NNCopy nn)

instance (NN nn i o, Add i i i2, Add o o o2, Reflectable i Int, Reflectable o Int) => NN (NNCopy nn) i2 o2 where
  forwardPropagation (NNCopy nn) input =
    let
      input1 /\ input2 = vDivide input
      output1 = forwardPropagation nn input1
      output2 = forwardPropagation nn input2
    in
      vAppend output1 output2

  backPropagation rate (NNCopy nn) input outputDiff =
    let
      input1 /\ input2 = vDivide input
      outputDiff1 /\ outputDiff2 = vDivide outputDiff
      newNNTop /\ inputDiff1 = backPropagation rate nn input1 outputDiff1
      newNNBottom /\ inputDiff2 = backPropagation rate newNNTop input2 outputDiff2
    in
      NNCopy newNNBottom /\ vAppend inputDiff1 inputDiff2

mRandom :: forall h w. Reflectable h Int => Reflectable w Int => Effect (Matrix h w Number)
mRandom = mGenerateA \_ _ -> randomRange (-1.0) 1.0

vRandom :: forall n. Reflectable n Int => Effect (Vector n Number)
vRandom = vGenerateA \_ -> randomRange (-1.0) 1.0
