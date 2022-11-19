module Reversi.Heuristics.NN where

import Prelude

import Control.Apply (lift2)
import Data.Array (drop, length, range, take, zipWith, (!!), (:))
import Data.Array.Partial (tail)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, sum)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..), fromJust)
import Data.Number (exp)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomRange)
import Partial.Unsafe (unsafePartial)
import Prim.Int (class Add)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON_, writeJSON)
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

class NN nn i o where
  run :: nn -> Vector i Number -> Vector o Number

x = show @Int

-- data NNInternal i o a model = NNInternal
--   { run :: model -> Vector i a -> Vector o a
--   , learn :: Ring a => a -> model -> Vector i a -> Vector o a -> Tuple model (Vector i a) -- 学習係数、データ、入力側ベクトル、出力側の微分ベクトル -> (次回のデータ、入力側を微分したベクトル)
--   , toString :: model -> String
--   , fromString :: String -> Maybe model
--   , model :: model
--   }

-- type NN i o a = Exists (NNInternal i o a)

-- runNN :: forall i o. Reflectable i Int => Reflectable o Int => NN i o Number -> Vector i Number -> Vector o Number
-- runNN nn input = runExists (\(NNInternal { run, model }) -> run model input) nn

-- learnNN :: forall i o a. Reflectable i Int => Reflectable o Int => Ring a => a -> NN i o a -> Vector i a -> Vector o a -> Tuple (NN i o a) (Vector o a)
-- learnNN learningRate nn input outputTarget =
--   let
--     f :: forall model. NNInternal i o a model -> Tuple (NN i o a) (Vector o a)
--     f (NNInternal internal) =
--       let
--         output = internal.run internal.model input
--         outputDiff = lift2 (-) output outputTarget
--         Tuple model' _ = internal.learn learningRate internal.model input outputDiff
--       in
--         Tuple (mkExists $ NNInternal internal { model = model' }) output
--   in
--     runExists f nn

-- nnToString :: forall i o a. NN i o a -> String
-- nnToString nn = runExists (\(NNInternal { toString, model }) -> toString model) nn

-- nnLoadString :: forall i o a. String -> NN i o a -> Maybe (NN i o a)
-- nnLoadString str nn = runExists
--   ( \(NNInternal internal) -> case internal.fromString str of
--       Just model -> Just $ mkExists $ NNInternal internal { model = model }
--       Nothing -> Nothing
--   )
--   nn

-- vDiff2 :: forall i a. Ring a => Reflectable i Int => Vector i a -> Vector i a -> a
-- vDiff2 v1 v2 = sum $ lift2 (\x y -> (x - y) * (x - y)) v1 v2

-- nnFunc :: forall i a. Reflectable i Int => (a -> a) -> (a -> a) -> NN i i a
-- nnFunc f fDiff = mkExists $ NNInternal
--   { run: \_ -> map f
--   , learn: \_ _ input outputDiff -> Tuple unit $ lift2 (\i oDiff -> fDiff i * oDiff) input outputDiff
--   , toString: const ""
--   , fromString: const $ Just unit
--   , model: unit
--   }

-- nnSigmoid :: forall i. Reflectable i Int => NN i i Number
-- nnSigmoid = nnFunc sigmoid sigmoid'

-- nnRelu :: forall i. Reflectable i Int => NN i i Number
-- nnRelu = nnFunc relu relu'

-- nnAppend :: forall i m o a. Reflectable i Int => Reflectable m Int => Reflectable o Int => NN i m a -> NN m o a -> NN i o a
-- nnAppend nnLeft nnRight =
--   let
--     f :: forall modelLeft modelRight. NNInternal i m a modelLeft -> NNInternal m o a modelRight -> NN i o a
--     f (NNInternal left) (NNInternal right) = mkExists $ NNInternal
--       { run: \(Tuple lModel rModel) input -> right.run rModel $ left.run lModel input
--       , learn: \learningRate (Tuple lModel rModel) input outputDiff ->
--           let
--             rInput = left.run lModel input
--             Tuple rModel' rInputDiff = right.learn learningRate rModel rInput outputDiff
--             Tuple lModel' lInputDiff = left.learn learningRate lModel input rInputDiff
--           in
--             Tuple (Tuple lModel' rModel') lInputDiff
--       , toString: \model -> writeJSON $ [ left.toString $ fst model, right.toString $ snd model ]
--       , fromString: \str -> do
--           arr <- readJSON_ str
--           Tuple lStr rStr <- case arr of
--             [ lStr, rStr ] -> pure (Tuple lStr rStr)
--             _ -> Nothing
--           lModel <- left.fromString lStr
--           rModel <- right.fromString rStr
--           pure $ Tuple lModel rModel
--       , model: Tuple left.model right.model
--       }
--   in
--     runExists (\nnInternalLeft -> runExists (f nnInternalLeft) nnRight) nnLeft

-- -- | Append Vertically
-- nnStack
--   :: forall i1 o1 i2 o2 i o a
--    . Reflectable i1 Int
--   => Reflectable o1 Int
--   => Reflectable i2 Int
--   => Reflectable o2 Int
--   => Reflectable i Int
--   => Reflectable o Int
--   => Add i1 i2 i
--   => Add o1 o2 o
--   => NN i1 o1 a
--   -> NN i2 o2 a
--   -> NN i o a
-- nnStack nnTop nnBottom =
--   let
--     f :: forall modelTop modelBottom. NNInternal i1 o1 a modelTop -> NNInternal i2 o2 a modelBottom -> NN i o a
--     f (NNInternal top) (NNInternal bottom) = mkExists $ NNInternal
--       { run: \(Tuple tModel bModel) input ->
--           let
--             Tuple tInput bInput = vDivide input
--           in
--             vAppend (top.run tModel tInput) (bottom.run bModel bInput)
--       , learn: \learningRate (Tuple tModel bModel) input outputDiff ->
--           let
--             Tuple tInput bInput = vDivide input
--             Tuple tOutputDiff bOutputDiff = vDivide outputDiff
--             Tuple tModel' tInputDiff = top.learn learningRate tModel tInput tOutputDiff
--             Tuple bModel' bInputDiff = bottom.learn learningRate bModel bInput bOutputDiff
--           in
--             Tuple (Tuple tModel' bModel') $ vAppend tInputDiff bInputDiff
--       , toString: \model -> writeJSON $ [ top.toString $ fst model, bottom.toString $ snd model ]
--       , fromString: \str -> do
--           arr <- readJSON_ str
--           Tuple lStr rStr <- case arr of
--             [ lStr, rStr ] -> pure (Tuple lStr rStr)
--             _ -> Nothing
--           lModel <- top.fromString lStr
--           rModel <- bottom.fromString rStr
--           pure $ Tuple lModel rModel
--       , model: Tuple top.model bottom.model
--       }
--   in
--     runExists (\nnInternalTop -> runExists (f nnInternalTop) nnBottom) nnTop

-- -- | Use same model
-- nnStackCopy :: forall i i' o o' a. Reflectable i Int => Reflectable o Int => Reflectable i' Int => Reflectable o' Int => Add i i i' => Add o o o' => NN i o a -> NN i' o' a
-- nnStackCopy nn =
--   let
--     f :: forall model. NNInternal i o a model -> NN i' o' a
--     f (NNInternal internal) = mkExists $ NNInternal
--       { run: \model input ->
--           let
--             Tuple tInput bInput = vDivide input
--           in
--             vAppend (internal.run model tInput) (internal.run model bInput)
--       , learn: \learningRate model input outputDiff ->
--           let
--             Tuple tInput bInput = vDivide input
--             Tuple tOutputDiff bOutputDiff = vDivide outputDiff
--             Tuple model' tInputDiff = internal.learn learningRate model tInput tOutputDiff
--             Tuple model'' bInputDiff = internal.learn learningRate model' bInput bOutputDiff
--           in
--             Tuple model'' $ vAppend tInputDiff bInputDiff
--       , toString: internal.toString
--       , fromString: internal.fromString
--       , model: internal.model
--       }
--   in
--     runExists f nn

-- nnMatrix :: forall i o i' a. Reflectable o Int => Reflectable i' Int => Semiring a => Reflectable i Int => Add i 1 i' => WriteForeign a => ReadForeign a => Matrix o i' a -> Exists (NNInternal i o a)
-- nnMatrix initMatrix = mkExists $ NNInternal
--   { run: \matrix input -> mulMV matrix $ vCons one input
--   , learn: \leaningRate matrix input outputDiff ->
--       let
--         input' = vCons one input
--         inputDiff = mulMV (transpose matrix) outputDiff
--         matrix' = lift2 (-) matrix (map (_ * leaningRate) $ outer (*) outputDiff input')
--       in
--         Tuple matrix' $ vTail inputDiff
--   , toString: writeJSON
--   , fromString: readJSON_
--   , model: initMatrix
--   }

-- mRandom :: forall h w. Reflectable h Int => Reflectable w Int => Effect (Matrix h w Number)
-- mRandom = mGenerateA \_ _ -> randomRange (-1.0) 1.0

-- vRandom :: forall n. Reflectable n Int => Effect (Vector n Number)
-- vRandom = vGenerateA \_ -> randomRange (-1.0) 1.0

-- nnIdentity :: forall i. Reflectable i Int => NN i i Number
-- nnIdentity = mkExists $ NNInternal
--   { run: \_ -> identity
--   , learn: \_ _ _ outputDiff -> Tuple unit outputDiff
--   , toString: const ""
--   , fromString: const $ Just unit
--   , model: unit
--   }

-- nnSum :: forall i. Reflectable i Int => NN i 1 Number
-- nnSum = mkExists $ NNInternal
--   { run: \_ -> sum >>> vSingleton
--   , learn: \_ _ _ outputDiff -> Tuple unit $ pure $ vToA outputDiff
--   , toString: const ""
--   , fromString: const $ Just unit
--   , model: unit
--   }

-- infixl 4 nnAppend as >|>
