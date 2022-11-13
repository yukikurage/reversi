module Reversi.Heuristics.NN where

import Prelude

import Data.Array (range, zipWith, (!!), (:))
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, sum)
import Data.Functor (mapFlipped)
import Data.Leibniz (type (~), coerce, liftLeibniz1of2)
import Data.Maybe (Maybe)
import Data.Number (exp)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Traversable (class Traversable, sequence, traverse)
import Prim.Int (class Add)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

vIndex :: forall s a. Reflectable s Int => Vector s a -> Int -> Maybe a
vIndex (Vector v) i = v !! i

vAppend :: forall s s' s'' a. Reflectable s Int => Add s s' s'' => Vector s a -> Vector s' a -> Vector s'' a
vAppend (Vector a) (Vector b) = Vector $ a <> b

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

relu :: forall a. Ord a => Ring a => a -> a
relu x = max zero x

relu' :: forall a. Ord a => Ring a => a -> a
relu' x = if x > zero then one else zero

sigmoid :: Number -> Number
sigmoid x = one / (one + exp (-x))

sigmoid' :: Number -> Number
sigmoid' x = sigmoid x * (one - sigmoid x)

data NNConsInternal :: Int -> Int -> Type -> Int -> Type
data NNConsInternal i o a n = NNConsInternal (NN i n a) (Layer n o a)

type NNConsExists i o a = Exists (NNConsInternal i o a)

-- | A simple neural network
data NN :: Int -> Int -> Type -> Type
data NN i o a
  = NNPure (i ~ o)
  | NNCons (NNConsExists i o a)

data Layer :: Int -> Int -> Type -> Type
data Layer i o a
  = LayerSigmoid
  | LayerRelu
  | LayerCombined (Matrix o i a)

runNN :: forall i o. Reflectable i Int => Reflectable o Int => NN i o Number -> Vector i Number -> Vector o Number
runNN nn input = case nn of
  NNPure leibniz -> coerce (liftLeibniz1of2 leibniz) input
  NNCons ex -> runExists
    ( \(NNConsInternal prevNN layer) -> case layer of
        LayerSigmoid -> sigmoid <$> runNN (unsafeCoerce prevNN :: NN i o Number) input
        LayerRelu -> relu <$> runNN (unsafeCoerce prevNN :: NN i o Number) input
        LayerCombined matrix -> mulMV (unsafeCoerce matrix :: Matrix o i Number) $ (runNN (unsafeCoerce prevNN :: NN i i Number) input)
    )
    ex

layerSig :: forall i. Reflectable i Int => Layer i i Number
layerSig = LayerSigmoid

layerRelu :: forall i. Reflectable i Int => Layer i i Number
layerRelu = LayerRelu

layerComb :: forall i o. Reflectable i Int => Reflectable o Int => Matrix o i Number -> Layer i o Number
layerComb = LayerCombined

nnCons :: forall i o a. Reflectable i Int => Reflectable o Int => NN i a Number -> Layer a o Number -> NN i o Number
nnCons nn layer = NNCons $ mkExists (NNConsInternal nn layer)

nnPure :: forall i. Reflectable i Int => NN i i Number
nnPure = NNPure identity

infixl 4 nnCons as >|>
