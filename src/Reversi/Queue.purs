module Reversi.Queue where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import data Queue :: Type -> Type

foreign import newQueue :: forall a. a -> Effect Unit
foreign import enqueue :: forall a. Queue a -> a -> Effect Unit
foreign import dequeueImpl :: forall a. (a -> Maybe a) -> (Maybe a) -> Queue a -> Effect (Maybe a)

dequeue :: forall a. Queue a -> Effect (Maybe a)
dequeue = dequeueImpl Just Nothing
