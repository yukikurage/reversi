module Stdin where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)

foreign import questionImpl :: String -> Effect (Promise String)

question :: String -> Aff String
question str = toAffE $ questionImpl str

questionValid :: forall a. String -> (String -> Maybe a) -> Aff Unit -> Aff a
questionValid str f err = do
  res <- question str
  case f res of
    Just a -> pure a
    Nothing -> err *> questionValid str f err
