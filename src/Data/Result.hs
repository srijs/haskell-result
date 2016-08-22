{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Result
  ( Result
  , get, errors
  , raise
  ) where


import Data.Monoid
import Data.Key
import qualified Data.List.NonEmpty as NonEmpty


newtype Result e a =
    Result (Either (NonEmpty.NonEmpty e) a)
  deriving (Eq, Ord, Show, Functor, Applicative, Monad)


instance Zip (Result e) where
  zipWith f mea meb =
    case (mea, meb) of
      (Result (Right eaa), Result (Right eba)) ->
        Result (Right (f eaa eba))
      _ ->
        Result (Left (NonEmpty.fromList (errors mea <> errors meb)))


raise :: e -> Result e a
raise e =
  Result (Left (e NonEmpty.:| []))


get :: Result e a -> Maybe a
get (Result e) =
  case e of
    Left _ ->
      Nothing
    Right a ->
      Just a


errors :: Result e a -> [e]
errors (Result e) =
  case e of
    Left es ->
      NonEmpty.toList es
    Right _ ->
      []
