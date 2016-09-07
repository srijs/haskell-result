{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Result
  ( Result
  , get, errors
  , raise, raiseAll
  , accumulate
  ) where


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Key
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid


newtype Result e a =
    Result (Either (NonEmpty.NonEmpty e) a)
  deriving (Eq, Ord, Show, Functor, Applicative, Monad, Foldable)


instance Traversable (Result e) where
  traverse f (Result e) =
    case e of
      Left es ->
        pure (Result (Left es))
      Right a ->
        Result . Right <$> f a


instance Zip (Result e) where
  zipWith f mea meb =
    case (mea, meb) of
      (Result (Right eaa), Result (Right eba)) ->
        Result (Right (f eaa eba))
      _ ->
        Result (Left (NonEmpty.fromList (errors mea <> errors meb)))


instance Bifunctor Result where
  bimap f g (Result e) =
    case e of
      Left es ->
        Result (Left (fmap f es))
      Right a ->
        Result (Right (g a))


instance Bifoldable Result where
  bifoldMap f g (Result e) =
    case e of
      Left es ->
        foldMap f es
      Right a ->
        g a


instance Bitraversable Result where
  bitraverse f g (Result e) =
    case e of
      Left es ->
        Result . Left <$> traverse f es
      Right a ->
        Result . Right <$> g a


raise :: e -> Result e a
raise e =
  Result (Left (e NonEmpty.:| []))


raiseAll :: [e] -> Result e ()
raiseAll es =
  Result $ case es of
    [] ->
      Right ()
    (e:es') ->
      Left (e NonEmpty.:| es')


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


data AccumulatingResult e a = AccRes { getResult :: Result e a }


instance Functor (AccumulatingResult e) where
  fmap f r =
    AccRes (fmap f (getResult r))


instance Applicative (AccumulatingResult e) where
  pure a =
    AccRes (pure a)

  x <*> y =
    AccRes (getResult x `zap` getResult y)


accumulate :: Traversable t => t (Result e a) -> Result e (t a)
accumulate t =
  getResult (traverse AccRes t)
