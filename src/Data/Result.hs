{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Result
  ( Result
  , get, errors
  , raise, raiseAll
  , accumulate
  , fromEither
  ) where


import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Key
import Data.Monoid


newtype Result e a =
    Result (Either (e, [e]) a)
  deriving (Eq, Ord, Show, Functor, Applicative, Monad, Foldable)


instance Traversable (Result e) where
  traverse f (Result x) =
    case x of
      Left (e, es) ->
        pure (Result (Left (e, es)))
      Right a ->
        Result . Right <$> f a


instance Zip (Result e) where
  zipWith f mea meb =
    case (mea, meb) of
      (Result (Right eaa), Result (Right eba)) ->
        Result (Right (f eaa eba))
      (Result (Left (e, es)), Result (Left (e', es'))) ->
        Result (Left (e, es ++ e' : es'))
      (Result (Left ees), _) ->
        Result (Left ees)
      (_, Result (Left ees)) ->
        Result (Left ees)


instance Bifunctor Result where
  bimap f g (Result x) =
    case x of
      Left (e, es) ->
        Result (Left (f e, fmap f es))
      Right a ->
        Result (Right (g a))


instance Bifoldable Result where
  bifoldMap f g (Result x) =
    case x of
      Left (e, es) ->
        f e <> foldMap f es
      Right a ->
        g a


instance Bitraversable Result where
  bitraverse f g (Result x) =
    case x of
      Left (e, es) ->
        (\e' es' -> Result (Left (e', es'))) <$> f e <*> traverse f es
      Right a ->
        Result . Right <$> g a


raise :: e -> Result e a
raise e =
  Result (Left (e, []))


raiseAll :: [e] -> Result e ()
raiseAll es =
  Result $ case es of
    [] ->
      Right ()
    (e:es') ->
      Left (e, es')


get :: Result e a -> Maybe a
get (Result e) =
  case e of
    Left _ ->
      Nothing
    Right a ->
      Just a


errors :: Result e a -> [e]
errors (Result x) =
  case x of
    Left (e, es) ->
      e:es
    Right _ ->
      []


fromEither :: Either e a -> Result e a
fromEither eith =
  Result $ case eith of
    Left e ->
      Left (e, [])
    Right a ->
      Right a


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
