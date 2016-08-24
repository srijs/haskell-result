{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Control.Monad.Trans.Result
  ( ResultT, runResultT
  , raiseT, accumulateT
  ) where


import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.Trans.Class
import qualified Control.Monad.State.Class as State

import Data.Functor.Identity
import Data.Result


data ResultT e m a = ResultT { runResultT :: m (Result e a) }


instance Functor f => Functor (ResultT e f) where
  fmap f (ResultT m) =
    ResultT (fmap (fmap f) m)


instance Applicative f => Applicative (ResultT e f) where
  pure a =
    ResultT (pure (pure a))

  (ResultT mf) <*> (ResultT ma) =
    ResultT $
      liftA2 (<*>) mf ma


instance Monad m => Monad (ResultT e m) where
  return =
    pure

  (ResultT m) >>= f =
    ResultT $
      m >>= fmap join . traverse (runResultT . f)


instance MonadTrans (ResultT e) where
  lift m =
    ResultT (pure <$> m)


raiseT :: Applicative f => e -> ResultT e f a
raiseT e =
  ResultT (pure (raise e))


accumulateT :: (Traversable t , Applicative f) => t (ResultT e f a) -> ResultT e f (t a)
accumulateT results =
  ResultT (accumulate <$> traverse runResultT results)


instance State.MonadState s m => State.MonadState s (ResultT e m) where
  state f =
    ResultT (pure <$> State.state f)
