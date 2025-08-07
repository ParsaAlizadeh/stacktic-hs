{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}

module Control.Monad.Linear.Cont where

import Prelude.Linear
import qualified Data.Functor.Linear as Data
import Control.Functor.Linear
import Control.Monad.IO.Class.Linear
import Control.Monad.Linear.Identity

newtype ContT r m a = ContT ((a %1 -> m r) %1 -> m r)
  deriving Data.Functor via Data (ContT r m)
  deriving Data.Applicative via Data (ContT r m)

runContT :: ContT r m a %1 -> (a %1 -> m r) %1 -> m r
runContT (ContT f) = f

instance Functor (ContT r m) where
  fmap f (ContT g) = ContT (\q -> g (q . f))

pureCont :: a %n -> ContT r m a
pureCont a = ContT (\k -> k a)

instance Applicative (ContT r m) where
  pure = pureCont
  liftA2 f (ContT p) (ContT q) = ContT (\g -> p (\a -> q (g . f a)))

instance Monad (ContT r m) where
  ContT p >>= f = ContT (\g -> p (\a -> runContT (f a) g))

instance MonadTrans (ContT r) where
  lift ma = ContT (\k -> ma >>= k)

instance MonadIO m => MonadIO (ContT r m) where
  liftIO m = lift (liftIO m)

instance MonadFail m => MonadFail (ContT r m) where
  fail a = lift (fail a)

evalContT :: Applicative m => ContT r m r %1 -> m r
evalContT m = runContT m pure

mapContT :: (m r %1 -> m r) %1 -> ContT r m a %1 -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((b %1 -> m r) %1-> a %1 -> m r) %1 -> ContT r m a %1 -> ContT r m b
withContT f m = ContT $ \k -> runContT m (f k)

type Cont r a = ContT r Identity a

mkCont :: ((a %1 -> r) %1 -> r) %1 -> Cont r a
mkCont f = ContT $ \g -> mkIdentity $ f (runIdentity . g)

runCont :: Cont r a %1 -> (a %1 -> r) %1 -> r
runCont c f = runIdentity $ runContT c (mkIdentity . f)

evalCont :: Cont r r %1 -> r
evalCont = runIdentity . evalContT
