{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Stacktic.Base
  ( (>>), (>>=), (>=>)
  , nil, lift, run, pure
  , drop
  , lift_
  , dowhile
  , fix
  , apply
  , kleisliapply, kleislilift
  , pureapply, pureapply2, purelift, purelift2
  ) where

import Prelude (Bool(..), (.))
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.Fix as P

ifThenElse :: Bool -> a -> a -> a
ifThenElse cnd bthen belse = case cnd of
  True -> bthen
  False -> belse

(>>) :: P.Monad m => (x -> m y) -> (y -> m z) -> (x -> m z)
f >> g = f P.>=> g

(>>=) :: P.Monad m => (x -> m (y, a)) -> (a -> (y -> m z)) -> (x -> m z)
f >>= h = \x -> P.do
  (y, a) <- f x
  h a y

(>=>) :: P.Monad m => (a -> (x -> m (y, b))) -> (b -> (y -> m z)) -> (a -> (x -> m z))
f >=> g = \a -> f a >>= g

nil :: P.Applicative m => x -> m x
nil = P.pure

lift :: P.Applicative m => m a -> (x -> m (x, a))
lift ma x = P.pure (\a -> (x, a)) P.<*> ma

run :: P.Applicative m => (x, m a) -> m (x, a)
run (x, ma) = lift ma x

pure :: P.Applicative m => a -> (x -> m (x, a))
pure = lift . P.pure

drop :: P.Monad m => (x, a) -> m x
drop = do
  _ <- nil
  nil

lift_ :: P.Monad m => m a -> x -> m x
lift_ ma = do
  lift ma
  drop

dowhile :: P.Monad m => (z -> m (z, Bool)) -> (z -> m z)
dowhile body = do
  b <- body
  if b then dowhile body else nil

fix :: P.MonadFix m => (a -> (x -> m (y, a))) -> (x -> m (y, a))
fix f x = P.mfix (\ ~(_, a) -> f a x)

apply :: P.Monad m => (y, y -> m z) -> m z
apply = do
  f <- nil
  f

pureapply :: P.Monad m => ((y, t), t -> a) -> m (y, a)
pureapply = do
  f <- nil
  a <- nil
  pure (f a)

pureapply2 :: P.Monad m => (((y, b), a), a -> b -> c) -> m (y, c)
pureapply2 = do
  pureapply
  pureapply

purelift :: P.Monad m => (t -> a) -> ((y, t) -> m (y, a))
purelift f = do
  pure f
  pureapply

purelift2 :: P.Monad m => (a -> b -> c) -> ((y, b), a) -> m (y, c)
purelift2 f = do
  pure f
  pureapply2

kleisliapply :: P.Monad m => ((y, t), t -> m a) -> m (y, a)
kleisliapply = do
  pureapply
  run

kleislilift :: P.Monad m => (t -> m a) -> ((y, t) -> m (y, a))
kleislilift f = do
  pure f
  kleisliapply
