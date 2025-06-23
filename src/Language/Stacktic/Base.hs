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
  , dowhile, thenelse
  , fix
  , apply
  , kleisliapply, kleislilift
  , pureapply, pureapply2, purelift, purelift2
  , when
  , callCC, label
  , throwError, catchError
  , translift, translift_
  , liftIO, liftIO_
  , get, put, state
  ) where

import Prelude (Bool(..), (.))
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.Fix as P
import qualified Control.Monad.Cont as P
import qualified Control.Monad.Except as P
import qualified Control.Monad.Trans as P

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

thenelse :: P.Monad m => (y -> m z) -> (y -> m z) -> ((y, Bool) -> m z)
thenelse thenbody elsebody = do
  b <- nil
  if b then thenbody else elsebody

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

when :: P.Applicative m => Bool -> (x -> m x) -> (x -> m x)
when cnd body = if cnd then body else nil

callCC :: P.MonadCont m => ((x -> m y) -> (z -> m x)) -> (z -> m x)
callCC f z = P.callCC \h -> f h z

label :: P.MonadCont m => x -> m (x, x -> m y)
label x = (\(m, x) -> (x, m)) P.<$> P.label x

throwError :: P.MonadError e m => e -> m y
throwError = P.throwError

catchError :: P.MonadError e m => (x -> m y) -> (e -> m y) -> (x -> m y)
catchError trybody catchbody x =
  P.catchError (trybody x) catchbody

translift :: (P.MonadTrans t, P.Monad m) => m a -> x -> t m (x, a)
translift = lift . P.lift

translift_ :: (P.MonadTrans t, P.Monad m) => m a -> x -> t m x
translift_ = lift_ . P.lift

liftIO :: P.MonadIO m => P.IO a -> x -> m (x, a)
liftIO = lift . P.liftIO

liftIO_ :: P.MonadIO m => P.IO a -> x -> m x
liftIO_ = lift_ . P.liftIO

get :: P.Applicative m => x -> m (x, x)
get x = P.pure (x, x)

put :: P.Applicative m => (x, y) -> m y
put (_, y) = P.pure y

state :: P.Applicative m => (x -> y) -> (x -> m y)
state f = P.pure . f
