{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Use <$>" #-}

module Language.Stacktic.Linear.Base where

import Prelude.Linear
  ( (.), Consumable, Bool(..), ($), Dupable, Movable )
import qualified Prelude.Linear as P
import Control.Functor.Linear
  ( Monad, Applicative )
import qualified Control.Functor.Linear as Control
import Control.Monad.Linear.Cont
  ( Cont, ContT(..) )
import qualified Control.Monad.Linear.Cont as Cont

(>>) :: Control.Monad m
  => (x %1 -> m y) %1
  -> (y %1 -> m z) %1
  -> (x %1 -> m z)
(>>) f g x = Control.do
  y <- f x
  g y

(>>=) :: Monad m
  => (x %1 -> m (y, a)) %1
  -> (a %1 -> (y %1 -> m z)) %1
  -> (x %1 -> m z)
(>>=) f h x = Control.do
  (y, a) <- f x
  h a y

(>=>) :: Monad m
  => (a %1 -> (x %1 -> m (y, b))) %1
  -> (b %1 -> (y %1 -> m z)) %1
  -> (a %1 -> (x %1 -> m z))
(>=>) f g a = f a >>= g

nil :: Applicative m => x %1 -> m x
nil = Control.pure

lift :: Applicative m => m a %1 -> (x %1 -> m (x, a))
lift ma x = Control.pure (x,) Control.<*> ma

run :: Applicative m => (x, m a) %1 -> m (x, a)
run (x, ma) = lift ma x

pure :: Applicative m => a %1 -> (x %1 -> m (x, a))
pure = lift . Control.pure

drop :: (Monad m, Consumable a) => (x, a) %1 -> m x
drop = do
  a <- nil
  case P.consume a of
    () -> nil

dup :: (Monad m, Dupable a) => (y, a) %1 -> m ((y, a), a)
dup = do
  a <- nil
  case P.dup2 a of
    (a1, a2) -> do
      pure a1
      pure a2

move :: (Monad m, Movable a) => (y, a) %1 -> m (y, P.Ur a)
move = do
  a <- nil
  pure (P.move a)

swap :: Monad m => ((y, a1), a2) %1 -> m ((y, a2), a1)
swap = do
  a <- nil
  b <- nil
  pure a
  pure b

apply :: Monad m => (y, y %1 -> m z) %1 -> m z
apply = do
  f <- nil
  f

when :: Applicative m => Bool %1 -> (x %1 -> m x) -> x %1 -> m x
when cnd body =
  case cnd of
    True -> body
    False -> nil

mkContT :: ((a %1 -> m r) %1 -> m r) %1 -> x %1 -> ContT r m (x, a)
mkContT f x = Control.fmap (x,) $ ContT f

runCont :: (x %p -> Cont r r) %1 -> x %p -> r
runCont f x = Cont.runCont (f x) P.id
