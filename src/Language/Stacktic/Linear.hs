{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TupleSections #-}

module Language.Stacktic.Linear where

import Prelude.Linear
  ( Bool(..), (.), Int, ($), Ur(..) )
import qualified Prelude.Linear as PL
import qualified Data.Functor.Linear as Data
import qualified Control.Functor.Linear as Control
import qualified Prelude as P
import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array

-- Unrestricted!
ifThenElse :: Bool %1 -> a -> a -> a
ifThenElse cnd bthen belse =
  case cnd of
    True -> bthen
    False -> belse

(>>) :: Control.Monad m => (x %1 -> m y) %1 -> (y %1 -> m z) %1 -> (x %1 -> m z)
(>>) f g x = Control.do
  y <- f x
  g y

(>>=) :: Control.Monad m => (x %1 -> m (y, a)) %1 -> (a %1 -> (y %1 -> m z)) %1 -> (x %1 -> m z)
(>>=) f h x = Control.do
  (y, a) <- f x
  h a y

(>=>) :: Control.Monad m => (a %1 -> (x %1 -> m (y, b))) %1 -> (b %1 -> (y %1 -> m z)) %1 -> (a %1 -> (x %1 -> m z))
(>=>) f g a = f a >>= g

nil :: Control.Applicative m => x %1 -> m x
nil = Control.pure

lift :: Control.Applicative m => m a %1 -> (x %1 -> m (x, a))
lift ma x = Control.pure (x, ) Control.<*> ma

run :: Control.Applicative m => (x, m a) %1 -> m (x, a)
run (x, ma) = lift ma x

pure :: Control.Applicative m => a %1 -> (x %1 -> m (x, a))
pure = lift . Control.pure

drop :: (Control.Monad m, PL.Consumable a) => (x, a) %1 -> m x
drop = do
  a <- nil
  case PL.consume a of
    () -> nil

lift_ :: (Control.Monad m, PL.Consumable a) => m a %1 -> x %1 -> m x
lift_ ma = do
  lift ma
  drop

dowhile :: Control.Monad m => (z %1 -> m (z, Bool)) -> z %1 -> m z
dowhile body = do
  b <- body
  case b of
    True -> dowhile body
    False -> nil

-- thenelse = ...

-- fix = ...

apply :: Control.Monad m => (y, y %1 -> m z) %1 -> m z
apply = do
  f <- nil
  f

pureapply :: Control.Monad m => ((y, t), t %1 -> a) %1 -> m (y, a)
pureapply = do
  f <- nil
  a <- nil
  pure (f a)

pureapply2 :: Control.Monad m => (((y, t1), t2), t2 %1 -> t1 %1 -> a) %1 -> m (y, a)
pureapply2 = do
  pureapply
  pureapply

purelift :: Control.Monad m => (t %1 -> a) -> (y, t) %1 -> m (y, a)
purelift f = do
  pure f
  pureapply

purelift2 :: Control.Monad m => (t2 %1 -> t1 %1 -> a) -> ((y, t1), t2) %1 -> m (y, a)
purelift2 f = do
  pure f
  pureapply2

kleisliapply :: Control.Monad m => ((y, t), t %1 -> m a) %1 -> m (y, a)
kleisliapply = do
  pureapply
  run

kleislilift :: Control.Monad m => (t %1 -> m a) -> (y, t) %1 -> m (y, a)
kleislilift f = do
  pure f
  kleisliapply

when :: Control.Applicative m => Bool -> (x %1 -> m x) -> x %1 -> m x
when cnd body =
  case cnd of
    True -> body
    False -> nil

newtype Cont r a = Cont ((a %1 -> r) %1 -> r)

instance Data.Functor (Cont r) where
  fmap f = Control.fmap f

instance Control.Functor (Cont r) where
  fmap f (Cont g) = Cont \h -> g (h . f)

instance Data.Applicative (Cont r) where
  pure a = Control.pure a
  (<*>) f = (Control.<*>) f

instance Control.Applicative (Cont r) where
  pure a = Cont \h -> h a
  (Cont f) <*> (Cont g) = Cont \h ->
    f \ab -> g $ h . ab

instance Control.Monad (Cont r) where
 (Cont fa) >>= gb = Cont \h ->
   fa \a ->
   case gb a of
     Cont fb ->
       fb h

toCont :: PL.Movable r => ((a %1 -> r) %1 -> r) %1 -> x %1 -> Cont r (x, a)
toCont f x = Cont \g -> f \a -> g (x, a)

alloc :: PL.Movable r => Int -> a -> x %1 -> Cont r (x, Array a)
alloc n a = toCont $ Array.alloc n a

fromList :: PL.Movable r => [a] -> x %1 -> Cont r (x, Array a)
fromList xs = toCont $ Array.fromList xs

set :: Control.Monad m => Int -> a -> (y, Array a) %1 -> m (y, Array a)
set i a = purelift (Array.set i a)

get :: Control.Monad m => Int -> (y, Array a) %1 -> m ((y, Array a), Ur a)
get i = do
  array <- nil
  case Array.get i array of
    (u, array') -> do
      pure array'
      pure u
