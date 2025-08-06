{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant S.pure" #-}
{-# HLINT ignore "Use <$>" #-}

module Language.Stacktic.Linear.Library where

import Prelude.Linear
import Control.Functor.Linear
import qualified Language.Stacktic.Linear.Base as S
import Data.Array.Mutable.Linear ( Array )
import qualified Data.Array.Mutable.Linear as Array
import Control.Monad.Linear.Cont ( ContT, Cont )
import qualified Control.Monad.Linear.Cont as Cont
import Control.Monad.Linear.Identity

lift_ :: (Monad m, Consumable a) => m a %1 -> x %1 -> m x
lift_ ma = S.do
  S.lift ma
  S.drop

dowhile :: Monad m => (z %1 -> m (z, Bool)) -> z %1 -> m z
dowhile body = S.do
  b <- body
  case b of
    True -> dowhile body
    False -> S.nil

pureapply :: Monad m => ((y, t), t %1 -> a) %1 -> m (y, a)
pureapply = S.do
  f <- S.nil
  a <- S.nil
  S.pure (f a)

pureapply2 :: Monad m
  => (((y, t1), t2), t2 %1 -> t1 %1 -> a) %1 -> m (y, a)
pureapply2 = S.do
  pureapply
  pureapply

purelift :: Monad m
  => (t %1 -> a) %1
  -> (y, t) %1 -> m (y, a)
purelift f = S.do
  S.pure f
  pureapply

purelift2 :: Monad m
  => (t2 %1 -> t1 %1 -> a) %1
  -> ((y, t1), t2) %1 -> m (y, a)
purelift2 f = S.do
  S.pure f
  pureapply2

kleisliapply :: Monad m => ((y, t), t %1 -> m a) %1 -> m (y, a)
kleisliapply = S.do
  pureapply
  S.run

kleislilift :: Monad m => (t %1 -> m a) -> (y, t) %1 -> m (y, a)
kleislilift f = S.do
  S.pure f
  kleisliapply

arrayAlloc :: Movable (m r) => Int -> a -> x %1 -> ContT r m (x, Array a)
arrayAlloc n a = S.mkContT $ Array.alloc n a

arrayAllocBeside :: Monad m
  => Int -> a -> (y, Array b) %1 -> m ((y, Array b), Array a)
arrayAllocBeside n a = S.do
  src <- S.nil
  case Array.allocBeside n a src of
    (dst, src) -> S.do
      S.pure src
      S.pure dst

arrayFromList :: Movable (m r) => [a] -> x %1 -> ContT r m (x, Array a)
arrayFromList xs = S.mkContT $ Array.fromList xs

arraySet :: Monad m => Int -> a -> (y, Array a) %1 -> m (y, Array a)
arraySet i a = purelift $ Array.set i a

arrayGet :: Monad m => Int -> (y, Array a) %1 -> m ((y, Array a), Ur a)
arrayGet i = S.do
  array <- S.nil
  case Array.get i array of
    (ura, array) -> S.do
      S.pure array
      S.pure ura

arrayResize :: Monad m => Int -> a -> (y, Array a) %1 -> m (y, Array a)
arrayResize n a = purelift $ Array.resize n a

arraySize :: Monad m => (y, Array a) %1 -> m ((y, Array a), Ur Int)
arraySize = S.do
  array <- S.nil
  case Array.size array of
    (Ur n, array) -> S.do
      S.pure array
      S.pure (Ur n)

arraySlice :: Monad m
  => Int -> Int -> (y, Array a) %1 -> m ((y, Array a), Array a)
arraySlice i n = S.do
  array <- S.nil
  case Array.slice i n array of
    (old, new) -> S.do
      S.pure old
      S.pure new

arrayToList :: Monad m => (y, Array a) %1 -> m (y, Ur [a])
arrayToList = purelift Array.toList

-- evalContT :: Applicative m => (t %1 -> ContT r m r) %1 -> t %1 -> m r
-- evalContT f x = Cont.evalContT (f x)

-- evalCont :: (x %1 -> Cont (Identity b) (Identity b)) %1 -> x %1 -> b
-- evalCont f x = runIdentity $ Cont.evalCont (f x)
