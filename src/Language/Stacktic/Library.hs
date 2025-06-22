{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Stacktic.Library where

import Prelude hiding (length, sum)
import qualified Language.Stacktic.Base as S
import Control.Monad.Fix

fibfix :: (MonadFix m, Ord a, Num a) => (y, a) -> m (y, a)
fibfix = S.do
  S.fix \fib -> S.pure S.do
    n <- S.nil
    if n <= 1
      then S.pure n
      else S.do
        S.pure (n - 1)
        a <- fib
        S.pure (n - 2)
        b <- fib
        S.pure (a + b)
  S.apply

fib :: (Monad m, Eq a1, Num a1, Num a2) => (y, a1) -> m (y, a2)
fib = S.do
  n <- S.nil
  S.pure 0
  S.pure 1
  S.pure n
  S.dowhile S.do
    n <- S.nil
    case n of
      0 -> S.do
        S.pure 0
        S.pure False
      _ -> S.do
        b <- S.nil
        a <- S.nil
        S.pure b
        S.pure (a + b)
        S.pure (n - 1)
        S.pure True
  S.drop
  S.drop

for :: Monad m => ((y, a) -> m (y, b)) -> ((y, [a]) -> m (y, [b]))
for f = S.do
  xs <- S.nil
  case xs of
    [] -> S.pure []
    y : ys -> S.do
      a <- S.pure y S.>> f
      as <- S.pure ys S.>> for f
      S.pure (a : as)

for_ :: Monad m => ((y, a) -> m y) -> ((y, [a]) -> m y)
for_ f = S.do
  xs <- S.nil
  case xs of
    [] -> S.nil
    y : ys -> S.do
      S.pure y S.>> f
      S.pure ys S.>> for_ f

sum :: (Monad m, Num a) => (y, [a]) -> m (y, a)
sum = S.do
  xs <- S.nil
  S.pure 0
  S.pure xs
  for_ (S.purelift2 (+))

length :: (Monad m, Num b) => (y, [a]) -> m (y, b)
length = S.do
  xs <- S.nil
  S.pure 0
  S.pure xs
  for_ S.do
    S.drop
    S.purelift (+ 1)

dup :: Monad m => (y, a) -> m ((y, a), a)
dup = S.do
  x <- S.nil
  S.pure x
  S.pure x

swap :: Monad m => ((y, a), b) -> m ((y, b), a)
swap = S.do
  x <- S.nil
  y <- S.nil
  S.pure x
  S.pure y

with :: Monad m => (t -> (y, t) -> m z) -> (y, t) -> m z
with f = S.do
  a <- S.nil
  S.pure a
  f a
