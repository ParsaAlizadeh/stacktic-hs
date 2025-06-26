{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Stacktic.Library 
  ( fibfix
  , fib
  , for, for_
  , sum, length
  , dup, swap, with
  , rotate3
  ) where

import Prelude hiding (length, sum)
import qualified Language.Stacktic.Base as S
import Control.Monad.Fix

-- | Calculate n\'th Fibonacci number, using 'Language.Stacktic.Base.fix'.
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

-- | Calculate n\'th Fibonacci number, using 'Language.Stacktic.Base.dowhile'.
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

-- | Map S Monad function over the elements of the input list, returning the resulting list.
for :: Monad m => ((y, a) -> m (y, b)) -> ((y, [a]) -> m (y, [b]))
for f = S.do
  xs <- S.nil
  case xs of
    [] -> S.pure []
    y : ys -> S.do
      a <- S.pure y S.>> f
      as <- S.pure ys S.>> for f
      S.pure (a : as)

-- | Map S Monad function over the elements of the input list with no output.
for_ :: Monad m => ((y, a) -> m y) -> ((y, [a]) -> m y)
for_ f = S.do
  xs <- S.nil
  case xs of
    [] -> S.nil
    y : ys -> S.do
      S.pure y S.>> f
      S.pure ys S.>> for_ f

-- | Sum of elements of the input list.
sum :: (Monad m, Num a) => (y, [a]) -> m (y, a)
sum = S.do
  xs <- S.nil
  S.pure 0
  S.pure xs
  for_ (S.purelift2 (+))

-- | Length of the input list.
length :: (Monad m, Num b) => (y, [a]) -> m (y, b)
length = S.do
  xs <- S.nil
  S.pure 0
  S.pure xs
  for_ S.do
    S.drop
    S.purelift (+ 1)

-- | Duplicate the top element of the stack.
dup :: Monad m => (y, a) -> m ((y, a), a)
dup = S.do
  x <- S.nil
  $(S.pureN 2) (x, x)

-- | Swap the top two elements of the stack.
swap :: Monad m => ((y, a), b) -> m ((y, b), a)
swap = S.do
  (x, y) <- $(S.letN 2)
  $(S.pureN 2) (y, x)

-- | Get the value on top of stack wihtout removing it from the stack.
with :: Monad m => (t -> (y, t) -> m z) -> (y, t) -> m z
with f = S.do
  a <- S.nil
  S.pure a
  f a

rotate3 :: Monad m => (((y, b1), b2), b3) -> m (((y, b3), b1), b2)
rotate3 = S.do
  (x, y, z) <- $(S.letN 3)
  $(S.pureN 3) (y, z, x)
