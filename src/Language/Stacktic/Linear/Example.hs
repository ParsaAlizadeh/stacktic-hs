{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant S.pure" #-}

module Language.Stacktic.Linear.Example where

import Prelude.Linear
import qualified Prelude as P
import Control.Functor.Linear
import qualified Language.Stacktic.Linear.Base as S
import qualified Language.Stacktic.Linear.Library as S
import Control.Monad.Linear.Cont

fib :: Movable y => (y, Ur Int) %1 -> (y, Ur Integer)
fib = S.runCont S.do
  Ur n <- S.nil
  if n <= 1 then S.pure $ Ur $ fromIntegral n else S.do
    S.arrayAlloc (n + 1) (0 :: Integer)
    S.arraySet 0 0
    S.arraySet 1 1
    S.pure (Ur 2)
    S.dowhile S.do
      Ur i <- S.nil
      if i <= n then S.do
        Ur a <- S.arrayGet (i - 2)
        Ur b <- S.arrayGet (i - 1)
        S.arraySet i (a + b)
        S.pure $ Ur (i + 1)
        S.pure True
      else S.do
        S.pure (Ur i)
        S.pure False
    S.drop
    Ur fn <- S.arrayGet n
    S.drop
    S.pure (Ur fn)

for :: Monad m => ((y, a) %1 -> m (y, b)) -> (y, [a]) %1 -> m (y, [b])
for f = S.do
  xs <- S.nil
  case xs of
    [] -> S.pure []
    y : ys -> S.do
      a <- S.pure y S.>> f
      as <- S.pure ys S.>> for f
      S.pure (a : as)

for_ :: Monad m => ((y, a) %1 -> m y) -> (y, [a]) %1 -> m y
for_ f = S.do
  xs <- S.nil
  case xs of
    [] -> S.nil
    y : ys -> S.do
      S.pure y S.>> f
      S.pure ys S.>> for_ f

sum :: (Monad m, P.Num a, Additive a) => (y, [a]) %1 -> m (y, a)
sum = S.do
  xs <- S.nil
  S.pure 0
  S.pure xs
  for_ S.do
    x <- S.nil
    s <- S.nil
    S.pure $ x + s
