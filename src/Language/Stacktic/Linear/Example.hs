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
import qualified Language.Stacktic.Linear as S

fib :: Movable r => (y, Ur Int) %1 -> S.Cont r (y, Ur Integer)
fib = S.do
  Ur n <- S.nil
  S.alloc (n + 1) (0 :: Integer)
  S.pure $ Ur 2
  S.dowhile S.do
    Ur i <- S.nil
    if i <= n then S.do
      Ur a <- S.get (i - 2)
      Ur b <- S.get (i - 1)
      S.set i (a + b)
      S.pure $ Ur (i + 1)
      S.pure True
    else S.do
      S.pure $ Ur i
      S.pure False
  S.drop
  Ur fn <- S.get n
  S.drop
  S.pure $ Ur fn
