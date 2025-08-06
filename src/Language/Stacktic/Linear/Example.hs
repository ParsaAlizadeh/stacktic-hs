{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant S.pure" #-}

module Language.Stacktic.Linear.Example where

import Prelude.Linear
import qualified Language.Stacktic.Linear.Base as S
import qualified Language.Stacktic.Linear.Library as S
import Control.Monad.Linear.Cont

fib :: Movable (m r) => (y, Ur Int) %1 -> ContT r m (y, Ur Integer)
fib = S.do
  Ur n <- S.nil
  S.arrayAlloc (n + 1) (0 :: Integer)
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
