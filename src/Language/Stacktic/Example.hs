{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Stacktic.Example where

import qualified Language.Stacktic.Base as S
import qualified Language.Stacktic.Library as S

subsets :: (y, [a]) -> [(y, [a])]
subsets = S.do
  xs <- S.nil
  S.pure []
  S.pure xs
  S.for_ S.do
    x <- S.nil
    ys <- S.nil
    S.lift [x : ys, ys]

increasingsubseqs :: (Bounded a, Ord a) => (y, [a]) -> [(y, [a])]
increasingsubseqs = S.do
  xs <- S.nil
  S.pure (minBound, [])
  S.pure xs
  S.for_ S.do
    x <- S.nil
    (last, ys) <- S.nil
    if x > last then S.do
      S.lift [(x, x : ys), (last, ys)]
    else S.do
      S.pure (last, ys)
  (_, ys) <- S.nil
  S.pure (reverse ys)
