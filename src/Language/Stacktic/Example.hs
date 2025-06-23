{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Stacktic.Example where

import qualified Language.Stacktic.Base as S
import qualified Language.Stacktic.Library as S
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.IO.Class

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

allcont :: Monad m => (y, [Bool]) -> m (y, Bool)
allcont = evalContT . S.callCC \kont -> S.do
  S.for_ S.do
    x <- S.nil
    S.when (not x) S.do
      S.pure False
      kont
  S.pure True

divzip :: (MonadError y m, Integral b) => ((y, [b]), [b]) -> m (y, [b])
divzip = go `S.catchError` S.do { S.pure [] } where
  go = S.do
    xs <- S.nil
    ys <- S.nil
    S.pure (zip xs ys)
    S.for S.do
      (x, y) <- S.nil
      S.when (y == 0) S.do
        S.throwError
      S.pure (x `div` y)

stubborngreeting :: MonadIO m => y -> m (y, Int)
stubborngreeting = evalContT . S.do
  S.liftIO_ . putStrLn $ "Hello there. What is your name?"
  S.pure 2
  back <- S.label
  name <- S.liftIO getLine
  S.when (null name) S.do
    n <- S.nil
    S.liftIO_ . putStrLn $ "I am going to ask for the " <> show n <> " time. What is your name?"
    S.pure (n + 1)
    back
  S.liftIO_ . putStrLn $ "Welcome " <> name
