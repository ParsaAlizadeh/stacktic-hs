{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Language.Stacktic.Example where

import qualified Language.Stacktic.Base as S
import qualified Language.Stacktic.Library as S
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Void
import Debug.Trace

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

phonebook :: MonadIO m => y -> m (y, [String])
phonebook = evalContT . S.do
  S.pure (0 :: Int)
  S.label
  S.pure []
  S.liftIO_ . putStrLn $ "book is empty"
  S.dowhile S.do
    line <- S.liftIO getLine
    case line of
      "end" -> S.pure False
      "save" -> S.do
        xs <- S.nil
        oldlabel <- S.nil
        oldb <- S.nil
        S.pure 0
        newlabel <- S.label
        b <- S.nil
        if b > 1 then S.do
          S.pure oldb
          oldlabel
          S.absurd
        else S.do
          S.pure (b + 1)
          S.pure newlabel
          S.pure xs
          S.pure True
          S.liftIO_ . putStrLn $ "book is " <> show xs
      "undo" -> S.do
        S.drop
        lab <- S.nil
        lab
        S.absurd
      _ -> S.do
        xs <- S.nil
        S.pure (line : xs)
        S.pure True
  xs <- S.nil
  S.drop
  S.drop
  S.pure xs

-- undoer :: MonadCont m => 
--   (
--     (((x, Int), (x, Int) -> m (z, Void)) -> m ((x, Int), (x, Int) -> m (z, Void)))
--     -> (((x, Int), (x, Int) -> m (z, Void)) -> m y)
--     -> ((x, Int), (x, Int) -> m (z, Void)) -> m (((x, Int), (x, Int) -> m (z, Void)), a)
--   )
--   -> (x -> m (x, a))
undoer f = S.do
  S.pure (0 :: Int)
  S.label
  a <- f save undo
  S.drop
  S.drop
  S.pure a
  where
    save = S.do
      oldlabel <- S.nil
      oldcnt <- S.nil
      S.pure (0 :: Int)
      newlabel <- S.label
      cnt <- S.nil
      if cnt > 1 then S.do
        S.pure oldcnt
        oldlabel
        S.absurd
      else S.do
        S.pure (cnt + 1)
        S.pure newlabel
    undo = S.do
      lab <- S.nil
      lab
      S.absurd

phonebook' :: MonadIO m => y -> m (y, [String])
phonebook' = evalContT . undoer \save undo -> S.do
  S.pure []
  S.liftIO_ . putStrLn $ "book is empty"
  S.dowhile S.do
    line <- S.liftIO getLine
    case line of
      "end" -> S.pure False
      "save" -> S.do
        xs <- S.nil
        save
        S.pure xs
        S.liftIO_ . putStrLn $ "book is " <> show xs
        S.pure True
      "undo" -> S.do
        S.drop
        undo
      _ -> S.do
        xs <- S.nil
        S.pure (line : xs)
        S.pure True
