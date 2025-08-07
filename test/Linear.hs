{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Prelude.Linear

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import qualified Language.Stacktic.Linear.Example as S
import Control.Monad.Linear.Identity

fib :: Int -> Integer
fib n = go n 0 1 where
  go 0 !a _ = a
  go n !a !b = go (n - 1) b (a + b)

main :: IO ()
main = hspec do
  describe "linear.library" do
    describe "fib" do
      prop "calculates n'th fib number" $
        withMaxSize 1000 \ (NonNegative n) ->
          S.fib ((), Ur n) == ((), Ur (fib n))
    describe "sum" do
      prop "calculates sum of elements in a list" $ \(xs :: [Int]) ->
        runIdentity (S.sum ((), xs)) == ((), sum xs)

