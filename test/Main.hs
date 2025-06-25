{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import qualified Language.Stacktic as S
import Data.Functor.Identity

fib :: Integer -> Integer
fib n = go n 0 1 where
  go 0 !a _ = a
  go n !a !b = go (n - 1) b (a + b)

main :: IO ()
main = hspec do
  describe "library" do
    describe "fib" do
      prop "calculates n'th fib number" $ 
        withMaxSize 1000 \ (NonNegative n) ->
          runIdentity (S.fib ((), n)) == ((), fib n)
    describe "fibfix" do
      prop "calculates n'th fib number, using y combinator" $ 
        withMaxSize 20 \ (NonNegative n) ->
          runIdentity (S.fibfix ((), n)) == ((), fib n)
    describe "sum" do
      prop "same as Prelude.sum" \ (xs :: [Integer]) ->
        runIdentity (S.sum ((), xs)) == ((), sum xs)
    describe "length" do
      prop "same as Prelude.length" \ (xs :: [()]) ->
        runIdentity (S.length ((), xs)) == ((), length xs)
