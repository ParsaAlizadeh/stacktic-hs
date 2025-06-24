{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Data.Functor.Identity
import qualified Language.Stacktic as S

fib :: Integer -> Integer
fib n = go n 0 1 where
  go 0 !a _ = a
  go n !a !b = go (n - 1) b (a + b)

chooseNonNegative :: Integer -> Gen Integer
chooseNonNegative n = chooseInteger (0, n)

main :: IO ()
main = hspec do
  describe "library" do
    describe "fib" do
      prop "calculates n'th fib number" $
        forAll (chooseNonNegative 1000) \n ->
          runIdentity (S.fib ((), n)) == ((), fib n)
    describe "fibfix" do
      prop "calculates n'th fib number, using y combinator" $ 
        forAll (chooseNonNegative 20) \n ->
          runIdentity (S.fibfix ((), n)) == ((), fib n)
    describe "sum" do
      prop "same as Prelude.sum" \ (xs :: [Integer]) ->
        runIdentity (S.sum ((), xs)) == ((), sum xs)
    describe "length" do
      prop "same as Prelude.length" \ (xs :: [()]) ->
        runIdentity (S.length ((), xs)) == ((), length xs)
