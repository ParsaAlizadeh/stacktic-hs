{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Hspec

import Data.Functor.Identity
import qualified Language.Stacktic as S

unS :: (() -> IO ()) -> IO ()
unS f = f ()

main :: IO ()
main = hspec do
  describe "library" do
    describe "fib" do
      it "calculates n'th fib number" do
        runIdentity (S.fib ((), 2 :: Integer))
          `shouldBe` ((), 1 :: Integer)
        runIdentity (S.fib ((), 10 :: Integer)) 
          `shouldBe` ((), 55 :: Integer)
    describe "fibfix" do
      it "calculates n'th fib number, using y combinator" $ unS S.do
        x :: Integer <- S.pure (2 :: Integer) S.>> S.fib
        S.lift_ $ x `shouldBe` 1
        y :: Integer <- S.pure (10 :: Integer) S.>> S.fib
        S.lift_ $ y `shouldBe` 55
    describe "sum" do
      it "finds sum of elements in a list" $ unS S.do
        x :: Integer <- S.pure [1 :: Integer, 4, 10] S.>> S.sum
        S.lift_ $ x `shouldBe` 15
