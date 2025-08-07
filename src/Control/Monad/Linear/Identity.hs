{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DerivingVia #-}

module Control.Monad.Linear.Identity where

import Prelude.Linear
import qualified Data.Functor.Linear as Data
import Control.Functor.Linear
import qualified Data.Functor.Identity as Old
import Data.Bifunctor.Linear

newtype Identity a = Identity (Old.Identity a)
  deriving Data.Functor via Old.Identity
  deriving Data.Applicative via Old.Identity
  deriving Functor via Old.Identity
  deriving Applicative via Old.Identity
  deriving Monad via Old.Identity

mkIdentity :: a %1 -> Identity a
mkIdentity a = Identity (Old.Identity a)

runIdentity :: Identity a %1 -> a
runIdentity (Identity (Old.Identity a)) = a

instance Consumable a => Consumable (Identity a) where
  consume = consume . runIdentity

instance Dupable a => Dupable (Identity a) where
  dup2 = bimap mkIdentity mkIdentity . dup2 . runIdentity

instance Movable a => Movable (Identity a) where
  move = Data.fmap mkIdentity . move . runIdentity
