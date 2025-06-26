{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Stacktic.Base
  ( (>>), (>>=), (>=>)
  , nil, lift, run, pure
  , drop
  , lift_
  , dowhile, thenelse
  , fix
  , apply
  , kleisliapply, kleislilift
  , pureapply, pureapply2, purelift, purelift2
  , when
  , callCC, label
  , throwError, catchError
  , translift, translift_
  , liftIO, liftIO_
  , get, put, state
  , absurd
  , letN, pureN
  ) where

import Prelude (Bool(..), (.), fromInteger, Int, ($))
import qualified Prelude as P
import qualified Control.Monad as P
import qualified Control.Monad.Fix as P
import qualified Control.Monad.Cont as P
import qualified Control.Monad.Except as P
import qualified Control.Monad.Trans as P
import qualified Data.Void as P
import Language.Haskell.TH

-- | defined for RebindableSyntax
ifThenElse :: Bool -> a -> a -> a
ifThenElse cnd bthen belse = case cnd of
  True -> bthen
  False -> belse

-- | Composition
(>>) :: P.Monad m => (x -> m y) -> (y -> m z) -> (x -> m z)
f >> g = f P.>=> g

-- | Let expression
(>>=) :: P.Monad m => (x -> m (y, a)) -> (a -> (y -> m z)) -> (x -> m z)
f >>= h = \x -> P.do
  (y, a) <- f x
  h a y

-- | Composition of let expressions
(>=>) :: P.Monad m => (a -> (x -> m (y, b))) -> (b -> (y -> m z)) -> (a -> (x -> m z))
f >=> g = \a -> f a >>= g

-- | Empty expression
nil :: P.Applicative m => x -> m x
nil = P.pure

-- | Lift a monadic value, run, and push onto the stack.
lift :: P.Applicative m => m a -> (x -> m (x, a))
lift ma x = P.pure (\a -> (x, a)) P.<*> ma

-- | Run a monadic value on top of the stack.
run :: P.Applicative m => (x, m a) -> m (x, a)
run (x, ma) = lift ma x

-- | Lift a pure value and push onto the stack.
pure :: P.Applicative m => a -> (x -> m (x, a))
pure = lift . P.pure

-- | Drop one element from the stack.
drop :: P.Monad m => (x, a) -> m x
drop = do
  _ <- nil
  nil

-- | Lift a monadic value, run, but ignore the resulting pure value.
lift_ :: P.Monad m => m a -> x -> m x
lift_ ma = do
  lift ma
  drop

-- | Run body as long as it pushes 'True' on top of the stack.
dowhile :: P.Monad m 
  => (z -> m (z, Bool)) -- ^ The body
  -> (z -> m z)
dowhile body = do
  b <- body
  if b then dowhile body else nil

-- | @then ... else ...@ expression from Stacktic. Reads boolean condition from the stack.
thenelse :: P.Monad m 
  => (y -> m z) -- ^ @then@ part of expression
  -> (y -> m z) -- ^ @else@ part of expression
  -> ((y, Bool) -> m z)
thenelse thenbody elsebody = do
  b <- nil
  if b then thenbody else elsebody

-- | Fixed-point function for S Monad expressions.
fix :: P.MonadFix m => (a -> (x -> m (y, a))) -> (x -> m (y, a))
fix f x = P.mfix (\ ~(_, a) -> f a x)

-- | Apply the topmost element of the stack on the stack itself.
apply :: P.Monad m => (y, y -> m z) -> m z
apply = do
  f <- nil
  f

-- | Apply the pure function on top of the stack.
pureapply :: P.Monad m => ((y, t), t -> a) -> m (y, a)
pureapply = do
  f <- nil
  a <- nil
  pure (f a)

-- | Apply the 2-input pure function on top of the stack.
-- Notice the order of arguments:
-- 
-- > S.do { 
-- >   S.pure b 
-- >   S.pure a
-- >   S.pureapply2 (\a b -> ...) 
-- > }
pureapply2 :: P.Monad m => (((y, b), a), a -> b -> c) -> m (y, c)
pureapply2 = do
  pureapply
  pureapply

-- | Lift a pure function and apply it.
purelift :: P.Monad m => (t -> a) -> ((y, t) -> m (y, a))
purelift f = do
  pure f
  pureapply

-- | Lift a 2-input pure function and apply it. See 'pureapply2' about order of arguments.
purelift2 :: P.Monad m => (a -> b -> c) -> ((y, b), a) -> m (y, c)
purelift2 f = do
  pure f
  pureapply2

-- | Apply the Kliesli arrow (function of the form 'a -> m b') on top of stack.
kleisliapply :: P.Monad m => ((y, t), t -> m a) -> m (y, a)
kleisliapply = do
  pureapply
  run

-- | Lift and apply a Kliesli arrow.
kleislilift :: P.Monad m => (t -> m a) -> ((y, t) -> m (y, a))
kleislilift f = do
  pure f
  kleisliapply

-- | Run body when the condition is 'True', otherwise do not run anything.
when :: P.Applicative m 
  => Bool -- ^ The condition passed as a pure value
  -> (x -> m x) -- ^ The body
  -> (x -> m x)
when cnd body = if cnd then body else nil

-- | Similar to @callCC@ from @ContT@ monad, except for a different type to signify S Monad
-- expressions. When used as @callCC \\kont -> body@, either the @body@ returns with the stack state
-- @x@, or @kont@ (an S Monad expression) is used inside the @body@ at a moment where the stack
-- state is @x@. The entier expression returns with stack state @x@.
callCC :: P.MonadCont m 
  => ((x -> m y) -> (z -> m x)) -- ^ The function taking @kont@ and returning @body@
  -> (z -> m x)
callCC f z = P.callCC \h -> f h z

-- | Similar to @label_@ from @ContT@ monad. See 'callCC'. Note that it is different to @label@ in
-- the sense that you have to manually store the required state of @label@ on the stack.
label :: P.MonadCont m => x -> m (x, x -> m y)
label x = (\(m, x) -> (x, m)) P.<$> P.label x

-- | Same as @throwError@ from @ExceptT@ monad.
throwError :: P.MonadError e m => e -> m y
throwError = P.throwError

-- | Similar to @catchError@ from @ExceptT@ monad, except for a different output type to signify S
-- Monad expressions.
catchError :: P.MonadError e m => (x -> m y) -> (e -> m y) -> (x -> m y)
catchError trybody catchbody x =
  P.catchError (trybody x) catchbody

-- | Lift through a monad transformer and push on top of the stack.
translift :: (P.MonadTrans t, P.Monad m) => m a -> x -> t m (x, a)
translift = lift . P.lift

-- | Lift through a monad transformer and ignore the resulting pure value.
translift_ :: (P.MonadTrans t, P.Monad m) => m a -> x -> t m x
translift_ = lift_ . P.lift

-- | Lift @IO@ action and push on top of the stack.
liftIO :: P.MonadIO m => P.IO a -> x -> m (x, a)
liftIO = lift . P.liftIO

-- | Lift @IO@ action and ignore the resulting pure value.
liftIO_ :: P.MonadIO m => P.IO a -> x -> m x
liftIO_ = lift_ . P.liftIO

-- | Get the stack as an element on top the stack.
get :: P.Applicative m => x -> m (x, x)
get x = P.pure (x, x)

-- | Put the top element of the stack as the stack itself.
put :: P.Applicative m => (x, y) -> m y
put (_, y) = P.pure y

-- | Apply a pure function on the stack itself.
state :: P.Applicative m => (x -> y) -> (x -> m y)
state f = P.pure . f

-- | The @absurd@ function for the top element of the stack.
absurd :: (x, P.Void) -> t
absurd (_, x) = P.absurd x

-- | @$('letN' 2) = \((x, b), a) -> (x, m (a, b))@
letN :: Int -> Q Exp
letN n | n P.>= 0 = P.do
  stack <- newName "x"
  names <- P.mapM (newName . ('a' :) . P.show) [1..n]
  let mkPat [] = varP stack
      mkPat (a : names) = tupP [mkPat names, varP a]
      pat = mkPat names
      expr = tupE [varE stack, tupE (P.map varE names)]
  [| \ $pat -> P.pure $expr |]
letN n = P.fail $ "letN: N must be non-negative, got " P.<> P.show n

-- | @$('pureN' 2) = \(a, b) -> (x -> m ((x, b), a))@
pureN :: Int -> Q Exp
pureN n | n P.>= 0 = P.do
  stack <- newName "x"
  names <- P.mapM (newName . ('a' :) . P.show) [1..n]
  let pat = tupP (P.map varP names)
      mkExpr [] = varE stack
      mkExpr (a : names) = tupE [mkExpr names, varE a]
      expr = mkExpr names
  [| \ $pat $(varP stack) -> P.pure $expr |]
pureN n = P.fail $ "pureN: N must be non-negative, got " P.<> P.show n
