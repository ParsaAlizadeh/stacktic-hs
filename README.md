# Stacktic-hs

## Showcase

This is part of the module [Language.Stacktic.Library](src/Language/Stacktic/Library.hs).

``` hs
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}

import qualified Language.Stacktic.Base as S

-- | Duplicate the top element of the stack.
dup :: Monad m => (y, a) -> m ((y, a), a)
dup = S.do
  x <- S.nil
  S.pure x
  S.pure x

-- | Swap the top two elements of the stack.
swap :: Monad m => ((y, a), b) -> m ((y, b), a)
swap = S.do
  x <- S.nil
  y <- S.nil
  S.pure x
  S.pure y

-- | Calculate n\'th Fibonacci number, using 'Language.Stacktic.Base.dowhile'.
fib :: (Monad m, Eq a1, Num a1, Num a2) => (y, a1) -> m (y, a2)
fib = S.do
  n <- S.nil
  S.pure 0
  S.pure 1
  S.pure n
  S.dowhile S.do
    n <- S.nil
    case n of
      0 -> S.do
        S.pure 0
        S.pure False
      _ -> S.do
        b <- S.nil
        a <- S.nil
        S.pure b
        S.pure (a + b)
        S.pure (n - 1)
        S.pure True
  S.drop
  S.drop

-- | Map S Monad function over the elements of the input list with no output.
for_ :: Monad m => ((y, a) -> m y) -> ((y, [a]) -> m y)
for_ f = S.do
  xs <- S.nil
  case xs of
    [] -> S.nil
    y : ys -> S.do
      S.pure y S.>> f
      S.pure ys S.>> for_ f

-- | Length of the input list.
length :: (Monad m, Num b) => (y, [a]) -> m (y, b)
length = S.do
  xs <- S.nil
  S.pure 0
  S.pure xs
  for_ S.do
    S.drop
    S.purelift (+ 1)
```

## Idea

Among programming languages, there are many stack-oriented languages,
 where the main storage unit of the program is a stack,
 and the program specifies instructions to be carried out on the stack.
The instructions are simple, but allow the language to be as powerful as a Turing Machine.
Such examples of these instructions are:

- `dup`: duplicates the top element of the stack.
    If we write stacks as a list,
    and the top element would be the rightmost element of that list
    then `dup` turns a stack of `[b]` into `[b, b]`.
    Other elements of the stack will be untouched.
- `swap`: swaps the two elements at the top of the stack.
- `drop`: drops (or removes) the top element of the stack.

Combining these instructions with other instructions for manipulating the stack
 and numeric instructions builds a stack-oriented language.
The most notable one is `Forth`, yet there are many others too.

### A. Stacktic

As a PL design project, we designed a stack-oriented language with a static type system.
We also considered adding Quality of Life features, but they turned out to be an essential part of the language.
To show this language, it suffices to show some basic functions.

```
define swap : { $a $b } -> { $b $a } {
    let x
    let y
    x
    y
}
```

This is the `swap` function that swaps the top two elements of the stack.
In the first line we have a type signature: `$a` and `$b` represent type parameters.
The `let` keyword pops an element from the top of the stack and assigns a name to it.
The name can be used later in the scope of `{}`.
A name can be used on its own, to push the value represented by that name on top of the stack.

This is an example of a recursive Fibonacci function.
```
define fib : Int -> Int {
    let n
    { n 1 <= } then {
        n
    } else {
        { n 1 - fib } { n 2 - fib } +
    }
}
```
The language does not need an explicit `if` statement.
The `then T else F` statement takes a boolean from the stack
 and executes `T` or `F` depending on the boolean value.
Type (as I will define later) of expression `T` and `F` must match,
 and we see here that both of them produce a single integer.
Some of the curly braces used in this example are purely decorative
such as the ones used in `{ n 1 <= }` and `{ n 1 - fib }`;
The syntax can be written to look like a postfix S-expression.

### B. Type System

The idea of Stacktic was to assign a type to each expression.
Each type looks like this:
$$[ A_1 , A_2 , \ldots , A_n ] \to [ B_1 , B_2 , \ldots , B_m ]$$

Meaning, if the stack had elements $A_1 , \ldots , A_n$ on top ($A_n$ being the topmost element),
 then by executing the given expression on the stack,
 it removes the $n$ element with types $A_n, \ldots, A_2, A_1$ from the stack
 and pushes $m$ element with types $B_1, B_2, \ldots, B_m$ onto the stack
 ($B_m$ being the topmost element after the execution of the expression).
(we later add parametric polymorphism, but for simplicity here we assume a monomorphic function)

There are two situations where type rules differ from a functional language.
One of them is the composition of two expressions $f ; g$: Given expression $f$
with type $A \to B$ and expression $g$ with type $C \to D$, compute the type of their
composition $f ; g$ (execute $g$ after executing $f$) or report a type error.
I do not go into details on how to compute this, but it is fairly straightforward to write
 a function with list inputs $A, B, C, D$ that outputs type lists $E$ and $F$, where
 $f ; g$ has the type of $[E_1, \ldots, E_n] \to [F_1, \ldots, F_m]$.

The other rule occurs in `then ... else ...` statements.
Two branches must have the same type.
However there are situations where the branches could have the same effect on the stack,
 yet their types would be different.
Consider this example:
```
then {
    0
} else {
    let n
    { n 1 + }
    n
}
```

The `then` branch has the type $[] \to [I]$, where $I$ is the integer type,
 and the `else` branch has the type $[I] \to [I, I]$.
If the stack has an integer on top, the resulting stack after executing this
 expression is the same regardless of whether executing the `then` or the `else`
 branch.

So, for expressions similar to `then ... else ...` we also consider generalizing
 either branches, so that the generalized types of all branches become equal.
These two rules, which I call "chain" and "parallel" form the basic
 rules of this type system.
Other extensions, such as `then ... else ...` consuming a boolean value,
 are straightforward.

We can add parametric polymorphism by considering expression types to have the form:
$$\forall \alpha_1,\ldots,\alpha_k \quad [A_1, \ldots, A_n] \to [B_1, \ldots, B_m]$$
where each $A_i$ and $B_i$ may mention parametric types $\alpha_j$.
Rules of instantiating and generalizing follow from the Hindley-Milner type system.

### C. Functional Representation

While previous sections introduce the language itself,
 the goal of this project is rather different.
The usage of the Hindley-Milner type system in a stack-oriented language
 suggests that there might be an encoding of Stacktic inside a functional programming
 language, such as Haskell.

In functional programming, currying is the rule for multi-argument functions.
A function accepting a tuple of three elements can be curried, so that it accepts its three inputs
 one at a time.
```hs
foo  :: (a, b, c) -> d
foo' :: a -> (b -> (c -> d))
```

We notice that this rule breaks a symmetry in the system.
Namely a function `bar` of type
```hs
bar :: e -> (a, b, c)
```
can not easily compose with a curried function `foo'`,
 yet it can be composed with the uncurried function `foo`.

This forms the basics of how Stacktic can be written in Haskell.
We can represent the stack type with a $n$ element tuple,
 and each function transforms the tuple.
The issue here is the matter of sub-typing:
A function such as `swap` acts on any tuple with at least two elements.
So we can not assign a general type to `swap` with tuple representation, at least in Haskell.

Here comes the idea: represent stack type with a series of pairs inside each other.
For example, here is the type of `swap` function:
```hs
swap :: ((x, a), b) -> ((x, b), a)
```
The type of stack is represented as a pair `(y, a)`, where `y` is
 of stack type, and `a` represents the topmost element on the stack.
Here in the `swap` function, `x` represents the rest of the stack,
 while `a` and `b` are two elements on top of stack `x`.

Although we can define `swap` in Haskell terms, we want
 the combinators suited for this language.
One of them is the notion of composition or chain, which is the same as composition
 in Haskell
```hs
(>>) :: (x -> y) -> (y -> z) -> (x -> z)
f >> g = g . f
```

The empty expression and the push instruction are as follows.
```hs
nil :: x -> x
nil = id

pure :: a -> (x -> (x, a))
pure a x = (x, a)
```
Notice that `nil` is the identity element for composition, and `pure` is
 the pair constructor.

The `let` expression can be defined in this way, although we will use a different notation later on.
```hs
slet :: (a -> (x -> y)) -> ((x, a) -> y)
slet f (x, a) = f a x
```
The `slet` function takes a function that has a value of `a` as input, and turns the returned expression
 into a new expression that reads this value of `a` from the top of the stack.
Notice that `slet` is the same as the `uncurry` function in Haskell.

Although we defined a parallel type rule for Stacktic language, we do not need one here.
The fact that some type parameters refer to the whole stack instead of single element types
 allows us to defer the task of generalization to the wonderful Haskell type system.
Namely, to define a `then ... else ...` expression, we can simply write:
```hs
thenelse :: (x -> y) -> (x -> y) -> ((x, Bool) -> y)
thenelse thenbody elsebody = slet \b -> case b of
    True -> thenbody
    False -> elsebody
```

### D. S Monad

Using the `RebindableSyntax` extension, I did try to simulate Stacktic as a do-notation syntax.
The related definitions are as follows:
```hs
(>>) :: (x -> y) -> (y -> z) -> (x -> z)
f >> g = g . f

(>>=) :: (x -> (y, a)) -> (a -> (y -> z)) -> (x -> z)
f >>= g = \x ->
    let (y, a) = f x
    in g a y
```

The monadic bind `>>=` is the application of `let` expression on a given expression.
Using these definitions, we can write `swap` entirely in Haskell.
```hs
swap :: ((x, a), b) -> ((x, b), a)
swap = do
    b <- nil
    a <- nil
    pure b
    pure a
```
The `b <- nil` has the same effect as `slet \b -> ...`.
Although we can have the generic `b <-` expression with nothing on the right-hand side of the arrow,
 we are limited to what we have in the do-notation.

Remember that S monad is a different type of monad.
As we will see, it is closely related to the state monad and indexed monads,
 but there are differences that I will explain.
For now, we know that any Stacktic program can be translated
 into a program in Haskell using this S monad.

### E. Relation To State Monad

Every element of the state monad `State s a` is equivalent to a function of type `s -> (s, a)`.
In the language of Stacktic, this is a function that pushes a single element of `a` on top of the stack.

The important difference between the S monad and the state monad is that states can change in the S monad.
This is visible in the type `x -> (y, a)`, which changes the type of state too.
S monad provides a way to add or remove states to the current state, so
 we can say that it is an ergonomic solution to compose different state monads.

This difference comes from the definition of `>>` (and similarly `>>=`).
```hs
-- | (>>) in state monad
(>>) :: (s -> (s, a)) -> (s -> (s, b)) -> (s -> (s, b))

-- | (>>) in S monad
(>>) :: (s -> t) -> (t -> r) -> (s -> r)
```
The function `>>` does not require functions to produce a specific value on top of the stack
 and does not drop any value produced by the functions.

### F. Relation To Indexed Monads

Indexed monads are defined by the following typeclass.
```hs
class IxMonad m where
  ireturn :: a -> m p p a
  ibind :: m p q a -> (a -> m q r b) -> m p r b
```
We can simulate the composition of state types using the indices:
```hs
newtype S p q a = S (p -> (q, a))
```
which is similar to what we had defined for S monad.

Notice that there is no equivalent notion of `nil`.
The nearest one is:
```hs
nil :: S (p, a) p a
nil = S id
```
This function assumes a stack element `a` on top of the stack.
The S monad version of `nil` is preferable because of two reasons:
- From the type of `nil :: x -> x`, we can deduce `nil = id`.
  This is also true for the function `nil :: (p, a) -> (p, a)`,
   but it forces the type of stack to be a pair.
- The empty stack type must also be a pair so that we can apply the new `nil` to it.
  This is different for S monad which can represent the empty stack type with any desired type (such as `()`).

The same issue with the `nil` function happens when we want to define `>>`.
```hs
-- | Drops value. Similar to Monad.>>
(>>) :: S p q a -> S q r b -> S p r b

-- | Concatantes programs. Similar to S.>>
(>>) :: S p q a -> S (q, a) r b -> S p r b
```
Although we desire the second `>>`, the type is not as simple as the `>>` function in S monad.

The problem that S monad tried to solve was the symmetry between the multi-argument and multi-return-value functions.
Indexed state monad, although solves this problem, it breaks the symmetry by specifying an element `a`
 as the single result value in the type `S p q a`.
For this reason, functions that do not require any input argument must mention a dummy argument,
 and functions that return more than one value must mention them in different places (as in `S p (q, b) c`, where `b` and `c` are return values).

### G. Using Kleisli Arrows

One generalization I found was to represent functions `x -> y` as Kliesli functions `x -> m y`.
This makes S monad a kind of monad transformer.
The following definitions reflect this change.
```hs
-- | Composition
(>>) :: Prelude.Monad m => (x -> m y) -> (y -> m z) -> (x -> m z)
f >> g = f Control.Monad.>=> g

-- | Let expression
(>>=) :: Prelude.Monad m => (x -> m (y, a)) -> (a -> (y -> m z)) -> (x -> m z)
f >>= h = \x -> Prelude.do
  (y, a) <- f x
  h a y

-- | Empty expression
nil :: Prelude.Applicative m => x -> m x
nil = Prelude.pure

-- | Lift a monadic value, run, and push it onto the stack.
lift :: Prelude.Applicative m => m a -> (x -> m (x, a))
lift ma x = Prelude.pure (\a -> (x, a)) Prelude.<*> ma

-- | Lift a pure value and push onto the stack.
pure :: Prelude.Applicative m => a -> (x -> m (x, a))
pure = lift . Prelude.pure
```

For the choice of monad `m`, using `Reader`/`Writer`/`State`
 is not exciting as the S monad provides all of them in a single notation.
We can choose `m` to be `IO` which allows stacktic to perform imperative operations.
I also consider choosing `m` to be the `Cont` monad, which I explain in a later section.

I considered generalizing Kleisli arrows to generic arrows (`Arrow arr => arr x y`).
However, there is not much to gain because of the function `S.apply` which currently has the following definition:
```hs
-- | Apply the topmost element of the stack on the stack itself.
apply :: Prelude.Monad m => (y, y -> m z) -> m z
apply = do
  f <- nil
  f
```
In order to define `apply` for the arrow generalization, the type `arr` must be an instance of `ArrowApply`,
 which means `arr` is equivalent to some Kleisli arrow and represents a monad.
So the arrow generalization only works if one wants to consider stack-oriented languages without
 the fundamental combinator `apply`.

### H. Using Continuation Monad

I did find the case for `x -> Cont y` interesting.
These are the definitions of `callCC` and `label` for the continuation monad.
```hs
callCC :: MonadCont m => ((a -> m b) -> m a) -> m a
label :: MonadCont m => a -> m (a -> m b, a)
```
I realized that simple changes to these definitions make them suitable
 for the S monad that uses a continuation monad.
These are my modified definitions.
```hs
callCC :: P.MonadCont m => ((x -> m y) -> (z -> m x)) -> (z -> m x)
callCC f z = P.callCC \h -> f h z

label :: P.MonadCont m => x -> m (x, x -> m y)
label x = (\(m, x) -> (x, m)) P.<$> P.label x
```
Using these definitions, the function `callCC` gives you a S monad expression
 and expects another S monad expression from you.
The `label` function pushes a label on top of the stack,
 which you can use to go back to, by applying the label on top of the stack.
Notice how type parameters work, and how they assure jumps only happen
 when the stack states exactly match.

This is a very simple loop implemented by `label`.
```hs
-- | A greeter who is stubborn to know your name. Uses 'Language.Stacktic.Base.label' to loop.
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
```
Before creating the label, there is a number on the stack which represents the current counter.
Although the label jumps back to where it was defined,
 the number on top of the stack is changed.
I have written more complicated examples in [Language.Stacktic.Example](src/Language/Stacktic/Example.hs).

### I. Limitations

[Cat language](https://github.com/cdiggins/cat-language) is one of the implementations of
 a statically typed stack-oriented language.
The author focuses on a language with a set of combinators, which makes it different from the S monad
 because of our use of the `let` expression.
However, he mentions the main limitation of implementing S monad in Haskell:
> Cat cannot be embedded in languages which only support rank-1 polymorphic types (e.g. Haskell, ML).
> Short version is that these languages can't properly infer the type of the expression `quote dup`.
> For more information and demonstration see [the type-inference repository](https://github.com/cdiggins/type-inference?tab=readme-ov-file#compared-to-haskell).

Although Haskell does support Rank-N types, the issue is the lack of [impredicative types](https://downloads.haskell.org/ghc/9.0.1/docs/html/users_guide/exts/impredicative_types.html#impredicative-polymorphism).
The function `quote` needs to be of type:
```hs
quote :: Monad m => a -> m (x, forall y. y -> m (y, a))
```
This is possible using the `ImpredicativeTypes` extension, but it comes with a lot of troubles.
Haskell has new plans for impredicativity, which I mention here.
- [Previous GHC page for impredicative polymorphism](https://gitlab.haskell.org/ghc/ghc/-/wikis/impredicative-polymorphism)
- [New impredicativity in GHC (June 2015)](https://gitlab.haskell.org/ghc/ghc/-/wikis/impredicative-polymorphism/impredicative-2015)
