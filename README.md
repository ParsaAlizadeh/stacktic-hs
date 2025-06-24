# Stacktic-hs

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
We also considered adding Quality of Life features, but as we will see, they turned out to be an essential part of the language.
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
Type (as we will define later) of expression `T` and `F` must match,
 and we see here that both of them produce a single integer.
Some of the curly braces used in this example are purely decorative
such as the ones used in `{ n 1 <= }` and `{ n 1 - fib }`;
The syntax can be written to look like a postfix S-expression.

### B. Type System

The idea of Stacktic was to assign a type to each expression.
Each type looks like this:
$$
[ A_1 , A_2 , \ldots , A_n ] \to [ B_1 , B_2 , \ldots , B_m ]
$$
Meaning, if the stack had elements $A_1 , \ldots , A_n$ on top ($A_n$ being the top most element),
 then by executing the given expression on the stack,
 it removes the $n$ element with types $A_n, \ldots, A_2, A_1$ from the stack
 and pushes $m$ element with types $B_1, B_2, \ldots, B_m$ onto the stack
 ($B_m$ being the topmost element after the execution of the expression).
(We later add parametric polymorphism, but for simplicity here we assume a monomorphic function)

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
These two rules, which we call "chain" and "parallel" form the basic
 rules of this type system.
Other extensions, such as `then ... else ...` consuming a boolean value,
 are straightforward.

We can add parametric polymorphism by considering expression types to have the form:
$$
\forall \alpha_1,\ldots,\alpha_k \quad [A_1, \ldots, A_n] \to [B_1, \ldots, B_m]
$$
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
One might suggest that we can have the generic `b <-` expression with nothing on the right-hand side of the arrow,
 but we are limited to what we have in the do-notation.

Remember that S monad is a different type of monad.
As we will see, it is closely related to the state monad and indexed monads,
 but there are differences that we will explain.
For now, we know that any Stacktic program can be translated
 into a program in Haskell using this S monad.

### ?. Relation To State Monad

### ?. Relation To Indexed Monads

### ?. Using Kleisli Arrows

### ?. Using Continuation Monad

