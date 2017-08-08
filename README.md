# Ratl: Resource-Aware Toy Language

Ratl is a toy: a baby language that can infer small upper bounds.

More precisely, it is a strongly-typed language based on lists and naturals
with a resource-aware type system capable of reporting upper bounds for
functions and expressions that are linear in their inputs, along with an
accompanying interpreter.

This is based off of work done by Jan Hoffmann.

## Syntax

The syntax of Ratl is somewhat scheme-like.  The top level is exclusively
function definitions, denoted by the keyword `fn`.  Functions are typed using
arrow syntax, and must have a single argument, whose name is given in
parentheses.  The body is an expression, which evaluates to the function's
return value.

Expressions include addition with `+`, fetching the `head` and `tail` of a
list, recalling a variable by name, literal values, testing truthiness with
`if`, and function application.  Looping is achieved via recursion.

Functions and builtins (`+`, `head`, `tail`, and `if`) are called in prefix
position, wrapped in parentheses.

Literal values include the natural numbers starting at zero, and lists.  Lists
can be over the naturals, or over lists of naturals, etc.  Naturals are
depicted in the usual way.  Lists are comma-delimited, in square brackets.

Identifiers are made of lower-case letters and underscores.

E.g., if you wanted to find the length of a list:

~~~~
(fn length ([Nat] -> Nat) (xs)
    (if xs
        (+ 1 (length (tail xs)))
        0))

(fn main (Nat -> Nat) (n)
    (length [1, 2, 3, 4, 5, 6, 7, 8, 9]))
~~~~

For more examples, take a look under the `examples/ratl` directory.

## Resource-Aware Type Theory

Expressions, functions, and list types each carry some resource variable.  Each
expression's type is associated with some cost relationship.  These combine to
form constraints, which are accumulated algorithmically across the type rules
and that along with an objective function constructed from the function types
form a linear program that can be submitted to an off-the-shelf LP solver.  The
solution produced maps to the function's resource upper bound.

The resource of interest in Ratl is execution time, but the concept can be
mapped to other resources such as space or even arbitrary resources measured
through some user-supplied annotation.

## Building

Ratl depends on Stack and the Haskell platform.

On Ubuntu, these can be installed with:
~~~~
apt-get install haskell-stack
~~~~

Ratl also depends on an LP library, Coin-Or Clp:
~~~~
apt-get install coinor-clp coinor-libclp-dev
~~~~

Then to build, just run:
~~~~
stack build
~~~~

Ratl can then be run:
~~~~
stack exec ratl
~~~~
