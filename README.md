# Ratl: Resource-Aware Toy Language

Ratl is a toy: a baby language that can infer small upper bounds.

More precisely, it is a strongly-typed language based on lists and naturals
with a resource-aware type system capable of reporting upper bounds for
functions and expressions that are linear in their inputs, along with an
accompanying interpreter.

This is based off of work done by Jan Hoffmann and others.

## Examples

Ratl executes on a `.ratl` file, and outputs the bounds of the functions and
the result returned by `main`.  In the directory where Ratl was downloaded,
run the following commands:

    $ echo "(fn main ([Nat] -> [Nat]) (args) args)" > id.ratl
    $ ./ratl id.ratl [1,2,3]
    Ratl: Resource-aware toy language
    Using Clp version 1.15.10: (1,15,10)
    main: 1.0
    [1,2,3]

Ratl printed out a friendly autobiography.  The program faithfully returned its
input.  Even better, Ratl analyzed `main`, and decided that it would execute in
constant time: 1.0!  Sounds fast.

Now, a more complex example (I will skip the autobiography from now on):

    $ echo "(fn main ([Nat] -> Nat) (args) (head args))" > head.ratl
    $ ./ratl head.ratl [3,2,1]
    main: 2.0
    3

With the addition of the call to `head`, the program outputs the first element
of its input, and Ratl tells us that `main` now runs in constant time of 2.0,
which seems appropriately less fast.

Let's analyze a more interesting program, shall we?

    $ echo "(fn main ([Nat] -> Nat) (args)
                (if args (+ 1 (main (tail args))) 0))" > length.ratl
    $ ./ratl length.ratl [9,8,7,6,5,4,3,2,1]
    main: 9.0*n + 5.0
    9

The program outputs the length of the list, but more importantly Ratl outputs
that the length of time required to produce that output is linear in the length
of the input!

## Syntax

The syntax of Ratl is somewhat scheme-like.  The top level is exclusively
function definitions, denoted by the keyword `fn`.  Functions are typed using
arrow syntax, and must have a single argument, whose name is given in
parentheses.  The body is an expression.

Expressions include addition with `+`, fetching the `head` and `tail` of a
list, recalling a variable by name, literal values, testing truthiness with
`if`, and function application.

Functions and builtins (`+`, `head`, `tail`, and `if`) are called in prefix
position, wrapped in parentheses.

Literal values include the natural numbers starting at zero, and lists.  Lists
can be over the naturals, or over lists of naturals, etc.  Naturals are
depicted in the usual way.  Lists are comma-delimited, in square brackets.

Identifiers are made of lower-case letters and underscores.

E.g., if you wanted to find the length of a list:

    (fn length ([Nat] -> Nat) (xs)
        (if xs
            (+ 1 (length (tail xs)))
            0))

    (fn main (Nat -> Nat) (n)
        (length [1, 2, 3, 4, 5, 6, 7, 8, 9]))

For more examples, take a look under the `examples/ratl` directory.

## Semantics

Ratl is almost useless.  It is a language that can only increase numbers and
decrease lists.  There are no conjunctions or disjunctions.  It can't even
compare values, so predecessor can't be defined.

All expressions return a value.  Function bodies evaluate to the function's
return value.  Looping is achieved via recursion.  The interpreter starts
executing a program by calling the function `main` with the argument passed
in on the command line.

If `head` or `tail` is used on `[]`, the program will halt, so it is a good
idea to check the truthiness of a list with `if` before using it.

## Resource-Aware Type Theory

Ratl's usefulness lies not in its semantics but its type system.

Expressions, functions, and list types each carry some resource variable.  Each
expression's type is associated with some cost relationship.  These combine to
form constraints, which are accumulated algorithmically across the type rules
and that along with an objective function constructed from the function types
form a linear program that can be submitted to an off-the-shelf LP solver.  The
solution produced maps to the function's resource upper bound.

The resource of interest in Ratl is execution time, but the concept can be
mapped to other resources such as space or even arbitrary resources measured
through some user-supplied annotation.

## Caveats

Ratl is not appropriate, suitable, or fit for (practically) any purpose.

Ratl is also not defect-free. There are a few bugs in it.  For example, if a
function unconditionally recurses on the tail of its input, it loops instead of
halting.  If you feed it super-linear programs, Ratl may give confusing and
incorrect answers rather than deciding that the analysis is infeasible.  It
also derives incorrect resource usage for literals.  If you name two functions
the same, the analysis of their callers will probably be wrong.

Ratl analysis gets quadratically larger with the size of the abstract syntax
tree of the program it's told to analyze.  Much more than 500 modestly-sized
functions is likely to run out of memory.

## Building

Ratl depends on Stack and the Haskell platform.

On Ubuntu, these can be installed with:

    apt-get install haskell-stack

Ratl also depends on an LP library, Coin-Or Clp:

    apt-get install coinor-clp coinor-libclp-dev

Then to build, just run:

    stack build

Ratl can then be run:

    stack exec ratl examples/ratl/sum.ratl [1, 2, 3, 4, 5]
