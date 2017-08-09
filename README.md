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

## The Algorithm

The goal of a linear programming problem is to optimize some objective, subject
to certain constraints.  Each of these is a linear equation of some number of
variables; the objective is a vector in the dimension of the number of
variables, while the constraints are all inequalities that can be thought of as
lines, or in higher dimensions planes or hyperplanes, that bound the problem.

As mentioned above, Ratl uses LP by constructing linear inequalities out of
resource annotations and costs.  Let's look at an example.  Consider the Ratl
program found in `./examples/ratl/sum.ratl`, reproduced below:

    (fn sum ([Nat] -> Nat) (vals)
        (if vals
            (+ (head vals)
               (sum (tail vals)))
            0))
    
    (fn main ([Nat] -> Nat) (args)
        (sum args))

Everything is given a resource annotation.  An annotation is made of one or
more resource variables, zero or more relationships between resource variables,
and a cost.  For this example, let's start with `main` (Ratl itself actually
starts by annotating `sum`).  First, during parsing the type, `[Nat]`, is given
a variable because it's a list type, and `main` itself is given a variable
because it's a function declaration.  Then during type checking, the abstract
syntax tree of the function body is traversed, and every node in the tree is
annotated in post-order.  In the case of `main`, that means the expression
`args` is annotated, followed by `(sum args)`.  During each annotation step,
leaf expressions like `args` receive a variable *q* and a cost *k*, but no
resource relationships.  The linear equation that results is simply *q*≥*k*.
Interior nodes in the syntax tree like `(sum args)` are aware of their
children's variables, and build inequalities that force them to be related.
The linear equation that results is *q*≥*p*+*k*, where *q* is the parent node's
resource variable, and *p* is the child node's resource variable.  The more
complex the relationship between nodes, the more variables and equations are
required.  For example, `if` expressions have two constraints: one that
requires the `if` expression to be more expensive than the predicate and the
cost of the false branch, and another that requires the `if` expression to be
more expensive than the predicate and the cost of the true branch.  Finally,
after annotating the body, the resulting expression and type are given
equivalence relations to the function declarations (these are not strictly
necessary, but are helpful in decoupling typing and solving).

After annotating the entire program, the objective function is then created,
with a weight for each of the list type variables and the function variables.
To calculate a runtime upper bound, this objective should be minimized.

The end result of the annotation for this example is presented in the table
below.  The variables are by column, and the header row gives the syntax node
that corresponds to that variable ("app" refers to function application - calls
to the `sum` function: the first recursive, the second from inside `main`).
The first row is the objective function, and the remaining rows are the
inequalities collected during type checking.  The right-most column gives the
cost for that constraint.  Empty cells are variables with zero coefficients for
that constraint.

|   `[Nat]`  |  `sum`   |   `[Nat]`  |  `main`  |`vals`|`vals`|`head`|`head`|`vals`|`tail`|`tail`| app  |  `+` |  `0` | `if` |`args`| app  |   |          |
| ---------- | -------- | ---------- | -------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |---| -------- |
| **1000.0** |  **1.0** | **1000.0** |  **1.0** |      |      |      |      |      |      |      |      |      |      |      |      |      |   |          |
|            |    1.0   |            |          |      |      |      |      |      |      |      |      |      |      | -1.0 |      |      | ≥ |  **0.0** |
|            |   -1.0   |            |          |      |      |      |      |      |      |      |      |      |      |  1.0 |      |      | ≥ |  **0.0** |
|            |          |            |          | -1.0 |      |      |      |      |      |      |      | -1.0 |      |  1.0 |      |      | ≥ |  **2.0** |
|            |          |            |          | -1.0 |      |      |      |      |      |      |      |      | -1.0 |  1.0 |      |      | ≥ |  **2.0** |
|            |          |            |          |  1.0 |      |      |      |      |      |      |      |      |      |      |      |      | ≥ |  **1.0** |
|            |          |            |          |      |      | -1.0 |  1.0 |      |      |      | -1.0 |  1.0 |      |      |      |      | ≥ |  **1.0** |
|     1.0    |          |            |          |      | -1.0 |  1.0 | -1.0 |      |      |      |      |      |      |      |      |      | ≥ |  **1.0** |
|            |          |            |          |      |  1.0 |      |      |      |      |      |      |      |      |      |      |      | ≥ |  **1.0** |
|            |   -1.0   |            |          |      |      |      |      |      | -1.0 |  1.0 |  1.0 |      |      |      |      |      | ≥ |  **2.0** |
|     1.0    |          |            |          |      |      |      |      | -1.0 |  1.0 | -1.0 |      |      |      |      |      |      | ≥ |  **1.0** |
|            |          |            |          |      |      |      |      |  1.0 |      |      |      |      |      |      |      |      | ≥ |  **1.0** |
|            |          |            |          |      |      |      |      |      |      |      |      |      |  1.0 |      |      |      | ≥ |  **1.0** |
|            |          |            |    1.0   |      |      |      |      |      |      |      |      |      |      |      |      | -1.0 | ≥ |  **0.0** |
|            |          |            |   -1.0   |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 | ≥ |  **0.0** |
|            |   -1.0   |            |          |      |      |      |      |      |      |      |      |      |      |      | -1.0 |  1.0 | ≥ |  **2.0** |
|    -1.0    |          |     1.0    |          |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ |  **0.0** |
|     1.0    |          |    -1.0    |          |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ |  **0.0** |
|            |          |            |          |      |      |      |      |      |      |      |      |      |      |      |  1.0 |      | ≥ |  **1.0** |

To consider the last row, this corresponds to the term `args` discussed
previously.  It has a presumed cost of 1.0, and a coefficient of 1.0 (as it
occurs once in the equation).  This in turn corresponds to the equation
*q*≥*k*, or more specifically, 1.0\**q*≥1.0.

The optmimum determined by Ratl is given in the next table:

|   `[Nat]`  |  `sum`   |   `[Nat]`  |  `main`  |`vals`|`vals`|`head`|`head`|`vals`|`tail`|`tail`| app  |  `+` |  `0` | `if` |`args`| app  |
| ---------- | -------- | ---------- | -------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
|     5.0    |    4.0   |     5.0    |    7.0   |  1.0 |  1.0 |  0.0 |  3.0 |  1.0 |  0.0 |  3.0 |  3.0 |  1.0 |  1.0 |  4.0 |  1.0 |  7.0 |

The optimum values for the list type variables are the linear upper bounds,
while the optimum values for the function types are the constant factors.  This
corresponds to the reported bounds for the two functions:

    sum: 5.0*n + 4.0
    main: 5.0*n + 7.0

For the actual implementation, this formulation is actually not how the problem
is fed to the solver.  Instead it is put into standard form, which requires a
number of transformations.  Standard form requires the problem to be a
minimization problem, with only inequalities (no equalities), have inequalities
all be less-than-or-equal, and for all variables to have non-negativity
constraints.

In Ratl's case, the problem is a minimization problem, there are equalities,
all inequalities are greater-than-or-equal, and some variables don't have
non-negativity constraints.  To transform, the objective is negated and
maximized, and the constraints are all made into less-than-or-equal constraints
by negating them.  Equalities are made into inequalities by negating a
duplicate constraint.  Non-negativity constraints are added by splitting
variables that can range negatively in two in every constraint.



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
