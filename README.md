# Ratl: Resource-Aware Toy Language

Ratl is a toy: a baby language that can infer small upper bounds.

More precisely, it is a strongly-typed, monomorphic, functional language based
on lists and naturals with a resource-aware type system capable of reporting
upper bounds for functions and expressions that are linear in their inputs,
along with an accompanying interpreter.

This is based off of work done by Jan Hoffmann and others.

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

## Usage

To run ratl, supply the ratl file to analyze, the arguments to main (only
needed when `mode` is `Run`), and optionally, a maximum degree:

    $ stack exec ratl -- -h
    Usage: ratl [-d <n>] [-m <m>] filename <args>
    Resource-Aware Toy Language

      -v         --version           Version information.
      -h         --help              Print this message.
      -m MODE    --mode=MODE         One of: Analyze, Run
      -d DEGREE  --degreemax=DEGREE  Maximum degree of analysis.

## Examples

Ratl executes on a `.ratl` file, and outputs the bounds of the functions and
the result returned by `main`.  In the directory where Ratl was downloaded,
run the following commands:

    $ echo "(define main ([Nat] -> [Nat]) (xs) xs)" > id.ratl
    $ stack exec ratl id.ratl [1,2,3]
    main: 1.0
    [1,2,3]

The program faithfully returned its input.  Even better, Ratl analyzed `main`,
and decided that it would execute in constant time: 1.0!  Sounds fast.

Now, a more complex example:

    $ echo "(define main ([Nat] -> Nat) (xs) (car xs))" > car.ratl
    $ stack exec ratl car.ratl [3,2,1]
    main: 4.0
    3

With the addition of the call to `car`, the program outputs the first element
of its input, and Ratl tells us that `main` now runs in constant time of 4.0,
which seems appropriately less fast.

Let's analyze a more interesting program, shall we?

    $ echo "(define main ([Nat] -> Nat) (xs)
            (if (not (null? xs)) (+ 1 (main (cdr xs))) 0))" > length.ratl
    $ stack exec ratl length.ratl [9,8,7,6,5,4,3,2,1]
    main: 27.0*n + 21.0
    9

The program outputs the length of the list, but more importantly Ratl outputs
that the length of time required to produce that output is linear in the length
of the input!

## Syntax

The syntax of Ratl is somewhat scheme-like.  The top level is exclusively
function definitions, denoted by the keyword `define`.  Functions are typed
using arrow syntax, and must have a single argument, whose name is given in
parentheses.  The body is an expression.

Expressions include recalling a variable by name, literal values, function
application, and branching with `if`.

Functions are called in prefix position, wrapped in parentheses.

Builtin functions include arithmetic with `+` and `*`, comparison with `<`, `>`
and `=`, fetching the `car` and `cdr` of a list, prepending to a list with
`cons`, testing a list with `null?`, and negating booleans with `not`.

Literal values include the natural numbers starting at zero, booleans, and
lists.  Lists can be over naturals or booleans, or over lists, lists of lists,
etc.  Naturals are depicted in the usual way.  Lists are comma-delimited, in
square brackets.  Booleans are represented by the symbols `#t` for true and
`#f` for false.

Identifiers are made of any characters besides whitespace and `()[];,`.

E.g., if you wanted to find the length of a list:

    (define length ([Nat] -> Nat) (xs)
        (if (not (null? xs))
            (+ 1 (length (cdr xs)))
            0))

    (define main (Nat -> Nat) (n)
        (length [1, 2, 3, 4, 5, 6, 7, 8, 9]))

For more examples, take a look under the `examples/ratl` directory.

## Semantics

Ratl is almost useful.  It is a language that can only increase and compare
numbers, and create and consume lists.

All expressions return a value.  Function bodies evaluate to the function's
return value.  Looping is achieved via recursion.  The interpreter starts
executing a program by calling the function `main` with the argument passed
in on the command line.

If `car` or `cdr` is used on `[]`, the program will halt, so it is a good
idea to check a list with `null?` before using it.

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
through some user-supplied annotation metric.

Consider a few example Ratl type rules.  First, accessing a variable:

![vareq](http://latex.codecogs.com/gif.latex?%5Cfrac%7Bq%5Cgeq%20q%27%20&plus;%20K_%7Bvar%7D%7D%7B%5CSigma%3B%5CGamma%2Cx%3AB%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dq%7D_%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B.9ex%5D%7B0pt%7D%7B0pt%7Dq%27%7D%20x%3AB%7D)

Here there is nothing to consider but the cost of accessing the variable, since
it is a leaf term.

Next, applying a function:

![appeq](http://latex.codecogs.com/gif.latex?%5Cfrac%7B%5CSigma%3B%5CGamma%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dq_e%7D_%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B.9ex%5D%7B0pt%7D%7B0pt%7Dq%27_e%7D%20e%3AA%5C%20%5C%20%5C%20%5C%20%5CSigma%28f%29%20%3D%20A%5Cxrightarrow%7Bq_f/q%27_f%7DB%5C%20%5C%20%5C%20%5C%20q%5Cgeq%20q_e%20&plus;%20K_%7Barg%7D%5C%20%5C%20%5C%20%5C%20q%27_e%3Dq_f&plus;c&plus;K_%7Bapp1%7D%5C%20%5C%20%5C%20%5C%20q%27%3Dq_f%27&plus;c-K_%7Bapp2%7D%7D%7B%5CSigma%3B%5CGamma%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dq%7D_%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B.9ex%5D%7B0pt%7D%7B0pt%7Dq%27%7D%20%28f%5C%20e%29%3AB%7D)

Applying a function requires considering the cost of that function as well as
the cost of the argument; in total the expression must be more expensive than
evaluating the argument and executing the function.

Finally, the if expression:

![ifeq](http://latex.codecogs.com/gif.latex?%5Cfrac%7B%5CSigma%3B%5CGamma%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dq_p%7D_%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B.9ex%5D%7B0pt%7D%7B0pt%7Dq%27_p%7D%20e_p%3AA%5C%20%5C%20%5C%20%5C%20%5CSigma%3B%5CGamma%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dq_t%7D_%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B.9ex%5D%7B0pt%7D%7B0pt%7Dq%27_t%7D%20e_t%3AB%5C%20%5C%20%5C%20%5C%20%5CSigma%3B%5CGamma%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dq_f%7D_%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B.9ex%5D%7B0pt%7D%7B0pt%7Dq%27_f%7D%20e_f%3AB%5C%20%5C%20%5C%20%5C%20q%5Cgeq%20q_p%20&plus;%20K_%7Bifp%7D%5C%20%5C%20%5C%20%5C%20q%27_p%5Cgeq%20q_t%20&plus;%20K_%7Bift%7D%5C%20%5C%20%5C%20%5C%20q%27_p%5Cgeq%20q_f&plus;K_%7Biff%7D%5C%20%5C%20%5C%20%5C%20q%27_t%5Cgeq%20q%27&plus;K_%7Bifc%7D%5C%20%5C%20%5C%20%5C%20q%27_f%5Cgeq%20q%27&plus;K_%7Bifc%7D%7D%7B%5CSigma%3B%5CGamma%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dq%7D_%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B.9ex%5D%7B0pt%7D%7B0pt%7Dq%27%7D%20%28%5Ctextrm%7Bif%20%7D%20e_p%5C%20e_t%5C%20e_f%29%3AB%7D)

Here, the variables for the predicate and both branches are considered, and
constraints are added such that the `if` expression is guaranteed to be more
expensive than its pieces.

## The Algorithm

The goal of a linear programming problem is to optimize some objective, subject
to certain constraints.  Each of these is a linear equation of some number of
variables; the objective is a vector in the dimension of the number of
variables, while the constraints are all inequalities that can be thought of as
lines, or in higher dimensions planes or hyperplanes, that bound the problem.

As mentioned above, Ratl uses LP by constructing linear inequalities out of
resource annotations and costs.  Let's look at an example.  Consider the Ratl
program found in `./examples/ratl/sum.ratl`, reproduced below:

    (define sum ([Nat] -> Nat) (xs)
        (if (null? xs)
            0
            (+ (car xs)
               (sum (cdr xs)))))

Everything is given a resource annotation.  An annotation is made of one or
more resource variables, zero or more relationships between resource variables,
and a cost.  For this example, let's start with the definition for `sum`.
First, during parsing the type, `[Nat]`, is given a variable because it's a
list type, and `sum` itself is given variables for cost before and after
evaluation because it's a function declaration.  Then during type checking, the
abstract syntax tree of the function body is traversed, and every node in the
tree is annotated in post-order.  In the case of `sum`, that means the
expression `xs` is annotated, followed by `(null? xs)`, `0`, `xs`, `(car xs)`,
`xs`, `(cdr xs)`, `(sum ...)`, `(+ ...)`, and finally `(if ...)`.

During each annotation step, leaf expressions like `xs` receive variables *q*
and *q'* and a cost *k*.  The linear equation that results is simply
*q*≥*q'*+*k*.  Interior nodes in the syntax tree like `(car xs)` are aware of
their children's variables, and build inequalities that force them to be
related.  The more complex the relationship between nodes, the more variables
and equations are required.  For example, `if` expressions have multiple
constraints, including one that requires the `if` expression to be more
expensive than the predicate and the cost of the false branch, and another that
requires the `if` expression to be more expensive than the predicate and the
cost of the true branch.  Finally, after annotating the body, the resulting
expression and type are given equivalence relations to the function
declarations.

After annotating the entire program, the objective function is then created,
with a weight for each of the list type variables and the function variables.
To calculate a runtime upper bound, this objective should be minimized.

The end result of the annotation for this example is presented in the table
below.  The variables are by column, and the header row gives the syntax node
that corresponds to that variable.  The first row is the objective function,
and the remaining rows are the inequalities collected during type checking.
The right-most column gives the cost for that constraint.  Empty cells are
variables with zero coefficients for that constraint.

| `[Nat]` |  `sum`  |  `sum`  | `[Nat]` | `xs` | `xs` | `['a]`|`null?`|`null?`|`null?`|`null?`| `#c` |  `0` |  `0` |`[Nat]`| `xs` | `xs` |`['a]`| `car`| `car`| `car`| `car`| `#c` |`[Nat]`| `xs` | `xs` |`['a]`|`['a]`| `cdr`| `cdr`| `cdr`| `cdr`| `#c` | `sum`| `sum`| `#c` |  `+` |  `+` |  `+` |  `+` | `#c` |`['a]`|`['a]`| `if` | `if` |   |         |
| ------- | ------- | ------- | ------- | ---- | ---- | ----- | ----- | ----- | ----- | ----- | ---- | ---- | ---- | ----- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ----- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |---| ------- |
| **1.0** | **1.0** |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |   |         |
|         |         |         |         |  1.0 | -1.0 |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |  1.0  | -1.0  |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **0.0** |
|         |         |         |   1.0   |      |      | -1.0  |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **0.0** |
|         |         |         |         | -1.0 |      |       |       |       |  1.0  |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |  1.0 |       | -1.0  |       |       |       | -1.0 |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **1.0** |
|         |         |         |         |      |      |       |       |  1.0  |       | -1.0  |  1.0 |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |  1.0 | -1.0 |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |  1.0 | -1.0 |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |  1.0 | -1.0 |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **0.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |  1.0  |      |      | -1.0 |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **0.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       | -1.0 |      |      |      |      |  1.0 |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |  1.0 |      | -1.0 |      |      |      | -1.0 |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |  1.0 |      | -1.0 |  1.0 |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |  1.0 | -1.0 |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |  1.0 |      |  1.0 | -1.0 |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **0.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |  1.0 | -1.0 |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **0.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |  1.0  |      |      | -1.0 |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **0.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       | -1.0 |      |      |      |      |      |  1.0 |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |  1.0 |      |      | -1.0 |      |      |      | -1.0 |      |      |      |      |      |      |      |      |      |      |      |      | = | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |  1.0 |      | -1.0 |  1.0 |      |      |      |      |      |      |      |      |      |      |      |      | = | **1.0** |
|  -1.0   |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |  1.0 |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | = | **0.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      | -1.0 |      |      |  1.0 |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |  -1.0   |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |  1.0 |      |      |      | -1.0 |      |      |      |      |      |      |      |      |      | = | **1.0** |
|         |         |   1.0   |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      | -1.0 |  1.0 |      |      |      |      |      |      |      |      |      | = | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 | -1.0 |      |      |      |      |      |      |      | ≥ | **0.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      | -1.0 |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |  1.0 |      |       |      |      |      |      |      |      |      |      |      | -1.0 |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |  1.0 |      | -1.0 |      |      |      | -1.0 |      |      |      |      | = | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 |      | -1.0 |  1.0 |      |      |      |      | = | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 | -1.0 |      |      | ≥ | **0.0** |
|         |         |         |         |      |      |       |       |       | -1.0  |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |  1.0  |      | -1.0 |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |  1.0  |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      | -1.0 |      |      |      |      |      |      | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |  1.0 |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | -1.0 | ≥ | **1.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 |      |      |      |      | -1.0 | ≥ | **1.0** |
|         |   1.0   |         |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | -1.0 |      | = | **0.0** |
|         |         |  -1.0   |         |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 | = | **0.0** |
|   1.0   |         |         |  -1.0   |      |      |       |       |       |       |       |      |      |      |       |      |      |      |      |      |      |      |      |       |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      | -1.0 |      |      |      | = | **0.0** |
|         |         |         |         |      |      |       |       |       |       |       |      |      |      | -1.0  |      |      |      |      |      |      |      |      | -1.0  |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |      |  1.0 |      |      | = | **0.0** |

To consider the second row, this corresponds to one instance of the term `xs`
discussed previously.  It has a presumed cost of 1.0, and coefficients of 1.0
(as they occurs once in the equation).  This in turn corresponds to the
equation *q*≥*q'*+*k*, or more specifically, 1.0\**q*≥1.0\**q'*+1.0.

The optmimum determined by Ratl is given in the next table:

| `[Nat]` |  `sum`  |  `sum`  | `[Nat]` | `xs` | `xs` | `['a]`|`null?`|`null?`|`null?`|`null?`| `#c` |  `0` |  `0` |`[Nat]`| `xs` | `xs` |`['a]`| `car`| `car`| `car`| `car`| `#c` |`[Nat]`| `xs` | `xs` |`['a]`|`['a]`| `cdr`| `cdr`| `cdr`| `cdr`| `#c` | `sum`| `sum`| `#c` |  `+` |  `+` |  `+` |  `+` | `#c` |`['a]`|`['a]`| `if` | `if` |
| ------- | ------- | ------- | ------- | ---- | ---- | ----- | ----- | ----- | ----- | ----- | ---- | ---- | ---- | ----- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ----- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
|  22.0   |  16.0   |   0.0   |   0.0   | 14.0 | 13.0 |  0.0  | 12.0  | 12.0  | 15.0  | 11.0  |  0.0 | 10.0 |  9.0 |  0.0  |  8.0 |  7.0 |  0.0 |  6.0 |  6.0 |  9.0 |  5.0 |  0.0 | 22.0  |  2.0 |  1.0 | 22.0 | 22.0 |  0.0 | 22.0 |  3.0 | 21.0 |  0.0 |  4.0 |  3.0 |  4.0 |  2.0 |  2.0 | 10.0 |  1.0 |  0.0 | 22.0 | 22.0 | 16.0 |  0.0 |


The optimum values for the list type variables are the linear upper bounds,
while the optimum values for the function types are the constant factors.  This
corresponds to the reported bounds for the two functions:

    sum: 22.0*n + 16.0

For the actual implementation, this formulation is actually how the problem is
fed to the solver.  It is kept in this form in order to minimize the number of
transformations, because in Ratl's case, the problem is a minimization problem,
there are equalities, and all inequalities are greater-than-or-equal.

## Analysis

When run on the sample programs in `examples/ratl`, here are the resulting
bounds predicted:

    examples/ratl/all.ratl
    main: 29.0*n + 27.0
    all: 29.0*n + 24.0

    examples/ratl/any.ratl
    main: 29.0*n + 27.0
    any: 29.0*n + 24.0

    examples/ratl/filtzero.ratl
    main: 50.0*n + 46.0
    filtzero: 50.0*n + 43.0

    examples/ratl/id.ratl
    main: 12.0
    id_nat: 1.0
    id_list: 1.0

    examples/ratl/last.ratl
    main: 25.0*n + 26.0
    last: 25.0*n + 22.0

    examples/ratl/length.ratl
    main: 27.0*n + 24.0
    length: 27.0*n + 21.0

    examples/ratl/loop.ratl
    main: Analysis was infeasible
    loop: Analysis was infeasible

    examples/ratl/mono.ratl
    main: 112.0*n + 43.0
    mono_dec: 56.0*n + 18.0
    mono_inc: 56.0*n + 18.0

    examples/ratl/nil.ratl
    main: 5.0
    nil: 1.0

    examples/ratl/product.ratl
    main: 22.0*n + 19.0
    product: 22.0*n + 16.0

    examples/ratl/sum.ratl
    main: 22.0*n + 19.0
    sum: 22.0*n + 16.0

    examples/ratl/zero.ratl
    main: 1.0

Most of these seem plausible.  `all` and `any` are virtually the same program,
and their bounds are identical.  The same is true of `sum` and `product`.  As
expected, `id` is constant time.  The program `loop` does not terminate, so
Ratl is unsurprisingly unable to produce an upper bound.

Performance-wise, Ratl is bound quadratically by the program size.  This makes
sense, as Simplex runs in linear time for most problems, and the problem size
in Ratl grows quadratically with the number of variables, which is within a
constant factor of the number of nodes in the abstract syntax tree.  To verify
that, I collected performance data on Ratl in use analyzing 500 functions,
using an Ubuntu Linux system with an AMD Ryzen 5 1600 and 32GB of RAM.  These
were broken down in groups by the number of functions in the module in
question, and run some number of times with the minimal possible input, so as
to not impact the analysis time by including the cost of actually executing the
Ratl program.  The programs include the `zero` program, then `sum` program with
empty input, and all of the functions from the examples combined into one
program, along with a `main`, which I call "everything" below.  I then repeated
this program 10 times for 100 functions, and 5 more for 500 functions in a
single program.  The user and sys times are the total time reported by the
Linux `time` utility.

|   Program  | Executions | Functions |  User  |   Sys  |
| ---------- | ---------- | --------- | ------ | ------ |
|       zero |        500 |         1 | 1.032s | 1.428s |
|        sum |        250 |         2 | 0.628s | 0.944s |
| everything |         50 |        10 | 0.252s | 0.208s |
| everything |          5 |       100 | 1.928s | 0.128s |
| everything |          1 |       500 | 9.224s | 0.592s |

This supports the assertion that after getting past the overhead costs of
startup, Ratl's analysis is approximately quadratically upper bounded by the
program size.

Comparing the performance of Simplex and Megiddo's algorithm requires linking
Ratl using MegiddoLP.  It also requires only considering very simple programs
(see "Limits" below).  Therefore, I only consider the `zero` program, analyzed
and executed 10000 times.

|   Algorithm   | Executions |   User  |   Sys   |
| ------------- | ---------- | ------- | ------- |
| Clp (Simplex) |      10000 | 20.880s | 30.056s |
| MegiddoLP     |      10000 |  8.988s | 29.260s |

At least for the tiny 2D problems presented by Ratl, MegiddoLP appears to be
less than half the overhead of Clp.  It's impossible to know how well this
would scale for Ratl, since MegiddoLP doesn't scale.

## Prior Work

Ratl is based on the ideas behind the Raml project from Jan Hoffmann, et. all.
Raml is much more powerful than Ratl.  Aside from being a language capable of
feats such as subtraction, Raml's analysis can decide polynomial upper bounds,
not just linear.  It can also decide multi-variate bounds, for example, taking
the outer product of two vectors of potentially different lengths.

Hoffmann covers these topics systematically and rigorously from the ground up
in his Ph.D. thesis<sup id="thesismention">[1](#thesisfootnote)</sup>, where he
lays down the complete conceptual background, then produces the semantics and
type rules for linear potential, followed by polynomial, followed by
multivariate.  These analyses are progressively more powerful, but that power
comes at the cost that the problem size increases exponentially.

For polynomial bounds, each type and expression is annotated with variables for
each degree.  The analysis accounts for each of these variables by representing
them as binomial coefficients.  Each binomial's lower index corresponds to its
polynomial degree, produced by the binomial's expansion.  Unlike the polynomial
however, a binomial has a natural mapping to recursive data structures: the
successive position in that data structure.  If this expandeds to coefficients
for each combination of multiple variables, this will produce multivariate
bounds.

Hoffmann has continued this work with Raml, which is growing to include an
increasing share of OCaml<sup id="papermention">[2](#paperfootnote)</sup>.

## Limits

Ratl requires the LP solver to be able to handle problems of an arbitrary
number of dimensions.  When I first started working on this problem, I had
misread a key observation from Hoffmann's thesis that stated that when deciding
polynomial bounds each constraint involves at most three variables.  I took
this to mean that certain programs would be tractable with two variables.
This was my inspiration for writing MegiddoLP - I thought I could have an
end-to-end system for some limited number of problems, and compare and contrast
Megiddo's algorithm with Simplex.  Not so.  The implementation of MegiddoLP
is two-dimensional, and is limited to programs with one function of type
`Nat -> Nat` that either return their input or a natural literal, i.e.,

    (define main (Nat -> Nat) (args)
        args)

Computing literally anything immediately makes the program larger dimensional.
Because Megiddo's algorithm is fixed in dimension, this means that even
expanding the implementation to higher dimensions would eventually hit a wall.
Thus, to do anything useful, Ratl requires the use of other LP algorithms such
as Simplex, which is what Coin-Or Clp uses.

## Caveats

Ratl is not appropriate, suitable, or fit for (practically) any purpose.

Ratl is also not defect-free. There are a few bugs in it.  For example, if a
function unconditionally recurses on the cdr of its input, it loops instead of
halting.  It also derives incorrect resource usage for literals.  If you name
two functions the same, the analysis of their callers will probably be wrong.

Ratl analysis gets quadratically larger with the size of the abstract syntax
tree of the program it's told to analyze.  Much more than 500 modestly-sized
functions is likely to run out of memory.

## Beyond Ratl: Next Steps

Ratl was an unexpected pitstop along the way to understanding Hoffmann's work.
My original intent was to do a limited analysis on the Haskell programming
language itself.  While trying to understand how the problem could be solved
using linear programming, I found it simpler to have complete control over the
language.  Thus, Ratl was born.

This work will continue where it started: by returning to Haskell, in the form
of a library and compiler plugin.

That doesn't mean that Ratl is finished, though.  It will continue to be a
sandbox for me to mature these concepts.  At a minimum, there are a couple bugs
that need to be fixed before I can be confident in targeting Haskell.  At a
maximum, I may choose to go the distance and allow for analyses of returnable
resources like memory, as well as fully implement polynomial and multivariate
analyses in Ratl, in which case the baby will be all grown up.

## References

<b id="thesisfootnote">1</b> Hoffmann, J. 2011. Types with potential:
Polynomial resource bounds via automatic amortized analysis. Ph.D. thesis,
Ludwig-Maximilians-Universität, München, Germany.[↩](#thesismention)

<b id="paperfootnote">2</b> Jan Hoffmann, Ankush Das, and Shu-Chun Weng. 2017.
Towards automatic resource bound analysis for OCaml. *SIGPLAN Not.* 52, 1
(January 2017), 359-373.  DOI: https://doi.org/10.1145/3093333.3009842
[↩](#papermention)
