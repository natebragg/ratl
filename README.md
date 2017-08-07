# Ratl: Resource-Aware Toy Language

Ratl is a toy: a baby language that can infer small upper bounds.

More precisely, it is a strongly-typed language based on lists and naturals
with a resource-aware type system capable of reporting upper bounds for
functions and expressions that are linear in their inputs, along with an
accompanying interpreter.

This is based off of work done by Jan Hoffmann.

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
