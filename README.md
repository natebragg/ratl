# Ratl: Resource-Aware Toy Language

Ratl is a toy language that can infer polynomial upper bounds.

More precisely, it is a strongly-typed, polymorphic, functional language based
on lists and integers with a resource-aware type system capable of reporting
upper bounds for functions and expressions that are multi-variate polynomials of
their inputs, along with an accompanying interpreter.

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

    stack exec ratl examples/ratl/sum.ratl [1 2 3 4 5]

## Usage

To run ratl, supply the ratl file to analyze, the arguments to main (only
needed when `mode` is `Run`), and optionally, a maximum degree:

    $ stack exec ratl -- -h
    Usage: ratl [-d <n>] [-m <m>] [-e] [-g <g>] filename <args>
    Resource-Aware Toy Language

    -v          --version           Version information.
    -h          --help              Print this message.
    -m MODE     --mode=MODE         One of: Analyze, Run, Check
    -c COMMAND  --command=COMMAND   Specify the command to execute.  Default is "(main <args>)".
    -n NAME     --name=NAME         Restrict analysis to supplied names.  Default is all top-level names in the file.  Special name "initial basis" analyzes same.
    -e          --explicit          Always include explanation of bounds.
    -g DEBUG    --debug=DEBUG       Print debugging info; one of: Grid, Compact, Eqns
    -d DEGREE   --degreemax=DEGREE  Maximum degree of analysis.

## Examples

Ratl executes on a `.ratl` file, and outputs the bounds of the functions and
the result returned by `main`.  In the directory where Ratl was downloaded,
run the following commands:

    $ echo "(define main ([list 'a] -> [list 'a]) (xs) xs)" > id.ratl
    $ stack exec ratl id.ratl [1 2 3]
    main: 1
    (1 2 3)

The program faithfully returned its input.  Even better, Ratl analyzed `main`,
and decided that it would execute in constant time: 1!  Sounds fast.

Now, a more complex example:

    $ echo "(define main ([list 'a] -> 'a) (xs) (car xs))" > car.ratl
    $ stack exec ratl car.ratl [3 2 1]
    main: 4
    3

With the addition of the call to `car`, the program outputs the first element
of its input, and Ratl tells us that `main` now runs in constant time of 4,
which seems appropriately less fast.

Let's analyze a more interesting program, shall we?

    $ echo "(define main ([list 'a] -> int) (xs)
            (if (not (null? xs)) (+ 1 (main (cdr xs))) 0))" > length.ratl
    $ stack exec ratl length.ratl [9 8 7 6 5 4 3 2 1]
    main: 27*n + 19
    9

The program outputs the length of the list, but more importantly Ratl outputs
that the length of time required to produce that output is linear in the length
of the input!

## Syntax

The syntax of Ratl is modeled after nano-ML from Norman Ramsey's forthcoming
textbook ["Programming Languages: Build, Prove, and Compare,"][1] with a few
tweaks and restrictions.  The top level is exclusively function definitions,
denoted by the keyword `define`.  Functions are typed using arrow syntax after
the name, and may have one or more formal parameters, whose names are given in
parentheses.  The body is an expression.

[1]: https://www.cs.tufts.edu/~nr/build-prove-compare

Expressions include recalling a variable by name, literal values, function
application, local variables with `let`, and branching with `if`.

Functions are called in prefix position, wrapped in parentheses.

Primitive functions include arithmetic with `+`, `-`, `*`, and `/`, comparison
with `<`, `>` and `=`, fetching the `car` and `cdr` of a list, prepending to a
list with `cons`, testing a list with `null?`, fetching the `fst` and `snd` of
a `pair`, and raising an `error`.  Notably, the result of subtraction cannot be
negative; rather than producing an error, it is bounded below by zero.

The basis supplies `bind`, `isbound?`, `find`, `caar`, `cadr`, `cdar`,
`length`, `and`, `or`, `not`, `append`, `revapp`, `reverse`, `<=`, `>=`, `!=`,
`max`, `min`, `mod`, `gcd`, `lcm`, `list1`, `list2`, `list3`, `list4`, `list5`,
`list6`, `list7`, `list8`, each with the usual meaning.

Literal values include the integers starting at zero, booleans, symbols, pairs,
and lists.  Pairs and lists can be over integers or booleans, or over pairs or
lists, lists of lists, etc.  Integers are depicted in the usual way.  Booleans
are represented by the symbols `#t` for true and `#f` for false.  Symbols are
represented by any characters besides whitespace and `()[];`.  Lists are
space-delimited, in square brackets or parentheses.  Symbols, pairs and lists
must be prefixed by a quote mark, or be embedded in a `(quote ...)` expression.
Literal pairs are input with a period in the second-to-last position, as in
`(1 . 2)`.  *Note* that if the value after the period is nil, this will be
interpreted as a list.

Identifiers are made of the same characters as symbols.

E.g., if you wanted to find the length of a list:

    (define length ([list 'a] -> int) (xs)
        (if (not (null? xs))
            (+ 1 (length (cdr xs)))
            0))

    (define main ('a -> int) (_)
        (length '[1 2 3 4 5 6 7 8 9]))

For more examples, take a look under the `examples/ratl` directory.

## Semantics

Ratl is almost useful.  It is a language that can mostly be used to create and
consume lists and perform arithmetic.

All expressions return a value.  Function bodies evaluate to the function's
return value.  Looping is achieved via recursion.  The interpreter starts
executing a program by calling the function `main` with the argument passed
in on the command line.

If `car` or `cdr` is used on `[]`, the program will halt, so it is a good
idea to check a list with `null?` before using it.

## Resource-Aware Type Theory

Ratl's usefulness lies not in its semantics but its type system.

Types, and by extension type environments, are associated with a set of
resource "indices" (theoretically infinite, but bounded in practice by the
maximum degree of analysis), each of which corresponds to some resource
variable.  These variables are combined to form constraints, which accumulate
algorithmically across the type rules for different syntactic forms,
and that along with an objective function constructed from the function types
form a linear program that can be submitted to an off-the-shelf LP solver.  The
solution produced maps to the function's resource upper bound.

The resource of interest in Ratl is execution time, but the concept can be
mapped to other resources such as space or even arbitrary resources measured
through some user-supplied annotation metric.

Consider a few example Ratl type rules.  First, accessing a variable:

![vareq](http://latex.codecogs.com/gif.latex?%5Cfrac%7B%20%5Cbegin%7Bmatrix%7D%20Q%20%3D%20%5Coperatorname%7BfreshCost%7D%28%5C%7B%5Ctau%5C%7D%29%20%5C%5C%20Q%27%20%3D%20%5Coperatorname%7BfreshCost%7D%28%5Ctau%29%20%5C%5C%20Q%20-%20%5Coperatorname%7Bpack%7D%28Q%27%29%20%3D%20K_%7Bvar%7D%20%5Cend%7Bmatrix%7D%20%7D%20%7B%5C%7Bx%20%3A%20%5Ctau%5C%7D%3B%20Q%20%5Cvdash%20%5Ctextsc%7BVAR%7D%28x%29%20%3A%20%5Ctau%3B%20Q%27%7D)

Here there is nothing to consider but the cost of accessing the variable, since
it is a leaf term.  Fresh cost environments are created to track this relation,
and since the variable *x* is free, it is included as part of that upper bound.

Other syntactic form have other requirements.  For example, consider functions.
Applying a function requires considering the cost of that function as well as
the cost of the arguments; in total the expression must be more expensive than
evaluating the arguments and executing the function.

The simplest example of most of these requirements is the if expression:

![ifeqtop](http://latex.codecogs.com/gif.latex?%5Cbegin%7Bmatrix%7D%20%5CGamma_t%3B%20Q_t%20%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dk%7D%20e_t%20%3A%20%5Ctau_c%3B%20Q%27_t%20%26%20%5CGamma_f%3B%20Q_f%20%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dk%7D%20e_f%20%3A%20%5Ctau_c%3B%20Q%27_f%20%26%20%5CGamma_c%20%3D%20%5CGamma_t%20%5Ccup%20%5CGamma_f%20%5Cend%7Bmatrix%7D%20%5C%5C%20%5Cbegin%7Bmatrix%7D%20Q_c%20%3D%20%5Coperatorname%7BfreshCost%7D%28%5Coperatorname%7Brange%7D%20%5CGamma_c%29%20%26%20Q%27_c%20%3D%20%5Coperatorname%7BfreshCost%7D%28%5Ctau_c%29%20%5C%5C%20%5Cpi_%7B%5Coperatorname%7Bzero%7D%28%5Coperatorname%7Brange%7D%20%5CGamma_c%20%5Csetminus%20%5CGamma_t%29%7D%5E%7B%5CGamma_t%7D%28%5CGamma_c%3B%20Q_c%29%20-%20Q_t%20%5Cgeq%20K_%7Biftrue%7D%20%26%20Q%27_t%20-%20Q%27_c%20%5Cgeq%20K%27_%7Biftrue%7D%20%5C%5C%20%5Cpi_%7B%5Coperatorname%7Bzero%7D%28%5Coperatorname%7Brange%7D%20%5CGamma_c%20%5Csetminus%20%5CGamma_f%29%7D%5E%7B%5CGamma_f%7D%28%5CGamma_c%3B%20Q_c%29%20-%20Q_f%20%5Cgeq%20K_%7Biffalse%7D%20%26%20Q%27_f%20-%20Q%27_c%20%5Cgeq%20K%27_%7Biffalse%7D%20%5Cend%7Bmatrix%7D)
![ifeqbot](http://latex.codecogs.com/gif.latex?%5Cfrac%7B%20%5Cbegin%7Bmatrix%7D%20%5Cbegin%7Bmatrix%7D%20%5CGamma_%7Bp_0%7D%3B%20Q_%7Bp_0%7D%20%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dk%7D%20e_p%20%3A%20%5Ctau_p%3B%20Q%27_%7Bp_0%7D%20%26%20%5CGamma_p%20%3D%20%5CGamma_%7Bp_0%7D%20&plus;%20%5CGamma_c%20%26%20Q_p%20%3D%20%5Coperatorname%7BfreshCost%7D%28%5Coperatorname%7Brange%7D%20%5CGamma_p%29%20%5C%5C%20%5Cend%7Bmatrix%7D%20%5C%5C%20%5Cpi_%7B%5Coperatorname%7Bzero%7D%28%5Coperatorname%7Brange%7D%20%5CGamma_c%29%7D%5E%7B%5CGamma_%7Bp_0%7D%7D%28%5CGamma_p%3B%20Q_p%29%20-%20Q_%7Bp_0%7D%20%3D%20K_%7Bifpred%7D%20%5C%5C%20%5Coperatorname%7Bzero%7D%28Q%27_%7Bp_0%7D%29%20-%20%5Coperatorname%7Bzero%7D%28Q_c%29%20%3D%20K%27_%7Bifpred%7D%20%5C%5C%20%5Cbegin%7Bmatrix%7D%20%5Cforall%20j%2C%20%5Coperatorname%7Bdeg%7D%28j%29%20%5Cneq%200%5Cin%20%5Cmathcal%7BI%7D%28%5Coperatorname%7Brange%7D%20%5CGamma_c%29%20%5Cldotp%20%26%5CGamma_%7Bp_j%7D%3B%20Q_%7Bp_j%7D%20%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7D%5Coperatorname%7Bcf%7D%28k%20-%20%5Coperatorname%7Bdeg%7D%28j%29%29%7D%20e_p%20%3A%20%5Ctau_p%3B%20Q%27_%7Bp_j%7D%20%5C%5C%20%26%5Cpi_%7Bj%7D%5E%7B%5CGamma_%7Bp_j%7D%7D%28%5CGamma_p%3B%20Q_p%29%20-%20Q_%7Bp_j%7D%20%3D%200%20%5C%5C%20%5Cend%7Bmatrix%7D%20%5C%5C%20%5CGamma%3B%20Q%20%3D%20%5Ccurlyvee%20%5CGamma_p%3B%20Q_p%20%5Cend%7Bmatrix%7D%20%7D%7B%5CGamma%3B%20Q%20%5Cvdash%5E%7B%5Cmkern-10mu%5Cscriptscriptstyle%5Crule%5B-.9ex%5D%7B0pt%7D%7B0pt%7Dk%7D%20%5Ctextsc%7BIF%7D%28e_p%2C%20e_t%2C%20e_f%29%20%3A%20%5Ctau%3B%20Q%27%7D)

Here, both the true and false branches are considered.  To get the upper bound,
constraints are added such that the `if` expression is guaranteed to be more
expensive than either the consequent or alternative, in addition to the
predicate.  Without exhaustively describing the mechanics, the costs are
plumbed through, connecting like cost environments.  Of note is that the
predicate must be analyzed multiple times: once for each index from the
subsequent code.  This allows each possible potential for those indices to
translate across the predicate.  Finally, the sharing operation is invoked to
eliminate duplicate free variables collected from each subexpression.

## The Algorithm

The goal of a linear programming problem is to optimize some objective, subject
to certain constraints.  Each of these is a linear equation of some number of
variables; the objective is a vector in the dimension of the number of
variables, while the constraints are all inequalities that can be thought of as
lines, or in higher dimensions planes or hyperplanes, that bound the problem.

As mentioned above, Ratl uses LP by constructing linear inequalities out of
resource annotations and costs.  Let's look at an example.  Consider the Ratl
program found in `./examples/ratl/sum.ratl`, reproduced below:

    (define sum ([list int] -> int) (xs)
        (if (null? xs)
            0
            (+ (car xs)
               (sum (cdr xs)))))

The type of each result value and free variable is given a resource annotation.
An annotation is made of one or more resource variables, which can then be
combined in cost relationships between resource variables induced by traversing
the abstract syntax tree.

During each annotation step, leaf expressions like `xs` receive variables *q*
and *q'* and a cost *k*.  The linear equation that results is simply
*q*=*q'*+*k*.  Interior nodes in the syntax tree like `(car xs)` are aware of
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
with weights corresponding to each polynomial of list variables, and the
constant function variables.  To calculate a runtime upper bound, this
objective should be minimized.

Unfortunately, even for such a simple program, the analysis produced is too
large to be understood in its entirety; it must be broken down.  The output
below is the annotated result of running with the `-g Eqns` flag.

To start, let's establish the objective:

    minimize m + n

Here, the variable `m` tracks the linear cost of the function `sum`, while `n`
tracks the constant cost.  This is a good time to point out that these variable
names are chosen arbitrarily, and don't necessarily correspond to the bounds
reported by the analysis, where `n` is often used for the linear potential.

Next, a backwards analysis is performed, starting with the leaves of the AST.
The first leaf is the literal `0` in the `if` branch:

    -h + i = 1

With this branch finished, the `else` branch is analyzed next.  Before
descending the tree, fresh annotations are supplied for the applied functions,
first the call to `+`:

    -f + g = 0
    e - g = 1
    -d + f = 1

Then the recursive call to `sum`, which is given a slack variable `z` to let
the potential float for the recursion (without which, the cost would be
anchored to the overall cost, and the problem would be infeasible):

    -z + b - n = 1
    z - a + k = 1

Then the call to `cdr`, which is special because it includes a "potential
shift", which allows `v` to pay for both the linear and constant costs:

    -s + v + w = 0
    -r + v = 0
    -s + u = 0
    -r + t = 0
    q - w = 1
    -p + u = 1

With these fresh signatures created, equations for the cost of var `xs` are
derived:

    -i_2 + k_2 = 1
    -h_2 + j_2 = 0

With these, it can apply var `xs` as a parameter to `cdr`:

    i_2 - n_2 = 1
    h_2 - m_2 = 0
    g_2 - k_2 = 0
    f_2 - j_2 = 0
    n_2 - q = 0
    m_2 - v = 0
    e_2 - g_2 = 0
    d_2 - f_2 = 0

And then apply the result of `cdr` as a parameter to `sum`:

    p - y = 1
    t - x = 0
    c_2 - e_2 = 0
    b_2 - d_2 = 0
    y - b = 0
    x - m = 0
    a_2 - c_2 = 0
    z_2 - b_2 = 0
    a - c = 1
    y_2 - a_2 = 0
    x_2 - z_2 = 0

Then the call to `car`, which is similarly special, thanks to `v_2`:

    -t_2 + v_2 + w_2 = 0
    -s_2 + v_2 = 0
    -t_2 + u_2 = 0
    r_2 - w_2 = 1
    -q_2 + u_2 = 1

Now equations for the cost of this var `xs`:

    -j_3 + m_3 = 1
    -i_3 + k_3 = 0

Which can be used to apply that var `xs` as a parameter to `car`:

    j_3 - p_2 = 1
    i_3 - n_3 = 0
    h_3 - m_3 = 0
    g_3 - k_3 = 0
    p_2 - r_2 = 0
    n_3 - v_2 = 0
    f_3 - h_3 = 0
    e_3 - g_3 = 0

At this point, it is necessary to do something that appears unintuitive.  In
order to correctly propagate the different kinds of costs induced through the
recursive call to `sum`, beyond the "ground truth" about the preceding code
(the call to `car` just shown), the analysis must plumb in additional paths
using a "cost-free" metric.  This can be considered as an alternate world of
sorts which allows subsequent higher-degree costs to pass through and be
captured.  Worth noting is the degree of this cost-free analysis is less than
the base degree of the analysis by the degree of the subsequent code it plumbs.
This is a necessary condition of termination, but even if not it would prevent
an exponential blowup of the analysis that would otherwise occur.

That said, in fresh equations cost-free call to `car`, there is no potential
shift operation:

    -b_3 + d_3 = 0
    -b_3 + c_3 = 0
    a_3 - d_3 = 0
    -z_3 + c_3 = 0

In the cost-free var `xs`, there are no linear variables:

    -w_3 + x_3 = 0

Likewise in the application of the cost-free var `xs` as a parameter to
cost-free `car`:

    w_3 - y_3 = 0
    v_3 - x_3 = 0
    y_3 - a_3 = 0
    u_3 - v_3 = 0

With the cost-free analysis of `car` finished, the two can be combined to
sequence the call to `car` before the call to `sum`:

    q_2 - y_2 = 1
    z_3 - x_2 = 0
    t_3 - f_3 = 0
    r_3 - e_3 = 0
    s_3 - u_3 = 0

Here, another dimensional trick must be employed: the two different uses of
var `xs` must be combined through what is called the "sharing" operation:

    q_3 - t_3 = 0
    p_3 - r_3 - s_3 = 0

At last, the `car` and `sum` parameters to `+` can be applied:

    c - e = 0
    n_4 - q_3 = 0
    m_4 - p_3 = 0

With the two branches of the `if` expression finished, the analysis combines
them by "relaxing" their bounds to the greater of the two.  While in this case
it is obvious that the literal `0` must cost less than the addition, the
analysis can simply encode this by creating new upper and lower bounds.

First, it relaxes the `if` branch:

    k_4 - i ≥ 1
    j_4 ≥ 0
    h - j ≥ 1

Then, it relaxes the `else` branch:

    k_4 - n_4 ≥ 1
    j_4 - m_4 ≥ 0
    d - j ≥ 1

And with that the analysis turns to the condition, supplying fresh annotations
for the call to `null?`:

    -f_4 + h_4 = 0
    e_4 - h_4 = 1
    -d_4 + f_4 = 1

As before, equations for the cost of this var `xs`:

    -y_4 + a_4 = 1
    -x_4 + z_4 = 0

Which are applied as a parameter to `null?`:

    y_4 - c_4 = 1
    x_4 - b_4 = 0
    w_4 - a_4 = 0
    v_4 - z_4 = 0
    c_4 - e_4 = 0
    b_4 - g_4 = 0
    u_4 - w_4 = 0
    t_4 - v_4 = 0

The call to `null?` must be sequenced before the `if` and `else` branches,
which in started here:

    d_4 - i_4 = 1
    s_4 - u_4 = 0
    r_4 - t_4 = 0

Just like previously, this call to `null?` precedes the code in the branches of
the `if` expression, and a cost-free call to `null?` is required:

    -n_5 + p_4 = 0
    m_5 - p_4 = 0
    -k_5 + n_5 = 0

With its very own cost-free var `xs`:

    -h_5 + i_5 = 0

And an application of that cost-free var `xs` as a parameter to that cost-free
`null?`:

    h_5 - j_5 = 0
    g_5 - i_5 = 0
    j_5 - m_5 = 0
    f_5 - g_5 = 0

The sequencing of `null?` before `if` and `else` branches is then completed:

    k_5 - q_4 = 0
    e_5 - f_5 = 0
    i_4 - k_4 = 0
    q_4 - j_4 = 0
    d_5 - s_4 = 0
    b_5 - r_4 = 0
    c_5 - e_5 = 0

Because it is used multiple times, the analysis must again share `xs`:

    a_5 - d_5 = 0
    z_5 - b_5 - c_5 = 0

Finally, the `if` expression has been completed!  All that is left to do is
connect it with the overall cost of the function `sum`:

    -a_5 + n = 0
    -z_5 + m = 0
    -j + k = 0

The linear program is finished.  It can now be fed to the solver, which happily
finds the optimum:

    22*m + 14*n + ... (assignments for other variables omitted)

The optimum values for the list type variables are the linear upper bounds,
while the optimum values for the function's constants are the constant factors.
This corresponds to the reported bounds for the function (recall as mentioned
above that the two `n`s are not the same):

    sum: 22.0*n + 14.0

## Analysis

When run on the sample programs in `examples/ratl` (excluding `parser*`), here
are the resulting bounds predicted:

    examples/ratl/all.ratl
    main: 29*n + 27
    all: 29*n + 23

    examples/ratl/any.ratl
    main: 29*n + 27
    any: 29*n + 23

    examples/ratl/bubble.ratl
    main: 54*n^2 + 94*n + 34
    bubble: 54*n^2 + 94*n + 31
    swapback: 54*n + 18

    examples/ratl/cart.ratl
    main: 34*n*m + 34*m + 37*n + 45
    where
        m is for index {[], [∗]}
        n is for index {[∗], []}
    cart: 34*n*m + 34*m + 37*n + 40
    where
        m is for index {[], [∗]}
        n is for index {[∗], []}
    pairs: 34*n + 26

    examples/ratl/drop.ratl
    drop: 23*n + 18

    examples/ratl/eratos.ratl
    main: Analysis was infeasible
    range: Analysis was infeasible
    eratos: 81*n^2 + 42*n + 23
    filtercar: 81*n + 9
    divides?: 31

    examples/ratl/filtzero.ratl
    main: 50*n + 44
    filtzero: 50*n + 41

    examples/ratl/id.ratl
    main: 12
    id: 1
    id_list: 1

    examples/ratl/insertion.ratl
    main: 54*n^2 + 111*n + 17
    insertion: 54*n^2 + 111*n + 14
    insert: 54*n + 38

    examples/ratl/last.ratl
    main: 25*n + 23
    last: 25*n + 20

    examples/ratl/length.ratl
    main: 19*n + 14

    examples/ratl/loop.ratl
    main: Analysis was infeasible
    loop_lit_list: Analysis was infeasible
    loop_to_list: Analysis was infeasible
    loop: Analysis was infeasible

    examples/ratl/mono.ratl
    main: 112*n + 43
    mono_dec: 56*n + 18
    mono_inc: 56*n + 18

    examples/ratl/multivar.ratl
    prod: 29*n*m + 29*m + 44*n + 41
    where
        m is for index {[], [∗]}
        n is for index {[∗], []}
    dist: 29*m + 29*n + 59
    where
        m is for index {[], [∗]}
        n is for index {[∗], []}
    mapmul: 29*n + 21

    examples/ratl/nil.ratl
    main: 5
    nil: 1

    examples/ratl/preserve.ratl
    preserve_sndpair: 14*n + 11
    preserve_fstpair: 14*n + 11
    preserve_carcons: 14*n + 11

    examples/ratl/product.ratl
    main: 22*n + 17
    product: 22*n + 14

    examples/ratl/quick.ratl
    main: 75*n^2 + 77*n + 18
    quick: 75*n^2 + 77*n + 15
    split: 51*n + 13

    examples/ratl/reverse.ratl
    reverseLin: 24*n + 14
    reverseQuad: 24*n^2 + 35*n + 9
    snoc: 24*n + 14

    examples/ratl/selection.ratl
    main: 81*n^2 + 156*n + 90
    selection: 81*n^2 + 156*n + 87
    delnextofcar: 44*n + 9
    minimum: 37*n + 19

    examples/ratl/sum.ratl
    main: 22*n + 17
    sum: 22*n + 14

    examples/ratl/take.ratl
    take: 31*n + 23

    examples/ratl/unzip.ratl
    main: 50*n + 17
    unzip: 50*n + 13

    examples/ratl/zero.ratl
    main: 1

    examples/ratl/zip.ratl
    main: 37*m + 5*n + 33
    where
        m is for index {[], [∗]}
        n is for index {[∗], []}
    zip: 37*m + 5*n + 28
    where
        m is for index {[], [∗]}
        n is for index {[∗], []}

Informally, all are plausible.  `all` and `any` are virtually the same program,
and their bounds are identical.  The same is true of `sum` and `product`.  As
expected, `id` is constant time.  The program `loop` does not terminate, so
Ratl is unsurprisingly unable to produce an upper bound.  In `eratos`, `range`
depends solely on parameters which aren't potential-carrying, and so cannot be
analyzed.  Further, `all`, `any`, `sum`, `product`, `take`, `drop`, `length`,
`last`, `zip`<sup id="zipmention">[\*](#zipfootnote)</sup>, `unzip`, and
`reverse`  all yield linear bounds, and of particular interest, `bubble`,
`insertion`, `selection`, and `quick` all derive quadratic bounds, as are
`eratos` and naïve reverse, while `cart`'s inferred bound is a quadratic
product of the size of its inputs.

Performance-wise, Ratl is bound quadratically by the maximum degree of
analysis, and within a degree, the program size.  This makes sense, as Simplex
runs in linear time for most problems, and the problem size in Ratl grows
quadratically both with the number of variables, which is within a constant
factor of the number of nodes in the abstract syntax tree, and the maximum
degree for the types of those variables.  To verify that, I collected
performance data on Ratl in use analyzing 500 functions, using an Ubuntu Linux
system with an AMD Ryzen 5 1600 and 32GB of RAM.  These were broken down in
groups by the number of functions in the module in question.  The programs
analyzed were `zero`, `sum`, `parser_tiny`, `parser_small`, and `parser`.  Only
the `main` functions in each program were analyzed, and each was run with max
degree set to 1, then 2.  The user and sys times are the total time reported by
the Linux `time` utility.  Each program's analysis was repeated so that the
number of executions times the module's function count equaled 500.  These are
presented along with their averages.

    ratl -m Analyze -d 1 -n main $program.ratl

|     Program  | Executions | Functions | Total User | Total Sys | Mean User | Mean Sys |
| ------------ | ---------- | --------- | ---------- | --------- | --------- | -------- |
|         zero |        500 |         1 |     45.10s |    23.28s |     0.09s |    0.05s |
|          sum |        250 |         2 |     27.82s |    16.07s |     0.11s |    0.06s |
|  parser_tiny |        100 |         5 |     58.10s |    81.12s |     0.58s |    0.81s |
| parser_small |         50 |        10 |    136.44s |   193.81s |     2.73s |    3.88s |
|       parser |         10 |        50 |    360.44s |   498.32s |    36.04s |   49.83s |

    ratl -m Analyze -d 2 -n main $program.ratl

|     Program  | Executions | Functions | Total User | Total Sys | Mean User | Mean Sys |
| ------------ | ---------- | --------- | ---------- | --------- | --------- | -------- |
|         zero |        500 |         1 |     43.19s |    23.72s |     0.09s |    0.05s |
|          sum |        250 |         2 |     30.56s |    24.58s |     0.12s |    0.10s |
|  parser_tiny |        100 |         5 |    280.61s |   402.08s |     2.81s |    4.02s |
| parser_small |         50 |        10 |    746.38s |  1084.85s |    14.93s |   21.70s |
|       parser |         10 |        50 |   3783.10s |  2701.06s |   378.31s |  270.11s |

This supports the claim above that after getting past the overhead costs of
startup, Ratl's analysis is approximately quadratically upper bounded by the
program size.

<b id="zipfootnote">\*</b>Worth mentioning is that `zip`'s bound is a bit odd
at first glance.  We think of the "work" done zipping two lists as being split
equally across both, but there is no rule stating that this must be so; one
list or the other could pay for nearly the entirety of the operation.  In this
case the analysis results in more than one feasible solution, of which one is
selected. [↩](#zipmention)

## Prior Work

Ratl is based on the ideas behind the Raml project from Jan Hoffmann, et. al.
Raml is much more powerful than Ratl.  It targets a production language, and
supports higher-order functions, algebraic data types, integers, lower bounds,
and resources other than time.

Hoffmann covers resource analysis systematically and rigorously from the ground
up in his Ph.D. thesis<sup id="thesismention">[1](#thesisfootnote)</sup>, where
he lays down the complete conceptual background, then produces the semantics
and type rules for linear potential, followed by polynomial, followed by
multivariate, each analysis more powerful than the previous.

For polynomial bounds, each type and expression is annotated with variables for
each degree.  The analysis accounts for each of these variables by representing
them as binomial coefficients.  Each binomial's lower index corresponds to its
polynomial degree, produced by the binomial's expansion.  Unlike the polynomial
however, a binomial has a natural mapping to recursive data structures: the
successive position in that data structure.  If expanded to coefficients
for each combination of multiple variables, this will produce multivariate
bounds.

Hoffmann has continued this work with Raml, which is growing to include an
increasing share of OCaml<sup id="papermention">[2](#paperfootnote)</sup>.

## Caveats

Ratl is not appropriate, suitable, or fit for (practically) any purpose.  It is
missing many features that would be expected in a normal language, even many
present in nano-ML.

Ratl is also not defect-free. There are a few bugs in it.  For example, it
derives incorrect resource usage for literals.  If you name two functions the
same, the analysis of their callers will probably be wrong.

Ratl analysis gets quadratically larger with the size of the abstract syntax
tree of the program it's told to analyze.  For maximum degree 2, peak memory
usage noted while analyzing `parser.ratl`, a module containing 50 functions,
exceeded 3 GB; as such, for degree 2 a call graph with more than around 200
modestly-sized functions is likely to run out of memory on most systems.

## Beyond Ratl: Next Steps

Ratl was an unexpected pitstop along the way to understanding Hoffmann's work.
My original intent was to do a limited analysis on the Haskell programming
language itself.  While trying to understand how the problem could be solved
using linear programming, I found it simpler to have complete control over the
language.  Thus, Ratl was born.

This work will continue where it started: by returning to Haskell, in the form
of a library and compiler plugin.

That doesn't mean that Ratl is finished, though.  It will continue to be a
sandbox to mature these concepts.  Whenever the analysis must be extended, it
will be done in Ratl first.  Possible future extensions include higher-order
functions, algebraic data types, integers, laziness, and analyses of returnable
resources like memory.

## References

<b id="thesisfootnote">1</b> Hoffmann, J. 2011. Types with potential:
Polynomial resource bounds via automatic amortized analysis. Ph.D. thesis,
Ludwig-Maximilians-Universität, München, Germany.[↩](#thesismention)

<b id="paperfootnote">2</b> Jan Hoffmann, Ankush Das, and Shu-Chun Weng. 2017.
Towards automatic resource bound analysis for OCaml. *SIGPLAN Not.* 52, 1
(January 2017), 359-373.  DOI: https://doi.org/10.1145/3093333.3009842
[↩](#papermention)
