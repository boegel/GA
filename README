GA, a Haskell library for working with genetic algorithms
---------------------------------------------------------

version 1.0, Sept. 2011, written by Kenneth Hoste (kenneth.hoste@gmail.com)
see http://hackage.haskell.org/package/GA

* DESCRIPTION

This package provides a framework for working with genetic
algorithms. A genetic algorithm is an evolutionary technique, 
inspired by biological evolution, to evolve entities that perform
as good as possible in terms of a predefined criterion (the scoring 
function). 
Note: lower scores are assumed to indicate better entities.

The GA module provides a type class for defining entities and the
functions that are required by the genetic algorithm.

Checkpointing in between generations is available, as is automatic
restoring from the last available checkpoint (see evolveChkpt). 

* BUILDING AND USING

Building the supplied examples can be done by running 'make'
in the examples directory after the installation of the GA library.

Using the GA module should be clear after studying the examples.

* EXAMPLES

This release includes two toy examples that show how to use the GA module.

The first example (see theNumber.hs) evolves an integer number that has
8 integer divisors, and for which the sum of its divisors equals 96.
Although using a genetic algorithm is probably not the best way to find 
such an integer (it would be easier/faster to just go over integer values
one by one starting from e.g. 8), but it serves well as a toy example.

This example shows how the pool and score data do not have to be used; it
suffices to supply '()' as values to the evolve function, and to simply ignore
the respective arguments passed to the Entity typeclass functions.
We use the score' function in this example, because the scoring itself
doesn't operate in a monad.

A second example evolves the string "Hello World!". The string that the
genetic algorithm should generate is supplied by the user in this example,
and is printed to a file where the GA will read it from during scoring.
This is of course not representative of a real world problem that could 
be solved using genetic algorithms, but again, it does serve well as a toy 
example.

The code in hello.hs illustrates how you can define the "genRandom", 
"crossover", "mutation" and "score" functions that are required to run 
the genetic algorithm using the 'evolveVerbose' function. It also shows
an example of defining the "isPerfect" function to determine whether a
perfect entity was observed (and thus evolution can stop).

This example demonstrates the use of a 'pool' that can be used to generate 
random entities (a list of characters, in this particular case), and 
user-supplied data that can be used to evaluate the fitness of entities (in 
this case, the name of the file where the target string was written to).

It also shows how the GA module support operating in a monad, in this case 
the IO monad, and illustrates the usefulness of the 'randomSearch' function.
