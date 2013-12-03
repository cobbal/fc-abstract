**fc-abstract** is a functional, yet woefully incomplete, GHC plugin
for an abstract interpreter for Haskell's [Core Syntax][0] which is
based on [System F<sub>C</sub>][1].

The design of the interpreter is based closely on the Abstract CESK*
machine described by Matthew Might and David Van Horn in
"[Abstracting Abstract Machines][2]". The difference in semantics is
defined without much explanation in lazyCESK.pdf

Currently working:

 - GHC integration (see fc-tests/Makefile)
 - Potentially unbounded CESK-style interpretation.
 - Bounded abstract interpretation

Horrific failings:

 - Types are pretty much ignored, even though they provide very useful
   information for determining where finite closure have seriously
   messed up a previously type-checked program
 - Pattern matching and constructors look complicated. Not attempted
   yet.

  [0]: https://www.haskell.org/ghc/docs/latest/html/libraries/ghc/CoreSyn.html
  [1]: https://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/tldi22-sulzmann-with-appendix.pdf
  [2]: http://matt.might.net/papers/vanhorn2010abstract.pdf
