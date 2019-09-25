# monoparser
implementation of Monadic Parser Combinator's Graham Hutton and Erik Meijer 1996.

In functional programming, a popular approach to building recursive descent parsers is
to model parsers as functions, and to define higher-order functions (or combinators) that
implement grammar constructions such as sequencing, choice, and repetition. Such parsers
form an instance of a monad , an algebraic structure from mathematics that has proved
useful for addressing a number of computational problems.
