# JSSi

A [JavascriptScript](https://github.com/brusteca/JavascriptScript) interpreter in OCaml; a century too early.

## Instructions

Compile with `make`. The executable will be found locally named `jssi`.

## Features

Implemented:

- Interactive interpreter.
- Assignment with = and ==
- Operators "+", "++", "+++", "===", "====", "====="
- Stacked assignment (A = B = C as A = B and B = C)

Backlog:

- Ternary ? :
- Parenthesis
- Objects
- Functions? (redefinition possible)

Long-term:

- Unix interface by "listening" to the values in specific variables?
- Graphical interface with an absurd implementation to be defined.
