cplint
======

cplint is a suite of programs for reasoning with probabilistic logic programs.
It contains programs for both inference and learning.

You can find the manual at http://ds.ing.unife.it/~friguzzi/software/cplint/manual.html

Example of use
---------------

    $ cd <pack>/cplint/prolog/examples
    $ swipl
    ?- use_module(library(pita)).
    ?- parse(coin).
    ?- s(heads(coin),P).
