cplint
======

cplint is a suite of programs for reasoning with probabilistic logic programs.
It contains programs for both inference and learning.

You can find the manual at https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf

Example of use
---------------

    $ cd <pack>/cplint/prolog/examples
    $ swipl
    ?- [coin].
    ?- prob(heads(coin),P).
