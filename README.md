cplint
======

cplint is a suite of programs for reasoning with probabilistic logic programs.
It contains programs for both inference and learning.

You can find the manual at https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html

You can try it online at http://cplint.lamping.unife.it/

Requirements
-------------
It requires packs auc and matrix:

    $ swipl
    ?- pack_install(auc).
    ?- pack_install(matrix).

Example of use
---------------

    $ cd <pack>/cplint/prolog/examples
    $ swipl
    ?- [coin].
    ?- prob(heads(coin),P).
