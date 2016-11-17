cplint
======

cplint is a suite of programs for reasoning with probabilistic logic programs.
It contains programs for both inference and learning.

You can find the manual at https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html

You can try it online at http://cplint.lamping.unife.it/

Installation
------------
It can be installed with `pack_install/1`
    $ swipl
    ?- pack_install(cplint).

The pack uses a foreign library and contains the library binaries for 32 and 64 bits Linux and 32 and 64 bits Windows. If you want to recompile the foreign library you can use

    ?- pack_rebuild(cplint).

On 32 and 64 bits Linux this should work out of the box. On 32 and 64 bits Windows the library must be rebuilt by hand. 
First run `pack_rebuild(cplint)`. This typically fails but produces the file
`buildenv.sh` in the root folder. You can modify this file looking at the
example files
`buildenvmingw32.sh` and `buildenvmingw64.sh`. Then you can run
    $ source buildenv.sh
    $ source configure
    $ make install


Requirements
-------------
It requires packs `auc` and `matrix`:

    $ swipl
    ?- pack_install(auc).
    ?- pack_install(matrix).

Example of use
---------------

    $ cd <pack>/cplint/prolog/examples
    $ swipl
    ?- [coin].
    ?- prob(heads(coin),P).
