cplint
======

cplint is a suite of programs for reasoning with probabilistic logic programs.
It contains programs for both inference and learning.

You can find the manual at https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html

You can try it online at http://cplint.lamping.unife.it/

Installation
------------
This is a SWI-Prolog (http://www.swi-prolog.org/) pack.

It can be installed with `pack_install/1`

    $ swipl
    ?- pack_install(cplint).

Requirements
-------------
It requires the packs

 * `bddem` https://github.com/friguzzi/bddem
 * `auc` https://github.com/friguzzi/auc
 * `matrix` https://github.com/friguzzi/matrix
 
 They are installed automatically when installing pack `cplint` or can installed manually as

    $ swipl
    ?- pack_install(bddem).
    ?- pack_install(auc).
    ?- pack_install(matrix).

`bddem` uses a foreign library and contains the library binaries for 32 and 64 bits Linux and 64 bits Windows. If you want to recompile the foreign library you can use

    ?- pack_rebuild(bdeem).

On 32 and 64 bits Linux this should work out of the box. On 64 bits Windows the library must be rebuilt by hand, see the pack page https://github.com/friguzzi/bddem

You can upgrade the pack with

    $ swipl
    ?- pack_upgrade(cplint).

Note that the packs on which `cplint` depends are not upgraded automatically in this case so they need to be upgraded manually.

Example of use
---------------

    $ cd <pack>/cplint/prolog/examples
    $ swipl
    ?- [coin].
    ?- prob(heads(coin),P).

Datasets
--------

Other machine learning datasets are available in pack cplint_datasets 
(https://github.com/friguzzi/cplint_datasets)

