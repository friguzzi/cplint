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

The pack uses a foreign library and contains the library binaries for 32 and 64 bits Linux and 64 bits Windows. If you want to recompile the foreign library you can use

    ?- pack_rebuild(cplint).

On 32 and 64 bits Linux this should work out of the box. On 64 bits Windows the library must be rebuilt by hand.
The procedure for building it using MSYS2 can be found at the bottom of this document.

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

Datasets
--------

Other machine learning datasets are available in pack cplint_datasets 
(https://github.com/friguzzi/cplint_datasets)

Building under Windows using MSYS2
----------------------------------
Install MSYS2 from https://www.msys2.org/
First run `?- pack_rebuild(cplint)`. This typically fails but produces the file
`buildenv.sh` in the root folder. 
Modify this file by commenting the line defining the PATH variable in this way

    #PATH=...

Open a Mingw-w64 64 bit shell. Install required packages using `pacmam`

    pacman -S mingw64/mingw-w64-x86_64-gcc
    pacman -S msys/automake-wrapper
    pacman -S msys/autoconf
    pacman -S mingw64/mingw-w64-x86_64-libtool
    pacman -S msys/make

The compilation was successful with the following package versions

    $ pacman -Qe
    autoconf 2.69-5
    automake-wrapper 11-1
    bash 4.4.019-3
    bash-completion 2.8-2
    bsdcpio 3.3.3-3
    bsdtar 3.3.3-3
    bzip2 1.0.6-2
    coreutils 8.30-1
    curl 7.61.0-2
    dash 0.5.10.2-1
    dtc 1.4.7-1
    file 5.34-1
    filesystem 2018.08-1
    findutils 4.6.0-1
    flex 2.6.4-1
    gawk 4.2.1-1
    gcc-libs 7.3.0-3
    getent 2.18.90-2
    grep 3.1-1
    gzip 1.9-1
    inetutils 1.9.4-1
    info 6.5-2
    less 530-1
    libargp 20110921-2
    lndir 1.0.3-1
    make 4.2.1-1
    mingw-w64-x86_64-gcc 8.2.0-3
    mingw-w64-x86_64-libtool 2.4.6-12
    mintty 1~2.9.1-1
    msys2-keyring r9.397a52e-1
    msys2-launcher-git 0.3.32.56c2ba7-2
    msys2-runtime 2.11.1-2
    ncurses 6.1.20180908-1
    pacman 5.1.1-3
    pacman-mirrors 20180604-2
    pactoys-git r2.07ca37f-1
    pax-git 20161104.2-1
    pkgfile 17-1
    rebase 4.4.4-1
    sed 4.5-1
    tftp-hpa 5.2-2
    time 1.9-1
    ttyrec 1.0.8-2
    tzcode 2018.c-1
    util-linux 2.32.1-1
    which 2.21-2

In the Mingw shell, go the folder where `cplint` is located, typically `AppData/Roaming/SWI-Prolog/pack/cplint/` in your home.
Execute `buildenv.sh` 

    $ source buildenv.sh

Compile with

    $ ./configure
    $ make
    $ make install
