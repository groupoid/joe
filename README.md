Joe: MinCaml Compiler and Virtual Machine
=========================================

Features
--------

* 4K LOC
* Intel/ARM 64-bit native compiler
* Byte-code IR compiler
* Byte-code IR virtual machine
* Original MinCaml codebase

Setup
-----

```
$ opam switch set 4.14.2
$ opam install ppx_deriving ppx_inline_test
$ dune build
$ _build/install/default/bin/joevm -no-sh -interp examples/ack.ml
```

Credits
-------

* Namdak Tonpa
