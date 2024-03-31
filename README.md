Joe: MinCaml Compiler and Virtual Machine
=========================================

Features
--------

* 4K LOC
* Intel/ARM 64-bit native compiler (joe)
* Byte-code IR compiler (joe)
* Byte-code IR virtual machine (vm)
* Original MinCaml codebase

Setup
-----

```
$ opam switch set 4.14.2
$ opam install ppx_deriving ppx_inline_test
$ dune build
```

Samples
-------

```sh
_build/install/default/bin/vm -no-sh -interp examples/ack.ml
509
```

```
_build/install/default/bin/joe -intel examples/ack.ml
generating assembly...OK
```

Credits
-------

* Namdak Tonpa
