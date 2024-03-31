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

Publications
------------

* <a href="https://esumii.github.io/min-caml/index-e.html">Eijiro Sumii. A Crash Course for the MinCaml compiler</a>.
* <a href="">Eijiro Sumii. MinCaml: A Simple and Efficient Compiler for a Minimal Functional Language</a>.
* <a href="https://tonpa.guru/stream/2024/2024-03-30%20Інформатика.htm">М. Сохацький. Інформатика</a>.

Credits
-------

* Namdak Tonpa
