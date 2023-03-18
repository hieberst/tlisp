# TinyLISP (TLISP) - a LISP interpreter written in Forth.

TinyLISP is a Forth implementation of LISP based on Dieter Müller's book "LISP - An elementary introduction to the programming of non-numerical tasks", which I originally developed in 1988 on the Commodore 128 under CP/M 3.0 using the Forth 83 implementation F83 by Henry Laxen and Mike Perry.

It's a hobby project that I occasionally continue to work on under [Gforth](https://gforth.org), though trying to maintain F83 compatibility for nostalgic reasons.

## Quick start

### Starting TLISP

The LISP interpreter is started executing the Forth word `driver` after loading the main source file [tlisp.fs](tlisp.fs).

```sh
gforth -e "warnings off" tlisp.fs -e "driver"
```

If warnings are enabled, Gforth will complain about entering double-cell integers without using a base-prefix, which is done for compatibility reasons.

### Exiting TLISP

Type `(EXIT)` at the REPL.
