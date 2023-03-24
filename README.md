# TinyLISP (TLISP) - a LISP interpreter written in Forth.

TinyLISP is LISP interpreter written in Forth based on Dieter Müller's book
"LISP - An elementary introduction to the programming of non-numerical tasks",
which I originally developed in 1988 on the Commodore 128 under CP/M 3.0
using the Forth 83 implementation F83 by Henry Laxen and Mike Perry.

It's a hobby project that I occasionally continue to work on under
[Gforth](https://gforth.org), though trying to maintain F83 compatibility
for nostalgic reasons.

## Quick start

### Starting TLISP

The LISP interpreter is started executing the Forth word `driver` after loading
the main source file [tlisp.fs](tlisp.fs).

```sh
gforth -e "warnings off" tlisp.fs -e "driver"
```

At startup, TLISP loads the file [tlisp.lsp](tlisp.lsp) and enters the REPL.

> **Note**<br/>
> If warnings are enabled, Gforth will complain about entering double-cell
> integers without using a base-prefix, which is done for compatibility reasons.

### Exiting TLISP

Typing `(EXIT)` at the REPL exits both TLISP and Forth.

```
tlisp> (exit)
```

## Settings

The behavior of TLISP can be changed at runtime by changing the desired property
value of the CONFIG symbol, see the following table:

| Indicator | Description                            | Default |
|-----------|----------------------------------------|---------|
| APVAL     | Evaluate APVAL before/after the a-list | NIL     |
| DEBUG     | Enable/disable tracing                 | NIL     |
| ECHO      | Enable/disable echoing                 | NIL     |
| EVALQUOTE | Enter/leave evalquote mode             | NIL     |
| EXIT      | Exit Forth when exiting TLISP on/off   | T       |
| GC        | Enable/disable garbage collection      | T       |
| PAUSE     | Enable/disable single step execution   | NIL     |

There are two shortcuts `DEBUG` and `ECHO` predefined in [tlisp.lsp](tlisp.lsp) for the
corresponding settings.

## Evalquote mode

Changing the value of the setting EVALQUOTE to T enters evalquote mode and
changing it back to NIL leaves evalquote mode:

```
tlisp> (putprop 'config t 'evalquote)
T

evalquote> caddr ((first second third fourth))
THIRD

evalquote> putprop (config nil evalquote)
NIL

tlisp>
```
