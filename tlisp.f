\ TLISP.F (Win32Forth, SwiftForth, VFX)
\
\ vim: set syntax=forth:
\
\ Supported Forth systems:
\   Win32Forth: 4.2.0671
\   SwiftForth: 3.6.5
\   VFX Forth:  4.80.3621
\
\ Win32Forth: READ-LINE funktioniert nur mit DOS-Zeilenenden (CRLF),
\             nicht mit UNIX-LFs.

[UNDEFINED] U>= [IF]
    : U>= ( u1 u2 -- flag ) 2DUP = -ROT U> OR ;
[THEN]

[UNDEFINED] PERFORM [IF]
    : PERFORM ( a-aadr -- ) @ EXECUTE ;
[THEN]

[UNDEFINED] SP0 [IF]
    : SP0 ( -- a-addr ) S0 ;
[THEN]

[UNDEFINED] .ID [IF]
    : .ID ( nfa -- ) .NAME ;
[THEN]

\ SwiftForth
[DEFINED] SWF-ONLINE [IF]
REQUIRES fpmath
-BALANCE            \ balance checking does not work with nested IFs
[THEN]

\ MPE VFX Forth
[DEFINED] VFXFORTH [IF]
INCLUDE %lib%\x86\ndp387.fth
\ Change to the TLISP directory
CurrSourceName PAD PLACE PAD stripFilename PAD COUNT $CD
[THEN]

INCLUDE tlisp.fs
driver
