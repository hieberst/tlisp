\ TLISP_SP.F (SP-Forth 4.20)
\
\ vim: set syntax=forth:
\
\ SP-Forth: READ-LINE funktioniert unter Windows nur mit
\           DOS-Zeilenenden (CRLF), nicht mit UNIX-LFs.

REQUIRE DEFER     lib/include/defer.f
REQUIRE 2VARIABLE lib/include/double.f
REQUIRE U.R       lib/include/core-ext.f
REQUIRE CASE      lib/ext/case.f
REQUIRE OFF       lib/ext/onoff.f

: U>= U< 0=  ;
: >= < 0= ;
: SP0 S0 ;
: UPC DUP [CHAR] Z U> IF 0xDF AND THEN ;
: PERFORM @ EXECUTE ;
: .ID ID. SPACE ;

: >NAME \ borrowed from ~ac\lib\lin\xml\expat.f
  4 - DUP
  BEGIN
    1- DUP C@ 0<> DUP
    IF DROP 2DUP
      COUNT + U< 0=
    THEN
  UNTIL NIP
;

REQUIRE DRIVER tlisp.fs
driver
