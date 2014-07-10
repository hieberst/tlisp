\ TLISP.F (Win32Forth)
\
\ READ-LINE funktioniert nur mit DOS-Zeilenenden (CRLF), nicht mit UNIX-LFs

: U>= 2DUP = -ROT U> OR ;

FLOAD TLISP.FS
