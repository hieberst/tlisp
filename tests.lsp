; $Id$

(LOAD "sqr.lsp")

(DEFUN CHECK (sexpr result)
    (PROG (flag)
          (SETQ flag (EQUAL (EVAL sexpr) result))
          (PRIN sexpr)
          (PRIN2 " --> ")
          (PRIN result)
          (PRIN2 " : ")
          (PRINT flag)
    ))

(CHECK '(CONS 'A 'B)              '(A . B))
(CHECK '(PAIR '(A B C) '(D E F))  '((A . D) (B . E) (C . F)))
(CHECK '(APPEND '(A B C) 'D)      '(A B C . D))
(CHECK '(PLUS)                    0)
(CHECK '(PLUS 1 2 3)              6)
(CHECK '(TIMES)                   1)
(CHECK '(LAMBDA FOO)              '(LAMBDA FOO))
(CHECK '((LAMBDA (N) (ADD1 N)) 1) 2)
(CHECK '(SQR 3)                   9)
(CHECK '(CADR '(A B C))           'B)
(CHECK '(CADR '(A,B,C))           'B)

; PROP
(CSETQ A 3)
(DEFUN FOO () (PRINT "FOO"))
(CHECK '(PROP 'A  APVAL   NIL) '(3))
(CHECK '(PROP 'A  APVAL  'FOO) '(3))
(CHECK '(PROP 'A 'APVAL2  NIL) NIL)
(CHECK '(PROP 'A 'APVAL2 'FOO) "FOO")
(REMPROP 'FOO EXPR)
(REMPROP 'A   APVAL)

; Fakultaet
(CHECK '(LOAD "fac.lsp") T)
(CHECK '(FACTORIAL 7) 5040)
(CHECK '(FAC2      7) 5040)
(GC)

; Fibonacci-Folge
(CHECK '(LOAD "fib.lsp") T)
(CHECK '(FIB1 10) 55)
(CHECK '(FIB2 10) 55)
(GC)

; Umkehrung einer Liste
(CHECK '(LOAD "rev.lsp")      T)
(CHECK '(REV  '(A B C))       '(C B A))
(CHECK '(REV1 '(A B C))       '(C B A))
(CHECK '(REV2 '(A B C))       '(C B A))
(CHECK '(REV3 '(A B C))       '(C B A))
(CHECK '(REVALL '(A (B C) D)) '(D (C B) A))
(GC)

; MAP-Funktionen
(CHECK '(LOAD "map.lsp")                                        T)
(CHECK '(MAPCAR  '(1 2 3 4) 'SQR)                               '(1 4 9 16))
(CHECK '(MAPC    '(1 2 3 4) (LAMBDA (N) (PRINT (SQR N))))       NIL)
(CHECK '(MAPLIST '(1 2 3 4) (LAMBDA (L) (SQR (CAR L))))         '(1 4 9 16))
(CHECK '(MAP     '(1 2 3 4) (LAMBDA (L) (PRINT (SQR (CAR L))))) NIL)
(GC)

; Pascal'sches Dreieck (MAPLIST)
(CHECK '(LOAD "pasc.lsp") T)
(CHECK '(PASC 4)          '(1 4 6 4 1))
(GC)

; Mengenoperationen
(CHECK '(LOAD "set.lsp")                 T)
(CHECK '(UNION '(A B C) '(B C D))        '(A B C D))
(CHECK '(INTERSECTION '(A B C) '(B C D)) '(B C))
(CHECK '(MEMBER 'C '(A B C))             T)
(CHECK '(MEMBER 'B '(A (B) C))           NIL)
(GC)

; T und NIL
(CHECK '(ATOM  T)              T)
(CHECK '(LISTP T)              NIL)
(CHECK '(ATOM  NIL)            T)
(CHECK '(LISTP NIL)            T)
(CHECK '((NULL '(A)) (EXIT))   NIL)
(CHECK '((NULL ()) (PLUS 1 2)) 3)

; Funktionale Argumente
(DEFUN TF (FN N) (FN N))
(CHECK '(TF 'SQR 3) 9)

; Anonyme LAMBDA-Ausdruecke
(CHECK '(LOAD "split.lsp") T)
(CHECK '(SPLIT  1234) '(12 . 34))
(CHECK '(SPLIT1 1234) '(12 . 34))

; Funktionale Argumente (FUNARG)
(CHECK '(LOAD "sqx.lsp") T)

; Tracing
(CHECK '(LOAD "trace.lsp") T)
(TRACE SQR FACTORIAL)
(SQR 3)
(FACTORIAL 7)
(UNTRACE SQR FACTORIAL)
(GC)

; EVALQUOTE
(CHECK '(LOAD "tests.txt" T) T)
(CHECK '(EVALQUOTE PLUS (2 3))   5)
(CHECK '(EVALQUOTE CSET (A B))   'B)
(CHECK '(EVALQUOTE EVAL (A NIL)) 'B)
(PRINT (EVALQUOTE EVAL (OBLIST NIL)))
