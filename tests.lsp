; $Id$

(DEFUN CHECK (sexpr result)
    (PROG (flag)
          (SETQ flag (EQUAL (EVAL sexpr) result))
          (PRIN sexpr)
          (PRIN2 " --> ")
          (PRIN result)
          (PRIN2 " : ")
          (PRINT flag)
    ))

(CHECK '(LOAD "sqr.lsp") T)

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
(CHECK '(CADR '(A,B,C))           'B)           ; Komma als Listentrenner
(CHECK '''A                       '(QUOTE A))   ; Mehrfach-Quote

; More than one sexpr per line
(CHECK '(PLUS 1 2) 3) (CHECK '(TIMES 2 3) 6)

; PROP
(CHECK '(CSETQ A 3) 3)
(CHECK '(DEFUN FOO () (PRINT "FOO")) 'FOO)
(CHECK '(PROP 'A  APVAL   NIL) '(3))
(CHECK '(PROP 'A  APVAL  'FOO) '(3))
(CHECK '(PROP 'A 'APVAL2  NIL) NIL)
(CHECK '(PROP 'A 'APVAL2 'FOO) "FOO")
(CHECK '(REMPROP 'FOO EXPR)    T)
(CHECK '(REMPROP 'A   APVAL)   T)

; Fakultaet
(TERPRI)
(CHECK '(LOAD "fac.lsp") T)
(CHECK '(FACTORIAL 7) 5040)
(CHECK '(FAC2      7) 5040)

; Fibonacci-Folge
(TERPRI)
(CHECK '(LOAD "fib.lsp") T)
(CHECK '(FIB1 10) 55)
(CHECK '(FIB2 10) 55)

; Umkehrung einer Liste
(TERPRI)
(CHECK '(LOAD "rev.lsp")      T)
(CHECK '(REV  '(A B C))       '(C B A))
(CHECK '(REV1 '(A B C))       '(C B A))
(CHECK '(REV2 '(A B C))       '(C B A))
(CHECK '(REV3 '(A B C))       '(C B A))
(CHECK '(REVALL '(A (B C) D)) '(D (C B) A))

; MAP-Funktionen
(TERPRI)
(CHECK '(LOAD "map.lsp")                                        T)
(CHECK '(MAPCAR  '(1 2 3 4) 'SQR)                               '(1 4 9 16))
(CHECK '(MAPC    '(1 2 3 4) (LAMBDA (N) (PRINT (SQR N))))       NIL)
(CHECK '(MAPLIST '(1 2 3 4) (LAMBDA (L) (SQR (CAR L))))         '(1 4 9 16))
(CHECK '(MAP     '(1 2 3 4) (LAMBDA (L) (PRINT (SQR (CAR L))))) NIL)

; Pascal'sches Dreieck (MAPLIST)
(TERPRI)
(CHECK '(LOAD "pasc.lsp") T)
(CHECK '(PASC 4)          '(1 4 6 4 1))

; Mengenoperationen
(TERPRI)
(CHECK '(LOAD "set.lsp")                 T)
(CHECK '(UNION '(A B C) '(B C D))        '(A B C D))
(CHECK '(INTERSECTION '(A B C) '(B C D)) '(B C))
(CHECK '(MEMBER 'C '(A B C))             T)
(CHECK '(MEMBER 'B '(A (B) C))           NIL)

; T und NIL
(TERPRI)
(CHECK '(ATOM  T)              T)
(CHECK '(LISTP T)              NIL)
(CHECK '(ATOM  NIL)            T)
(CHECK '(LISTP NIL)            T)
(CHECK '((NULL '(A)) (EXIT))   NIL)
(CHECK '((NULL ()) (PLUS 1 2)) 3)

; Funktionale Argumente
(TERPRI)
(CHECK '(DEFUN TWICE (FN N) (FN (FN N))) 'TWICE)
(CHECK '(TWICE 'SQR 3) 81)

; Anonyme LAMBDA-Ausdruecke
(TERPRI)
(CHECK '(LOAD "split.lsp") T)
(CHECK '(SPLIT  1234) '(12 . 34))
(CHECK '(SPLIT1 1234) '(12 . 34))

; Funktionale Argumente (FUNARG)
(CHECK '(LOAD "sqx.lsp") T)

; Tracing
(CHECK '(LOAD "trace.lsp") T)
(CHECK '(TRACE SQR FACTORIAL) 'OK)
(CHECK '(SQR 3) 9)
(CHECK '(FACTORIAL 7) 5040)
(CHECK '(UNTRACE SQR FACTORIAL) NIL)

; EVALQUOTE
(CHECK '(LOAD "tests.txt" T) T)
(CHECK '(EVALQUOTE PLUS (2 3))   5)
(CHECK '(EVALQUOTE CSET (A B))   'B)
(CHECK '(EVALQUOTE EVAL (A NIL)) 'B)
(PRINT (EVALQUOTE EVAL (OBLIST NIL)))

; QuickSort
(TERPRI)
(CHECK '(LOAD "qsort.lsp") T)
(CHECK '(QSORT '(7 4 3 9 1 8 2 5 3 6)) '(1 2 3 3 4 5 6 7 8 9))

; Menge aller Partitionen einer Menge
(TERPRI)
(CHECK '(LOAD "part.lsp") T)
(CHECK '(PARTITIONS '(1 2)) '(((1) (2)) ((1 2))))
(CHECK '(PART       '(1 2 3) 3) '(((1) (2) (3))))
