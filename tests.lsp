; $Id$

(DEFSP CHECK
    (PROG (result ok)
          (SETQ result (EVAL (CAR argl) ali))
          (SETQ ok (EQUAL result (CADR argl)))
          (PRIN (CAR argl))
          (PRIN2 " --> ")
          (PRIN (CADR argl))
          (PRIN2 " = ")
          (PRIN result)
          (PRIN2 " : ")
          (RETURN (PRINT ok))
    ))

(CHECK (LOAD "sqr.lsp") T)

(CHECK (CONS 'A 'B)              (A . B))
(CHECK (PAIR '(A B C) '(D E F))  ((A . D) (B . E) (C . F)))
(CHECK (APPEND '(A B C) 'D)      (A B C . D))
(CHECK (PLUS)                    0)
(CHECK (PLUS 1 2 3)              6)
(CHECK (TIMES)                   1)
(CHECK (LAMBDA FOO)              (LAMBDA FOO))
(CHECK ((LAMBDA (N) (ADD1 N)) 1) 2)
(CHECK (SQR 3)                   9)
(CHECK (CADR '(A B C))           B)
(CHECK (CADR '(A,B,C))           B)                 ; Komma als Listentrenner
(CHECK ''A                       (QUOTE A))         ; Mehrfach-Quote
(CHECK #'A                       (FUNARG A NIL))

; EVAL und APPLY
(CHECK (CSETQ A 4)          4)
(CHECK (EVAL 'A)            4)
(CHECK (EVAL 'A '((A . 5))) 5)
(CHECK (APPLY #'SQR '(3))   9)
(CHECK (REMPROP 'A APVAL)   T)

; More than one sexpr per line
(CHECK (PLUS 1 2) 3) (CHECK (TIMES 2 3) 6)

; LOAD
(CHECK (LOAD "empty.lsp")     T)
(CHECK (LOAD "missing.lsp")   NIL)
(CHECK (LOAD "missing.txt" T) NIL)

; PROP
(CHECK (CSETQ A 3) 3)
(CHECK (DEFUN FOO () (PRINT "FOO")) FOO)
(CHECK (PROP 'A  APVAL   NIL) (3))
(CHECK (PROP 'A  APVAL  'FOO) (3))
(CHECK (PROP 'A 'APVAL2  NIL) NIL)
(CHECK (PROP 'A 'APVAL2 'FOO) "FOO")
(CHECK (REMPROP 'FOO EXPR)    T)
(CHECK (REMPROP 'A   APVAL)   T)

; Fakultaet
(TERPRI)
(CHECK (LOAD "fac.lsp") T)
(CHECK (FACTORIAL 7) 5040)
(CHECK (FAC2      7) 5040)
(CHECK (FAK       7) 5040)

; Fibonacci-Folge
(TERPRI)
(CHECK (LOAD "fib.lsp") T)
(CHECK (FIB1 10) 55)
(CHECK (FIB2 10) 55)

; Groesster gemeinsamer Teiler
(TERPRI)
(CHECK (LOAD "gcd.lsp") T)
(CHECK (GCD 27 6) 3)
(CHECK (GCD1 27 6) 3)
(CHECK (GCD2 27 6) 3)

; Umkehrung einer Liste
(TERPRI)
(CHECK (LOAD "rev.lsp")      T)
(CHECK (REV  '(A B C))       (C B A))
(CHECK (REV1 '(A B C))       (C B A))
(CHECK (REV2 '(A B C))       (C B A))
(CHECK (REV3 '(A B C))       (C B A))
(CHECK (REVALL '(A (B C) D)) (D (C B) A))

; MAP-Funktionen
(TERPRI)
(CHECK (LOAD "map.lsp")                                        T)
(CHECK (MAPCAR  '(1 2 3 4) 'SQR)                               (1 4 9 16))
(CHECK (MAPC    '(1 2 3 4) (LAMBDA (N) (PRINT (SQR N))))       NIL)
(CHECK (MAPLIST '(1 2 3 4) (LAMBDA (L) (SQR (CAR L))))         (1 4 9 16))
(CHECK (MAP     '(1 2 3 4) (LAMBDA (L) (PRINT (SQR (CAR L))))) NIL)

; Pascal'sches Dreieck (MAPLIST)
(TERPRI)
(CHECK (LOAD "pasc.lsp") T)
(CHECK (PASC 4)          (1 4 6 4 1))

; Mengenoperationen
(TERPRI)
(CHECK (LOAD "set.lsp")                 T)
(CHECK (UNION '(A B C) '(B C D))        (A B C D))
(CHECK (INTERSECTION '(A B C) '(B C D)) (B C))
(CHECK (MEMBER 'C '(A B C))             T)
(CHECK (MEMBER 'B '(A (B) C))           NIL)

; Mustererkennung
(TERPRI)
(CHECK (LOAD "match.lsp")                                      T)
(CHECK (SMATCH '(DAS "?" HAUS "*") '(DAS GELBE HAUS IST ALT))  T)
(CHECK (SMATCH '("*" IST EIN "?") '(DER LOEWE IST KEIN VOGEL)) NIL)
(CHECK (MATCH '(DAS ?X HAUS *Y) '(DAS GELBE HAUS IST ALT))     T)
(CHECK X                                                       GELBE)
(CHECK Y                                                       (IST ALT))

; T und NIL
(TERPRI)
(CHECK (ATOM  T)              T)
(CHECK (LISTP T)              NIL)
(CHECK (ATOM  NIL)            T)
(CHECK (LISTP NIL)            T)
(CHECK ((NULL '(A)) (EXIT))   NIL)
(CHECK ((NULL ()) (PLUS 1 2)) 3)

; Funktionale Argumente
(TERPRI)
(CHECK (DEFUN TWICE (FN N) (FN (FN N))) TWICE)
(CHECK (TWICE  'SQR 2) 16)
(CHECK (TWICE #'SQR 3) 81)

; Anonyme LAMBDA-Ausdruecke
(TERPRI)
(CHECK (LOAD "split.lsp") T)
(CHECK (SPLIT  1234) (12 . 34))
(CHECK (SPLIT1 1234) (12 . 34))

; Funktionale Argumente (FUNARG)
(TERPRI)
(CHECK (LOAD "sqx.lsp") T)

; Tracing
(TERPRI)
(CHECK (LOAD "trace.lsp") T)
(CHECK (TRACE SQR FACTORIAL) OK)
(CHECK (SQR 3) 9)
(CHECK (FACTORIAL 7) 5040)
(CHECK (UNTRACE SQR FACTORIAL) NIL)

; EVALQUOTE
(TERPRI)
(CHECK (LOAD "tests.txt" T) T)
(CHECK (EVALQUOTE PLUS (2 3))   5)
(CHECK (EVALQUOTE CSET (A B))   B)
(CHECK (EVALQUOTE EVAL (A NIL)) B)
(PRINT (EVALQUOTE EVAL (OBLIST NIL)))

; Filterfunktionen
(TERPRI)
(CHECK (LOAD "filter.lsp") T)
(CHECK (REMOVE-IF     (LAMBDA (X) (GREATERP X 3)) '(1 2 3 4 5)) (1 2 3))
(CHECK (REMOVE-IF-NOT (LAMBDA (X) (GREATERP X 3)) '(1 2 3 4 5)) (4 5))

; Common Lisp
(TERPRI)
(CHECK (LOAD "clisp.lsp") T)
(CHECK (CSETQ A 4)          4)
(CHECK (FUNCALL #'SQR A)    16)
(CHECK (REMPROP 'A APVAL)   T)
(CHECK (EVENP 1)            NIL)
(CHECK (EVENP 2)            T)
(CHECK (IF (EVENP 2) 'even 'odd) even)
(CHECK (NTH 2 '(1 2 3 4 5)) 3)

; QuickSort
(TERPRI)
(CHECK (LOAD "qsort.lsp") T)
(CHECK (QSORT '(7 4 3 9 1 8 2 5 3 6)) (1 2 3 3 4 5 6 7 8 9))

; Potenzmenge
(TERPRI)
(CHECK (LOAD "powerset.lsp") T)
(CHECK (LENGTH (POWERSET '(1 2 3))) 8)

; Permutationen
(TERPRI)
(CHECK (LOAD "perm.lsp") T)
(CHECK (PERM-ZERL '(4 7 6 1 2 3 5)) ((4 1) (7 5 2) (6 3)))

; Menge aller Partitionen einer Menge
(TERPRI)
(CHECK (LOAD "part.lsp") T)
(CHECK (PARTITIONS '(1 2)) (((1) (2)) ((1 2))))
(CHECK (B 3) 5)

; Stirling-Zahlen
(TERPRI)
(CHECK (LOAD "stirling.lsp") T)
(CHECK (S1A 3 2) 3)
(CHECK (S1B 4 2) 11)
(CHECK (S2A 4 2) 7)
(CHECK (S2B 4 2) 7)
