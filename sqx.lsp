; $Id$
;
; Gebundene und freie Variablen, das FUNARG-Problem
; siehe "LISP" von Dieter Mueller, Seite 124ff

; Im Unterschied zu obigem Buch weicht die Ausgabe
; von FUNKTAB unter TLISP wie folgt ab:
;
; 1. Die Reihenfolge der Bindungen der Funktionsparameter an die A-Liste
;    erfolgt mittels PAIR und ist damit nicht invertiert.
;
; 2. (FUNCTION <Funktion>) ::> (FUNARG <Funktion> <momentane A-Liste>)
;    analog zum klassischen LISP-Interpreter auf Seite 158f
;
; D.h. statt wie im Buch beispielsweise
;
;   ((F FUNARG (NIL (TIMES X X)) NIL) (X.2))
;
; lautet die Ausgabe in TLISP stattdessen
;
;   ((X . 2) (F FUNARG SQX NIL))

(DEFUN SQX () (TIMES X X))

(CSETQ X 10)

(DEFUN FUNKTAB (X F)
    (COND ((LESSP X 0) NIL)
          (T (PROGN
                (PRINT (ALIST))
                (CONS (CONS X (F)) (FUNKTAB (SUB1 X) F))))))


(PRINT (SQX))
(PRINT (FUNKTAB 3 'SQX))
(PRINT (FUNKTAB 2 (FUNCTION SQX)))
