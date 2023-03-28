; Differentiation
; siehe "LISP" von Dieter Mueller, Seite 22ff

; Abweichungen zum Buch:
; 1. Fuer die Subtraktion wird DIFFERENCE anstelle von MINUS verwendet,
;    da MINUS in TLISP wie in LISP 1.5 die Negation ist.
; 2. In der M-Notation auf Seite 15 wird der Nenner in der
;    Quotientenregel falsch berechnet.

(DEFUN DIFF (EX V)
    (COND ((ATOM EX) (COND ((EQ EX V) 1)
                           (T         0)))        ; Konstante
          ; Summenregel
          ((EQ (CAR EX) 'PLUS)
               (LIST 'PLUS (DIFF (CADR EX) V) (DIFF (CADDR EX) V)))
          ; Produktregel
          ((EQ (CAR EX) 'TIMES)
               (LIST 'PLUS
                     (LIST 'TIMES (DIFF (CADR EX) V) (CADDR EX))
                     (LIST 'TIMES (CADR EX) (DIFF (CADDR EX) V))))
          ; Differenzregel
          ((EQ (CAR EX) 'DIFFERENCE)
               (LIST 'DIFFERENCE (DIFF (CADR EX) V) (DIFF (CADDR EX) V)))
          ; Quotientenregel
          ((EQ (CAR EX) 'QUOTIENT)
               (LIST 'QUOTIENT
                    (LIST 'DIFFERENCE
                         (LIST 'TIMES (DIFF (CADR EX) V) (CADDR EX))
                         (LIST 'TIMES (CADR EX) (DIFF (CADDR EX) V)))
                    (LIST 'TIMES (CADDR EX) (CADDR EX))))
))
