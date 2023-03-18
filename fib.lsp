; Berechnung der Fibonacci-Zahlen
; siehe "LISP" von Dieter Mueller

; Rekursive Version, siehe Seite 70
(DEFUN FIB1 (J)
    (COND ((EQ J 0) 0) ((EQ J 1) 1)
          (T (PLUS (FIB1 (DIFFERENCE J 1))
                   (FIB1 (DIFFERENCE J 2))))
))

; Iterative Version, siehe Seite 83
(DEFUN FIB2 (J)
    (PROG (A B AH K)
          (SETQ A 1) (SETQ B 0)
          (SETQ K 0)
     SCHL (COND ((EQ K J) (RETURN B)))
          (SETQ AH A)
          (SETQ A (PLUS A B))
          (SETQ B AH)
          (SETQ K (ADD1 K))
          (GO SCHL)
))
