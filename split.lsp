; Anonyme LAMBDA-Ausdruecke
; siehe "LISP" von Dieter Mueller
; Kapitel 5.4 Seite 76f

; doppelte Berechung der ganzzahligen Division
(DEFUN SPLIT (N)
    (CONS (QUOTIENT N 100)
          (DIFFERENCE N (TIMES 100 (QUOTIENT N 100)))))

; einfache Berechnung mit Hilfe eines anonymen LAMBDA-Ausdrucks
(DEFUN SPLIT1 (N)
    ((LAMBDA (Z) (CONS Z (DIFFERENCE N (TIMES Z 100))))
     (QUOTIENT N 100)))
