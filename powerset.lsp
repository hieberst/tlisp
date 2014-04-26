; Berechnung der Potenzmenge einer Menge
;
; siehe "Introduction to Standard ML" von Robert Harper
;       Aufgabe 2.5.7 Seite 30

(LOAD "map.lsp")

(DEFUN POWERSET (X)
    (COND ((NULL X) '(()))
          (T        (PROG (TMP)
                          (SETQ TMP (POWERSET (CDR X)))
                          (RETURN (APPEND TMP
                                  (MAPCAR TMP (LAMBDA (L) (CONS (CAR X) L)))))
                    ))))
