; Pascal'sches Dreieck
; siehe "LISP" von Dieter Mueller, Kapitel 9.2, Seite 121f

(LOAD "map.lsp")

(DEFUN PLUS12 (LI)
    (COND ((NULL (CDR LI)) (CAR LI))
          (T (PLUS (CAR LI) (CADR LI)))))

(DEFUN PASC (NMAX)
    (COND ((ZEROP NMAX) (PRINT (LIST 1)))
          (T            (PRINT (CONS 1
                                     (MAPLIST (PASC (SUB1 NMAX))
                                              'PLUS12))))))
