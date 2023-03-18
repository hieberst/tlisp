; Fakultaet (Faktorielle)

; rekursiv
(DEFUN FACTORIAL (N)
    (COND ((ZEROP N) 1)
          (T (TIMES N (FACTORIAL (SUB1 N))))
))

; iterativ
(DEFUN FAC2 (N)
    (PROG (Y)
             (SETQ Y 1)
        TAG1 (COND ((ZEROP N) (RETURN Y)))
             (SETQ Y (TIMES N Y))
             (SETQ N (SUB1 N))
             (GO TAG1)
))

; Lambda-Kalkuel
; siehe "LISP" von Dieter Mueller, Kapitel 15.2, Seite 181ff

(DEFUN FAK (N)
    ((YCU (FUNCTION EFAK)) N))

(DEFUN EFAK (PSI)
    (FUNCTION (LAMBDA (N) (COND ((ZEROP N) 1)
                                (T (TIMES N (PSI (DIFFERENCE N 1))))))
))

; Currys paradoxer Y-Kombinator mit Parameter U
(DEFUN YCU (E)
    ((FUNCTION (LAMBDA (X)
        (FUNCTION (LAMBDA (U)
            ((E (X X)) U)))))
     (FUNCTION (LAMBDA (X)
        (FUNCTION (LAMBDA (U)
            ((E (X X)) U)))))
))
