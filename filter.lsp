; $Id$

; Filtern aller  Elemente aus der Liste LI,
; welche das Praedikat FN erfuellen.

(DEFUN REMOVE-IF (FN LI)
    (REMOVE-IF-NOT #'(LAMBDA (X) (NOT (FN X))) LI))

; Filtern aller  Elemente aus der Liste LI,
; welche das Praedikat FN nicht erfuellen.

(DEFUN REMOVE-IF-NOT (FN LI)
    (PROG (X AKK)
     LOOP (COND ((NULL LI) (RETURN (REVERSE AKK))))
          (SETQ X  (CAR LI))
          (SETQ LI (CDR LI))
          (COND ((FN X) (SETQ AKK (CONS X AKK))))
          (GO LOOP)))
