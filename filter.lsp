; $Id$
;
; Filtern aller  Elemente aus der Liste LI,
; welche das Praedikat FN nicht erfuellen.

(LOAD "rev.lsp")

(DEFUN FILTER (LI FN)
    (PROG (X AKK)
     LOOP (COND ((NULL LI) (RETURN (REV1 AKK))))
          (SETQ X  (CAR LI))
          (SETQ LI (CDR LI))
          (COND ((FN X) (SETQ AKK (CONS X AKK))))
          (GO LOOP)))
