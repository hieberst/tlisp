; $Id$
;
; QuickSort-Implementierung mit naiver Auswahl des Pivot-Elements und
; damit schlechtem Laufzeitverhalten bei bereits sortierten Listen.

(LOAD "filter.lsp")

(DEFUN <= (X Y) (OR (LESSP X Y) (EQ X Y)))

(DEFUN QSORT (LI)
    (PROG (P L R)
          (COND ((NULL LI) (RETURN NIL)))
          (SETQ P  (CAR LI))                ; Pivot
          (SETQ LI (CDR LI))
          (SETQ L (QSORT (FILTER LI #'(LAMBDA (X) (<=       X P)))))
          (SETQ R (QSORT (FILTER LI #'(LAMBDA (X) (GREATERP X P)))))
          (RETURN (APPEND L (CONS P R)))))
