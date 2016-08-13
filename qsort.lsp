; $Id$

; QuickSort-Implementierung mit naiver Auswahl des Pivot-Elements und
; damit schlechtem Laufzeitverhalten bei bereits sortierten Listen.

(DEFUN QSORT (LI)
    (PROG (P L R)
          (COND ((NULL LI) (RETURN NIL)))
          (SETQ P  (CAR LI))                ; Pivot
          (SETQ LI (CDR LI))
          (SETQ L (QSORT (REMOVE-IF-NOT #'(LAMBDA (X) (<= X P)) LI)))
          (SETQ R (QSORT (REMOVE-IF-NOT #'(LAMBDA (X) (>  X P)) LI)))
          (RETURN (APPEND L (CONS P R)))))
