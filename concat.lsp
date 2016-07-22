; $Id$

(DEFUN CONCAT (LI)
    (COND ((NULL LI) NIL)
          (T (APPEND (CAR LI) (CONCAT (CDR LI))))))

(DEFUN CONCAT1 (LI)
    (PROG (AKK)
     LOOP (COND ((NULL LI) (RETURN AKK)))
          (SETQ AKK (APPEND AKK (CAR LI)))
          (SETQ LI (CDR LI))
          (GO LOOP)))
