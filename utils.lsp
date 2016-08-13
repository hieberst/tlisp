; $Id$

(DEFUN CONCAT1 (LI)
    (COND ((NULL LI) NIL)
          (T (APPEND (CAR LI) (CONCAT1 (CDR LI))))))

(DEFUN CONCAT (LI)
    (PROG (AKK)
     LOOP (COND ((NULL LI) (RETURN AKK)))
          (SETQ AKK (APPEND AKK (CAR LI)))
          (SETQ LI (CDR LI))
          (GO LOOP)))

(DEFUN SEQ (N)
    (PROG (X)
     LOOP (COND ((ZEROP N) (RETURN X)))
          (SETQ X (CONS N X))
          (SETQ N (1- N))
          (GO LOOP)))

(DEFUN TAKE1 (N L)
    (COND ((OR (NULL L) (ZEROP N)) NIL)
          (T (CONS (CAR L) (TAKE (1- N) (CDR L))))))

(DEFUN TAKE (N L)
    (PROG (S)
     LOOP (COND ((OR (NULL L) (ZEROP N)) (RETURN (REVERSE S))))
          (SETQ S (CONS (CAR L) S))
          (SETQ N (1- N))
          (SETQ L (CDR L))
          (GO LOOP)))
