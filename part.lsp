; $Id$

(LOAD "utils.lsp")

; Menge aller Partitionen einer Menge LI
(DEFUN PARTITIONS (LI)
    (PROG (X INSERT)
          (COND ((NULL LI) (RETURN '(()))))
          (SETQ X  (CAR LI))
          (SETQ LI (CDR LI))
          (SETQ INSERT (LAMBDA (LI X)
                (COND ((NULL LI) NIL)
                      (T (CONS (CONS (CONS X (CAR LI)) (CDR LI))
                         (MAPCAR #'(LAMBDA (Y) (CONS (CAR LI) Y))
                                 (FUNCALL INSERT (CDR LI) X)))))))
          (RETURN (CONCAT (MAPCAR
                #'(LAMBDA (Y) (CONS (CONS (LIST X) Y) (FUNCALL INSERT Y X)))
                (PARTITIONS LI))))))

; Bellsche Zahl B
(DEFUN B (N) (LENGTH (PARTITIONS (SEQ N))))
