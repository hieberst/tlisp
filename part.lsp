; $Id$

(load "concat.lsp")
(load "filter.lsp")
(load "map.lsp")

; Menge aller Partitionen einer Menge LI
(DEFUN PARTITIONS (LI)
    (PROG (X INSERT)
          (COND ((NULL LI) (RETURN '(()))))
          (SETQ X  (CAR LI))
          (SETQ LI (CDR LI))
          (SETQ INSERT (LAMBDA (LI X)
                (COND ((NULL LI) NIL)
                      (T (CONS (CONS (CONS X (CAR LI)) (CDR LI))
                         (MAPCAR (INSERT (CDR LI) X)
                            (FUNCTION (LAMBDA (Y) (CONS (CAR LI) Y)))))))))
          (RETURN (CONCAT1 (MAPCAR (PARTITIONS LI) (FUNCTION
                  (LAMBDA (Y) (CONS (CONS (LIST X) Y) (INSERT Y X)))))))))


; Menge aller Partitionen der Maechtigkeit N einer Menge LI
(DEFUN PART (LI N) (FILTER (PARTITIONS LI)
                           (FUNCTION (LAMBDA (X) (EQ N (LENGTH X))))))
