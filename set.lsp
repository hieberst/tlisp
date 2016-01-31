; MEMBER ist bereits als SUBR vorhanden
(DEFUN MEMBER (EL MENGE)
    (COND ((NULL MENGE) NIL)
          ((EQ EL (CAR MENGE)) T)
          (T (MEMBER EL (CDR MENGE)))))

(DEFUN UNION (M1 M2)
    (COND ((NULL M1) M2)
          ((NULL M2) M1)
          ((MEMBER (CAR M1) M2) (UNION (CDR M1) M2))
          (T                    (UNION (CDR M1) (CONS (CAR M1) M2)))))

(DEFUN INTERSECTION (M1 M2)
    (COND ((NULL M1) NIL)
          ((NULL M2) NIL)
          ((MEMBER (CAR M1) M2)
                (CONS (CAR M1) (INTERSECTION (CDR M1) M2)))
          (T                   (INTERSECTION (CDR M1) M2))))
