; Invertieren einer Liste
; siehe "LISP" von Dieter Mueller

; Quadratischer Speicherbedarf O(n^2), siehe Seite 74
(DEFUN REV (LI)
    (COND ((NULL LI) NIL)
          (T (APPEND (REV (CDR LI)) (CONS (CAR LI) NIL)))
))

; Linearer Speicherbedarf O(n), siehe Seite 75
(DEFUN REVH (XRED AKK)
    (COND ((NULL XRED) AKK)
          (T (REVH (CDR XRED) (CONS (CAR XRED) AKK)))
))
(DEFUN REV1 (X) (REVH X NIL))

(DEFUN REV2 (X)
    (PROG (AKK XRED)
          (SETQ XRED X)
     LOOP (COND ((NULL XRED) (RETURN AKK)))
          (SETQ AKK (CONS (CAR XRED) AKK))
          (SETQ XRED (CDR XRED))
          (GO LOOP)
))

(DEFUN REV3 (X)
    (PROG (AKK)
     LOOP (COND ((NULL X) (RETURN AKK)))
          (SETQ AKK (CONS (CAR X) AKK))
          (SETQ X (CDR X))
          (GO LOOP)
))

(DEFUN REVALL (Y)
    (COND ((ATOM Y) Y)
          ((NULL Y) NIL)
          (T (APPEND (REVALL (CDR Y))
                     (CONS (REVALL (CAR Y)) NIL)))
 ))
