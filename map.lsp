; MAP-Funktionen
; siehe "LISP" von Dieter Mueller, Kapitel 9.2, Seiten 118-122

(DEFUN MAPCAR (LI FCT)
    (COND ((NULL LI) NIL)
          (T (CONS (FCT (CAR LI))
                   (MAPCAR (CDR LI) FCT)))))

(DEFUN MAPC (LI FCT)
    (PROG ()
     LOOP (COND ((NULL LI) (RETURN NIL)))
          (FCT (CAR LI))
          (SETQ LI (CDR LI))
          (GO LOOP)
))

(DEFUN MAPLIST (LI FCT)
    (COND ((NULL LI) NIL)
          (T (CONS (FCT LI)
                   (MAPLIST (CDR LI) FCT)))))

(DEFUN MAP (LI FCT)
    (PROG ()
     LOOP (COND ((NULL LI) (RETURN NIL)))
          (FCT LI)
          (SETQ LI (CDR LI))
          (GO LOOP)
))
