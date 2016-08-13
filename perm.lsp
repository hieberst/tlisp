; $Id$
;
; Permutationen

(LOAD "utils.lsp")

(DEFUN PERM (L) (PERMH L 0))

(DEFUN PERMH (L N)
    (PROG (P S)
          (COND ((NULL L) (RETURN NIL)))
          (SETQ P (TAKE   N L))                 ; prefix
          (SETQ S (NTHCDR N L))                 ; suffix
          (RETURN (APPEND (PERMS P S)
                          (COND ((NULL S) NIL)
                                (T        (PERMH L (1+ N))))))))

(DEFUN PERMS (P S)
    (PROG (L X)
          (COND ((NULL S) (RETURN NIL)))
          (SETQ L (PERM (APPEND P (CDR S))))
          (SETQ X (CAR S))
          (RETURN (COND ((NULL L) (CONS (CONS X NIL) NIL))
                        (T        (PREPEND L X))))))

(DEFUN PREPEND (LI X) (MAPCAR #'(LAMBDA (L) (CONS X L)) LI))

(DEFUN PERM-ZERL (P)
    (PROG (N A B Z L)
          (SETQ N (SEQ (LENGTH P)))
     L1   (COND ((NULL N) (RETURN (REVERSE L))))
          (SETQ A (CAR N))
          (SETQ B A)
     L2   (SETQ B (NTH (1- B) P))
          (SETQ Z (CONS B Z))
          (COND ((EQ A B) (GO L3)))
          (SETQ N (REMOVE-IF #'(LAMBDA (X) (EQ X B)) N))
          (GO L2)
     L3   (SETQ L (CONS (REVERSE Z) L))
          (SETQ Z NIL)
          (SETQ N (REMOVE-IF #'(LAMBDA (X) (EQ X A)) N))
          (GO L1)))
