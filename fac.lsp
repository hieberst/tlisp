(DEFUN FACTORIAL (N)
    (COND ((ZEROP N) 1)
          (T (TIMES N (FACTORIAL (SUB1 N))))
))

(DEFUN FAC2 (N)
    (PROG (Y)
             (SETQ Y 1)
        TAG1 (COND ((ZEROP N) (RETURN Y)))
             (SETQ Y (TIMES N Y))
             (SETQ N (SUB1 N))
             (GO TAG1)
))
