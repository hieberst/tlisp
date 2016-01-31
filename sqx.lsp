(DEFUN SQX () (TIMES X X))

(CSETQ X 10)

(DEFUN FUNKTAB (X F)
    (COND ((LESSP X 0) NIL)
          (T (PROGN 
                (PRINT (ALIST))
                (CONS (CONS X (F)) (FUNKTAB (SUB1 X) F))))))


(PRINT (SQX))
(PRINT (FUNKTAB 3 'SQX))
(PRINT (FUNKTAB 2 (FUNCTION SQX)))
