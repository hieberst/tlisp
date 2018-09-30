; GOVOL = Gary's Own Version of Lisp
; Interpreting LISP: Programming and Data Structures, 2nd edition, 2017
; Gary D. Knott

; Get and set the property list of an atom
(DEFUN GETPLIST (A)   (CDR A))
(DEFUN PUTPLIST (A L) (RPLACD A L))

; Return the value of X raised to the power of Y
(DEFUN POWER (X Y)
    (COND ((ZEROP Y) 1)
          ((LESSP Y 0) (QUOTIENT 1 (POWER X (MINUS Y))))
          (T (TIMES X (POWER X (SUB1 Y))))
))

; Return the body of a function
(DEFSP BODY
    (PROG (X Y)
          (SETQ X '(EXPR FEXPR NEXPR)) ; function types
     LOOP (COND ((NULL X) (PROG2 (PRINT "::illegal BODY argument")
                                 (RESET))))
          (SETQ Y (GET (CAR ARGL) (CAR X)))
          (COND (Y (RETURN (CONS (CADR Y) (CADDR Y)))))
          (SETQ X (CDR X))
          (GO LOOP)
))

; SETQ
(PUTPLIST 'SETQ NIL)

(RPLACD '$ASSOC   (COPY-LIST (CDR 'ASSOC)))
(RPLACD '$PUTPROP (COPY-LIST (CDR 'PUTPROP)))

(DEFSP SETQ (PROGN
    (PUTPLIST (CAR ARGL) NIL)
    (COND (($ASSOC (CAR ARGL) ALI)
           (SET (CAR ARGL) (EVAL (CADR ARGL) ALI)))
          ((NUMBERP (CADR ARGL))
           ($PUTPROP (CAR ARGL) (EVAL (CADR ARGL) ALI) APVAL))
          ((SYMBOLP (CADR ARGL))
           (RPLACD (CAR ARGL) (COPY-LIST (CDR (CADR ARGL)))))
          ((EQ (CAADR argl) 'LAMBDA)
           ($PUTPROP (CAR ARGL) (CADR ARGL) EXPR))
          ((EQ (CAADR argl) 'SPECIAL)
           ($PUTPROP (CAR ARGL) (RPLACA (CADR ARGL) 'LAMBDA) NEXPR))
          (T
           ($PUTPROP (CAR ARGL) (EVAL (CADR ARGL) ALI) APVAL))
)))

(SETQ DO PROGN)
(SETQ SUM PLUS)
