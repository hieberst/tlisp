; TLISP default definitions

(PUTPROP 'DEFLIST
	(LAMBDA (X Y)
        (COND ((NULL X) NIL)
              (T (PROGN (PUTPROP (CAAR X) (CADAR X) Y)
                        (APPEND (LIST (CAAR X)) (DEFLIST (CDR X) Y))
                  ))))
	EXPR)

(DEFLIST '((DEFINE (LAMBDA (X)   (DEFLIST X EXPR)))
           (CSET   (LAMBDA (X Y) (PUTPROP X Y APVAL)))
           (DEBUG  (LAMBDA (X)   (PUTPROP 'CONFIG X 'DEBUG)))
           (ECHO   (LAMBDA (X)   (PUTPROP 'CONFIG X 'ECHO))))
         EXPR)

(DEFINE '((DEF (LAMBDA (FN PL FORM)
		(DEFINE (LIST (LIST FN
				(LIST LAMBDA PL FORM)
		)))
	))))

(DEFLIST '((DEFUN (LAMBDA (ARGL ALI)
    (CAR (DEFINE (LIST (LIST (CAR ARGL)
                        (LIST LAMBDA (CADR ARGL) (CADDR ARGL))
                   )))
    )))) FEXPR)

(DEFLIST '((DEFSP (LAMBDA (P1 P2)
    (DEFLIST (LIST (LIST (CAR P1)
                         (LIST LAMBDA '(ARGL ALI) (CADR P1))
             )) FEXPR)
    ))) FEXPR)

(DEFSP ALIST ALI)
(DEFSP CSETQ (PUTPROP (CAR ARGL) (EVAL (CADR ARGL) ALI) APVAL))

(DEFUN EQUAL (X Y)
    (COND ((EQ X Y) T)
          ((ATOM X) NIL) ((ATOM Y) NIL)
          ((EQUAL (CAR X) (CAR Y)) (EQUAL (CDR X) (CDR Y)))
          (T NIL)))
