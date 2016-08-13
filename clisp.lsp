; Some Common Lisp definitions
;
; clisp    cd
; gcl      si:chdir
; ccl      (:cd "d:/work/src/tlisp")
; sbcl     sb-posix:chdir       setf default-pathname-defaults #P../

; Conditionals

(DEFSP IF (COND ((EVAL (CAR ARGL) ALI) (EVAL (CADR  ARGL) ALI))
                ((NULL (CDDR ARGL))    NIL)
                (T                     (EVAL (CADDR ARGL) ALI))))

(DEFSP WHEN (COND ((EVAL (CAR ARGL) ALI) (EVAL (CONS 'PROGN (CDR ARGL)) ALI))
                  (T NIL)))

(DEFSP UNLESS (APPLY 'WHEN (CONS (CONS 'NOT (LIST (CAR ARGL))) (CDR ARGL))))

; Functions

(DEFSP FUNCALL (APPLY (EVAL (CAR ARGL) ALI)
                      (EVAL (CONS 'LIST (CDR ARGL)) ALI)))

(DEFUN FUNCTIONP (FN)
    (COND ((SYMBOLP FN) (COND ((GET FN 'EXPR)  T)
                              ((GET FN 'FEXPR) T)
                              ((GET FN 'SUBR)  T)
                              ((GET FN 'FSUBR) T)
                              (T NIL)))
          ((LISTP FN) (COND ((EQ (CAR FN) 'FUNARG) (FUNCTIONP (CADR FN)))
                            (T NIL)))
          (T NIL)))

; Lists

(DEFUN MAPCAR (FCT LI)      ; siehe map.lsp mit gedrehter Parameterreihenfolge
    (PROG (S)
     LOOP (COND ((NULL LI) (RETURN (REVERSE S))))
          (SETQ S (CONS (FCT (CAR LI)) S))
          (SETQ LI (CDR LI))
          (GO LOOP)))

(DEFUN NTH (N L)
    (PROG ()
     LOOP (COND ((NULL L)  (RETURN NIL))
                ((ZEROP N) (RETURN (CAR L))))
          (SETQ N (SUB1 N))
          (SETQ L (CDR L))
          (GO LOOP)))

(DEFUN NTHCDR (N L)     ; DROP
    (COND ((OR (NULL L) (ZEROP N)) L)
          (T (NTHCDR (1- N) (CDR L)))))

; Comparision operators

(DEFUN =  (X Y) (EQUAL X Y))
(DEFUN <  (X Y) (LESSP X Y))
(DEFUN <= (X Y) (OR (LESSP X Y) (EQ X Y)))
(DEFUN >  (X Y) (GREATERP X Y))
(DEFUN >= (X Y) (OR (GREATERP X Y) (EQ X Y)))

; Arithmetic operators

(DEFSP + (APPLY 'PLUS       ARGL))
(DEFSP - (APPLY 'DIFFERENCE ARGL))
(DEFSP * (APPLY 'TIMES      ARGL))

(DEFUN 1+ (N) (ADD1 N))
(DEFUN 1- (N) (SUB1 N))

(DEFUN ODDP  (N) (EQ (REMAINDER N 2) 1))
(DEFUN EVENP (N) (NOT (ODDP N)))

; Utilities

(DEFUN QUIT () (EXIT))

; Load other files

(UNLESS (FUNCTIONP 'REMOVE-IF) (LOAD "filter.lsp"))
