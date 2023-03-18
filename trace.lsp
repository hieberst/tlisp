; $Id$
;
; siehe "LISP" von Dieter Mueller
;   Kapitel 11, Seiten 138-141 "Tracing"
;
; Erweiterung gemaess LISP 1.5 Programmers Manual:
;   The argument of TRACE is a list of functions

; AUFAB[INCR;FNAME] mit INCR=(CAR ARGL), FNAME=(CADR ARGL)
(DEFSP AUFAB
    (PROG (K)
            (CSETQ ST (PLUS ST (CAR ARGL)))
;           (DEFLIST (LIST (LIST 'ST (PLUS ST (CAR ARGL)))) APVAL)
            (TERPRI)
            (SETQ K ST)
     LOOP   (COND ((LESSP K 1) (GO W)))
            (PRIN2 '"-")
            (SETQ K (SUB1 K))
            (GO LOOP)
     W      (COND ((LESSP (CAR ARGL) 0) (PRIN2 '"=")))
            (PRIN  (LIST (CADR ARGL)))                      ; PRIN2
))

(DEFSP TRACE
    (PROG (FN PNEU PARLI)
            (CSETQ ST 0)                                    ; Stufenzaehler
     NEXT   ((NULL ARGL) (RETURN 'OK))
            (SETQ FN (CAR ARGL))
            (COND ((ATOM FN) (GO ATOM)))
            (EVAL (CONS 'TRACE FN) ALI)
            (RETURN NIL)
     ATOM   (COND ((GET FN 'ALTEXPR) (GO WEITER))
                  ((GET FN FEXPR)    (GO CHANGE))
                  ((GET FN EXPR)     (GO CHANGE)))
            (GO WEITER)
     CHANGE (SETQ PARLI (CADADDR FN))
            (RPLACD (CDR FN) (CONS 'ALTEXPR (CDDR FN)))
            (SETQ PNEU (LIST 'LAMBDA PARLI
                (LIST 'PROG (LIST '"#")
                            (LIST 'AUFAB 1 FN)
                            (LIST 'SETQ '"#"
                                (CONS (LIST 'GET (LIST 'QUOTE FN)
                                                 (LIST 'QUOTE 'ALTEXPR)) PARLI))
                            (LIST 'AUFAB -1 FN)
                            (LIST 'RETURN (LIST 'PRINT '"#"))
                )))
            (RPLACD (CDR FN) (CONS PNEU (CDDR FN)))
     WEITER (SETQ ARGL (CDR ARGL))
            (GO NEXT)
))

(DEFSP UNTRACE
    (PROG (K M)
     A      (COND ((NULL ARGL) (RETURN NIL)))
            (SETQ K (CAR ARGL))
            (COND ((ATOM K) (GO ATOM)))
            (EVAL (CONS 'UNTRACE K) ALI)
            (RETURN NIL)
     ATOM   (COND ((SETQ M (GET K 'ALTEXPR))
                   (RPLACD (CDR K) (LIST M))))
            (SETQ ARGL (CDR ARGL))
            (GO A)
))
