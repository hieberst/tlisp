; Mustererkennung
; siehe "LISP" von Dieter Mueller, Kapitel 13, Seite 151ff

; Einfache (simple) Mustererkennung

(DEFUN SMATCH (PATT OB)

    (COND ((AND (NULL PATT) (NULL OB)) T)
          ((OR (NULL PATT) (NULL OB)) NIL)
          ((OR (EQUAL (CAR PATT) '"?") (EQUAL (CAR PATT) (CAR OB)))
           (SMATCH (CDR PATT) (CDR OB)))
          ((EQUAL (CAR PATT) '"*")
           (COND ((SMATCH (CDR PATT) (CDR OB)) T)
                 ((SMATCH PATT (CDR OB)) T)
                 ((SMATCH (CDR PATT) OB) T) (T NIL)))
           (T NIL)))

; Mustererkennung mit Match-Variablen

(DEFUN ATOMCAR (AT) (CAR (UNPACK AT)))
(DEFUN ATOMCDR (AT) (PACK (CDR (UNPACK AT))))

(DEFUN MSET (LH RH) (PROG2 (PUTPROP LH RH APVAL) T))

(DEFUN MATCH (PATT OB)
    (COND ((AND (NULL PATT) (NULL OB)) T)
          ((NULL PATT) NIL)
          ((NULL OB)
           (COND ((EQUAL PATT '(*)) T)
                 ((AND (ATOM (CAR PATT)) (NULL (CDR PATT))
                       (EQ (ATOMCAR (CAR PATT)) '*))
                  (MSET (ATOMCDR (CAR PATT)) NIL))
                 (T NIL)
          ))

          ((OR (EQUAL (CAR PATT) '?) (EQUAL (CAR PATT) (CAR OB)))
           (MATCH (CDR PATT) (CDR OB)))

          ((AND (ATOM (CAR PATT))
                (EQUAL (ATOMCAR (CAR PATT)) '?)
                (MATCH (CDR PATT) (CDR OB)))
           (MSET (ATOMCDR (CAR PATT)) (CAR OB)))

          ((EQUAL (CAR PATT) '*)
           (COND ((MATCH (CDR PATT) (CDR OB)) T)
                 ((MATCH PATT (CDR OB)) T)
                 ((MATCH (CDR PATT) OB) T) (T NIL)))

          ((AND (ATOM (CAR PATT)) (EQUAL (ATOMCAR (CAR PATT)) '*))
           (COND ((MATCH (CDR PATT) (CDR OB))
                  (MSET (ATOMCDR (CAR PATT)) (LIST (CAR OB))))
                 ((MATCH PATT (CDR OB))
                  (MSET (ATOMCDR (CAR PATT))
                        (CONS (CAR OB)
                              (EVAL (ATOMCDR (CAR PATT)) NIL))))
                 ((MATCH (CDR PATT) OB) T)
                 (T NIL)
           ))
          (T NIL)))
