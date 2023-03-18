; Groesster gemeinsamer Teiler (ggT) / Greatest common divisor (GCD)

; Klassischer euklidischer Algorithmus
(DEFUN GCD (A B)
    (COND ((GREATERP A B) (GCD (DIFFERENCE A B) B))
          ((GREATERP B A) (GCD A (DIFFERENCE B A)))
          (T              A)))

; Moderner euklidischer Algorithmis, rekursiv
(DEFUN GCD1 (A B)
    (COND ((ZEROP B) A)
          (T (GCD1 B (REMAINDER A B)))))

; Moderner euklidischer Algorithmus, iterativ
(DEFUN GCD2 (A B)
    (PROG (H)
     LOOP (COND ((ZEROP B) (RETURN A)))
          (SETQ H (REMAINDER A B))
          (SETQ A B)
          (SETQ B H)
          (GO LOOP)
))
