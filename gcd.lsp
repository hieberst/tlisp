(DEFUN GCD (A B)
    (COND ((GREATERP A B) (GCD (DIFFERENCE A B) B))
          ((GREATERP B A) (GCD A (DIFFERENCE B A)))
          (T              A)))
