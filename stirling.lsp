; $Id$
;
; Stirling-Zahlen

; ==========================================================================
; Die Stirling-Zahl erster Art s(n,k) ist die Anzahl der Permutationen
; einer n-elementigen Menge, die genau k Zykel haben.
;
; Folge A130534 in OEIS (https://oeis.org/A130534)
; ==========================================================================

; --------------------------------------------------------------------------
; Variante A: Berechnung mittels Bildung der Menge aller Permutationen
; --------------------------------------------------------------------------

(LOAD "perm.lsp")

(DEFUN S1A (N K)
    (APPLY #'+ (MAPCAR
        #'(LAMBDA (L) (IF (= K (LENGTH (PERM-ZERL L))) 1 0))
        (PERM (SEQ N)))))

; --------------------------------------------------------------------------
; Variante B: Rekursive Berechnung
; --------------------------------------------------------------------------

(DEFUN S1B (N K)
    (COND ((OR (ZEROP K) (> K N)) 0)
          ((= K N) 1)
          (T (+ (S1B (1- N) (1- K)) (* (1- N) (S1B (1- N) K))))))

; ==========================================================================
; Die Stirling-Zahl zweiter Art S(n,k) ist die Anzahl der
; k-elementigen Partitionen einer n-elementigen Menge.
;
; Folge A008277 in OEIS (https://oeis.org/A008277)
; ==========================================================================

; --------------------------------------------------------------------------
; Variante A: Berechnung mittels Bildung der Menge aller Partitionen
; --------------------------------------------------------------------------

(LOAD "part.lsp")

; Menge aller Partitionen der Maechtigkeit N einer Menge LI
(DEFUN PART (N LI) (REMOVE-IF-NOT (FUNCTION (LAMBDA (X) (EQ N (LENGTH X))))
                                  (PARTITIONS LI)))

(DEFUN S2A (N K) (LENGTH (PART K (SEQ N))))

; --------------------------------------------------------------------------
; Variante B: Rekursive Berechnung
; --------------------------------------------------------------------------

(DEFUN S2B (N K)
    (COND ((> K N) 0)
          ((OR (= K 1) (= K N)) 1)
          (T (+ (S2B (1- N) (1- K)) (* K (S2B (1- N) K))))))

; ==========================================================================
; Tabellierfunktion TF
; ==========================================================================

(DEFUN TF (N S)
    (PROG (M)
          (SETQ M 1)
     LOOP (COND ((ZEROP N) (RETURN T)))
          (PRINT (TFH M S))
          (SETQ M (1+ M))
          (SETQ N (1- N))
          (GO LOOP)))

(DEFUN TFH (N S)
    (PROG (K X)
          (SETQ K N)
     LOOP (COND ((ZEROP K) (RETURN X)))
          (SETQ X (CONS (FUNCALL S N K) X))
          (SETQ K (1- K))
          (GO LOOP)))
