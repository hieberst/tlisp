\ some test cases in forth

: find-symbol
    BL WORD COUNT 2DUP {upper} type-symbol find-object DROP
;


\ display a dotted pair (A . B)
new-symbol A
new-symbol B
cons
print DROP

\ display a simple list (A B C)
find-symbol A
find-symbol B
new-symbol  C
nil
cons cons cons
print CONSTANT l1

\ display a simple list (D E F)
new-symbol D
new-symbol E
new-symbol F
nil
cons cons cons
print CONSTANT l2

\ display a simple list (A B C . D)
find-symbol A
find-symbol B
find-symbol C
find-symbol D
cons cons cons
print DROP

\ display a more complex list (g (h . i))
new-symbol g
new-symbol h
new-symbol i
cons
nil
cons
cons
print DROP

\ display an association list ((p1 . v1) (p2 . v2) (p3 . v3))
new-symbol ali
new-symbol p1
new-symbol v1
cons
new-symbol p2
new-symbol v2
cons
new-symbol p3
new-symbol v3
cons
nil
cons
cons
cons
$apval putprop print DROP

\ display a property list (p1 v1 p2 v2 p3 v3)
new-symbol pli
DUP find-symbol v3 find-symbol p3 putprop DROP
DUP find-symbol v2 find-symbol p2 putprop DROP
DUP find-symbol v1 find-symbol p1 putprop DROP
DUP cdr print DROP
DUP find-symbol p2 get print DROP DROP

$oblist cdr print DROP

CR
CR
