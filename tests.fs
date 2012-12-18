\ some test cases in forth

: find-symbol
    BL WORD COUNT type-symbol (find-object) DROP
;


\ display a dotted pair (a . b)
new-symbol a
new-symbol b
cons
print DROP

\ display a simple list (c d e)
new-symbol c
new-symbol d
new-symbol e
nil
cons cons cons
print DROP

\ display a simple list (a b c . d)
find-symbol a
find-symbol b
find-symbol c
find-symbol d
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
