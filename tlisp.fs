\                   TinyLISP (TLISP)
\
\              Version 0.2 ref 2008-07-18
\
\        Written (w) 1987-2008 by Steffen Hieber
\
\        RCS $Id: tlisp.fs,v 1.8 2010-11-19 14:48:14 steffen Exp $
\
\
\ 07.03.1993 TFLOAD aus dem Buch "Forth 83" von Zech uebernommen. Definition
\            des Vokabulars TLISP. Erstellung der Batch-Datei TLISP.BTM fuer
\            den Start von TLISP unter 4DOS.
\
\ 08.03.1993 Das Wort FREE ermittelt die Groesse des zur Verfuegung stehenden
\            freien Speichers.
\
\ 09.03.1993 Verbesserung der Batch-Datei TLISP.BTM, die Daten sind jetzt vom
\            Programm (F83) getrennt. Der Benutzer wird mit HELLO begruesst.
\            Die beiden Speicherbereiche LSTMEM und OBJMEM werden angelegt,
\            ihre Groesse ist von den beiden Konstanten MAXLST und MAXOBJ ab-
\            haengig. Die Variable FREELIST zeigt auf das erste freie Zellen-
\            paar  im Speicher LSTMEM. Zugriff auf LSTMEM  ueber die  Worte
\            (CAR), (CDR), (RPLACA) und (RPLACD). Der Listen-Speicher wird
\            mittels INIT initialisiert. NIL.
\
\ 10.03.1993 Eaker-Case in EAKER.F83 implementiert. Fehlerbehandlung: ERROR.
\            Die Worte NEW und CONS. TLISP.BTM laedt EAKER.F83.
\
\ ===========================================================================
\
\ 15.10.1997 Umstellung des bisherigen Codes auf GNU Forth.
\
\            Der Eaker-Case ist bereits in GForth implementiert, auch TFLOAD
\            ist nicht mehr noetig. Den F83-eigenen Alias (S habe ich durch
\            den Standard ( ersetzt. Das Wort FREE war falsch und zudem noch
\            redundant, da mit UNUSED der freie Speicher ermittelt werden
\            kann - entfernt. Umstellung der Adressarithmetik auf die Worte
\            CELL, CELLS und CHARS. Erzeugung des Shell-Skripts TLISP.SH zum
\            Start von GForth und TLISP.
\
\ 16.10.1997 Kontrolle der Programmentwicklung durch das RCS gestartet.
\
\ 02.04.2000 Wiederaufnahme der Entwicklung.
\


FORTH DEFINITIONS       \ Grund-Vokabular erweitern ...

VOCABULARY tlisp        \ Vokabular TLISP anlegen
tlisp DEFINITIONS       \ alle neuen Woerter ab jetzt in das Vokabular TLISP


4096 CONSTANT num_nodes   \ Anzahl der vorhandenen Zellenpaare in MEM_NODE
2048 CONSTANT num_chars   \ Groesse des Objekt-Speichers MEM_CHAR in Zeichen


BL CONSTANT notation            \ List Notation mit Leerzeichen (BL) oder
                                \ Komma (44) als Separator oder Dot Notation (0)

2 CELLS CONSTANT sizeof_node    \ Groesse eines Knotens
1 CHARS CONSTANT sizeof_char    \ Groesse eines Zeichens


CREATE mem_node num_nodes sizeof_node * ALLOT   \ Listen-Speicher erzeugen
CREATE mem_char num_chars sizeof_char * ALLOT   \ Objekt-Speicher erzeugen


VARIABLE freelist               \ Zeiger auf das erste freie Zellenpaar
VARIABLE len_char               \ Aktuelle Grösse des Objektspeichers

0 CONSTANT nil                  \ "not in list"


: (car) ( sexpr -- head ) mem_node +        @ ;
: (cdr) ( sexpr -- tail ) mem_node + CELL + @ ;


: (rplaca) ( sexpr newhead -- sexpr ) OVER mem_node +        ! ;
: (rplacd) ( sexpr newtail -- sexpr ) OVER mem_node + CELL + ! ;


1 CONSTANT enomem_node      \ Out of node memory
2 CONSTANT enomem_char      \ Out of char memory
3 CONSTANT einval_sexpr     \ Argument is not a symbolic expression
4 CONSTANT einval_atom      \ Argument is not an atom


: error ( n -- )      \ Fehlerbehandlung !!!
\ =====
    CR ." ERROR! "
    CASE enomem_node  OF ." Out of node memory."                    ENDOF
         enomem_char  OF ." Out of char memory."                    ENDOF
         einval_sexpr OF ." Argument is not a symbolic expression." ENDOF
         einval_atom  OF ." Argument is not an atom."               ENDOF
                         ." CASE fell through."
    ENDCASE
;


: null ( sexpr -- flag )
\ ====
    nil =
;


: init ( -- )         \ Listen-Speicher initialisieren
\ ====
    num_nodes sizeof_node * 0 DO
        I nil (rplaca) I sizeof_node + (rplacd) DROP
    sizeof_node +LOOP
    num_nodes 1- sizeof_node * nil (rplacd) DROP
    0 freelist !
    0 len_char !
; init


: new ( -- sexpr )     \ der FREELIST ein Zellenpaar entnehmen
\ ===
    freelist @
    DUP (cdr) DUP
    null IF DROP enomem_node error ELSE freelist ! THEN
;


: cons ( sexpr1 sexpr2 -- sexpr )
\ ====
    new SWAP (rplacd)
        SWAP (rplaca)
;


: literal? ( sexpr -- flag )    \ liefert TRUE, wenn Literalatom, sonst FALSE
\ ========
    (car) DUP mem_char u>= SWAP mem_char num_chars sizeof_char * + u< AND
;


: subr? ( sexpr -- flag )     \ liefert TRUE, wenn Code, sonst FALSE
\ =====
   (car)  mem_char num_chars sizeof_char * + u>=
;


: atom? ( sexpr -- flag )     \ liefert TRUE, wenn Atomknoten, sonst FALSE
\ =====
    DUP literal? SWAP subr? OR
;


: list? ( sexpr -- flag )
\ =====
    atom? INVERT
;


: numberp ( sexpr -- flag )     \ liefert TRUE, wenn Zahl, sonst FALSE
\ =======
    DUP atom? IF (car) NUMBER? NIP ELSE DROP false THEN
;


: car ( sexpr -- sexpr )
\ ===
    DUP atom? IF DROP einval_sexpr error ELSE (car) THEN
;


: cdr ( sexpr -- sexpr )
\ ===
    DUP nil <> IF (cdr) THEN
;


: rplaca ( sexpr newhead -- sexpr )
\ ======
    OVER atom? IF DROP einval_sexpr error ELSE (rplaca) THEN
;


: rplacd ( sexpr newtail -- sexpr )
\ ======
    OVER atom? IF DROP einval_sexpr error ELSE (rplacd) THEN
;


: (object) ( c-addr u -- atom)
\ ========
    len_char @ DUP mem_char +       \ Naechste freie Speicheradresse ermitteln
    ROT ROT OVER + 1+               \ Neue Groesse des Objektspeichers ermitteln
    DUP num_chars u> IF
        2DROP 2DROP enomem_char
    ELSE
        len_char !
        2DUP SWAP 1+ 4 ROLL SWAP ROT CMOVE
        OVER C! nil cons
    THEN
;


: object ( -- atom )
\ ======
  bl word count (object)
;


: prop ( atom property fn -- plist )
\ ====
    -ROT SWAP DUP atom? IF
        cdr
        BEGIN
            DUP null IF
                FALSE
            ELSE
                2DUP car <> SWAP cdr SWAP       \ todo fn
            THEN
        WHILE
            cdr
        REPEAT
        -ROT 2DROP
    ELSE
        2DROP einval_atom error
    THEN
;


: get ( atom property -- value )
\ ===
    nil prop DUP atom? IF DROP nil ELSE car THEN
;


: putprop ( atom value property -- value )
\ =======
    ROT SWAP
    2DUP nil prop
    DUP null IF
        DROP OVER CDR 3 PICK swap cons cons (rplacd) drop
    ELSE
        NIP NIP OVER (rplaca) DROP
    THEN
;


: assoc ( atom sexpr -- sexpr )
\ =====
    BEGIN
        DUP atom? IF TRUE ELSE DUP (car) atom? THEN IF
            DROP nil FALSE
        ELSE
            2DUP (car) (car) = IF (car) FALSE ELSE TRUE THEN
        THEN
    WHILE
        (cdr)
    REPEAT
    NIP
;


: bind ( -- )
\ ====
;


object NIL    DROP
object OBLIST constant oblist   \ Objektliste
object APVAL  constant apval


\ NIL, APVAL und OBLIST in Objektliste eintragen
oblist nil apval oblist nil cons cons cons apval putprop drop


: new_object ( --  atom )
\ ==========
    object dup
    oblist apval get dup cdr 2 roll swap cons rplacd drop
;


new_object SUBR constant subr


: new_subr ( addr arity -- atom )
\ ========
    new_object -rot cons subr putprop
;


\ @CODE
' cons    2 new_subr CONS    drop
' car     1 new_subr CAR     drop
' cdr     1 new_subr CDR     drop
' rplaca  2 new_subr RPLACA  drop
' rplacd  2 new_subr RPLACD  drop
' prop    3 new_subr PROP    drop
' get     2 new_subr GET     drop
' putprop 3 new_subr PUTPROP drop
' assoc   2 new_subr ASSOC   drop


: strequal ( addr1 addr2 -- f )
    count rot count compare 0 =
;


: find ( -- atom )
    bl word
    oblist apval get
    BEGIN
        DUP list? IF
            DUP CAR (car) 2 PICK STREQUAL IF
                CAR FALSE
            ELSE
                TRUE
            THEN
        ELSE
            NIL
        THEN
    WHILE
        CDR
    REPEAT
    NIP
;


: copy ( sexpr -- sexpr ) recursive
\ ====
    DUP list? IF
        DUP car copy SWAP cdr copy cons
    THEN
;


: last ( sexpr -- sexpr )
\ ====
    DUP list? IF
        BEGIN
            DUP cdr list?
        WHILE
            cdr
        REPEAT
    THEN
;


: prin1 ( atom -- atom )
\ =====
    DUP atom? IF
        DUP literal? IF
            DUP (car) COUNT TYPE
        ELSE
            ." @CODE"
        THEN
    ELSE
        DROP
         einval_atom error
    THEN
;

: prin ( sexpr -- sexpr ) recursive
\ =====
    DUP atom? IF
        prin1
    ELSE
        ." ("
        DUP (car) prin DROP
        notation 0= IF
            ."  . "
            DUP (cdr) prin DROP
        ELSE \ list notation
            DUP last (cdr) nil <> IF ."  . " THEN
            DUP (cdr) nil <> IF
                DUP (cdr) atom? IF
                    DUP (cdr) prin1 DROP
                ELSE
                    DUP
                    BEGIN
                        DUP (cdr) list?
                    WHILE
                        DUP (cdr) last (cdr) nil <> IF
                            (cdr) DUP prin DROP
                        ELSE
                            notation DUP EMIT BL <> IF BL EMIT THEN
                            (cdr) DUP (car) prin DROP
                        THEN
                    REPEAT
                    DROP
                THEN
            THEN
        THEN
        ." )"
    THEN
;


: terpri ( -- )
\ ======
    CR
;


: print ( sexpr -- sexpr )
\ =====
    terpri
    prin
    BL EMIT
;


: eval ( sexpr env -- sexpr )
\ ====
    SWAP DUP numberp IF
    ELSE
    THEN
;


: read ( -- sexpr )
\ ===
;


: load ( c-addr u -- sexpr )
\ ====
;


: gc ( -- )
\ ==
;


: hello ( -- )          \ Begruessung des Anwenders
\ =====
    CR ." TLISP Version 0.2 ref 2008-07-18"
    CR ." Copyright (C) 1987-2008 Steffen Hieber"
    CR
;


hello


\ some test cases in forth


\ display a dotted pair (a . b)
new_object a
new_object b
cons
print drop

\ display a simple list (d e f)
new_object d
new_object e
new_object f
nil
cons
cons
cons
print drop

\ display a more complex list (g (h . i))
new_object g
new_object h
new_object i
cons
cons
print drop

\ display an association list ((p1 . v1) (p2 . v2) (p3 . v3))
new_object ali
new_object p1
new_object v1
cons
new_object p2
new_object v2
cons
new_object p3
new_object v3
cons
nil
cons
cons
cons
apval putprop print drop

\ display a property list (p1 v1 p2 v2 p3 v3)
new_object pli
dup find v3 find p3 putprop drop
dup find v2 find p2 putprop drop
dup find v1 find p1 putprop drop
dup (cdr) print drop
dup find p2 get print drop drop

oblist cdr terpri print drop

terpri
terpri
