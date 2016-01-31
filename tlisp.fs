\                   TinyLISP (TLISP)
\
\              Version 0.2 ref 2016-01-31
\
\        Written (w) 1987-2016 by Steffen Hieber
\
\        RCS $Id$
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
\            Der Eaker-Case ist bereits in Gforth implementiert, auch TFLOAD
\            ist nicht mehr noetig. Den F83-eigenen Alias (S habe ich durch
\            den Standard ( ersetzt. Das Wort FREE war falsch und zudem noch
\            redundant, da mit UNUSED der freie Speicher ermittelt werden
\            kann - entfernt. Umstellung der Adressarithmetik auf die Worte
\            CELL, CELLS und CHARS. Erzeugung des Shell-Skripts TLISP.SH zum
\            Start von Gforth und TLISP.
\
\ 16.10.1997 Kontrolle der Programmentwicklung durch das RCS gestartet.
\
\ 02.04.2000 Wiederaufnahme der Entwicklung.
\

\ Achtung: maximale Zeilenlaenge: (PAD HERE -)
\ --------      F83, F-PC:  80
\               Gforth   : 104
\
\          Groesse des Return-Stacks:
\               F83      : 100  (RP0@ TIB - CELL /)
\               FPC      : 125
\
\          Groesse von TLISP:
\               F83      : ' TFLOAD.BLK HERE SWAP - U.
\               Gforth   : ' tlisp      HERE SWAP - U.

\ Namenskonventionen:
\ -------------------
\   $name   Symbol, z.B. $apval (sigil)
\   {name}  Wort, das nicht in jedem Forth-System vorhanden ist, z.B. {upc}

ONLY FORTH DEFINITIONS ALSO   \ Grund-Vokabular erweitern ...
DECIMAL                       \ Zahlenbasis

VOCABULARY tlisp        \ Vokabular TLISP anlegen
tlisp DEFINITIONS       \ alle neuen Woerter ab jetzt in das Vokabular TLISP

                              \ Konfiguration (statisch)
1500 CELLS CONSTANT #nodes    \ Anzahl der vorhandenen Zellenpaare in MEM_NODE
2500    CONSTANT num_chars    \ Groesse des Objektspeichers MEM_CHAR in Zeichen
16      CONSTANT read-depth   \ Groesse des READ-Stacks
4       CONSTANT load-depth   \ Maximale LOAD-Verschachtelungstiefe (FCBS)
16      CONSTANT #progs       \ Maximale PROG-Verschachtelungstiefe
80      CONSTANT tlen         \ Groesse des Eingabepuffers fuer ACCEPT
6       CONSTANT d/gensym     \ Anzahl der Ziffern bei GENSYM

                              \ Hilfskonstanten
2 CELLS CONSTANT sizeof_node  \ Groesse eines Knotens
0       CONSTANT nil          \ "not in list"
1       CONSTANT gc-used      \ Vorbedingung: CELL ist eine gerade Zahl
9       CONSTANT tab          \ ASCII TAB
16      CONSTANT b/cell       \ TODO: 32-Bit

                                \ subr attr
63         CONSTANT subr-arity  \ Bit 0..5
192        CONSTANT subr-ret    \ Bit 6..7    0: ret-sexpr, 192: reserved
1 6 LSHIFT CONSTANT ret-bool
2 6 LSHIFT CONSTANT ret-number

b/cell 2 -                CONSTANT count-bits
3 count-bits    LSHIFT    CONSTANT count-flags      \ Bits 14..15
1 count-bits    LSHIFT 1- CONSTANT count-mask       \ Bits  0..13
1 count-bits    LSHIFT    CONSTANT count-quote
1 count-bits 1+ LSHIFT    CONSTANT count-dot

                              \ Fehlerkonstanten
                              \   wenn Bit 7 gesetzt, dann nur Warnung
  1 CONSTANT enomem_node      \ Out of node memory
  2 CONSTANT enomem_char      \ Out of char memory
  3 CONSTANT e_no_sexpr       \ Argument is not a symbolic expression
  4 CONSTANT e_no_atom        \ Argument is not an atom
  5 CONSTANT e_no_string      \ Argument is not a string
  6 CONSTANT e_no_number      \ Argument is not a number
  7 CONSTANT e_no_lexpr       \ Not a lambda expression
  8 CONSTANT e_not_bound      \ Atom is not bound
  9 CONSTANT e_too_few_args   \ Arguments missing
 10 CONSTANT e_too_many_args  \ Too many arguments
 11 CONSTANT e_unknown_type   \ Unknown atom type
 12 CONSTANT e_rquote_missing \ Missing right double quote
 13 CONSTANT e_misplaced_dot  \ Misplaced dot
 14 CONSTANT e_unbalanced     \ Unbalanced right parenthesis
 15 CONSTANT e_lbrace_open    \ Unbalanced left parenthesis
 16 CONSTANT e_read_exceeded  \ Too many s-expressions (per line)
 17 CONSTANT e_load_exceeded  \ Too many nested LOADs
 18 CONSTANT e_prog_exceeded  \ Too many nested PROGs
 19 CONSTANT e_no_prog        \ No PROG for GO or RETURN
 20 CONSTANT e_no_label       \ Label not found
128 CONSTANT warning
129 CONSTANT w_cond_failed    \ COND fell through


CREATE mem_node #nodes sizeof_node * ALLOT  \ Listen-Speicher erzeugen
CREATE mem_char num_chars    CHARS   ALLOT  \ Objekt-Speicher erzeugen
CREATE mem_read read-depth 1 CELLS * ALLOT  \ READ-Stack erzeugen
CREATE mem_load load-depth 1 CELLS * ALLOT  \ LOAD-Stack erzeugen
CREATE mem_prog #progs     3 CELLS * ALLOT  \ PROG-Stack erzeugen

CREATE tstate
0 C,                            \ first,    Anfangsposition
0 C,                            \ next,     aktuelle Position
0 C,                            \ count,    Anzahl der Zeichen im Puffer
tlen 1+ CHARS ALLOT             \ buffer,   Puffer, 1 Zeichen mehr fuer CONVERT


                                \ Konfiguration (dynamisch)
VARIABLE notation               \ Dot Notation (0) oder List Notation (<> 0)
                                \ Wenn List Notation, dann Separator, z.B.
                                \ Leerzeichen (BL) oder Komma (44)

                                \ Arbeitsvariablen
VARIABLE  freelist              \ Zeiger auf das erste freie Zellenpaar
VARIABLE  len_char              \ Aktuelle Groesse des Objektspeichers
VARIABLE  num_lbrace            \ Anzahl der offenen linken runden Klammern
VARIABLE  read-base
VARIABLE  num_read              \ Anzahl der gelesenen S-Ausdruecke
VARIABLE  running               \ Flag, ob TLISP laeuft
VARIABLE  rp_save               \ Merker des Return-Stack-Pointers (reset)
VARIABLE  rp_max                \ Maximale Return-Stack-Ausnutzung (EVAL, GC)
VARIABLE  load-level            \ Aktuelle LOAD-Verschachtelungstiefe
VARIABLE  prog-level            \ Aktuelle PROG-Verschachtelungstiefe
2VARIABLE gensym-num            \ Aktueller GENSYM-Zaehler


DEFER error                     \ error vs. reset und prin
DEFER eval                      \ eval vs. apply
DEFER >oblist


: (car) ( sexpr -- head ) [ gc-used INVERT ] LITERAL AND mem_node +       @ ;
:  cdr  ( sexpr -- tail ) [ gc-used INVERT ] LITERAL AND mem_node + CELL+ @ ;


: (rplaca) ( sexpr newhead -- sexpr ) OVER mem_node + ! ;


: (rplacd) ( sexpr newtail -- sexpr )
\ ========
    OVER [ gc-used INVERT ] LITERAL AND mem_node + CELL+ !
;


: null  ( sexpr -- flag ) nil =  ;      \ alternativ: 0=
: nil<> ( sexpr -- flag ) nil <> ;      \ alternativ: 0<>


: init ( -- )         \ Listen-Speicher und Variablen initialisieren
\ ====
    [ #nodes sizeof_node * ] LITERAL 0 ?DO
        I nil (rplaca) I sizeof_node + (rplacd) DROP
    sizeof_node +LOOP
    [ #nodes 1- sizeof_node * ] LITERAL nil (rplacd) DROP
    0 freelist !
    0 len_char !
    0 read-base !
    0 num_read !
    0 rp_max !
    0 load-level !
    0 prog-level !
    0. gensym-num 2!
    BL notation !
; init


: (new-node) ( -- sexpr )           \ der FREELIST ein Zellenpaar entnehmen
\ ==========
    freelist @
    DUP cdr ?DUP IF freelist ! ELSE enomem_node error THEN
;


: (free-node) ( sexpr -- )
\ ===========
    nil (rplaca) freelist @ (rplacd)
    freelist !
;


1 CONSTANT type-symbol      \ counted string
2 CONSTANT type-string      \ counted string
3 CONSTANT type-number      \ single precision, signed
4 CONSTANT type-code        \ attr + cfa


: (new-object) ( type size -- object )   \ Objektspeicher reservieren
\ ============
    CHAR+                   \ Platz fuer den Objekttyp (type) reservieren
    len_char @              \ Naechste freie Speicheradresse ermitteln
    TUCK +                  \ Neue Groesse des Objektspeichers ermitteln
    DUP num_chars U> IF
        2DROP nil
    ELSE
        len_char !
        mem_char +
        TUCK C!             \ type
    THEN
;


: cons ( sexpr1 sexpr2 -- sexpr )
\ ====
    (new-node) SWAP (rplacd)
               SWAP (rplaca)
;


: atom? ( sexpr -- flag )     \ liefert TRUE, wenn Atomknoten, sonst FALSE
\ =====
    (car) DUP mem_char U>= SWAP mem_char num_chars CHARS + U< AND
;


: (list?) ( sexpr -- flag )
\ =======
    atom? INVERT
;


: list? ( sexpr -- flag )
\ =====
    DUP (list?) SWAP null OR    \ NIL ist sowohl ein Atom als auch
;                               \ die leere Liste


: literal? ( sexpr -- flag )    \ liefert TRUE, wenn Literalatom, sonst FALSE
\ ========
    DUP atom? IF
        (car) C@ DUP type-symbol = SWAP type-string = OR
    ELSE
        DROP FALSE
    THEN
;


: symbolp ( sexpr -- flag )     \ liefert TRUE, wenn Symbol, sonst FALSE
\ =======
    DUP atom? IF (car) C@ type-symbol = ELSE DROP FALSE THEN
;


: stringp ( sexpr -- flag )     \ liefert TRUE, wenn Zeichenkette, sonst FALSE
\ =======
    DUP atom? IF (car) C@ type-string = ELSE DROP FALSE THEN
;


: numberp ( sexpr -- flag )     \ liefert TRUE, wenn Zahl, sonst FALSE
\ =======
    DUP atom? IF (car) C@ type-number = ELSE DROP FALSE THEN
;


: car ( sexpr -- sexpr )
\ ===
    DUP atom? IF e_no_sexpr error ELSE (car) THEN
;


: length ( sexpr -- n )
\ ======
    0 OVER atom? IF NIP ELSE
        BEGIN
            OVER (list?)
        WHILE
            1+ SWAP cdr SWAP
        REPEAT
        NIP
    THEN
;


: rplaca ( sexpr newhead -- sexpr )
\ ======
    OVER atom? IF DROP e_no_sexpr error ELSE (rplaca) THEN
;


: rplacd ( sexpr newtail -- sexpr )
\ ======
    OVER atom? IF DROP e_no_sexpr error ELSE (rplacd) THEN
;


: (new-literal) ( c-addr u type -- atom)
\ =============
    OVER CHAR+ (new-object)
    ?DUP IF
        2DUP CHAR+ C!                           \ Count byte
        TUCK 2SWAP 2 CHARS + 3 ROLL CMOVE       \ String
        nil cons
        >oblist
    ELSE
        enomem_char error
    THEN
;


: find-word ( -- )
\ =========
    BL WORD FIND
;


: {mu/mod} ( ud1 u1 -- u2 ud2 )
\ ========
    [ find-word MU/MOD ] 2LITERAL IF                \ F83
        EXECUTE
    ELSE
        DROP
        >R 0 R@ UM/MOD R> SWAP >R UM/MOD R>
    THEN
;


: {unused} ( --  u )
\ ========
    [ find-word UNUSED ] 2LITERAL IF
        EXECUTE                         \ gforth, Win32Forth
    ELSE
        DROP SP@ HERE -                 \ F83, F-PC
    THEN
;


: {upc} ( ch -- ch )
\ =====
    [ find-word UPC ] 2LITERAL IF
        EXECUTE
    ELSE DROP [ find-word TOUPPER ] 2LITERAL IF
        EXECUTE
    ELSE
        DROP
        ABORT" fatal: neither UPC nor TOUPPER are available"
    THEN
    THEN
;


: {upper} ( c-addr u -- )
\ =======
    [ find-word UPPER ] 2LITERAL IF
        EXECUTE
    ELSE
        DROP 0 ?DO
            DUP I CHARS +
            DUP C@ {upc} SWAP C!
        LOOP
        DROP
    THEN
;


: new-symbol ( -- atom )
\ ==========
    BL WORD COUNT 2DUP {upper} type-symbol (new-literal)
;


: check-args ( ist soll_min soll_max -- flag )
\ ==========
    2 PICK < IF
        e_too_many_args error
    ELSE < IF
        e_too_few_args error
    ELSE
        TRUE
    THEN
    THEN
;


: (prop) ( atom property -- plist )
\ ======
   SWAP DUP atom? IF
        cdr
        BEGIN
            DUP IF
                2DUP car <> SWAP cdr SWAP
            ELSE
                FALSE
            THEN
        WHILE
            cdr
        REPEAT
        NIP
    ELSE
        e_no_atom error
    THEN
;


: get ( atom property -- value )
\ ===
    (prop) DUP nil<> IF car THEN
;


: putprop ( atom value property -- value )
\ =======
    ROT SWAP 2DUP (prop)
    ?DUP IF
        NIP NIP OVER (rplaca) DROP
    ELSE
        OVER cdr 3 PICK SWAP cons cons (rplacd) DROP
    THEN
;


: remprop ( atom property -- flag )
\ =======
    SWAP DUP atom? IF
        FALSE
        BEGIN
            OVER cdr ?DUP IF
                NIP DUP car 3 PICK <> DUP INVERT IF
                    -ROT cdr cdr (rplacd) DROP NIP TRUE SWAP
                THEN
            ELSE
                NIP NIP FALSE
            THEN
        WHILE
            NIP cdr FALSE
        REPEAT
    ELSE
        e_no_atom error
    THEN
;


: rdepth ( -- u )
\ ======
    rp_save @ RP@ - CELL /
;


: update_rp_max ( -- )
\ =============
    rdepth DUP rp_max @ U> IF rp_max ! ELSE DROP THEN
;


: pair ( x y -- p )
\ =====
    update_rp_max
    2DUP null SWAP null OR IF
        2DROP nil
    ELSE
        2DUP
        SWAP car SWAP car cons
        ROT cdr ROT cdr RECURSE
        cons
    THEN
;


: pair2 ( x y -- p )
\ ====
    nil nil 2SWAP
    BEGIN
        2DUP (list?) SWAP (list?) AND
    WHILE
        2DUP
        SWAP car SWAP car cons nil cons
        ROT cdr ROT cdr
        4 ROLL DUP null IF
            DROP 3 ROLL DROP ROT DUP
        ELSE
            4 ROLL 4 ROLL TUCK rplacd DROP
        THEN
        2SWAP
    REPEAT
    2DROP DROP
;


: append ( x y -- xy )
\ =======
    update_rp_max
    SWAP ?DUP IF
        DUP car SWAP cdr ROT RECURSE cons
    THEN
;


: append2 ( x y -- xy )
\ ======
    nil nil 2SWAP SWAP
    BEGIN
        DUP (list?)
    WHILE
        DUP cdr SWAP car nil cons
        4 ROLL DUP null IF
            DROP 3 ROLL DROP DUP
        ELSE
            4 ROLL ROT TUCK rplacd DROP
        THEN
        2SWAP
    REPEAT
    DROP
    OVER null IF NIP NIP ELSE rplacd DROP THEN
;


: bind ( la lb a1 -- a2 )
\ ====
    -ROT pair SWAP append
;


: nil-must-be-zero ( sexpr -- )
\ ================
    nil <> IF ." NIL must be 0!" CR THEN
;


' NOOP IS >oblist

new-symbol NIL    nil-must-be-zero      \ nil
new-symbol OBLIST CONSTANT $oblist      \ Objektliste
new-symbol APVAL  CONSTANT $apval

$apval DUP DUP    putprop DROP
nil    DUP $apval putprop DROP

\ NIL, APVAL und OBLIST in die Objektliste eintragen
$oblist nil $apval $oblist nil cons cons cons $apval putprop DROP


: (>oblist) ( atom -- atom )
\ =========
    DUP $oblist $apval get DUP cdr ROT SWAP cons rplacd DROP
; ' (>oblist) IS >oblist


new-symbol T         DUP $apval putprop CONSTANT $t
new-symbol SUBR                         CONSTANT $subr
new-symbol STOP                         CONSTANT $stop
new-symbol QUOTE                        CONSTANT $quote
new-symbol LAMBDA    DUP $apval putprop CONSTANT $lambda
new-symbol GC                           CONSTANT $gc
new-symbol FUNARG                       CONSTANT $funarg
new-symbol FSUBR                        CONSTANT $fsubr
new-symbol FEXPR     DUP $apval putprop CONSTANT $fexpr
new-symbol EXPR      DUP $apval putprop CONSTANT $expr
new-symbol EXIT                         CONSTANT $exit
new-symbol EVALQUOTE                    CONSTANT $evalquote
new-symbol EVAL                         CONSTANT $eval
new-symbol EOF       DUP $apval putprop CONSTANT $eof
new-symbol ECHO                         CONSTANT $echo
new-symbol DEBUG                        CONSTANT $debug
new-symbol CONFIG                       CONSTANT $config


$config $t  $gc        putprop DROP     \ Garbage Collection (GC) EIN/aus
$config $t  $exit      putprop DROP     \ BYE nach EXIT ein/AUS
$config nil $evalquote putprop DROP     \ EVALQUOTE ein/AUS
$config nil $echo      putprop DROP     \ Echo ein/AUS
$config nil $debug     putprop DROP     \ Debug-Ausgaben ein/AUS
$config nil $apval     putprop DROP     \ APVAL vor A-Liste auswerten ein/AUS


: enabled? ( sexpr -- flag )
\ ========
    $config SWAP get nil<>
;


: free ( -- u )
\ ====
    CR ." Code: " {unused} U. ." bytes unused, rp_max=" rp_max @ U. CR
    ." Node: "
    freelist @ ?DUP IF length #nodes SWAP - ELSE 0 THEN
    DUP 4 U.R ."  nodes used, " #nodes OVER - 4 U.R ."  / " #nodes 4 U.R
         ."  nodes free (" 100 100 ROT #nodes */ - 2 U.R ." %)" CR
    ." Char: "
    len_char @
    DUP 4 U.R ."  bytes used, " num_chars OVER - 4 U.R ."  / " num_chars 4 U.R
         ."  bytes free (" 100 100 ROT num_chars */ - 2 U.R ." %)" CR
    nil
;


: new-subr ( atom property cfa attr -- )
\ ========
    type-code [ 1 CHARS CELL+ ] LITERAL (new-object)
    ?DUP IF
        TUCK [ 1 CHARS ] LITERAL + C!       \ attr (i.e. arity)
        TUCK [ 2 CHARS ] LITERAL +  !       \ cfa
        nil cons
        SWAP putprop DROP
    THEN
;


: find-object ( addr u type -- atom flag )
\ ===========
    $oblist $apval get
    BEGIN
        DUP (list?) IF
            DUP car (car) DUP C@ DUP 4 PICK = IF
                DUP type-symbol = OVER type-string = OR IF
                    DROP CHAR+ COUNT
                    5 PICK 5 PICK COMPARE 0=
                ELSE DUP type-number = IF
                    DROP CHAR+ @ 4 PICK =
                ELSE
                    2DROP FALSE
                THEN
                THEN
                DUP IF SWAP car TRUE ROT THEN INVERT
            ELSE
                2DROP TRUE
            THEN
        ELSE
            FALSE FALSE
        THEN
    WHILE
        cdr
    REPEAT
    ROT DROP ROT DROP ROT DROP \ Eingangsparameter loeschen
;


: new-literal ( c-addr u type -- atom )
\ ===========
    -ROT 2DUP 4 PICK find-object IF
        NIP NIP NIP
    ELSE
        DROP ROT (new-literal)
    THEN
;


: (new-symbol) ( c-addr u -- atom )
\ ============
    type-symbol new-literal
;


: new-string ( c-addr u -- atom )
\ ==========
    type-string new-literal
;


: new-number ( n -- atom )
\ ==========
    DUP 0 type-number find-object IF
        NIP
    ELSE
        DROP type-number CELL (new-object)
        ?DUP IF
            TUCK [ 1 CHARS ] LITERAL + !
            nil cons
            >oblist
        THEN
    THEN
;


: number@ ( atom -- n )
\ =======
    (car) CHAR+ @
;


: prin2 ( atom -- atom )
\ =====
    DUP literal? IF
        DUP (car) CHAR+ COUNT TYPE
    ELSE DUP atom? IF
        DUP (car) C@
        DUP type-number = IF
            DROP DUP number@ 0 .R
        ELSE DUP type-code = IF
            DROP ." @CODE"
            $debug enabled? IF
                DUP (car) CHAR+ DUP CHAR+ @
                ." [" >NAME .ID 8 EMIT [CHAR] / EMIT
                C@ subr-arity AND 0 U.R ." ]"
            THEN
        ELSE
            e_unknown_type error
        THEN
        THEN
    ELSE
        e_no_atom error
    THEN
    THEN
;


: prin1 ( atom -- atom )
\ =====
    DUP stringp IF [CHAR] " EMIT THEN
    prin2
    DUP stringp IF [CHAR] " EMIT THEN
;


: wrap ( -- )
\ ====
    [ find-word #OUT ] 2LITERAL IF
        EXECUTE @
        [ find-word RMARGIN ] 2LITERAL IF
            EXECUTE @ >= IF CR THEN
        ELSE
            DROP
        THEN
    ELSE
        DROP
    THEN
;


: prin ( sexpr -- sexpr )
\ ====
    DUP atom? IF
        prin1
    ELSE
        ." ("
        notation @ 0= IF                \ dot notation
            DUP car RECURSE DROP
            ."  . "
            DUP cdr RECURSE DROP
        ELSE                            \ list notation
            DUP
            BEGIN
                DUP (list?)
            WHILE
                DUP car RECURSE DROP
                cdr
                DUP nil<> IF
                    DUP atom? IF
                        ."  . " prin1
                    ELSE
                        notation @ DUP EMIT BL <> IF SPACE THEN
                    THEN
                THEN
            REPEAT
            DROP
        THEN
        ." )"
    THEN
    wrap    \ fuer F-PC 3.60
;


: print ( sexpr -- sexpr )
\ =====
    prin CR
;


: .indent ( -- )
\ =======
    rdepth 5 - SPACES
    \ 1=driver/reset, 2=top-level, 3=eval(quote), 4=.indent, 5=rdepth
;


: assoc ( atom sexpr -- sexpr )
\ =====
    $debug enabled? IF
        .indent ." searching " OVER prin DROP ."  in " print
    THEN
    BEGIN
        DUP atom? IF TRUE ELSE DUP car atom? THEN IF
            DROP nil FALSE
        ELSE
            2DUP (car) (car) = IF (car) FALSE ELSE TRUE THEN
        THEN
    WHILE
        cdr
    REPEAT
    NIP
;


: member ( atom list -- sublist )
\ ======
    BEGIN
        DUP (list?) IF
            DUP car 2 PICK <>
        ELSE
            DROP nil FALSE
        THEN
    WHILE
        cdr
    REPEAT
    NIP
;


0 CONSTANT prog-sp
1 CONSTANT prog-rp
2 CONSTANT prog-rp-eval


: prog-addr ( ix -- addr )
\ =========
    CELLS mem_prog prog-level @ 1- [ 3 CELLS ] LITERAL * + +
;


: prog-push ( sp rp -- flag )
\ =========
    prog-level @ #progs U< DUP IF
        1 prog-level +!
        -ROT
        prog-rp prog-addr !
        prog-sp prog-addr !
    ELSE
        NIP NIP
    THEN
;


: prog-pop ( -- )
\ ========
    -1 prog-level +!
;


: prog? ( -- flag )
\ =====
    prog-level @ 0<>
;


: prog-eval ( sexpr env -- sexpr )
\ =========
    RP@ prog-rp-eval prog-addr !
    eval
;


: prog-go ( argl ali -- prog ali sub nil )
\ =======
    prog? IF
        >R car >R
        prog-sp prog-addr @ SP! DROP
        R> 2DUP SWAP member ?DUP IF
            NIP R>
            SWAP nil
            prog-rp-eval prog-addr @ RP!
        ELSE
            e_no_label error
        THEN
    ELSE
        e_no_prog error
    THEN
;


: prog-return ( sexpr -- sexpr )
\ ===========
    prog? IF
        >R prog-sp prog-addr @ SP! 2DROP R>
        prog-rp prog-addr @ RP!
        prog-pop
    ELSE
        e_no_prog error
    THEN
;


: prog ( argl ali -- sexpr )
\ ====
    \ PROG-Parameter mit NIL initialisieren
    OVER car ?DUP IF
        DUP length >R
        R@ 0 DO nil  LOOP nil
        R> 0 DO cons LOOP
        ROT bind
    THEN

    \ PROG-Programm durchlaufen
    SP@ RP@ prog-push IF
        OVER cdr
        BEGIN
            DUP nil<>
        WHILE
            DUP car DUP (list?) IF
                2 PICK prog-eval
            THEN
            DROP
            cdr
        REPEAT
        NIP NIP
        prog-pop
    ELSE
        e_prog_exceeded error
    THEN
;


: exec ( subr args -- result )
\ ====
    $debug enabled? IF
        .indent ." exec: " OVER prin DROP SPACE print
    THEN
    SWAP (car) DUP -ROT DUP >R
    1+ C@ subr-arity AND 0 ?DO
        DUP atom? IF e_too_few_args error LEAVE ELSE DUP car THEN
        SWAP cdr
    LOOP
    R> SWAP nil<> IF
        e_too_many_args error
    ELSE
        [ 2 CHARS ] LITERAL + @ EXECUTE
        SWAP 1+ C@ subr-ret AND DUP ret-number = IF
            DROP new-number
        ELSE DUP ret-bool = IF
            DROP IF $t ELSE nil THEN   \ convert Forth's TRUE to Lisp's T
        ELSE
            DROP
        THEN
        THEN
    THEN
    $debug enabled? IF
        .indent ." result: " print
    THEN
;


: cxr? ( atom -- flag )     \ C[AD]+R
\ ====
    DUP symbolp IF
        (car) CHAR+ COUNT DUP 2 > IF
            2DUP OVER       C@ [CHAR] C =
            -ROT 1- CHARS + C@ [CHAR] R = AND IF
                TRUE SWAP 1- 1 DO
                    OVER I CHARS + C@
                    DUP [CHAR] A = SWAP [CHAR] D = OR AND
                LOOP
                NIP
            ELSE
                2DROP FALSE
            THEN
        ELSE
            2DROP FALSE
        THEN
    ELSE
        DROP FALSE
    THEN
;


: cxr ( atom args -- sexpr )
\ ===
    DUP length 1 1 check-args IF
        car SWAP (car) CHAR+ COUNT 2 - 1 SWAP DO
            TUCK I CHARS + C@
            [CHAR] A = IF car ELSE cdr THEN
            SWAP
        -1 +LOOP
        ( addr ) DROP
    THEN
;


: apply ( fun args env -- result )
\ =====
    ROT
    BEGIN
        $debug enabled? IF
            .indent ." apply: " prin SPACE ROT prin SPACE ROT print ROT
        THEN
        DUP atom? IF
            DUP $subr get ?DUP IF
                NIP NIP SWAP exec TRUE
            ELSE DUP $fsubr get ?DUP IF
                NIP -ROT nil cons cons exec TRUE
            ELSE DUP cxr? IF
                NIP SWAP cxr TRUE
            ELSE
                DUP $expr get ?DUP IF
                    NIP
                ELSE DUP $fexpr get ?DUP IF
                    NIP -ROT DUP -ROT nil cons cons SWAP ROT
                ELSE DUP 2 PICK assoc ?DUP IF
                    NIP cdr                         \ APPLY statt EVAL
                ELSE
                    e_no_lexpr error
                THEN
                THEN
                THEN
                FALSE                               \ Iteration
            THEN
            THEN
            THEN
        ELSE
            DUP car $lambda = IF
                cdr DUP car 3 ROLL 3 ROLL bind
                SWAP cdr car SWAP eval
            ELSE DUP car $funarg = IF
                NIP cdr
                DUP car DUP car             \ params
                SWAP cdr car                \ body
                ROT cdr car                 \ env
                ROT SWAP 3 ROLL SWAP bind
                eval                        \ ggf. APPLY statt EVAL
            ELSE
                e_no_lexpr error
            THEN
            THEN
            TRUE
        THEN
    UNTIL
;


: evcon ( sexpr env -- sexpr )
\ =====
    $debug enabled? IF
        .indent ." evcon: " SWAP prin SWAP SPACE print
    THEN
    BEGIN
        OVER null IF
            DROP prog? INVERT IF w_cond_failed error THEN
            TRUE
        ELSE OVER car car OVER eval IF
            SWAP car cdr car SWAP eval TRUE
        ELSE
            SWAP cdr SWAP FALSE
        THEN
        THEN
    UNTIL
;


: evlis ( sexpr env -- sexpr )
\ =====
    $debug enabled? IF
        .indent ." evlis: " SWAP prin SWAP SPACE print
    THEN
    nil nil 2SWAP
    BEGIN
        OVER (list?)
    WHILE
        OVER car OVER eval nil cons
        ROT cdr
        SWAP 3 ROLL DUP null IF
            DROP 3 ROLL DROP DUP
        ELSE
            OVER rplacd DROP
            3 ROLL SWAP
        THEN
        2SWAP SWAP
    REPEAT
    2DROP DROP
;


: special? ( sexpr -- flag )
\ ========
    DUP atom? IF
        DUP  $fsubr get nil<>
        SWAP $fexpr get nil<> OR
    ELSE
        DROP FALSE
    THEN
;


: (eval) ( sexpr env -- sexpr )
\ ======
    $debug enabled? IF
        .indent ." eval: " SWAP prin SWAP SPACE print
    THEN
    update_rp_max
    OVER atom? IF
        OVER numberp IF
            DROP
        ELSE
            OVER $apval (prop)
            DUP nil<> $apval enabled? AND IF    \ APVAL-Bindung vor A-Liste
                (car) NIP NIP
            ELSE 2 PICK ROT assoc ?DUP IF       \ A-Liste durchsuchen
                NIP NIP cdr
            ELSE ?DUP IF                        \ APVAL-Bindung nach A-Liste
                (car) NIP
            ELSE DUP stringp INVERT IF
                e_not_bound error
            THEN
            THEN
            THEN
            THEN
        THEN
    ELSE
        OVER car DUP (list?) IF
            OVER RECURSE
        THEN
        ROT cdr ROT
        2 PICK special? INVERT IF
            TUCK evlis SWAP
        THEN
        apply
    THEN
;


' (eval) IS eval


: load-fd ( -- addr )
\ =======
    mem_load load-level @ 1- CELLS +
;


: load-push ( fd -- flag )
\ =========
    load-level @ load-depth U< TUCK IF
        1 load-level +!
        load-fd !
    ELSE
        DROP
    THEN
;


: load-pop ( -- fd )
\ ========
    load-fd @
    -1 load-level +!
;


: tgetline ( -- u flag )
\ ========
    tstate 3 CHARS + tlen
    load-level @ 0= IF
        ACCEPT TRUE CR
    ELSE
        load-fd @ READ-LINE THROW
    THEN
;


: readc ( -- char )
\ =====
    tstate 2 CHARS + C@ 0= IF                   \ count == 0?
        tgetline IF
            tstate 2 CHARS + C!
        ELSE
            DROP -1 EXIT
        THEN
    THEN
    tstate CHAR+ DUP C@ SWAP CHAR+ C@ = IF      \ next == count?
        0
    ELSE
        tstate CHAR+ DUP C@ + 2 CHARS + C@      \ ch = buffer [next]
        tstate CHAR+ DUP C@ 1+ SWAP C!          \ next += 1
    THEN
;


: tclear ( -- )
\ ======
    tstate [ tlen 4 CHARS + ] LITERAL ERASE
;


: tnext ( -- )
\ =====
    tstate DUP CHAR+ C@ SWAP C!                 \ first = next
;


: tundo ( -- )
\ =====
    tstate CHAR+ DUP C@ 1- SWAP C!              \ next -= 1
;


1 CONSTANT token-eof        \ end of file
2 CONSTANT token-nl         \ new line
3 CONSTANT token-lbrace     \ left brace (
4 CONSTANT token-rbrace     \ right brace )
5 CONSTANT token-dot        \ dot .
6 CONSTANT token-quote      \ quote
7 CONSTANT token-atom       \ atom
8 CONSTANT token-string     \ string


: token ( -- token )        \ Scanner (Lexikalische Analyse)
\ =====
    tnext
    BEGIN
        readc
        DUP 0= IF
            DROP
            tstate DUP C@ SWAP CHAR+ C@ = IF    \ first == next?
                token-nl tclear
            ELSE
                tstate DUP C@ 3 + CHARS + C@ [CHAR] " = IF
                    e_rquote_missing error
                ELSE
                    token-atom
                THEN
            THEN
        ELSE DUP -1 = IF
            DROP token-eof
        ELSE
            \ first == next - 1, d.h. Laenge 0?
            tstate DUP CHAR+ C@ 1- SWAP C@ = IF
                CASE [CHAR] ( OF token-lbrace    ENDOF
                     [CHAR] ) OF token-rbrace    ENDOF
                     [CHAR] . OF token-dot       ENDOF
                     [CHAR] ' OF token-quote     ENDOF
                     BL       OF 0 tnext         ENDOF
                     tab      OF 0 tnext         ENDOF
                     [CHAR] , OF 0 tnext         ENDOF
                     [CHAR] ; OF token-nl tclear ENDOF
                                 0 SWAP
                ENDCASE
            ELSE
                tstate DUP C@ 3 + CHARS + C@ [CHAR] " = IF
                    [CHAR] " = IF token-string ELSE 0 THEN
                ELSE
                    DUP BL = OVER tab      = OR
                             OVER [CHAR] , = OR
                             OVER [CHAR] ) = OR
                             OVER [CHAR] ( = OR SWAP 0= OR IF
                        tundo token-atom
                    ELSE
                        0
                    THEN
                THEN
            THEN
        THEN
        THEN
        ?DUP 0<>
    UNTIL
;


: count++ ( count -- count )
\ =======
    DUP count-mask AND 1+ SWAP count-flags AND OR
;


: lbrace-- ( -- )
\ ========
    num_lbrace @ 0= IF
        e_unbalanced error
    ELSE
        -1 num_lbrace +!
    THEN
;


: handle-quote ( count -- flag )
\ ============
    DUP count-quote AND 0<> IF
        lbrace--
        nil SWAP
        count-mask AND 0 DO cons LOOP
        TRUE
    ELSE
        DROP FALSE
    THEN
;


: >snumber ( c-addr u -- n flag )   \ to signed number
\ ========
    DUP 0> IF
        OVER C@ [CHAR] - = DUP IF
            ROT CHAR+ ROT 1-
        ELSE
            -ROT
        THEN
        0. 2SWAP >NUMBER NIP 0= IF
            D>S SWAP IF NEGATE THEN TRUE
        ELSE
            2DROP FALSE
        THEN
    ELSE
        NIP FALSE
    THEN
;


: (read) ( -- sexpr... u )      \ Parser (Syntaxanalyse)
\ ======
    0 num_lbrace !
    0 >R
    BEGIN
        token
        CASE
            token-lbrace OF
                R> count++ >R
                0 >R
                1 num_lbrace +!                     \ inkrementieren
                FALSE
                ENDOF
            token-rbrace OF
                lbrace--                            \ dekrementieren
                R> DUP count-dot AND 0= IF
                    nil SWAP
                ELSE count-mask AND 1 = IF
                    R>
                ELSE
                    e_misplaced_dot error LEAVE
                THEN
                THEN
                0 ?DO cons LOOP                     \ ggf. leere Liste
                R@ handle-quote IF R> DROP THEN
                FALSE
                ENDOF
            token-dot OF
                num_lbrace @ 0= R@ 0= OR IF
                    e_misplaced_dot error LEAVE
                ELSE
                    count-dot >R
                THEN
                FALSE
                ENDOF
            token-quote OF
                R> count++ >R
                count-quote count++ >R
                1 num_lbrace +!                     \ inkrementieren
                $quote
                FALSE
                ENDOF
            token-atom OF
                tstate DUP C@ 3 + CHARS +           \ c-addr
                tstate CHAR+ C@ tstate C@ -         \ count
                2DUP >snumber IF
                    NIP NIP new-number
                ELSE
                    DROP 2DUP {upper} (new-symbol)
                THEN
                R> count++ >R
                R@ handle-quote IF R> DROP THEN
                DUP $stop =
                ENDOF
            token-string OF
                tstate DUP C@ 4 + CHARS +           \ c-addr + 1
                tstate CHAR+ C@ tstate C@ - 2 -     \ count  - 2
                new-string
                R> count++ >R
                R@ handle-quote IF R> DROP THEN
                FALSE
                ENDOF
            token-nl OF
                num_lbrace @ 0= R@ 0> AND
                ENDOF
            token-eof OF
                num_lbrace @ ?DUP IF
                    e_lbrace_open error
                THEN
                TRUE
                ENDOF
            FALSE SWAP
        ENDCASE
    UNTIL
    R>
;


: read-pending ( -- u )
\ ============
    read-base @ num_read @ +
;


: read-addr ( -- addr )
\ =========
    mem_read read-pending 1- CELLS +
;


: read-push ( sexpr -- )
\ =========
    read-pending read-depth U< IF
        1 num_read +!
        read-addr !
    ELSE
        e_read_exceeded error
    THEN
;


: read-pop ( -- sexpr )
\ ========
    read-addr @
    -1 num_read +!
;


: read ( -- sexpr )
\ ====
    num_read @ DUP 0= IF
        DROP (read) DUP
        0 ?DO SWAP read-push LOOP
    THEN
    0= IF $eof ELSE read-pop THEN
    $echo enabled? IF print THEN
;


: gc-mark ( sexpr -- sexpr )
\ =======
    DUP cdr gc-used OR (rplacd)
;


: gc-unmark ( sexpr -- sexpr )
\ =========
    DUP cdr [ gc-used INVERT ] LITERAL AND (rplacd)
;


: gc-walk ( sexpr -- )
\ =======
    update_rp_max
    BEGIN
        DUP (list?)
    WHILE
        DUP cdr gc-used AND INVERT IF
            gc-mark
            DUP car ?DUP IF RECURSE THEN
        THEN
        cdr
    REPEAT
    DROP
;


: gc ( -- flag )            \ Garbage Collection
\ ==
    $gc enabled? DUP IF
        \ Phase 1: Freelist flach durchlaufen und
        \          alle Zellenpaare als benutzt markieren
        freelist @ gc-walk

        \ Phase 2: Listenspeicher rekursiv durchlaufen und
        \          alle ueber Atome benutzten Zellenpaare markieren
        [ #nodes sizeof_node * ] LITERAL 0 ?DO
            I atom? IF I gc-mark cdr gc-walk THEN
        sizeof_node +LOOP

        \ Phase 3: READ-Stack rekursiv durchlaufen und
        \          alle benutzte Zellenpaare markieren
        read-pending 0 ?DO
            mem_read I CELLS + @ gc-walk
        LOOP

        \ Phase 4: Listenspeicher flach durchlaufen
        \          und unbenutzte Zellenpaare freigeben
        [ #nodes sizeof_node * ] LITERAL 0 ?DO
            I cdr gc-used AND IF
                I gc-unmark DROP            \ Markierung entfernen
            ELSE
                I (free-node)               \ Unbenutztes Zellenpaar freigeben
            THEN
        sizeof_node +LOOP
    THEN
;


: evalquote ( argl ali -- sexpr )
\ =========
    $debug enabled? IF
        .indent ." evalquote: " SWAP prin SWAP SPACE print
    THEN
    SWAP DUP length 2 2 check-args IF
        DUP car SWAP cdr car
        OVER special? IF
            \ Sonderbehandlung EVAL
            OVER $eval = IF
                NIP DUP length DUP 1 2 check-args IF
                    2 = IF NIP DUP cdr car SWAP THEN
                    car
                THEN
            ELSE
                cons
            THEN
            SWAP eval
        ELSE
            ROT apply
        THEN
    THEN
;


: eval(quote) ( sexpr1 flag -- sexpr2 )
\ ===========
    ( flag ) IF
        ( sexpr1 ) read nil cons cons   \ doublet vervollstaendigen
        nil evalquote
    ELSE
        ( sexpr1 ) nil eval
    THEN
;


: (load) ( c-addr u flag -- flag )
\ ======
    num_read @ DUP read-base +! 2SWAP       \ READ-Stack verschieben
    0 num_read !
    ." loading " 2DUP TYPE CR
    R/O OPEN-FILE 0= DUP IF
        OVER load-push TUCK IF
            4 PICK nil<> IF
                tgetline 2DROP tclear       \ skip TEST line
            THEN
            BEGIN
                read
                DUP $eof = OVER $stop = OR IF
                    TRUE
                ELSE
                    5 PICK nil<> eval(quote)
                    $echo enabled? IF print THEN
                    FALSE
                THEN
                NIP
            UNTIL
            ROT DROP load-pop
        ELSE
            DROP SWAP
        THEN
        CLOSE-FILE THROW
        INVERT IF e_load_exceeded error THEN
    ELSE
        NIP
    THEN
    ROT DROP
    SWAP DUP NEGATE read-base +! num_read !
    tclear
;


: .prompt ( -- )
\ =======
    $evalquote enabled? IF ." evalquote> " ELSE ." tlisp> " THEN
;


: top-level       \ read-eval-print (repl)
\ =========
    BEGIN
        running @
    WHILE
        num_read @ 0= IF .prompt THEN           \ prompt
        read                                    \ read
        $evalquote enabled? eval(quote)         \ eval
        running @ IF print THEN DROP            \ print
        gc DROP                                 \ gc
    REPEAT
    $exit enabled? IF BYE THEN
;


: reset ( --- )
\ =====
    BEGIN                   \ reset LOAD
        load-level @ 0<>
    WHILE
        load-pop CLOSE-FILE THROW
    REPEAT
    0 prog-level !          \ reset PROG
    running @ IF
        tclear              \ reset scanner
        0 read-base !       \ reset parser
        0 num_read !
        SP0 @ SP!           \ reset data stack (clearstack)
        rp_save @ RP!       \ reset return stack
        gc DROP             \ garbage collection
        top-level
    THEN
;


: (error) ( sexpr? n -- )      \ Fehlerbehandlung !!!
\ =======
    DUP ." ERROR! "
    CASE enomem_node      OF ." Out of node memory."                      ENDOF
         enomem_char      OF ." Out of char memory."                      ENDOF
         e_no_sexpr       OF ." Argument is not a symbolic expression: "
                                                           SWAP prin DROP ENDOF
         e_no_atom        OF ." Argument is not an atom: " SWAP prin DROP ENDOF
         e_no_string      OF ." Argument is not a string."                ENDOF
         e_no_number      OF ." Argument is not a number."                ENDOF
         e_no_lexpr       OF ." Not a lambda expression: " SWAP prin DROP ENDOF
         e_not_bound      OF ." Atom is not bound: "       SWAP prin DROP ENDOF
         e_too_few_args   OF ." Arguments missing."                       ENDOF
         e_too_many_args  OF ." Too many arguments."                      ENDOF
         e_unknown_type   OF ." Unknown atom type."                       ENDOF
         e_rquote_missing OF ." Missing right double quote."              ENDOF
         e_misplaced_dot  OF ." Misplaced dot."                           ENDOF
         e_lbrace_open    OF ." Unbalanced left parenthesis."             ENDOF
         e_unbalanced     OF ." Unbalanced right parenthesis."            ENDOF
         e_read_exceeded  OF ." Too many s-expressions (per line)."       ENDOF
         e_load_exceeded  OF ." Too many nested LOADs."                   ENDOF
         e_prog_exceeded  OF ." Too many nested PROGs."                   ENDOF
         e_no_prog        OF ." No PROG for GO or RETURN."                ENDOF
         e_no_label       OF ." Label not found: "         SWAP prin DROP ENDOF
         w_cond_failed    OF ." COND fell through."                       ENDOF
                             ." CASE fell through."
    ENDCASE
    CR
    warning AND INVERT IF reset THEN
;


' (error) IS error


: .hello ( -- )          \ Begruessung des Anwenders
\ ======
    CR
    CR ." TinyLISP Version 0.2 ref 2016-01-31"
    CR ." Copyright (C) 1987-2016 Steffen Hieber"
    CR CR
;


: driver ( -- )     \ main
\ ======
    RP@ rp_save !
    running ON
    .hello
    S" tlisp.lsp" nil (load) IF gc DROP THEN CR
    DEPTH 0= IF reset ELSE ." Data stack is not empty!" CR THEN
;


: n-iter ( argl ali cfa n -- result )
\ ======
    FALSE 4 ROLL
    BEGIN
        DUP nil<>
    WHILE
        DUP car 5 PICK eval
        DUP numberp IF
            number@
            ROT IF ROT SWAP 3 PICK EXECUTE ELSE ROT DROP THEN TRUE
        ELSE
            e_no_number error LEAVE
        THEN
        ROT cdr
    REPEAT
    2DROP NIP NIP
;


: l-iter ( argl ali flag -- result )
\ ======
    ROT
    BEGIN
        DUP nil<>
    WHILE
        DUP car 3 PICK eval nil<>
        2 PICK = IF DROP INVERT nil ELSE cdr THEN
    REPEAT
    DROP NIP INVERT
;


: n-unary-op ( num1 cfa -- num2 )
\ ==========
    OVER numberp IF
        SWAP number@ SWAP
        EXECUTE
    ELSE
        e_no_number error
    THEN
;


: n-binary-op ( num1 num2 cfa -- num3 )
\ ===========
    -ROT
    2DUP numberp SWAP numberp AND IF
        number@ SWAP
        number@ SWAP
        ROT
        EXECUTE
    ELSE
        e_no_number error
    THEN
;


\ some more functions
: add1       ( num1      -- num2   ) [ ' 1+ ] LITERAL n-unary-op ;
: difference ( argl ali  -- number ) [ ' - ] LITERAL 0 n-iter ;
: exitf      (           --        ) running OFF nil ;
: greaterp   ( num1 num2 -- flag   ) [ ' > ] LITERAL n-binary-op ;
: l-and      ( argl ali  -- flag   ) FALSE l-iter ;
: l-or       ( argl ali  -- flag   ) TRUE  l-iter ;
: lambda     ( argl ali  -- lexpr  ) DROP $lambda SWAP cons ;
: lessp      ( num1 num2 -- flag   ) [ ' < ] LITERAL n-binary-op ;
: minus      ( num1      -- num2   ) [ ' NEGATE ] LITERAL n-unary-op ;
: nilf       ( argl ali  -- flag   ) 2DROP FALSE ;
: plus       ( argl ali  -- number ) [ ' + ] LITERAL 0 n-iter ;
: quote      ( argl ali  -- sexpr  ) DROP car ;
: quotient   ( argl ali  -- number ) [ ' / ] LITERAL 1 n-iter ;
: remainder  ( num1 num2 -- num3   ) [ ' MOD ] LITERAL n-binary-op ;
: sub1       ( num1      -- num2   ) [ ' 1- ] LITERAL n-unary-op ;
: terpri     (           -- nil    ) CR nil ;
: times      ( argl ali  -- number ) [ ' * ] LITERAL 1 n-iter ;


: eval1 ( argl ali -- sexpr )
\ =====
    SWAP DUP length DUP 1 2 check-args IF
        1 = IF
            car OVER
        ELSE
            DUP cdr car 2 PICK (eval)   \ env
            SWAP car ROT
        THEN
        (eval) SWAP (eval)
    THEN
;


: function ( argl ali -- sexpr )
\ ========
    SWAP DUP length 1 1 check-args IF
        car DUP atom? IF
            $expr get
        THEN
        DUP car $lambda <> IF
            e_no_lexpr error
        ELSE
            cdr SWAP $funarg -ROT nil cons cons cons
        THEN
    THEN
;


: >digit ( u -- char )
\ ======
    DUP 10 < IF [CHAR] 0 ELSE [CHAR] A 10 - THEN +
\   [ CHAR A 10 - ] LITERAL geht wegen deferred Wort CHAR in F83 nicht
;


: gensym ( -- atom )
\ ======
    gensym-num DUP 2@ 1. D+ ROT 2!  \ increment gensym-num by 1
    [CHAR] G PAD C!
    gensym-num 2@
    1 d/gensym DO
        BASE @ {mu/mod} ROT >digit PAD I CHARS + C!
    -1 +LOOP 2DROP
    PAD d/gensym 1+ (new-symbol)
;


: load ( argl ali -- flag )
\ ====
    SWAP DUP length DUP 1 2 check-args IF
        OVER car 3 PICK eval
        SWAP 1 = IF
            NIP NIP nil
        ELSE
            SWAP cdr car ROT eval
        THEN
        SWAP DUP literal? IF
            (car) CHAR+ COUNT ROT (load)
        ELSE
            e_no_string error
        THEN
    THEN
;


: progn ( argl ali -- sexpr )
\ =====
    SWAP
    BEGIN
        DUP nil<>
    WHILE
        DUP car 2 PICK eval
        SWAP cdr
        DUP null IF ROT DROP ELSE NIP THEN
    REPEAT
    DROP
;


: prop ( argl ali -- plist )
\ ====
    OVER length 3 3 check-args IF
        OVER car OVER eval ROT cdr ROT                  \ atom
        OVER car OVER eval ROT cdr ROT                  \ property
        2SWAP (prop) ?DUP IF
            NIP NIP
        ELSE
            SWAP car OVER eval                          \ fn
            DUP null IF NIP ELSE nil ROT apply THEN
        THEN
    THEN
;


: (set) ( argl ali atom -- sexpr )
\ =====
    OVER 2DUP assoc DUP IF
        NIP NIP ROT cdr car ROT eval rplacd cdr
    ELSE
        2DROP e_not_bound error
    THEN
;


: set  ( argl ali -- sexpr ) OVER car OVER eval (set) ;
: setq ( argl ali -- sexpr ) OVER car           (set) ;


: zerop ( sexpr -- sexpr  )
\ =====
    DUP numberp IF
        number@ 0=
    ELSE
        DROP FALSE
    THEN
;


\ @CODE
new-symbol ZEROP      $subr  ' zerop           ret-bool   1 OR new-subr
new-symbol TIMES      $fsubr ' times           ret-number 2 OR new-subr
new-symbol TERPRI     $subr  ' terpri                     0    new-subr
$t                    $subr  ' NOOP                       1    new-subr
new-symbol SYMBOLP    $subr  ' symbolp         ret-bool   1 OR new-subr
new-symbol SUB1       $subr  ' sub1            ret-number 1 OR new-subr
new-symbol STRINGP    $subr  ' stringp         ret-bool   1 OR new-subr
new-symbol SETQ       $fsubr ' setq                       2    new-subr
new-symbol SET        $fsubr ' set                        2    new-subr
new-symbol RPLACD     $subr  ' rplacd                     2    new-subr
new-symbol RPLACA     $subr  ' rplaca                     2    new-subr
new-symbol RETURN     $subr  ' prog-return                1    new-subr
new-symbol RESET      $subr  ' reset                      0    new-subr
new-symbol REMPROP    $subr  ' remprop         ret-bool   2 OR new-subr
new-symbol REMAINDER  $subr  ' remainder       ret-number 2 OR new-subr
new-symbol READ       $subr  ' read                       0    new-subr
new-symbol QUOTIENT   $fsubr ' quotient        ret-number 2 OR new-subr
$quote                $fsubr ' quote                      2    new-subr
new-symbol PUTPROP    $subr  ' putprop                    3    new-subr
new-symbol PROP       $fsubr ' prop                       2    new-subr
new-symbol PROGN      $fsubr ' progn                      2    new-subr
new-symbol PROG       $fsubr ' prog                       2    new-subr
new-symbol PRINT      $subr  ' print                      1    new-subr
new-symbol PRIN2      $subr  ' prin2                      1    new-subr
new-symbol PRIN1      $subr  ' prin1                      1    new-subr
new-symbol PRIN       $subr  ' prin                       1    new-subr
new-symbol PLUS       $fsubr ' plus            ret-number 2 OR new-subr
new-symbol PAIR       $subr  ' pair                       2    new-subr
new-symbol OR         $fsubr ' l-or            ret-bool   2 OR new-subr
new-symbol NUMBERP    $subr  ' numberp         ret-bool   1 OR new-subr
new-symbol NULL       $subr  ' null            ret-bool   1 OR new-subr
new-symbol NOT        $subr  ' null            ret-bool   1 OR new-subr
nil                   $fsubr ' nilf            ret-bool   2 OR new-subr
new-symbol MINUS      $subr  ' minus           ret-number 1 OR new-subr
new-symbol MEMBER     $subr  ' member          ret-bool   2 OR new-subr
new-symbol LOAD       $fsubr ' load            ret-bool   2 OR new-subr
new-symbol LISTP      $subr  ' list?           ret-bool   1 OR new-subr
new-symbol LIST       $fsubr ' evlis                      2    new-subr
new-symbol LESSP      $subr  ' lessp           ret-bool   2 OR new-subr
new-symbol LENGTH     $subr  ' length          ret-number 1 OR new-subr
$lambda               $fsubr ' lambda                     2    new-subr
new-symbol GREATERP   $subr  ' greaterp        ret-bool   2 OR new-subr
new-symbol GO         $fsubr ' prog-go                    2    new-subr
new-symbol GET        $subr  ' get                        2    new-subr
new-symbol GENSYM     $subr  ' gensym                     0    new-subr
$gc                   $subr  ' gc              ret-bool   0 OR new-subr
new-symbol FUNCTION   $fsubr ' function                   2    new-subr
new-symbol FREE       $subr  ' free            ret-bool   0 OR new-subr
$exit                 $subr  ' exitf                      0    new-subr
$evalquote            $fsubr ' evalquote                  2    new-subr
$eval                 $fsubr ' eval1                      2    new-subr
new-symbol EQ         $subr  ' =               ret-bool   2 OR new-subr
new-symbol DIFFERENCE $fsubr ' difference      ret-number 2 OR new-subr
new-symbol CONS       $subr  ' cons                       2    new-subr
new-symbol COND       $fsubr ' evcon                      2    new-subr
new-symbol BIND       $subr  ' bind                       3    new-subr
new-symbol ATOM       $subr  ' atom?           ret-bool   1 OR new-subr
new-symbol ASSOC      $subr  ' assoc                      2    new-subr
new-symbol APPLY      $subr  ' apply                      3    new-subr
new-symbol APPEND     $subr  ' append                     2    new-subr
new-symbol AND        $fsubr ' l-and           ret-bool   2 OR new-subr
new-symbol ADD1       $subr  ' add1            ret-number 1 OR new-subr


: try-exec find-word IF EXECUTE ELSE DROP THEN ;
try-exec ENDPGM     \ TFLOAD (F83)
