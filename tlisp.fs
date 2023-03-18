\                   TinyLISP (TLISP)
\
\              Version 0.7 ref 2023-03-18
\
\        Written (w) 1988-2023 by Steffen Hieber
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
\               F83      : 100  (RP0 @ TIB - CELL /)
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


: units ( n -- m )
    CELL 2 = IF 1 ELSE 10 THEN *
;
                              \ Konfiguration (statisch)
2500 units CONSTANT #nodes    \ Anzahl der vorhandenen Zellenpaare in MEM_NODE
2000 units CONSTANT #chars    \ Groesse des Objektspeichers MEM_CHAR in Zeichen
16         CONSTANT #read     \ Groesse des READ-Stacks
20         CONSTANT #expr     \ Groesse des EXPR-Stacks
4          CONSTANT #load     \ Maximale LOAD-Verschachtelungstiefe (FCBS)
16         CONSTANT #progs    \ Maximale PROG-Verschachtelungstiefe
80         CONSTANT tlen      \ Groesse des Eingabepuffers fuer ACCEPT
6          CONSTANT d/gensym  \ Anzahl der Ziffern bei GENSYM

                                    \ Hilfskonstanten
2 CELLS       CONSTANT sizeof_node  \ Groesse eines Knotens
0             CONSTANT nil          \ "not in list"
1             CONSTANT gc-used      \ GC-Marker-Bit, Annahme: CELL ist gerade
8             CONSTANT bs           \ ASCII BS
9             CONSTANT tab          \ ASCII TAB
8 CELLS CHARS CONSTANT b/cell       \ Bitbreite N einer Zelle

                                    \ subr attr
63           CONSTANT subr-arity    \ Bit 0..5: Stelligkeit
192          CONSTANT subr-ret      \ Bit 6..7: Rueckgabetyp
\ 0 6 LSHIFT CONSTANT ret-sexpr
  1 6 LSHIFT CONSTANT ret-bool
  2 6 LSHIFT CONSTANT ret-number
\ 3 6 LSHIFT CONSTANT ret-unused    \ Reserviert

2             CONSTANT #cbf \ Bitanzahl F (cnt-bits-flags: Flags)
b/cell #cbf - CONSTANT #cbc \ Bitanzahl C (cnt-bits-count: Zaehler)

1 #cbf LSHIFT 1- #cbc    LSHIFT    CONSTANT cnt-mask-flags  \ Bits N-F..N-1
1                #cbc    LSHIFT 1- CONSTANT cnt-mask-count  \ Bits 0..N-F-1
1                #cbc    LSHIFT    CONSTANT cnt-flag-quote
1                #cbc 1+ LSHIFT    CONSTANT cnt-flag-dot

                              \ Fehlerkonstanten
                              \   wenn Bit 7 gesetzt, dann nur Warnung
  1 CONSTANT enomem_node      \ Out of node memory
  2 CONSTANT enomem_char      \ Out of char memory
  3 CONSTANT e_no_sexpr       \ Argument is not a symbolic expression
  4 CONSTANT e_no_atom        \ Argument is not an atom
  5 CONSTANT e_no_literal     \ Argument is not a literal
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
 16 CONSTANT e_range          \ Index out of range
 17 CONSTANT e_load_exceeded  \ Too many nested LOADs
 18 CONSTANT e_underflow      \ Stack underflow
 19 CONSTANT e_no_prog        \ No PROG for GO or RETURN
 20 CONSTANT e_no_label       \ Label not found
 21 CONSTANT e_overflow       \ Stack overflow
128 CONSTANT warning
129 CONSTANT w_cond_failed    \ COND fell through

\ =============================================================================

DEFER error                   \ error vs. reset und prin
DEFER eval                    \ eval vs. apply
DEFER >oblist

\ =============================================================================

: find-word ( "name" -- cfa flag )
\ =========
    BL WORD FIND
;


: {char} ( -- ch )
\ =======
    [ find-word ASCII ] 2LITERAL IF                 \ F83, F-PC
        EXECUTE
    ELSE
        DROP CHAR                                   \ ANS
    THEN
;


: {mu/mod} ( ud1 u1 -- u2 ud2 )
\ ========
    [ find-word MU/MOD ] 2LITERAL IF                \ F83, F-PC
        EXECUTE
    ELSE
        DROP
        >R 0 R@ UM/MOD R> SWAP >R UM/MOD R>
    THEN
;


: {unused} ( --  u )
\ ========
    [ find-word UNUSED ] 2LITERAL IF
        EXECUTE                                     \ gforth, Win32Forth
    ELSE
        DROP SP@ HERE -                             \ F83, F-PC
    THEN
;


: {upc} ( ch -- ch )
\ =====
    [ find-word UPC ] 2LITERAL IF
        EXECUTE                                     \ F83, F-PC
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
    [ find-word UPCASE ] 2LITERAL IF
        EXECUTE                                     \ SwiftForth
    ELSE DROP [ find-word UPPER ] 2LITERAL IF
        EXECUTE
    ELSE
        DROP 0 ?DO
            DUP I CHARS +
            DUP C@ {upc} SWAP C!
        LOOP
        DROP
    THEN
    THEN
;

\ =============================================================================

\ user defined 2d-array with stack functionality
\ inspired by http://www.forth.org/svfig/Len/softstak.htm
\
\ +-----+--------+-------+-------------------------------------------+
\ | ofs | name   | type  | description                               |
\ +=====+========+=======+===========================================+
\ | 0   | id     | char  | array id (debug)                          |
\ +-----+--------+-------+-------------------------------------------+
\ | 1   | cols   | char  | array width                               |
\ +-----+--------+-------+-------------------------------------------+
\ | 2   | rows   | cell  | array height                              |
\ +-----+--------+-------+-------------------------------------------+
\ | 4   | base   | cell  | stack base row                            |
\ +-----+--------+-------+-------------------------------------------+
\ | 6   | depth  | cell  | stack depth                               |
\ +-----+--------+-------+-------------------------------------------+
\ | 8.. | elems  | cells | array elements                            |
\ +-----+--------+-------+-------------------------------------------+


\ create a 2d-array identified by id
: array ( id cols rows -- ) ( -- array )
    CREATE
       ROT C, OVER C, ALIGN DUP ,
       * 2 + CELLS HERE OVER ALLOT SWAP ERASE
    DOES>
;


: aaddr ( n m -- )
    CREATE
        SWAP C, C,
    DOES> ( array -- a-addr )
        DUP C@ CHARS SWAP CHAR+ C@ CELLS -ROT + ALIGNED +
;


2 0 aaddr arows
2 1 aaddr abase
2 2 aaddr adepth
2 3 aaddr aelems


: acols   ( array -- n ) CHAR+ C@ ;
: acount  ( array -- n ) DUP abase @ SWAP adepth @ + ;


: aempty? ( array -- flag ) adepth @ 0= ;
: afull?  ( array -- flag ) DUP arows @ SWAP adepth @ = ;


: a-mark ( array -- n )
    DUP adepth @ SWAP 2DUP
    abase +!
    adepth 0 SWAP !
;


: a-unmark ( n array -- )
    OVER NEGATE SWAP abase TUCK +!
    CELL+ +!
;


: aclear ( array -- )
    abase [ 2 CELLS ] LITERAL ERASE
;


: a# ( col row array -- addr )
    2DUP arows @ U< IF
        ROT SWAP 2DUP acols U< IF
            ROT OVER acols *
            ROT + CELLS aelems +
        ELSE
            NIP NIP e_range
        THEN
    ELSE
        NIP NIP e_range
    THEN
;


: a@ ( col row stack -- n ) a# @ ;     \ array fetch
: a! ( n col row stack -- ) a# ! ;     \ array store


\ push the value n onto the stack
: >a ( n array -- )
    DUP afull? IF
        NIP e_overflow error
    ELSE
        1 OVER adepth +!           \ incr
        DUP acount 1- SWAP
        ( n1 n2 n3 row array )
        0 OVER acols 1- DO
            ROT >R 2DUP R> -ROT I -ROT a!
        -1 +LOOP
        2DROP
    THEN
;


\ pop a value from the stack
: a> ( array -- n )
    DUP aempty? IF
        e_underflow error
    ELSE
        -1 OVER adepth +!          \ decr
        DUP acount SWAP
        DUP acols 0 DO
            2DUP I -ROT a@ -ROT
        LOOP
        2DROP
    THEN
;


\ fetch the top element from the user stack
: atop ( col array -- n )
    DUP acols 1 = IF 0 SWAP THEN
    DUP aempty? IF
        NIP e_underflow error
    ELSE
        DUP acount 1- SWAP a@
    THEN
;

\ =============================================================================

CREATE mem_node #nodes sizeof_node * ALLOT  \ Listen-Speicher erzeugen
CREATE mem_char #chars CHARS         ALLOT  \ Objekt-Speicher erzeugen

{char} R 1 #read  array read_stack          \ READ-Stack erzeugen
{char} L 1 #load  array load_stack          \ LOAD-Stack erzeugen
{char} P 4 #progs array prog_stack          \ PROG-Stack erzeugen
{char} E 1 #expr  array expr_stack          \ EXPR-Stack erzeugen


CREATE tstate
0 C,                            \ first,    Anfangsposition
0 C,                            \ next,     aktuelle Position
0 C,                            \ count,    Anzahl der Zeichen im Puffer
tlen 1+ CHARS ALLOT             \ buffer,   Puffer, 1 Zeichen mehr fuer CONVERT


                                \ Konfiguration (dynamisch)
VARIABLE  notation              \ Dot Notation (0) oder List Notation (<> 0)
                                \ Wenn List Notation, dann Separator, z.B.
                                \ Leerzeichen (BL) oder Komma (44)

                                \ Arbeitsvariablen
VARIABLE  freelist              \ Zeiger auf das erste freie Zellenpaar
VARIABLE  len_char              \ Aktuelle Groesse des Objektspeichers
VARIABLE  num_lbrace            \ Anzahl der offenen linken runden Klammern
VARIABLE  envlist               \ Zeiger auf die Liste aller A-Listen
VARIABLE  running               \ Flag, ob TLISP laeuft
VARIABLE  rp_save               \ Merker des Return-Stack-Pointers (reset)
VARIABLE  rp_max                \ Maximale Return-Stack-Ausnutzung (EVAL, GC)
VARIABLE  sp_max                \ Maximale Datenstack-Ausnutzung
2VARIABLE gensym-num            \ Aktueller GENSYM-Zaehler


: gc-clear  ( sexpr -- sexpr )
\ ========
    [ gc-used INVERT ] LITERAL AND
;


: (car)  ( sexpr -- head ) gc-clear mem_node +       @ ;
:  cdr   ( sexpr -- tail ) gc-clear mem_node + CELL+ @ ;


: (rplaca)  ( sexpr newhead -- sexpr ) OVER gc-clear mem_node +       ! ;
:  rplacd   ( sexpr newtail -- sexpr ) OVER gc-clear mem_node + CELL+ ! ;


: null  ( sexpr -- flag ) nil =  ;      \ alternativ: 0=
: nil<> ( sexpr -- flag ) nil <> ;      \ alternativ: 0<>


: init ( -- )         \ Listen-Speicher und Variablen initialisieren
\ ====
    [ #nodes sizeof_node * ] LITERAL 0 ?DO
        I nil (rplaca) I sizeof_node + rplacd DROP
    sizeof_node +LOOP
    [ #nodes 1- sizeof_node * ] LITERAL nil rplacd DROP
    0 freelist !
    0 len_char !
    0 rp_max !
    0 sp_max !
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
    nil (rplaca) freelist @ rplacd
    freelist !
;


1 CONSTANT type-symbol      \ counted string
2 CONSTANT type-string      \ counted string, Ausgabe mit Anfuehrungszeichen
3 CONSTANT type-number      \ single precision, signed
4 CONSTANT type-code        \ attr + cfa


: (new-object) ( type size -- object )   \ Objektspeicher reservieren
\ ============
    CHAR+                   \ Platz fuer den Objekttyp (type) reservieren
    len_char @              \ Naechste freie Speicheradresse ermitteln
    TUCK +                  \ Neue Groesse des Objektspeichers ermitteln
    DUP #chars U> IF
        2DROP nil
    ELSE
        len_char !
        mem_char CHARS +
        TUCK C!             \ type
    THEN
;


: cons ( sexpr1 sexpr2 -- sexpr )
\ ====
    (new-node) SWAP  rplacd
               SWAP (rplaca)
;


: atom? ( sexpr -- flag )     \ liefert TRUE, wenn Atomknoten, sonst FALSE
\ =====
    (car) DUP mem_char U>= SWAP mem_char #chars CHARS + U< AND
;


: (list?) ( sexpr -- flag )
\ =======
    atom? INVERT
;


: list? ( sexpr -- flag )
\ =====
    DUP (list?) SWAP null OR    \ NIL ist sowohl ein Atom als auch
;                               \ die leere Liste


: type? ( type -- )
\ =====
    CREATE
        C,
    DOES> ( sexpr -- flag )
        OVER atom? IF C@ SWAP (car) C@ = ELSE 2DROP FALSE THEN
;


type-symbol type? symbolp   \ liefert TRUE, wenn Symbol, sonst FALSE
type-string type? stringp   \ liefert TRUE, wenn Zeichenkette, sonst FALSE
type-number type? numberp   \ liefert TRUE, wenn Zahl, sonst FALSE


: literal? ( sexpr -- flag )    \ liefert TRUE, wenn Literalatom, sonst FALSE
\ ========
    DUP symbolp SWAP stringp OR
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
        OVER cdr 3 PICK SWAP cons cons rplacd DROP
    THEN
;


: remprop ( atom property -- flag )
\ =======
    SWAP DUP atom? IF
        FALSE
        BEGIN
            OVER cdr ?DUP IF
                NIP DUP car 3 PICK <> DUP INVERT IF
                    -ROT cdr cdr rplacd DROP NIP TRUE SWAP
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


: store_max ( n addr -- )
\ =========
    2DUP @ U> IF ! ELSE 2DROP THEN
;


: update_max ( -- )
\ ==========
    rdepth rp_max store_max
    DEPTH  sp_max store_max
;


: pair ( x y -- p )       \ zip
\ ====
    update_max
    2DUP null SWAP null OR IF
        2DROP nil
    ELSE
        2DUP
        SWAP car SWAP car cons
        ROT cdr ROT cdr RECURSE
        cons
    THEN
;


: append ( x y -- xy )
\ ======
    update_max
    SWAP ?DUP IF
        DUP car SWAP cdr ROT RECURSE cons
    THEN
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

\ NIL, APVAL und OBLIST in dieser Reihenfolge in die Objektliste eintragen
$oblist nil $apval $oblist nil cons cons cons $apval putprop DROP


: (>oblist) ( atom -- atom )
\ =========
    DUP $oblist $apval get DUP cdr ROT SWAP cons rplacd DROP
; ' (>oblist) IS >oblist


new-symbol T         DUP $apval putprop CONSTANT $t
new-symbol SUBR                         CONSTANT $subr
new-symbol STOP                         CONSTANT $stop
new-symbol QUOTE                        CONSTANT $quote
new-symbol PAUSE                        CONSTANT $pause
new-symbol NEXPR     DUP $apval putprop CONSTANT $nexpr
new-symbol LAMBDA    DUP $apval putprop CONSTANT $lambda
new-symbol GC                           CONSTANT $gc
new-symbol FUNCTION                     CONSTANT $function
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


$config nil $pause     putprop DROP     \ Einzelschritt-Debugging ein/AUS
$config $t  $gc        putprop DROP     \ Garbage Collection (GC) EIN/aus
$config $t  $exit      putprop DROP     \ BYE nach EXIT EIN/aus
$config nil $evalquote putprop DROP     \ EVALQUOTE ein/AUS
$config nil $echo      putprop DROP     \ Echo ein/AUS
$config nil $debug     putprop DROP     \ Debug-Ausgaben ein/AUS
$config nil $apval     putprop DROP     \ APVAL vor A-Liste auswerten ein/AUS


: enabled? ( sexpr -- flag )
\ ========
    $config SWAP get nil<>
;


: pause ( -- )
\ ===========
    $pause enabled? IF
        ." Press any key to continue..." KEY DROP CR
    THEN
;


: free ( -- flag )
\ ====
    CR ." Code: " {unused} U. ." bytes unused, rp_max=" rp_max @ U. bs EMIT
                                          ." , sp_max=" sp_max @ U. CR
    ." Node: "
    freelist @ ?DUP IF length #nodes SWAP - ELSE 0 THEN
    DUP 5 U.R ."  nodes used, " #nodes OVER - 5 U.R ."  / " #nodes 5 U.R
         ."  nodes free (" 100 100 ROT #nodes */ - 2 U.R ." %)" CR
    ." Char: "
    len_char @
    DUP 5 U.R ."  bytes used, " #chars OVER - 5 U.R ."  / " #chars 5 U.R
         ."  bytes free (" 100 100 ROT #chars */ - 2 U.R ." %)" CR
    ." Envs: " envlist @ length U. CR
    TRUE
;


: new-subr ( atom property cfa attr -- )
\ ========
    type-code [ 1 CHARS CELL+ ] LITERAL (new-object)
    ?DUP IF
        TUCK CHAR+ C!                                   \ attr (i.e. arity)
        SWAP ['] tlisp - OVER [ 2 CHARS ] LITERAL + !   \ cfa
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
    TUCK PAD SWAP MOVE PAD SWAP 2DUP {upper} type-symbol new-literal
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
            TUCK CHAR+ !
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
                [CHAR] [ EMIT
                ['] tlisp + >NAME .ID bs EMIT
                [CHAR] / EMIT
                C@ subr-arity AND 0 U.R
                [CHAR] ] EMIT
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
        [CHAR] ( EMIT
        notation @ 0= IF                \ dot notation
            DUP car RECURSE DROP
            SPACE [CHAR] . EMIT SPACE
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
                        SPACE [CHAR] . EMIT SPACE prin1
                    ELSE
                        notation @ DUP EMIT BL <> IF SPACE THEN
                    THEN
                THEN
            REPEAT
            DROP
        THEN
        [CHAR] ) EMIT
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
    update_max
    $debug enabled? IF
        .indent ." assoc: searching " OVER prin DROP ."  in " print
        pause
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


: >envlist ( env -- )
\ ========
    envlist @ cons envlist !
;


0 CONSTANT prog-env
1 CONSTANT prog-sp
2 CONSTANT prog-rp
3 CONSTANT prog-rp-eval


: prog-eval ( sexpr env -- sexpr )
\ =========
    RP@ prog-rp-eval prog_stack DUP adepth @ 1- SWAP a!
    eval
;


: prog-go ( argl ali -- prog ali sub nil )
\ =======
    prog_stack aempty? IF
        e_no_prog error
    ELSE
        >R car >R
        prog-sp prog_stack atop SP! 2DROP
        R> 2DUP SWAP member ?DUP IF
            NIP R>
            SWAP nil
            prog-env     prog_stack atop envlist !
            prog-rp-eval prog_stack atop RP!
        ELSE
            e_no_label error
        THEN
    THEN
;


: prog-return ( sexpr -- sexpr )
\ ===========
    prog_stack aempty? IF
        e_no_prog error
    ELSE
        prog_stack a> DROP RP!
        SWAP cdr envlist !
        SWAP >R SP! 2DROP DROP R>
    THEN
;


: prog ( argl ali -- sexpr )
\ ====
    \ PROG-Parameter mit NIL initialisieren
    \ und der A-Liste voranstellen
    OVER car ?DUP IF
        DUP length >R
        R@ 0 DO nil  LOOP nil
        R> 0 DO cons LOOP
        ROT bind
    THEN
    DUP >envlist

    \ PROG-Programm durchlaufen
    envlist @ SP@ RP@ 0 prog_stack >a
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
    prog_stack a> 2DROP DROP cdr envlist !
;


: exec ( subr args -- result )
\ ====
    $debug enabled? IF
        .indent ." exec: " OVER prin DROP SPACE print
    THEN
    SWAP (car) DUP -ROT DUP >R
    CHAR+ C@ subr-arity AND 0 ?DO
        DUP atom? IF e_too_few_args error LEAVE ELSE DUP car THEN
        SWAP cdr
    LOOP
    R> SWAP nil<> IF
        e_too_many_args error
    ELSE
        [ 2 CHARS ] LITERAL + @ ['] tlisp + EXECUTE
        SWAP CHAR+ C@ subr-ret AND DUP ret-number = IF
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
        car SWAP (car) CHAR+ COUNT 2 CHARS - 1 SWAP DO
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
            pause
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
                ELSE DUP $nexpr get ?DUP IF
                    NIP
                ELSE DUP $fexpr get ?DUP IF
                    NIP -ROT DUP -ROT nil cons cons SWAP ROT
                ELSE DUP 2 PICK assoc ?DUP IF
                    NIP cdr                         \ APPLY statt EVAL
                ELSE
                    e_no_lexpr error LEAVE
                THEN
                THEN
                THEN
                THEN
                FALSE                               \ Iteration
            THEN
            THEN
            THEN
        ELSE DUP car $lambda = IF
            cdr DUP car 3 ROLL 3 ROLL bind
            SWAP cdr car SWAP eval
            TRUE
        ELSE DUP car $funarg = IF
            NIP cdr DUP cdr car SWAP car FALSE
        ELSE
            e_no_lexpr error LEAVE
        THEN
        THEN
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
            DROP prog_stack aempty? IF w_cond_failed error LEAVE THEN
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
        OVER $fexpr get nil<> OR
        SWAP $nexpr get nil<> OR
    ELSE
        DROP FALSE
    THEN
;


: (eval) ( sexpr env -- sexpr )
\ ======
    update_max
    $debug enabled? IF
        .indent ." eval: " SWAP prin SWAP SPACE print
    THEN
    DUP >envlist
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
    envlist @ cdr envlist !
; ' (eval) IS eval


: tgetline ( -- u flag )
\ ========
    tstate 3 CHARS + tlen
    load_stack aempty? IF
        ACCEPT TRUE CR
    ELSE
        load_stack atop READ-LINE THROW
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


 1 CONSTANT token-eof       \ end of file
 2 CONSTANT token-nl        \ new line
 3 CONSTANT token-lbrace    \ left brace (
 4 CONSTANT token-rbrace    \ right brace )
 5 CONSTANT token-sbrace    \ right bracket ] (aka super parenthesis)
 6 CONSTANT token-dot       \ dot .
 7 CONSTANT token-quote     \ quote '
 8 CONSTANT token-function  \ function quote #'
 9 CONSTANT token-atom      \ atom
10 CONSTANT token-string    \ string


: handle-sharp ( -- token )
    readc [CHAR] ' = IF token-function ELSE tundo 0 THEN
;


: handle-comment ( -- token )   \ line comment
\ ==============
    tundo readc
    DUP [CHAR] ; = SWAP [CHAR] / = tstate C@ 0= AND OR IF
        token-nl tclear
    ELSE
        0
    THEN
;


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
                    e_rquote_missing error LEAVE
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
                     [CHAR] ] OF token-sbrace    ENDOF
                     [CHAR] . OF token-dot       ENDOF
                     [CHAR] ' OF token-quote     ENDOF
                     [CHAR] # OF handle-sharp    ENDOF
                     BL       OF 0 tnext         ENDOF
                     tab      OF 0 tnext         ENDOF
                     [CHAR] , OF 0 tnext         ENDOF
                                 handle-comment SWAP
                ENDCASE
            ELSE
                tstate DUP C@ 3 + CHARS + C@ [CHAR] " = IF
                    [CHAR] " = IF token-string ELSE 0 THEN
                ELSE
                    DUP BL = OVER tab      = OR
                             OVER [CHAR] , = OR
                             OVER [CHAR] ; = OR
                             OVER [CHAR] ( = OR
                             OVER [CHAR] ) = OR
                             OVER [CHAR] ] = OR SWAP 0= OR IF
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
    DUP  cnt-mask-count AND 1+      \ Zaehler erhoehen
    SWAP cnt-mask-flags AND OR      \ und Flags uebernehmen
;


: lbrace-- ( -- )
\ ========
    num_lbrace @ 0= IF
        e_unbalanced error
    ELSE
        -1 num_lbrace +!
    THEN
;


: quote-start ( -- )
\ ===========
    expr_stack a> count++ expr_stack >a
    cnt-flag-quote count++ expr_stack >a
    1 num_lbrace +!                             \ inkrementieren
;


: handle-quotes ( -- )
\ =============
    BEGIN
        expr_stack atop
        DUP cnt-flag-quote AND 0<> IF
            lbrace--
            nil SWAP
            cnt-mask-count AND 0 DO cons LOOP
            TRUE
        ELSE
            DROP FALSE
        THEN
    WHILE
        expr_stack a> DROP
    REPEAT
;


: handle-rbrace ( -- )
\ =============
    lbrace--
    expr_stack a> DUP cnt-flag-dot AND 0= IF
        nil SWAP
    ELSE cnt-mask-count AND 1 = IF
        expr_stack a>
    ELSE
        e_misplaced_dot error LEAVE
    THEN
    THEN
    cnt-mask-count AND 0 ?DO cons LOOP      \ ggf. leere Liste
    handle-quotes
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
    0 expr_stack >a
    BEGIN
        token
        CASE
            token-lbrace OF
                expr_stack a> count++ expr_stack >a
                0 expr_stack >a
                1 num_lbrace +!                     \ inkrementieren
                FALSE
                ENDOF
            token-rbrace OF
                handle-rbrace
                FALSE
                ENDOF
            token-sbrace OF
                num_lbrace @ 0= IF
                    e_unbalanced error LEAVE
                ELSE
                    BEGIN num_lbrace @ 0<> WHILE handle-rbrace REPEAT
                THEN
                FALSE
                ENDOF
            token-dot OF
                num_lbrace @ 0= expr_stack atop 0= OR IF
                    e_misplaced_dot error LEAVE
                ELSE
                    cnt-flag-dot expr_stack >a
                THEN
                FALSE
                ENDOF
            token-quote OF
                quote-start $quote FALSE
                ENDOF
            token-function OF
                quote-start $function FALSE
                ENDOF
            token-atom OF
                tstate DUP C@ 3 + CHARS +           \ c-addr
                tstate CHAR+ C@ tstate C@ -         \ count
                2DUP >snumber IF
                    NIP NIP new-number
                ELSE
                    DROP (new-symbol)
                THEN
                expr_stack a> count++ expr_stack >a
                handle-quotes
                DUP $stop =
                ENDOF
            token-string OF
                tstate DUP C@ 4 + CHARS +           \ c-addr + 1
                tstate CHAR+ C@ tstate C@ - 2 -     \ count  - 2
                new-string
                expr_stack a> count++ expr_stack >a
                handle-quotes
                FALSE
                ENDOF
            token-nl OF
                num_lbrace @ 0=
                expr_stack atop cnt-mask-count AND 0> AND
                ENDOF
            token-eof OF
                num_lbrace @ ?DUP IF
                    e_lbrace_open error LEAVE
                THEN
                TRUE
                ENDOF
            FALSE SWAP
        ENDCASE
    UNTIL
    expr_stack a> cnt-mask-count AND
;


: read ( -- sexpr )
\ ====
    read_stack adepth @ DUP 0= IF
        DROP (read) DUP
        0 ?DO SWAP read_stack >a LOOP
    THEN
    0= IF $eof ELSE read_stack a> THEN
    $echo enabled? IF print THEN
;


: gc-mark ( sexpr -- sexpr )
\ =======
    DUP cdr gc-used OR rplacd
;


: gc-unmark ( sexpr -- sexpr )
\ =========
    DUP cdr gc-clear rplacd
;


: gc-walk ( sexpr -- )
\ =======
    update_max
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
        read_stack acount 0 ?DO
            0 I read_stack a@ gc-walk
        LOOP

        \ Phase 4: Liste aller A-Listen rekursiv durchlaufen und
        \          alle benutzte Zellenpaare markieren
        envlist @ gc-walk

        \ Phase 5: Listenspeicher flach durchlaufen
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


: eval(quote) ( sexpr1 env flag -- sexpr2 )
\ ===========
    ( flag ) IF
        SWAP read nil cons cons SWAP        \ doublet vervollstaendigen
        evalquote
    ELSE
        eval
    THEN
;


: (load) ( c-addr u flag -- flag )
\ ======
    nil
    2SWAP 2DUP ." loading " TYPE CR
    load_stack afull? IF
        e_load_exceeded error
    ELSE
        read_stack a-mark -ROT                      \ READ-Stack verschieben
        R/O OPEN-FILE 0= DUP IF
            SWAP load_stack >a
            2SWAP OVER IF
                tgetline 2DROP tclear               \ skip TEST line
            THEN
            BEGIN
                read
                DUP $eof = OVER $stop = OR IF
                    TRUE
                ELSE
                    OVER 3 PICK eval(quote)
                    $echo enabled? IF print THEN
                    gc DROP
                    FALSE
                THEN
                NIP
            UNTIL
            2DROP
            load_stack a> CLOSE-FILE THROW
        ELSE
            NIP 2SWAP 2DROP
        THEN
        SWAP read_stack a-unmark
        tclear
    THEN
;


: .prompt ( -- )
\ =======
    $evalquote enabled? IF ." evalquote> " ELSE ." tlisp> " THEN
;


: top-level ( -- )      \ read-eval-print loop (repl)
\ =========
    BEGIN
        running @
    WHILE
        read_stack aempty? IF .prompt THEN      \ prompt
        read                                    \ read
        nil $evalquote enabled? eval(quote)     \ eval
        running @ IF print THEN DROP            \ print
        gc DROP                                 \ gc
    REPEAT
    $exit enabled? IF BYE THEN
;


: reset ( --- )
\ =====
    BEGIN                           \ reset LOAD
        load_stack aempty? INVERT
    WHILE
        load_stack a> CLOSE-FILE THROW
    REPEAT
    prog_stack aclear               \ reset PROG
    running @ IF
        tclear                      \ reset scanner
        read_stack aclear           \ reset parser
        expr_stack aclear
        nil envlist !               \ reset environment list
        SP0 @ SP!                   \ reset data stack (clearstack)
        rp_save @ RP!               \ reset return stack
        gc DROP                     \ garbage collection
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
         e_no_literal     OF ." Argument is not a literal: "
                                                           SWAP prin DROP ENDOF
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
         e_range          OF ." Index out of range: "        SWAP C@ EMIT ENDOF
         e_underflow      OF ." Stack underflow: "           SWAP C@ EMIT ENDOF
         e_overflow       OF ." Stack overflow: "            SWAP C@ EMIT ENDOF
         e_load_exceeded  OF ." Too many nested LOADs."                   ENDOF
         e_no_prog        OF ." No PROG for GO or RETURN."                ENDOF
         e_no_label       OF ." Label not found: "         SWAP prin DROP ENDOF
         w_cond_failed    OF ." COND fell through."                       ENDOF
                             ." CASE fell through."
    ENDCASE
    CR
    warning AND INVERT IF reset THEN
; ' (error) IS error


: .hello ( -- )          \ Begruessung des Anwenders
\ ======
    CR
    CR ." TinyLISP Version 0.7 ref 2023-03-18"
    CR ." Copyright (C) 1988-2023 Steffen Hieber"
    CR CR
;


: driver ( -- )     \ main
\ ======
    RP@ rp_save !
    running ON
    .hello
    S" tlisp.lsp" FALSE (load) DROP CR
    DEPTH 0= IF reset ELSE ." Data stack is not empty!" CR THEN
;


: reduce ( cfa n -- )
\ ======
    CREATE ( cfa n -- )
        , ,
    DOES> ( argl ali -- result )
        2@
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


' + 0 reduce plus
' - 0 reduce difference
' * 1 reduce times
' / 1 reduce quotient


: lany ( flag -- )  \ liefert <flag>, falls <flag> in Liste, sonst INVERT
\ ====
    CREATE
        ,
    DOES> ( argl ali -- result )
        @ ROT
        BEGIN
            DUP nil<>
        WHILE
            DUP car 3 PICK eval nil<>
            2 PICK = IF DROP INVERT nil ELSE cdr THEN
        REPEAT
        DROP NIP INVERT
;


FALSE lany land     \ logical and
TRUE  lany lor      \ logical or


: n-unary-op ( cfa1 cfa2 -- )
\ ==========
    CREATE
        SWAP , ,
    DOES> ( num1 -- num2 )
        OVER numberp IF
            SWAP number@ SWAP
            PERFORM
        ELSE
            NIP CELL+ @ ?DUP IF
                EXECUTE
            ELSE
                e_no_number error
            THEN
        THEN
;


' 1+     0       n-unary-op add1
' 1-     0       n-unary-op sub1
' NEGATE 0       n-unary-op minus
' 0=     ' FALSE n-unary-op zerop


: n-binary-op ( cfa -- )
\ ===========
    CREATE
        ,
    DOES> ( num1 num2 -- num3 )
        -ROT
        2DUP numberp SWAP numberp AND IF
            number@ SWAP
            number@ SWAP
            ROT
            PERFORM
        ELSE
            e_no_number error
        THEN
;


' >   n-binary-op greaterp  ( num1 num2 -- flag )
' <   n-binary-op lessp     ( num1 num2 -- flag )
' MOD n-binary-op remainder ( num1 num2 -- num3 )


\ some more functions
: eq     ( sexpr1 sexpr2 -- flag  ) = ;
: exitf  (               --       ) running OFF nil ;
: lambda ( argl ali      -- lexpr ) DROP $lambda SWAP cons ;
: nilf   ( argl ali      -- flag  ) 2DROP FALSE ;
: quote  ( argl ali      -- sexpr ) DROP car ;
: t      ( sexpr         -- sexpr ) NOOP ;
: terpri (               -- nil   ) CR nil ;


: apply1 ( argl ali -- sexpr )
\ ======
    SWAP DUP length DUP 2 3 check-args IF
        2 = IF
            OVER
        ELSE
            2DUP cdr cdr car SWAP (eval)    \ env
        THEN
        2DUP SWAP car SWAP (eval) -ROT
        SWAP cdr car SWAP eval
        ROT apply
    THEN
;


: eval1 ( argl ali -- sexpr )
\ =====
    SWAP DUP length DUP 1 2 check-args IF
        1 = IF
            OVER
        ELSE
            2DUP cdr car SWAP (eval)    \ env
        THEN
        SWAP car ROT (eval) SWAP (eval)
    THEN
;


: function ( argl ali -- sexpr )
\ ========
    SWAP DUP length 1 1 check-args IF
        car SWAP $funarg -ROT nil cons cons cons
    THEN
;


: >digit ( u -- char )
\ ======
    DUP 10 < IF [CHAR] 0 ELSE [ {char} A 10 - ] LITERAL THEN +
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
        1 = IF FALSE ELSE 2DUP cdr car SWAP eval nil<> THEN
        -ROT car SWAP eval
        DUP literal? IF
            (car) CHAR+ COUNT ROT (load)
        ELSE
            e_no_literal error
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


: unpack ( literal -- sexpr )
    DUP literal? IF
        (car) CHAR+ COUNT >R R@ 0 DO
            DUP I + 1 (new-symbol) SWAP
        LOOP
        DROP NIL R@ 0 DO CONS LOOP
        R> DROP
    ELSE
        e_no_literal error
    THEN
;


: pack ( sexpr -- symbol )
    0 >R
    BEGIN
        DUP (list?)
    WHILE
        DUP (car) DUP atom? IF
            (car) CHAR+ COUNT PAD R@ + SWAP DUP R> + >R MOVE
        ELSE
            DROP
        THEN
        cdr
    REPEAT
    DROP R>
    DUP IF
        PAD SWAP (new-symbol)
    THEN
;


\ @CODE
new-symbol ZEROP      $subr  ' zerop           ret-bool   1 OR new-subr
new-symbol UNPACK     $subr  ' unpack                     1    new-subr
new-symbol TIMES      $fsubr ' times           ret-number 2 OR new-subr
new-symbol TERPRI     $subr  ' terpri                     0    new-subr
$t                    $subr  ' t                          1    new-subr
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
new-symbol PACK       $subr  ' pack                       1    new-subr
new-symbol OR         $fsubr ' lor             ret-bool   2 OR new-subr
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
$function             $fsubr ' function                   2    new-subr
new-symbol FREE       $subr  ' free            ret-bool   0 OR new-subr
new-symbol FIXP       $subr  ' numberp         ret-bool   1 OR new-subr
$exit                 $subr  ' exitf                      0    new-subr
$evalquote            $fsubr ' evalquote                  2    new-subr
$eval                 $fsubr ' eval1                      2    new-subr
new-symbol EQ         $subr  ' eq              ret-bool   2 OR new-subr
new-symbol DIFFERENCE $fsubr ' difference      ret-number 2 OR new-subr
new-symbol CONS       $subr  ' cons                       2    new-subr
new-symbol COND       $fsubr ' evcon                      2    new-subr
new-symbol BIND       $subr  ' bind                       3    new-subr
new-symbol ATOM       $subr  ' atom?           ret-bool   1 OR new-subr
new-symbol ASSOC      $subr  ' assoc                      2    new-subr
new-symbol APPLY      $fsubr ' apply1                     2    new-subr
new-symbol APPEND     $subr  ' append                     2    new-subr
new-symbol AND        $fsubr ' land            ret-bool   2 OR new-subr
new-symbol ADD1       $subr  ' add1            ret-number 1 OR new-subr


: try-exec find-word IF EXECUTE ELSE DROP THEN ;
try-exec ENDPGM     \ TFLOAD (F83)
