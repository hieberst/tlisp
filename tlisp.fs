\                   TinyLISP (TLISP)
\
\              Version 1.0 ref 2000-04-02
\
\        Written (w) 1993-2000 by Steffen Hieber
\
\        RCS $Id: tlisp.fs,v 1.6 2003-10-12 16:48:20 steffen Exp $
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


2 CELLS CONSTANT sizeof_node    \ Groesse eines Knotens
1 CHARS CONSTANT sizeof_char    \ Groesse eines Zeichens


CREATE mem_node num_nodes sizeof_node * ALLOT   \ Listen-Speicher erzeugen
CREATE mem_char num_chars sizeof_char * ALLOT   \ Objekt-Speicher erzeugen


VARIABLE freelist               \ Zeiger auf das erste freie Zellenpaar
VARIABLE oblist			\ Objektliste
VARIABLE len_char		\ Aktuelle Grösse des Objektspeichers


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


: new ( -- sexpr )     \ der FREELIST ein Zellenpaar entnehmen
\ ===
    freelist @
    DUP (cdr) DUP
    nil = IF DROP enomem_node error ELSE freelist ! THEN
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


: cons ( sexpr1 sexpr2 -- sexpr )
\ ====
    new SWAP (rplacd)
        SWAP (rplaca)
;


: atom? ( sexpr -- flag )    \ liefert TRUE, wenn Atomknoten, sonst FALSE
\ =====
    (car) mem_char >=
;


: list? ( sexpr -- flag )
\ =====
    atom? INVERT
;


: numberp ( sexpr -- flag )	\ liefert TRUE, wenn Zahl, sonst FALSE
\ =======
    DUP atom? IF (car) NUMBER? SWAP DROP ELSE DROP false THEN
;


: car ( sexpr -- sexpr )
\ ===
    DUP atom? IF DROP einval_sexpr error ELSE (car) THEN
;


: object ( c-addr u -- atom)
\ ======
    len_char @ DUP mem_char +	    \ Naechste freie Speicheradresse ermitteln
    ROT ROT OVER + 1+               \ Neue Groesse des Objektspeichers ermitteln
    DUP num_chars > IF
        2DROP 2DROP enomem_char
    ELSE
        len_char !
        2DUP SWAP 1+ 4 ROLL SWAP ROT CMOVE 
        OVER C! nil cons
    THEN
;


: putprop ( atom value property -- atom )
\ =======
    ROT DUP atom? IF
        DUP (cdr) nil = IF
	    ROT nil cons ROT SWAP cons (RPLACD)
	ELSE
	THEN
    ELSE
        DROP DROP DROP einval_atom error
    THEN
;


: last ( sexpr -- sexpr )
\ ====
    DUP atom? IF 
        DROP einval_sexpr error 
    ELSE 
    	BEGIN
	    DUP (cdr) list?
	WHILE
	    (cdr)
	REPEAT
    THEN
;


: prin1 ( sexpr -- sexpr ) recursive
\ =====
    DUP atom? IF 
        DUP (car) COUNT TYPE
    ELSE 
        ." ("
	DUP (car) prin1 DROP
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
		        (cdr) DUP prin1 DROP
		    ELSE
	                BL EMIT
	                (cdr) DUP (car) prin1 DROP
		    THEN
	        REPEAT
                DROP
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
    prin1
    BL EMIT
;


: eval ( sexpr -- sexpr )
\ ====
;


: read ( -- sexpr )
\ ===
;


: hello ( -- )          \ Begruessung des Anwenders
\ =====
    CR ." TLISP Version 1.0 ref 2002-09-12"
    CR ." Copyright (C) 1993-2003 Steffen Hieber"
    CR
;


S" NIL"    object DROP
S" OBLIST" object oblist ! 
S" APVAL"  object

DUP oblist @ nil cons cons nil SWAP cons SWAP
oblist @ ROT ROT putprop DROP


hello


\ some test cases in forth


\ display a dotted pair (a . b)
S" a" object
S" b" object
cons
print drop

\ display a simple list (d e f)
S" f" object
nil
cons
S" e" object
SWAP
cons
S" d" object
SWAP
cons
print drop

\ display a more complex list (g (h . i))
s" h" object
s" i" object
cons
s" g" object
swap
cons
print drop

terpri
terpri

