\                   TinyLISP (TLISP)
\
\              Version 1.0 ref 2000-04-02
\
\        Written (w) 1993-2000 by Steffen Hieber
\
\        RCS $Id: tlisp.fs,v 1.5 2003-10-12 16:44:23 steffen Exp $
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
\            CELL, CELLS und CHARS. Erzeugung des Shell-Skripts TLISP zum
\            Start von GForth und TLISP.
\
\ 16.10.1997 Kontrolle der Programmentwicklung durch das RCS gestartet. 
\
\ 02.04.2000 Wiederaufnahme der Entwicklung.
\


FORTH DEFINITIONS       \ Grund-Vokabular erweitern ...

VOCABULARY tlisp        \ Vokabular TLISP anlegen
tlisp DEFINITIONS       \ alle neuen Woerter ab jetzt in das Vokabular TLISP


: hello ( -- )          \ Begruessung des Anwenders
\ =====
    CR ." TLISP Version 1.0 ref 2002-09-12"
    CR ." Copyright (C) 1993-2002 Steffen Hieber"
;


4096 CONSTANT num_nodes   \ Anzahl der vorhandenen Zellenpaare in LSTMEM
2048 CONSTANT num_chars   \ Groesse des Objekt-Speichers OBJMEM in Zeichen


2 CELLS CONSTANT sizeof_node    \ Groesse eines Knotens
1 CHARS CONSTANT sizeof_char    \ Groesse eines Zeichens


CREATE lstmem num_nodes sizeof_node * ALLOT   \ Listen-Speicher erzeugen
CREATE objmem num_chars sizeof_char * ALLOT   \ Objekt-Speicher erzeugen


VARIABLE freelist               \ Zeiger auf das erste freie Zellenpaar


0 CONSTANT nil                  \ "not in list"


: (rplaca) ( list newhead -- list ) OVER lstmem +        ! ;
: (rplacd) ( list newtail -- list ) OVER lstmem + CELL + ! ;


: (car) ( list -- head ) lstmem +        @ ;
: (cdr) ( list -- tail ) lstmem + CELL + @ ;


: init ( -- )         \ Listen-Speicher initialisieren
\ ====
    num_nodes sizeof_node * 0 DO
        I nil (rplaca) I sizeof_node + (rplacd) DROP
    sizeof_node +LOOP
    num_nodes 1- sizeof_node * nil (rplacd) DROP
    0 freelist !
; init


1 CONSTANT enomem       \ out of memory
2 CONSTANT einval       \ invalid argument


: error ( n -- )      \ Fehlerbehandlung !!!
\ =====
    CR ." ERROR! "
    CASE enomem OF ." Out of memory."      ENDOF
         einval OF ." Invalid argument."   ENDOF
                   ." CASE fell through."
    ENDCASE
;


: new ( -- sexpr )     \ der FREELIST ein Zellenpaar entnehmen
\ ===
    freelist @
    DUP (cdr) DUP
    nil = IF DROP enomem error ELSE freelist ! THEN
;


: cons ( sexpr1 sexpr2 -- sexpr )
\ ====
    new SWAP (rplacd)
        SWAP (rplaca)
;


: atom? ( sexpr -- flag )    \ liefert TRUE, wenn Atomknoten, sonst FALSE
\ =====
    (car) objmem >=
;


: number? ( sexpr -- flag )	\ liefert TRUE, wenn Zahl, sonst FALSE
\ =======
    dup atom? IF ELSE DROP false THEN
;


: car ( sexpr -- sexpr )
\ ===
    dup atom? IF DROP einval error ELSE (car) THEN
;


: new_atom ( c-addr u -- atom)
\ ========
    type
;


s" NIL" new_atom


hello
