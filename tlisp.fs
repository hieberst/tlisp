\                   TinyLISP (TLISP)
\
\              Version 1.0 ref 1997-10-15
\
\        Written (w) 1993-1997 by Steffen Hieber
\
\        RCS $Id: tlisp.fs,v 1.3 1997-10-16 00:38:29 steffen Exp $
\
\
\ 07.03.1993 TFLOAD aus dem Buch "Forth 83" von Zech uebernommen. Definition
\            des Vokabulars TLISP. Erstellung der Batch-Datei TLISP.BTM für
\            den Start von TLISP unter 4DOS.
\
\ 08.03.1993 Das Wort FREE ermittelt die Größe des zur Verfügung stehenden
\            freien Speichers.
\
\ 09.03.1993 Verbesserung der Batch-Datei TLISP.BTM, die Daten sind jetzt vom
\            Programm (F83) getrennt. Der Benutzer wird mit HELLO begrüßt.
\            Die beiden Speicherbereiche LSTMEM und OBJMEM werden angelegt,
\            ihre Größe ist von den beiden Konstanten MAXLST und MAXOBJ ab-
\            hängig. Die Variable FREELIST zeigt auf das erste freie Zellen-
\            paar  im Speicher LSTMEM. Zugriff auf LSTMEM  über die  Worte
\            (CAR), (CDR), (RPLACA) und (RPLACD). Der Listen-Speicher wird
\            mittels INIT initialisiert. NIL.
\
\ 10.03.1993 Eaker-Case in EAKER.F83 implementiert. Fehlerbehandlung: ERROR.
\            Die Worte NEW und CONS. TLISP.BTM laedt EAKER.F83.
\
\ ===========================================================================
\
\ 15.10.1997 Umstellung des bisherigen Codes auf GNU Forth Version 0.3.0.
\            Der Eaker-Case ist bereits in GForth implementiert, auch TFLOAD
\            ist nicht mehr nötig. Den F83-eigenen Alias (S habe ich durch
\            den Standard ( ersetzt. Das Wort FREE war falsch und zudem noch
\            redundant, da mit UNUSED der freie Speicher ermittelt werden
\            kann - entfernt. Umstellung der Adreßarithmetik auf die Worte
\            CELL, CELLS und CHARS. Erzeugung des Shell-Skripts TLISP zum
\            Start von GForth und TLISP.
\
\ 16.10.1997 Kontrolle der Programmentwicklung durch das RCS gestartet. 


FORTH DEFINITIONS       \ Grund-Vokabular erweitern ...

VOCABULARY tlisp        \ Vokabular TLISP anlegen
tlisp DEFINITIONS       \ alle neuen Wörter ab jetzt in das Vokabular TLISP


: hello ( -- )          \ Begrüßung des Anwenders
\ =====
    CR ." TLISP Version 1.0 ref 1997-10-15"
    CR ." Copyright (C) 1993-1997 Steffen Hieber"
;


2048 CONSTANT maxobj    \ Größe des Objekt-Speichers OBJMEM in Zeichen
4096 CONSTANT maxlst    \ Anzahl der vorhandenen Zellenpaare in LSTMEM


CREATE objmem maxobj 1 chars * ALLOT   \ Objekt-Speicher erzeugen
CREATE lstmem maxlst 2 cells * ALLOT   \ Listen-Speicher erzeugen


VARIABLE freelist               \ Zeiger auf das erste freie Zellenpaar


0 CONSTANT nil                  \ "not in list"


: (rplaca) ( list newhead -- list ) OVER lstmem +        ! ;
: (rplacd) ( list newtail -- list ) OVER lstmem + cell + ! ;

: (car) ( list -- head ) lstmem +        @ ;
: (cdr) ( list -- tail ) lstmem + cell + @ ;


: init ( -- )         \ Listen-Speicher initialisieren
\ ====
    maxlst 2 cells * 0 DO
        I nil (rplaca) I 2 cells + (rplacd) DROP
    2 cells +LOOP
    maxlst 1- 2 cells * nil (rplacd) DROP
    0 freelist !
; init


: error ( n -- )      \ Fehlerbehandlung !!!
\ =====
    CR ." ERROR! "
    CASE  1 OF ." Out of memory."      ENDOF
               ." CASE fell through."
    ENDCASE
;


: new ( -- sexpr )     \ der FREELIST ein Zellenpaar entnehmen
\ ===
    freelist @
    DUP (cdr) DUP
    nil = IF 1 error ELSE freelist ! THEN
;


: cons ( sexpr1 sexpr2 -- sexpr )
\ ====
    new SWAP (rplacd)
        SWAP (rplaca)
;
