; $Id$

#NoEnv
#Warn

SetWorkingDir, %A_ScriptDir%

F83_COM := "C:\Forth\F83\F83.COM"

Run, %F83_COM%, %A_ScriptDir%,, F83_PID
WinWait, ahk_pid %F83_PID%
SendInput,
( LTrim
  (PUTPROP {ASC 39}CONFIG T {ASC 39}EXIT`)
  open tfload.blk 2 load
  open eaker.f83  tfload
  open ans.f83    tfload
  open tlisp.fs   tfload
  driver{Enter}
)
; TLISP hat (noch) einen eigenen Eingabepuffer (tstate), Warten auf Prompt
Sleep, 8000
SendInput, (PUTPROP {ASC 39}CONFIG T {ASC 39}EXIT`){Enter}
Exit 0
