@echo off

rem $Id: tlisp.cmd,v 1.4 2010-11-19 08:37:01 steffen Exp $

if not defined GFORTHPATH set GFORTHPATH=%ProgramFiles%\gforth

rem "%GFORTHPATH%\gforth.exe" -p .
"%GFORTHPATH%\gforth.exe" -p . tlisp.fs
pause

