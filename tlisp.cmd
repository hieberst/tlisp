@echo off

rem $Id: tlisp.cmd,v 1.5 2012-12-18 21:02:09 steffen Exp $

if not defined GFORTH set GFORTH=%ProgramFiles%\gforth\gforth.exe

cd /d %~dp0
rem "%GFORTH%" "%ProgramFiles%\gforth\ans-report.fs" tlisp.fs -e "print-ans-report bye"
"%GFORTH%" tlisp.fs -e "driver bye"
rem "%GFORTH%" tlisp.fs tests.fs
rem pause
