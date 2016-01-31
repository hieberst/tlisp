@echo off

rem $Id$

if not defined GFORTH set GFORTH=%ProgramFiles%\gforth\gforth.exe

cd /d %~dp0
"%GFORTH%" tlisp.fs -e "driver"

rem "%GFORTH%" "%ProgramFiles%\gforth\ans-report.fs" tlisp.fs -e "print-ans-report bye"
rem "%GFORTH%" tlisp.fs tests.fs
rem pause
