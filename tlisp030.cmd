@echo off

rem $Id$

rem Run TLISP on GForth 0.3.0
rem
rem SP0 ist erst ab 0.4.0 definiert
rem .ID ist erst ab 0.6.0 definiert

set GFORTH_HOME=C:\forth\gforth\0.3.0
set TLISP_HOME=%~dp0

cd /d %TLISP_HOME%

set GFORTHPATH=%GFORTH_HOME%;%TLISP_HOME%
%GFORTH_HOME%\gforth -m 400k -d 30k -r 30k -e "' .NAME Alias .ID" -e "' S0 Alias SP0" tlisp.fs -e driver
