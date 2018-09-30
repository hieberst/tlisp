@echo off

rem $Id$

rem Run TLISP on GForth 0.4.0
rem
rem .ID ist erst ab 0.6.0 definiert

set GFORTH_HOME=C:\forth\gforth\0.4.0
set TLISP_HOME=%~dp0

cd /d %TLISP_HOME%

set GFORTHPATH=%GFORTH_HOME%;%TLISP_HOME%
%GFORTH_HOME%\gforth -m 400k -d 30k -r 30k -e "' .NAME Alias .ID" tlisp.fs -e driver
