@echo off
if not defined VCINSTALLDIR set VCINSTALLDIR=%ProgramFiles%\Microsoft Visual Studio 9.0\VC
call "%VCINSTALLDIR%\bin\vcvars32.bat"
nmake clean RM=del
