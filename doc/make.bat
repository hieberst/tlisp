@echo off
if not defined VCINSTALLDIR set VCINSTALLDIR=%ProgramFiles%\Microsoft Visual Studio 9.0\VC
if exist "%VCINSTALLDIR%\bin\vcvars32.bat" call "%VCINSTALLDIR%\bin\vcvars32.bat"
nmake clean RM=del
nmake
pause
