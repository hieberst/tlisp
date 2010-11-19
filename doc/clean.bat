@echo off
if not exist VCINSTALLDIR set VCINSTALLDIR=D:\Program Files\Microsoft Visual Studio\VC98
call "%VCINSTALLDIR%\bin\vcvars32.bat"
nmake clean RM=del
