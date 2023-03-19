@echo off
REM
REM Installation script for SEE4CB (9/1/99).
REM
if (%windir%) == () goto WIN31
echo COPY SEE32.DLL %windir%
copy      SEE32.DLL %windir% > NUL
goto DONE
:WIN31
echo COPY SEE32.DLL C:\WINDOWS
copy      SEE32.DLL C:\WINDOWS > NUL
:DONE
echo Installation is done. Example programs must be edited with
echo your TCP/IP parameters before compiling.
:FINI

 