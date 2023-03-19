@echo off
REM Compile and link using Fujitsu COBOL
REM Copy COBOL.CBI from COBOL97 directory
REM Environment variable LIB must be set to COBOL97 directory.
if exist %1.CBL goto START
echo %1.CBL does not exist.
goto FINI
:START
COBOL32 -iCOBOL.CBI %1.CBL
if errorlevel 1 goto FINI
LINK %1.OBJ SEE32.LIB PROJECT.RES F3BICIMP.LIB LIBC.LIB KERNEL32.LIB
:FINI
