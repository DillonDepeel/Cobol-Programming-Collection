# NMAKE-compatible MAKE file for the REXX sample program CALLREXX.EXE.

callrexx.exe:     callrexx.obj
    ILINK /NOFREE /PM:VIO callrexx.obj,callrexx.exe,,REXX,,

callrexx.obj:     callrexx.c
    icc -c callrexx.c

