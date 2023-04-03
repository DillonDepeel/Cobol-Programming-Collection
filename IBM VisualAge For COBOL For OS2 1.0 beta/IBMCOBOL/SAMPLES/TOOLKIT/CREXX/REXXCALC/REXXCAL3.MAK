# NMAKE-compatible MAKE file for the REXX sample program REXXCAL3.EXE.

all: rexxcal3.exe rexxcal3.res

rexxcal3.obj: rexxcal3.h rexxcal3.c
        icc /c /o rexxcal3.c

rexxcal3.res: rexxcal3.h rexxcal3.rc
        rc -r rexxcal3.rc

rexxcal3.exe: rexxcal3.obj rexxcal3.res rexxcal3.def
        ilink /nofree rexxcal3,rexxcal3,nul,rexx,rexxcal3.def
        rc rexxcal3.res rexxcal3.exe

