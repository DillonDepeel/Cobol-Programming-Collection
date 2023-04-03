# NMAKE-compatible MAKE file for the REXX sample program REXXCAL1.EXE.

all: rexxcal1.exe rexxcal1.res

rexxcal1.obj: rexxcal1.h rexxcal1.c
        icc /c /o rexxcal1.c

rexxcal1.res: rexxcal1.h rexxcal1.rc
        rc -r rexxcal1.rc

rexxcal1.exe: rexxcal1.obj rexxcal1.res rexxcal1.def
        ilink /nofree rexxcal1,rexxcal1,nul,rexx,rexxcal1.def
        rc rexxcal1.res rexxcal1.exe

