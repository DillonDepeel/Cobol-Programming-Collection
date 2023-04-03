# NMAKE-compatible MAKE file for the REXX sample program REXXCAL2.EXE.

all: rexxcal2.exe rexxcal2.res

rexxcal2.obj: rexxcal2.h rexxcal2.c
        icc /c /o rexxcal2.c

rexxcal2.res: rexxcal2.h rexxcal2.rc
        rc -r rexxcal2.rc

rexxcal2.exe: rexxcal2.obj rexxcal2.res rexxcal2.def
        ilink /nofree rexxcal2,rexxcal2,nul,rexx,rexxcal2.def
        rc rexxcal2.res rexxcal2.exe

