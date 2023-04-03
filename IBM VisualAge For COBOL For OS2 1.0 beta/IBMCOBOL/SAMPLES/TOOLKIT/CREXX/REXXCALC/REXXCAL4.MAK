# NMAKE-compatible MAKE file for the REXX sample program REXXCAL4.EXE.

all: rexxcal4.exe rexxcal4.res

rexxcal4.obj: rexxcal4.h rexxcal4.c
        icc /c /o rexxcal4.c

rexxcal4.res: rexxcal4.h rexxcal4.rc
        rc -r rexxcal4.rc

rexxcal4.exe: rexxcal4.obj rexxcal4.res rexxcal4.def
        ilink /nofree rexxcal4,rexxcal4,nul,rexx,rexxcal4.def
        rc rexxcal4.res rexxcal4.exe

