# NMAKE-compatible MAKE file for the REXX sample program REXXCALC.EXE.

all: rexxcalc.exe rexxcalc.res 

rexxcalc.obj: rexxcalc.h rexxcalc.c
        icc /c /o rexxcalc.c

rexxcalc.res: rexxcalc.h rexxcalc.rc
        rc -r rexxcalc.rc

rexxcalc.exe: rexxcalc.obj rexxcalc.res rexxcalc.def
        ilink /nofree rexxcalc,rexxcalc,nul,rexx,rexxcalc.def
        rc rexxcalc.res rexxcalc.exe

