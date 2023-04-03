# NMAKE-compatible MAKE file for the REXX sample program macro.dll.

macro.dll:     macro.obj macro.def
         ilink /nofree macro.obj,macro.dll,,REXX,macro.def

macro.obj:     macro.c
         icc -c -Ge- macro.c

