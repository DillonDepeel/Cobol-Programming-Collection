# NMAKE-compatible MAKE file for the REXX sample program REXXUTIL.DLL.

rexxutil.dll:     rexxutil.obj  rexxutil.def
         ILINK /NOFREE rexxutil.obj,rexxutil.dll,,REXX,rexxutil.def;

rexxutil.obj:     rexxutil.c
         icc -c -Ge- rexxutil.c


