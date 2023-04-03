# NMAKE-compatible MAKE file for the REXX sample program macro.dll.

rxnlsinf.dll:     rxnlsinf.obj rxnlsinf.def
         ilink /nofree rxnlsinf.obj,rxnlsinf.dll,,REXX,rxnlsinf.def

rxnlsinf.obj:     rxnlsinf.c
         icc -c -Ge- rxnlsinf.c
