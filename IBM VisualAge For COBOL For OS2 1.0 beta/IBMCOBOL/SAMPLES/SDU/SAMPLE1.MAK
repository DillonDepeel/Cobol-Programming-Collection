# Makefile for NMAKE utility to build SAMPLE1.EXE.
#
# Change the pathes for Compiler and Linker to run the makefile
# with 'NMAKE SAMPLE1.MAK'

.SUFFIXES:

.SUFFIXES: .c

SAMPLE1.EXE:  \
  SAMPLE1.OBJ \
  SAMPLE1.MAK
   @REM @<<SAMPLE1.@0
     /BATCH /CO /MAP /NOD /STACK:10000 /PM:VIO /DE +
     SAMPLE1.OBJ,
     SAMPLE1.EXE,
     SAMPLE1.MAP,
     L:\TOOLKT20CPP\OS2LIB\OS2386.LIB+L:\IBMCPP\LIB\DDE4MBS.LIB,
     SAMPLE1.DEF
     ;
<<
   L:\TOOLKT20\OS2BIN\LINK386.EXE @SAMPLE1.@0

{.}.c.obj:
    L:\IBMCPP\BIN\ICC.EXE /Ss /Ti /Gm /Sm /IN:\DDC2\SRCREF /C $<

SAMPLE1.OBJ:  SAMPLE1.C \
   SAMPLE1.MAK
