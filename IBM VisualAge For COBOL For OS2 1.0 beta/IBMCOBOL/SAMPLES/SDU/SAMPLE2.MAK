# Makefile for NMAKE utility to build SAMPLE1.EXE.
#
# Change the pathes for Compiler and Linker to run the makefile
# with 'NMAKE SAMPLE1.MAK'

.SUFFIXES:

.SUFFIXES: .c

SAMPLE2.EXE:  \
  SAMPLE2.OBJ \
  SAMPLE2.MAK
   @REM @<<SAMPLE2.@0
     /BATCH /CO /MAP /NOD /STACK:10000 /PM:VIO /DE +
     SAMPLE2.OBJ,
     SAMPLE2.EXE,
     SAMPLE2.MAP,
     L:\TOOLKT20CPP\OS2LIB\OS2386.LIB+L:\IBMCPP\LIB\DDE4MBS.LIB,
     SAMPLE2.DEF
     ;
<<
   L:\TOOLKT20\OS2BIN\LINK386.EXE @SAMPLE2.@0

{.}.c.obj:
    L:\IBMCPP\BIN\ICC.EXE /Ss /Ti /Gm /Sm /IN:\DDC2\SRCREF /C $<

SAMPLE2.OBJ:  SAMPLE2.C \
   SAMPLE2.MAK
