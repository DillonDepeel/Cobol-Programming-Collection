#################################################################################
#
# Makefile for Toolkit MMotion Image IOProc
#
#   Generates
#         mmotproc.dll       - M Motion Image IOproc
#
#
# Copyright (c) IBM Corporation  1991, 1993
# All Rights Reserved
#
#################################################################################

.SUFFIXES: .exe .obj .asm .inc .def .lrf .lst .sym .map .c .h .lib .dlg .txt .cod  \
           .res .rc .l$(LANG) .dll

NAMEDLL=MMOTTK
NAMERES=MMOTPROC

#################################
# Definitions for C Compiler
# Compiler options (C Set/2 (Toronto))
#    Ge- == Build .DLL (default is .exe)
#    Gm+ == Multithreaded libraries
#    Gn+ == Do not generate default libraries in object
#    Q+  == Supress logo
#    O+  == Optimize code (don't use with Ti)
#    Kp+ == Produce messages about unused function parameters
#    Sn+ == Provide DBCS support in runtime string routines
#    Fl+ == Produce listing
#    L+  == Produce listing (hum, why 2)
#    Ls+ == Include source in listing
#################################
CC=icc
CFLAGS=/C /Ge- /Gm+ /Gn+  /Q+  /Sn+ /DLINT_ARGS

#################################
# Definitions for linker
# LIBRARIES:
#  OS2386 == 386 Library
#  DDE4MBS== DLL Library
#  MMPM2  == MMPM2 Library
#################################
LINK386=ilink /nofree
MAPSYM=mapsym
LFLAGS= /map /nod /noi /noe
LIBS= OS2386.LIB CPPOM30.LIB MMPM2.LIB

#########################################
# Definitions for RESOURCE compiler
#########################################
RC   = rc

#################################
# Object file lists
#################################
OBJS = mmotproc.obj mmotfunc.obj

#################################
# Inference Rules
#################################
.c.obj:
        $(CC) $(CFLAGS) /Fo$(<R).obj $(<R).c

#################################
# Target Descriptions
#################################
all: res dll

#################################
# DLL Target Descriptions
#################################
dll: $(NAMEDLL).dll

$(NAMEDLL).dll: $(OBJS) mmottk.mak $(NAMEDLL).lrf $(NAMEDLL).def
        $(LINK386)  @$(NAMEDLL).lrf
        $(MAPSYM) $(NAMEDLL)
        $(RC) $(NAMERES).res $*.dll

################################
# Make DEF file
################################
$(NAMEDLL).def: mmottk.mak
        @echo Creating file <<$(@B).def
LIBRARY $(NAMEDLL) INITINSTANCE TERMINSTANCE
DATA MULTIPLE NONSHARED
SEGMENTS
   shardata CLASS 'DATA' SHARED
HEAPSIZE 0
BASE = 0x1B130000
IMPORTS
EXPORTS
   IOProc_Entry    @1
<<keep

################################
# Make link response file
################################
$(NAMEDLL).lrf: mmottk.mak
        @echo Creating file <<$(@B).lrf
$(OBJS)
$(NAMEDLL).dll
$(NAMEDLL) $(LFLAGS)
$(LIBS)
$(NAMEDLL).def;
<<keep

##################################
# RES Target Descriptions
##################################
res: $(NAMERES).res
$(NAMERES).res: $(NAMERES).rc mmottk.mak
        @echo $@ > con
        $(RC) -r $*.rc $@
