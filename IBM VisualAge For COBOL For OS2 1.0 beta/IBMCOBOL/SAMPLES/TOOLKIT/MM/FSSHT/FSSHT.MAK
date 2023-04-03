# SCCSID = @(#)makefile 13.6 92/04/23
#################################################################################
#
# Makefile for Toolkit File System Stream Handler
#
#   Generates one dll:
#         fssht.dll          - File System Stream Handler Dll
#
#
# Copyright (c) IBM Corporation  1990, 1991, 1993
# All Rights Reserved
#
#################################################################################

.SUFFIXES: .com .sys .exe .obj .mbj .asm .inc .def .lrf .crf .ref \
           .lst .sym .map .c .h .lib .msg .pro .txt

NAMEDLL=FSSHT

###########################################################
# Definitions for C Compiler
# Compiler flags:
#    /C+  Compile only
#    /Gd- Static link run time library
#    /Ge- Build a .DLL file
#    /Gm+ Link with multi-threaded version of library
#    /Gs- Remove stack probes
#    /Re  Generate code for C Set/2 run-time environment
#    /J-  Set unspecified char variables to signed char
#    /Kb- Suppress basic diagnostics messages
#    /Ki- Suppress messages about uninitialized variables
#    /Ms  Use SYSTEM linkage for functions
#    /Sm  Allow migration extensions
#    /Sn  Do not allow DBCS
#    /Ss+ Allow double slash format for comments
#    /D   preprocessor macro name INCL_32
###########################################################

CC=icc
ASM=masm

CFLAGS=  /C+ /Gd- /Ge- /Gm+ /Gs- /Re /J-  /Ms /Sm /Sn /Ss+ /DINCL_32 /DINCL_OS2MM /I. /DINCL_NEWSH

AFLAGS = -ML -T -N
###########################################################################
# Definitions for linker
# LIBS:
#    DDE4MBS.LIB - C Set/2 Multi-threaded Standard Library function calls
#    DDE4MBM.LIB - C Set/2 Multi-threaded Migration Library function calls
#    OS2386.LIB  - OS.2 function calls
#    MMPM2.LIB   - MMPM2 function calls
###########################################################################
LINK=ilink /nofree
MAP = mapsym $(NAMEDLL)
LFLAGS= /map /nod /noe /noi /packcode /packdata /exepack /align:16
LIBS= CPPOM30.LIB os2386.lib MMPM2.LIB

#################################
# Inference Rules
#################################
.c.obj:
        $(CC) $(CFLAGS) /Fo$(<R).obj $(C_LST) $(<R).c

.asm.obj:
   $(ASM) $(AFLAGS) $(<R).asm,$(<R).obj;

#################################
# Object file lists
#################################

DLLOBJ1 = fsshread.obj \
          fsshwrit.obj \
          shioutil.obj \
          shmisc.obj   \
          shrouter.obj \
          shstart.obj  \
          shstop.obj   \
          fsshseek.obj \
          fsshass.obj  \
          hhpheap.obj  \
          seekcalc.obj
DLLOBJ2 = fsshcrea.obj \
          shnegot.obj  \
          shdestry.obj \
          shgprot.obj  \
          sheprot.obj  \
          shiprot.obj  \
          shinit.obj   \

LINKOBJS = fsshgdat.obj


#################################
# Target Descriptions
#################################
all: dll

#################################
# fssht.dll Target Descriptions
#################################

dll: $(NAMEDLL).dll

$(NAMEDLL).dll:   $(DLLOBJ1) $(DLLOBJ2) fsshgdat.obj fssht.mak $(NAMEDLL).lrf $(NAMEDLL).def
        $(LINK)  @$(NAMEDLL).lrf
        $(MAP)

fsshgdat.obj:
        $(CC) $(CFLAGS) fsshgdat.c


$(NAMEDLL).def: fssht.mak
        @echo Creating file <<$(@B).def
LIBRARY FSSH INITINSTANCE
DATA MULTIPLE NONSHARED
PROTMODE
SEGMENTS
   FSSH_SHR       CLASS 'DATA' SHARED
HEAPSIZE 0
BASE = 0x1B030000
<<keep

$(NAMEDLL).lrf: fssht.mak
        @echo Creating file <<$(@B).lrf
$(DLLOBJ1)+
$(DLLOBJ2)+
$(LINKOBJS)
$(NAMEDLL).dll
$(NAMEDLL).map $(LFLAGS)
$(LIBS)
$(NAMEDLL).def;
<<keep





