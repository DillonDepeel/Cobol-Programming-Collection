# SCCSID = @(#)makefile 13.6 92/04/23
#################################################################################
#
# Makefile for Toolkit AVI IO Procedure
#
#   Generates one dll:
#         uliot.dll          - AVI IO Proc DLL
#
#
# Copyright (c) IBM Corporation  1990, 1991, 1993
# All Rights Reserved
#
#################################################################################

.SUFFIXES: .com .sys .exe .obj .mbj .asm .inc .def .lrf .crf .ref \
           .lst .sym .map .c .h .lib .msg .pro .txt

NAMEDLL=ULIOT

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

CFLAGS=  /C+ /Gd- /Ge- /Gm+ /Gs- /Re /I.  /Ms /Sm /Sn /Ss+ /DINCL_32 /DINCL_BASE /DINCL_OS2MM

AFLAGS = -ML -T -N
###########################################################################
# Definitions for linker
# LIBS:
#    CPPOM30.LIB - C Set/2 Multi-threaded Standard Library function calls
#    OS2386.LIB  - OS.2 function calls
#    MMPM2.LIB   - MMPM2 function calls
###########################################################################
LINK=ilink /nofree
MAP = mapsym $(NAMEDLL)
LFLAGS= /map /nod /noe /noi /packcode /packdata /exepack /align:16
LIBS= CPPOM30.LIB os2386.lib MMPM2.LIB

#################################
# Definitions for resource compiler
#################################

RC = rc


#################################
# Inference Rules
#################################
.c.obj:
        $(CC) $(CFLAGS) /Fo$(<R).obj $(C_LST) $(<R).c

.asm.obj:
   $(ASM) $(AFLAGS) $(<R).asm,$(<R).obj;

.rc.res:
        $(RC) /r $(<B).rc

#################################
# Object file lists
#################################

DLLOBJS1 =      iocodec.obj   \
                ioclose.obj   \
                iodll.obj     \
                ioentry.obj   \
                ioformat.obj  \
                ioheader.obj  \
                ioopen.obj    \
                ioseek.obj    \
                ioset.obj     \
                iosrv.obj     \
                ioedit.obj    \
                hhpheap.obj   \
                checkmem.obj

DLLOBJS2 =      ulcodec.obj   \
                ulgdat.obj    \
                ulident.obj   \
                ulopen.obj    \
                ulread.obj    \
                ulsrv.obj

LINKOBJS =

#################################
# Target Descriptions
#################################
all: dll

#################################
# uliot.dll Target Descriptions
#################################

dll: $(NAMEDLL).dll

$(NAMEDLL).dll:   $(DLLOBJS1) $(DLLOBJS2) uliot.mak $(NAMEDLL).lrf \
        $(NAMEDLL).def
        $(LINK)  @$(NAMEDLL).lrf
        $(MAP)


#
# Make DEF file
#
$(NAMEDLL).def: uliot.mak
        @echo Creating file <<$(@B).def
LIBRARY ULIOT INITINSTANCE TERMINSTANCE
DESCRIPTION 'Toolkit IO Proc Sample'
SEGMENTS
   SHR_SEG   CLASS 'DATA' SHARED
DATA MULTIPLE NONSHARED
HEAPSIZE 0
IMPORTS
  mmio.mmioRead
  mmio.mmioSeek
  mmio.mmioOpen
  mmio.mmioClose
  mmio.mmioGetFormats
  mmio.mmioGetFormatName
  mmio.mmioIdentifyFile
  mmio.mmioIdentifyStorageSystem
  mmio.mmioQueryIOProcModuleHandle
  mmio.mmioInstallIOProc
  mmio.mmioQueryHeaderLength
  mmio.mmioStringToFOURCC
  mmio.mmioSendMessage
  mmio.mmioGetLastError
EXPORTS
        IOProc_Entry    @1
<<keep
#BASE = 0x1B130000

#
# Make link response file
#
$(NAMEDLL).lrf: uliot.mak
        @echo Creating file <<$(@B).lrf
$(DLLOBJS1) +
$(DLLOBJS2) +
$(LINKOBJS)
$(NAMEDLL).dll
$(NAMEDLL).map $(LFLAGS)
$(LIBS)
$(NAMEDLL).def;
<<keep










