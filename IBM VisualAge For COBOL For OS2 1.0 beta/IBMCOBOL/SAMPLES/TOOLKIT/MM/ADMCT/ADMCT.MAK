# SCCSID = @(#)makefile 13.8 92/04/23
# SCCSID = @(#)makefile 13.8 92/04/23
#################################################################################
#                                                                               #
# Makefile for OS/2 MMPM/2 WAVEFORM Audio MCI Driver                            #
#                                                                               #
#   Generates One DLL:                                                          #
#             Audiomct.DLL - Ring 3 Audio MCI Driver                            #
#                                                                               #
# Copyright (c) IBM Corporation  1991                                           #
# All Rights Reserved                                                           #
#################################################################################


.SUFFIXES:
.SUFFIXES: .com .sys .exe .obj .mbj .asm .inc .def .lrf .crf .ref \
           .lst .sym .map .c .h .lib .msg .txt .cvk

NAMEDLL=audiomct

#################################
# Definitions for C Compiler
# Compiler options (C Set/2 (Toronto))
#    Q+  == Supress logo
#    O+  == Optimize code (don't use with Ti)
#    Ti  == Debug information
#    Kp+ == Produce messages about unused function parameters
#    Ss+ == Allow // comments
#    Sm+ == Migration libraries (Microsoft C)
#    Ge- == Build .DLL (default is .exe)
#    Gm+ == Multithreaded libraries
#    Gn+ == Do not generate default libraries in object
#    Gs- == Do not remove stack probes (default)
#    Kb- == Suppress basic diagnostic messages (default)
#    Ki- == Suppress messages about uninitialized variables (default)
#    Ms  == Use "system" calling conventions (as opposed to Mp optlink)
#    Mp  =  Optlink (register passing convention ~10% faster)
#    Fl+ == Produce listing
#    L+  == Produce listing
#    Ls+ == Include source in listing
#    Fa+ == Produce assembler code
#    Le+ == Expand macros
#    La+ == Include offsets in structures in listing
#    Sn+ == Provide DBCS support in runtime string routines
#################################


CCOMP  = icc
CFLAGS = /C /Ge- /Gm+ /Gs- /Sm /Ss+ /Q /Mp /DINCL_32 /DTOOLKIT /DINCL_OS2MM /I.
DEBUG  = /Ti+ /O-

#################################
# Definitions for linker
#################################
LINK386=ILINK /NOFREE
LFLAGS386= /map /nod /noe /noi /map /packcode /packdata /exepack /align:16 /nologo
MAP = mapsym $(NAMEDLL)
LIBS386 = OS2386.LIB CPPOM30.LIB mmpm2.lib


#################################
# Definitions for resource compiler
#################################

RC = rc

#################################
# Inference Rules
#################################
.c.obj:
        $(CCOMP) $(CFLAGS) /Fo$(<R).obj $(C_LST) $(<R).c


.rc.res:
        $(RC) $(RINC) /r $(<B).rc

#################################
# Object file lists
#################################

DLLOBJS=admcdat.obj  \
        admcinit.obj \
        admcstat.obj \
        admcrest.obj \
        admcload.obj \
        admcrecd.obj

DLLOBJS2=admcplay.obj \
         audiosub.obj \
         audiomcd.obj \
         hhpheap.obj  \
         checkmem.obj \
         loadsubs.obj

DLLOBJS3=admcset.obj  \
         admcconn.obj \
         admccue.obj  \
         admcsave.obj \
         admccap.obj  \
         strmsubs.obj \
         admcopen.obj


DLLOBJS4=admcedit.obj \
         admcpst.obj  \
         admccopy.obj \
         admcseek.obj \
         admcclos.obj \
         admcini.obj  \
         admcini2.obj \
         connect.obj  \
         spicalls.obj

#################################
# Target Descriptions
#################################
all: res dll

##################################
# audiomcd Target Descriptions
##################################
dll: $(NAMEDLL).dll
res: AUDIOMCD.res

$(NAMEDLL).dll:   $(DLLOBJS) $(DLLOBJS2) $(DLLOBJS3) $(DLLOBJS4) admct.mak  $(NAMEDLL).lrf          \
                  $(NAMEDLL).def
                  $(LINK386) $(LFLAGS386) @$(NAMEDLL).lrf
                  $(RC) AUDIOMCD.RES $*.DLL
                  $(MAP)


AUDIOMCD.res: audiomcd.rc admct.mak
        @echo $@ > con
        $(RC) $(RINC) -r $*.rc $@



$(NAMEDLL).def: admct.mak
        @echo Creating file <<$(@B).def

 LIBRARY AUDIOMCT INITINSTANCE
 DATA MULTIPLE NONSHARED
 SEGMENTS
   SHR_SEG       CLASS 'FAR_DATA' SHARED
   SHR_SEG_CONST CLASS 'FAR_DATA' SHARED
   SHR_SEG_BSS   CLASS 'FAR_DATA' SHARED

 BASE = 0x1B2A0000
 EXPORTS   mciDriverEntry
<<keep

$(NAMEDLL).lrf: admct.mak
        @echo Creating file <<$(@B).lrf
 $(DLLOBJS) +
 $(DLLOBJS2) +
 $(DLLOBJS3) +
 $(DLLOBJS4)
 $(NAMEDLL).dll
 $(NAMEDLL).map
 $(LIBS386)
 $(NAMEDLL).def;
<<keep

