# SCCSID = @(#)makefile 13.44 92/04/27
#################################################################################
#                                                                               #
# Makefile for OS/2 Multimedia extensions MCI Driver Template                   #
#                                                                               #
#   Generates One DLL:                                                          #
#             MCDTemp.DLL - Ring 3 MCI Driver                                   #
#                                                                               #
# Copyright (c) IBM Corporation  1993                                           #
# All Rights Reserved                                                           #
#################################################################################


.SUFFIXES:
.SUFFIXES: .com .sys .exe .obj .mbj .asm .inc .def .lrf .crf .ref \
           .lst .sym .map .c .h .lib .msg .txt .cvk

NAMEDLL=mcdtemp

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


#################################
# Definitions for C Compiler
#################################
CC=icc
CFLAGS=  /C /Gd+ /Ge- /Gm+ /Gs- /Sm /Ss+ /Q /Mp /DINCL_32

#################################
# Definitions for linker
#################################
LINK386=ilink /nofree
LFLAGS386= $(LNK_DEBUG) /map /nod /noi /map /packcode /packdata /exepack /align:16 /nologo
LIBS = OS2386.LIB CPPOM30.LIB MMPM2.LIB

#################################
# Definitions for resource compiler
#################################
RC = rc
MAP=mapsym $(NAMEDLL)

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
DLLOBJS1=mcdproc.obj mcdopen.obj mcdclose.obj mcddrvsv.obj mcddrvrt.obj mcdstat.obj
DLLOBJS2=mcdinfo.obj mcdfuncs.obj

DLLOBJS=$(DLLOBJS1) $(DLLOBJS2)

#################################
# Target Descriptions
#################################
all: dll

##################################
# mcdtemp Target Descriptions
##################################
dll: $(NAMEDLL).dll

$(NAMEDLL).dll:   $(NAMEDLL).lrf $(NAMEDLL).def  \
                  $(DLLOBJS)
                  $(LINK386) $(LFLAGS386) @$(NAMEDLL).lrf
                  $(MAP)

$(NAMEDLL).def: mcdtemp.mak
        @echo Creating file <<$(@B).def
 LIBRARY MCDTEMP INITINSTANCE
 DATA MULTIPLE NONSHARED
 SEGMENTS
   SHR_SEG       CLASS 'FAR_DATA' SHARED
   SHR_SEG_CONST CLASS 'FAR_DATA' SHARED
   SHR_SEG_BSS   CLASS 'FAR_DATA' SHARED

 EXPORTS   mciDriverEntry
<<keep

$(NAMEDLL).lrf: mcdtemp.mak
        @echo Creating file <<$(@B).lrf
 $(DLLOBJS)
 $(NAMEDLL).dll
 $(NAMEDLL).map /map /nod /noi
 $(LIBS)
 $(NAMEDLL).def;
<<keep

clean:
        if exist *.lrf  del *.lrf
        if exist *.def  del *.def
        if exist *.obj  del *.obj
        if exist *.map  del *.map
        if exist *.old  del *.old
        if exist *.lst  del *.lst
        if exist *.cod  del *.cod
        if exist *.sys  del *.sys
        if exist *.sym  del *.sym
        if exist *.msg  del *.msg

