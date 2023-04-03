# SCCSID = @(#)makefile 13.6 92/04/23
#################################################################################
#
# Makefile for Toolkit Resource file
#
#   Generates one dll:
#         shrc.dll          - Sample Stream Handlers Resource Dll
#
#
# Copyright (c) IBM Corporation  1992, 1993
# All Rights Reserved
#
#################################################################################

.SUFFIXES: .com .sys .exe .obj .mbj .asm .inc .def .lrf .crf .ref \
           .lst .sym .map .c .h .lib .msg .pro .txt

RCDLL=SHRC
RCRES=SSMRES

#################################
# Definitions for C Compiler
#################################

CC=icc

CFLAGS=  /C+ /Gd- /Ge- /Gm+ /Gs- /Re /J- /Ms /Sm /Sn /Ss+ /DINCL_32

#################################
# Definitions for linker
#################################
LINK=ilink /nofree
MAP = mapsym $(RCDLL)
LFLAGS= /map /nod /noe /noi /packcode /packdata /exepack /align:16
LIBS= CPPOM30.LIB os2386.lib MMPM2.LIB

#########################################
# Definitions for RESOURCE compiler
#########################################
RC  = rc

#################################
# Inference Rules
#################################
.c.obj:
        $(CC) $(CFLAGS) /Fo$(<R).obj $(C_LST) $(<R).c

#################################
# Object file lists
#################################

RCOBJS = rcstub.obj

#################################
# Target Descriptions
#################################
all: rc

##################################
# audiores.dll Target Descriptions
##################################

rc: $(RCDLL).dll

$(RCDLL).dll:   $(RCOBJS) $(RCRES).rc shrc.mak $(RCDLL).lrf $(RCDLL).def
        $(LINK)  @$(RCDLL).lrf
        $(RC) $(RCRES).rc $(RCDLL).dll
        $(MAP)

$(RCDLL).def: shrc.mak
        @echo Creating file <<$(@B).def
LIBRARY $(RCDLL)
DESCRIPTION 'DLL file containing resources'
STUB 'OS2STUB.EXE'
DATA NONE
<<keep

$(RCDLL).lrf: shrc.mak
        @echo Creating file <<$(@B).lrf
$(RCOBJS)
$(RCDLL).dll
$(RCDLL).map $(LFLAGS)
$(LIBS)
$(RCDLL).def;
<<keep





