#################################################################################
#                                                                               #
# Makefile for ULTIEYES
#                                                                               #
#   Generates:
#         ultieyes.exx(exe)
#                                                                               #
#                                                                               #
# Copyright (c) IBM Corporation  1991, 1993                                     #
# All Rights Reserved                                                           #
#################################################################################

.SUFFIXES:
.SUFFIXES: .exe .obj .asm .inc .def .lrf  \
           .lst .sym .map .c .h .lib .dlg .txt .cod  \
           .res .rc .lrf .dll

NAMEEXE=ultieyes

#################################
# Definitions for C Compiler
#################################
CC=icc
C_DEBUG=/Ti /O- /DMMDEBUG
CFLAGS = /C /Ges /Gm+ /J- /Ss+ /Smns /Mp /Q

#################################
# Definitions for linker
#################################
LINK386=ilink /nofree
LNK_DEBUG=/CO
LFLAGS386= /map /nod /noe /noi /map /packcode /packdata /exepack /align:16
LIBS = CPPOM30.LIB OS2386.LIB MMPM2.LIB


#################################
# Object file lists
#################################
EXEOBJS1=ULTIEYES.OBJ

#################################
# Inference Rules
#################################
.c.obj:
        $(CC) $(CFLAGS)  $(<R).c

all: $(NAMEEXE).EXE

#################################
# EXE Target Descriptions
#################################
$(NAMEEXE).exe: $(EXEOBJS1) $(NAMEEXE).res \
                     $(NAMEEXE).mak $(NAMEEXE).lrf $(NAMEEXE).def
        $(LINK386)  @$(NAMEEXE).lrf
        $(RC) $(NAMEEXE).res
        mapsym $(NAMEEXE)

$(NAMEEXE).def: $(NAMEEXE).mak
        @echo Creating file <<$(@B).def
NAME         ULTIEYES  WINDOWAPI
DESCRIPTION  'UltiEyes'
CODE         MOVEABLE
DATA         MOVEABLE MULTIPLE
PROTMODE
STACKSIZE    32000
<<keep

#
# Make link response files
#
$(NAMEEXE).lrf: $(NAMEEXE).mak
        @echo Creating file <<$(@B).lrf
$(EXEOBJS1) +
$(EXEOBJS2)
$(NAMEEXE).exe
$(NAMEEXE) $(LFLAGS386)
$(LIBS)
$(NAMEEXE).def;
<<keep

$(NAMEEXE).res: $(NAMEEXE).rc
  rc -r $(NAMEEXE).rc
