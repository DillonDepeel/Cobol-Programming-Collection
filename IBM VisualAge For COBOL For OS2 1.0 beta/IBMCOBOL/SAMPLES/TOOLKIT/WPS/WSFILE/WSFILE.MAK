#****************************************************************************
#
#  Work Place Shell Sample makefile
#
#  Copyright (C) 1995 IBM Corporation
#
#      DISCLAIMER OF WARRANTIES.  The following [enclosed] code is
#      sample code created by IBM Corporation. This sample code is not
#      part of any standard or IBM product and is provided to you solely
#      for  the purpose of assisting you in the development of your
#      applications.  The code is provided "AS IS", without
#      warranty of any kind.  IBM shall not be liable for any damages
#      arising out of your use of the sample code, even if they have been
#      advised of the possibility of such damages.                                                    *
#
#****************************************************************************

!include ..\..\ibmsamp.inc

#****************************************************************************
#  Dot directive definition area (usually just suffixes)
#****************************************************************************

.SUFFIXES:
.SUFFIXES: .c .obj .dll .idl .sc .h .ih .psc .rc .res

#****************************************************************************
#  Environment Setup for the component(s).
#****************************************************************************

ROOT = ..\..\..
SOMTEMP = .\somtemp

!if [set SMEMIT=h;ih;sc;c]
!endif

!if [cd $(SOMTEMP)]
!  if [md $(SOMTEMP)]
!    error error creating $(SOMTEMP) directory
!  endif
!else
!  if [cd ..]
!    error - Couldn't cd .. from $(SOMTEMP) directory
!  endif
!endif

#
# Compiler/tools Macros
#
CFLAGS  = $(CFLAG)
CC2      =  /Ge- /Ti+
LINK    = ILINK
LDFLAGS = /nofree /noi /map /nol /nod /exepack /packcode /packdata /align:16 /codeview
DLLLIBS = $(DLLLIBS) somtk.lib

#****************************************************************************
# Set up Macros that will contain all the different dependencies for the
# executables and dlls etc. that are generated.
#****************************************************************************

OBJS = wsfile.obj wsfolder.obj

#****************************************************************************
#   Setup the inference rules for compiling source code to
#   object code.
#****************************************************************************

.idl.ih:
        sc -maddstar -mnoint -S128000 -C128000 -v -p $*.idl

.idl.h:
        sc -maddstar -mnoint -S128000 -C128000 -v -p -s h $*.idl

.idl.c:
        sc -maddstar -mnoint -S128000 -C128000 -v -p -s c $*.idl

.idl.def:
        sc -maddstar -mnoint -S128000 -C128000 -v -p -s def $*.idl

.c.obj:
        $(CC) $(CFLAGS) $(CC2)  -I$(HPATH)  -c  -q $<

all: wsfile.dll wsfolder.dll wsfile.hlp wsfolder.hlp

#
# Specific Process Tag
#

wsfile.ih:   $$(@B).idl $(ROOT)\h\wpdataf.h

wsfile.obj: $$(@B).c $$(@B).ih $$(@B).h  $$(@B).sc $(ROOT)\h\wpdataf.h

wsfile.dll: $$(@B).def wsfile.obj wsfile.res
         $(LINK) $(LDFLAGS) wsfile.obj,$@,,$(DLLLIBS),$*;
         rc -p -x $*.res $*.dll
         mapsym wsfile.map

wsfile.hlp: $$(@B).ipf
         ipfc $*.ipf

wsfile.res: $$(@B).rc $$(@B).ih
         rc -r $*.rc $*.res

wsfolder.ih:   $$(@B).idl $(ROOT)\h\wpfolder.h

wsfolder.obj: $$(@B).c $$(@B).ih $$(@B).h $$(@B).sc $(ROOT)\h\wpfolder.h

wsfolder.dll: $$(@B).def wsfolder.obj wsfolder.res
         $(LINK) $(LDFLAGS) wsfolder.obj,$@,,$(DLLLIBS),$*;
         rc -p -x $*.res $*.dll
         mapsym wsfolder.map

wsfolder.res: $$(@B).rc $$(@B).ih
         rc -r $*.rc $*.res

wsfolder.hlp: $$(@B).ipf
         ipfc $*.ipf

