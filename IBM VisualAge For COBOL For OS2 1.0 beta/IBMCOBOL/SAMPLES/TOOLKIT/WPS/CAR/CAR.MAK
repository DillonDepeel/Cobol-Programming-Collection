#****************************************************************************
#
#  Work Place Shell - Car Sample makefile - SOM 2.0 / IDL version
#
#  Copyright (C) 1992, 1994 IBM Corporation
#
#      DISCLAIMER OF WARRANTIES.  The following [enclosed] code is
#      sample code created by IBM Corporation. This sample code is not
#      part of any standard or IBM product and is provided to you solely
#      for  the purpose of assisting you in the development of your
#      applications.  The code is provided "AS IS", without
#      warranty of any kind.  IBM shall not be liable for any damages
#      arising out of your use of the sample code, even if they have been
#      advised of the possibility of such damages.
#
#      NOTE: you will have to modify the environment variables to suit
#            your environment
#
#****************************************************************************

!include ..\..\ibmsamp.inc

#****************************************************************************
#  Dot directive definition area (usually just suffixes)
#****************************************************************************

.SUFFIXES:
.SUFFIXES: .c .obj .dll .idl .h .ih .rc .res

#****************************************************************************
#  Environment Setup for the component(s).
#****************************************************************************

SOMTEMP = .\somtemp

!if [set SMTMP=$(SOMTEMP)] || \
    [set SMEMIT=ih;h;c] || \
    [set SMADDSTAR=1] || \
    [set SMNOTC=1]
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

CC      = $(CC) /Ge-
LINK    = ILINK
LDFLAGS = /nofree /noi /map /nol /nod /exepack /packcode /packdata /align:16
DLLLIBS    = $(DLLLIBS) somtk.lib

!ifdef DEBUG
LISTING  = $(CC) /Le /Li /Ls /Fc
ERRLEVEL = /W3 /Ka /Kg /Kp /Kr
CC       = $(CC) /Ti /DDEBUG
LDFLAGS  = $(LDFLAGS) /MAP /CODEVIEW
!endif

#****************************************************************************
# Set up Macros that will contain all the different dependencies for the
# executables and dlls etc. that are generated.
#****************************************************************************

OBJS = car.obj
!ifdef INCL_INT3
OBJS = $(OBJS) int3.obj
!endif

#****************************************************************************
#   Setup the inference rules for compiling source code to
#   object code.
#****************************************************************************

.idl.ih:
        sc -maddstar -mnoint -S128000 -C128000 -v -p $*.idl

.idl.c:
        sc -maddstar -mnoint -S128000 -C128000 -v -p -s c $*.idl

.idl.def:
        sc -maddstar -mnoint -S128000 -C128000 -v -p -s def $*.idl

.c.obj:
        $(CC) -I.; -c $<

all: car.dll

#
# Specific Process Tag
#

!ifdef INCL_INT3
int3.obj:
    $(ASM)   $*.asm
!endif

car.ih:  $$(@B).idl

car.obj: $$(@B).c $$(@B).ih $$(@B).h $$(@B).idl

car.dll: $$(@B).def $(OBJS) car.res
         $(LINK) $(LDFLAGS) $(OBJS),$@,,$(DLLLIBS),$*;
         rc -p -x $*.res $*.dll
         mapsym car.map

car.res: $$(@B).rc $$(@B).ih
         rc -r $*.rc $*.res

