#****************************************************************************
#
#  Work Place Shell - Browse Sample makefile - SOM 2.0 / IDL version
#
#  Copyright (C) 1993, 1994 IBM Corporation
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
.SUFFIXES: .c .obj .dll .idl .h .ih .res .def .rc .ifp .hlp

#****************************************************************************
#  Environment Setup for the component(s).
#****************************************************************************

SOMTEMP = .\somtemp

!if [set SMEMIT=ih;h;c] || \
    [set SMTMP=$(SOMTEMP)] || \
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


#****************************************************************************
# Compiler/tools Macros
#****************************************************************************
CPP     = icc
CC      = $(CPP) /Ge-
LINK    = ilink /nofree
LDFLAGS = /noi /map /nol /nod /exepack /packcode /packdata /align:16
DLLLIBS = $(DLLLIBS) somtk

#****************************************************************************
# Set up Macros that will contain all the different dependencies for the
# executables and dlls etc. that are generated.
#****************************************************************************
OBJS = BROWSE.OBJ PMBROWSE.OBJ

#****************************************************************************
#   Setup the inference rules for compiling source code to
#   object code.
#****************************************************************************
.idl.ih:
        sc -p -v -maddstar -mnoint -S128000 -C128000  $*.idl

.idl.c:
        sc -p -v -maddstar -mnoint -S128000 -C128000 -sc $*.idl

.c.obj:
        $(CC) -I.;  -c $<

all: BROWSE.DLL BROWSE.HLP

#****************************************************************************

#****************************************************************************
BROWSE.IH:     $$(@B).IDL

BROWSE.OBJ:    $$(@B).C    $$(@B).IH  $$(@B).H $$(@B).IDL
PMBROWSE.OBJ:  $$(@B).C    $$(@B).H

BROWSE.DLL:  $$(@B).DEF $(OBJS) $$(@B).RES
        $(LINK) $(LDFLAGS) $(OBJS),$@,,$(DLLLIBS),$*;
        rc -p -x $*.res $*.dll

BROWSE.RES:    $$(@B).RC   $$(@B).IH  $$(@B).ICO PMBROWSE.H
        rc -r $*.rc $*.res

BROWSE.HLP:    $$(@B).ipf
    ipfc $*.ipf /W3
