#****************************************************************************
#
#  WorkPlace Shell CAR C++ Sample makefile - SOM 2.0 / IDL version
#
#  Copyright (C) 1992-1995 IBM Corporation
#
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

#****************************************************************************
#  Dot directive definition area (usually just suffixes)
#****************************************************************************

.SUFFIXES:
.SUFFIXES: .cpp .obj .dll .idl .xh .xih .rc .res .def .ipf

#****************************************************************************
#  Environment setup for the component(s).
#****************************************************************************

TOOLKIT = ..\..\..\..\include
WPSHIDL = $(TOOLKIT)\os2

SOMTEMP = .\somtemp
HPATH   = $(TOOLKIT)\os2

!if [set SMEMIT=xih;xh;xc;def]
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

CPP     = icc
CC       = $(CPP) /W3 /Ge- /Ti /Q+
LINK    = ILINK
LDFLAGS = /nofree /noi /map /nol /nod /exepack /packcode /packdata /align:16
DLLLIBS = CPPON30 + os2386 + somtk

#****************************************************************************
# Set up Macros that will contain all the different dependencies for the
# executables and dlls etc. that are generated.
#****************************************************************************

OBJS = carpp.obj horn.obj brake.obj rpm.obj

#****************************************************************************
#   Setup the inference rules for compiling source code to
#   object code.
#****************************************************************************

.idl.xih:
        sc -v -r -mnoint -mnotc $*.idl

.cpp.obj:
        $(CC) -I.; -c $<

all: carpp.dll carpp.hlp

#
# Specific Process Tag
#

horn.obj :    $$(@B).cpp $$(@B).h carpp.xih

brake.obj:   $$(@B).cpp $$(@B).h carpp.xih

rpm.obj  :   $$(@B).cpp $$(@B).h carpp.xih

carpp.xih:   $$(@B).idl $(HPATH)\wpdataf.xh

carpp.def:   $$(@B).idl

carpp.obj:   $$(@B).cpp $$(@B).xih $$(@B).xh  $$(@B).idl \
             $(HPATH)\wpdataf.xh horn.h brake.h rpm.h

carpp.dll:   $$(@B).def $(OBJS) carpp.res
        $(LINK) $(LDFLAGS) $(OBJS),$@,,$(DLLLIBS),$*;
        rc -p -x $*.res $*.dll
        mapsym carpp.map

carpp.res: $$(@B).rc $$(@B).xih
        rc -r $*.rc $*.res

{.}.ipf.hlp:
   IPFC .\$*.IPF

