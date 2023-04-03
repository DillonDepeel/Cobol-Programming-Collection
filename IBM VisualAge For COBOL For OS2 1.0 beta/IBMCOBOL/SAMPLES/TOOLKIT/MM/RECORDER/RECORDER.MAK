# SCCSID = @(#)makefile 13.7 92/04/22
#**************************************************************************
#
# MAKE file for Wave Recorder
#
# Assumes that the environment is set up for development.  Specifically,
# the compiler, linker, rc and ipfc  should be in the path.  Also, the
# environment variables for the tools must be properly set, e.g. LIB,
# INCLUDE, IPFC. This should all be taken care of automatically by
# installing the OS/2 Toolkit and the MMPM/2 Toolkit.
#
# Copyright (C) IBM 1993
#**************************************************************************

CC          = icc
LINK        = ilink /nofree

BASELIBS    = CPPOM30.LIB OS2386.LIB MMPM2.LIB

#
# Compilation Switches
#
#     /G3s           : Generate 386 code with no stack checking.
#     /C+            : Compile only one module.
#     /W3            : Warning level.
#     /Gd-           : Link to static C libraries.
#     /Ms            : Puts Toronto compiler into C6.0 migration mode.
#     /Gm+           : Use multithreaded libraries.
#     /DINCL_32      : Use IBM code.
#     /Ti+           : Generate debugging code.
#     /Sm            : Generate debugging code.
#     /O-            : Turn optimization off.
#

CFLAGS     = /G3s /C+ /W3 /Ss+ /Gd- /Ms /Gm+ /DINCL_32
DEBUG      = /Ti+ /Sm /O-

COMPILE    = $(CFLAGS)

#
# Link Switches
#
#    /map     : Creates a listing file containing all pulbic symbols.
#    /nod     : Causes all default libraries to be ignored.
#    /noe     : The linker will not search the extended dictionary.
#

LFLAGS     = /map /nod /noe

all: recorder.exe recorder.hlp

recorder.exe: recorder.obj options.obj recorder.res recorder.def
  $(LINK) recorder.obj options.obj, recorder.exe, recorder.map $(LFLAGS) /ST:20000, $(BASELIBS), recorder.def
  $(RC) recorder.res
  mapsym recorder

recorder.obj: recorder.c recorder.h
  $(CC) $(COMPILE) recorder.c

options.obj: options.c recorder.h
  $(CC) $(COMPILE) options.c

recorder.res: recorder.rc recorder.dlg recorder.h recorder.ico
  rc -r recorder.rc

recorder.hlp: recorder.ipf
  ipfc recorder.ipf
