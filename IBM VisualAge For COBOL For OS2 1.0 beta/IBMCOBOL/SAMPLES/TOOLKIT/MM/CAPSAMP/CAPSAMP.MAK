# SCCSID = @(#)makefile 13.7 92/04/22
#**************************************************************************
#
# MAKE file for Closed-captioning Utility
#
# Assumes that the environment is set up for development.  Specifically,
# the compiler, linker, rc and ipfc  should be in the path.  Also, the
# environment variables for the tools must be properly set, e.g. LIB,
# INCLUDE, IPFC. This should all be taken care of automatically by
# installing the OS/2 Toolkit and the MMPM/2 Toolkit.
#
# Copyright (C) IBM 1993
#**************************************************************************

CC        = icc
LINK      = ilink /NOFREE

BASELIBS  = CPPOM30.LIB OS2386.LIB MMPM2.LIB

#
# Compilation Switches
#
#     /G3s           : Generate 386 code with no stack checking.
#     /C+            : Compile only one module.
#     /W3            : Warning level.
#     /Gd-           : Link to static C libraries.
#     /Gm+           : Use multithreaded libraries.
#     /DINCL_32      : Use IBM code.
#     /Ti+           : Generate debugging code.
#     /Sm            : Generate debugging code.
#     /O-            : Turn optimization off.
#

CFLAGS    = /G3s /C+ /W3 /Gd- /Ss+ /Gm+ /DINCL_32
DEBUG     = /Ti+ /Sm /O-

COMPILE    = $(CFLAGS)

#
# Link Switches
#
#    /map     : Creates a listing file containing all pulbic symbols.
#    /nod     : Causes all default libraries to be ignored.
#    /noe     : The linker will not search the extended dictionary.
#

LFLAGS    = /map /nod /noe /ignorecase

all: capsamp.exe capsamp.hlp

capsamp.exe: capsamp.obj capsamp.res capsamp.def
  $(LINK) capsamp.obj, capsamp.exe, capsamp.map $(LFLAGS) /ST:20000, $(BASELIBS), capsamp.def
  $(RC) capsamp.res
  mapsym capsamp

capsamp.obj: capsamp.c capsamp.h captions.h
  $(CC) $(COMPILE) capsamp.c

capsamp.res: capsamp.rc capsamp.dlg capsamp.h
  rc -r capsamp.rc

capsamp.hlp: capsamp.ipf
  ipfc capsamp.ipf
