#**************************************************************************
#
# MAKE file for Caption DLL
#
# Assumes that the environment is set up for development.  Specifically,
# the compiler, linker, rc and ipfc  should be in the path.  Also, the
# environment variables for the tools must be properly set, e.g. LIB,
# INCLUDE, IPFC. This should all be taken care of automatically by
# installing the OS/2 Toolkit and the MMPM/2 Toolkit.
#
# Copyright (C) IBM 1993
#**************************************************************************

CC       = icc
LINK     = ilink /nofree

DLLLIBS  = CPPON30 + os2386 + MMPM2.LIB

#
# Compilation Switches
#
#     /C+            : Compile only.
#     /Ge-           : Build a .DLL file
#     /W3            : Warning level.
#     /Gd-           : Link to static C libraries.
#     /Gm+           : Use multithreaded libraries.
#     /DINCL_32      : Use IBM code.
#     /Ti+           : Generate debugging code.
#     /Sm            : Generate debugging code.
#     /O-            : Turn optimization off.
#

CFLAGS     = /C+ /Ge- /Gd- /Ss+ /Gm+ /W3 /DINCL_32
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

all: cap.dll

#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------
ccdll.obj : ccdll.c captions.h
  $(CC) $(COMPILE) -Foccdll.obj ccdll.c

#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
ccdll.lnk: capdll.mak
    echo ccdll.obj             > ccdll.lnk
    echo cap.dll              >> ccdll.lnk
    echo cap.map              >> ccdll.lnk
    echo $(DLLLIBS)           >> ccdll.lnk
    echo cap.def              >> ccdll.lnk

#-------------------------------------------------------------------
#   Dependencies
#     This section lists all object files needed to be built for the
#     application, along with the files it is dependent upon (e.g.
#     its source and any header files).
#-------------------------------------------------------------------
ccdll.obj : ccdll.c captions.h

cap.dll: ccdll.obj ccdll.lnk cap.def
    $(LINK) $(LFLAGS) @ccdll.lnk
