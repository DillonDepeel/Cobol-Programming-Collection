# SCCSID = @(#)makefile 13.7 92/04/22
#**************************************************************************
#
# MAKE file for Movie Sample Application
#
# Assumes that the environment is set up for development.  Specifically,
# the compiler, linker, rc and ipfc  should be in the path.  Also, the
# environment variables for the tools must be properly set, e.g. LIB,
# INCLUDE, IPFC. This should all be taken care of automatically by
# installing the OS/2 Toolkit and the MMPM/2 Toolkit.
#
#**************************************************************************

CC          = icc
LINK        = ilink /nofree

BASELIBS    = CPPOM30.LIB OS2386.LIB MMPM2.LIB

DEBUG      = /Ti+ /O-

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

COMPILE = /G3s /C+ /W3 /Ss+ /Gd- /Ms /Gm+ /DINCL_32

CFLAGS = $(COMPILE)

#
# Link Switches
#
#    /map     : Creates a listing file containing all pulbic symbols.
#    /nod     : Causes all default libraries to be ignored.
#    /noe     : The linker will not search the extended dictionary.
#

LFLAGS  = /map /nod /noe

all: movie.exe movie.hlp

movie.exe: movie.obj movie.res movie.def
  $(LINK) movie.obj, movie.exe, $(LFLAGS) /ST:20000, $(BASELIBS), movie.def
  $(RC) movie.res

movie.obj: movie.c movie.h
  $(CC) $(CFLAGS) $(DEBUG) movie.c

movie.res: movie.rc movie.dlg movie.h movie.ico
  rc -r movie.rc

movie.hlp: movie.ipf
  ipfc movie.ipf
