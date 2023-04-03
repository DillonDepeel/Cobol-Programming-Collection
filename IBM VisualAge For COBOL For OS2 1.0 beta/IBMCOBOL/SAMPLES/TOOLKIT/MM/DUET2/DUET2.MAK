# SCCSID = @(#)makefile 13.7 92/04/22
#**************************************************************************
#
# MAKE file for Duet Player 2
#
# Assumes that the environment is set up for development.  Specifically,
# the compiler, linker, rc and ipfc  should be in the path.  Also, the
# environment variables for the tools must be properly set, e.g. LIB,
# INCLUDE, IPFC. This should all be taken care of automatically by
# installing the OS/2 Toolkit and the MMPM/2 Toolkit.
#
#**************************************************************************

CC          = icc
LINK        = ilink /NOFREE

BASELIBS    = CPPOM30.LIB OS2386.LIB MMPM2.LIB

DEBUG      = /Ti+ /O-
COMPILE = /G3s /C+ /W3  /Ss+ /Gd- /Ms /Gm+ /DINCL_32

CFLAGS = $(COMPILE)

LFLAGS  = /map /nod /noe

all: duet2.exe duet2.hlp

duet2.exe: duet2.obj duet2.res duet2.def
  $(LINK) duet2.obj, duet2.exe, $(LFLAGS) /ST:20000, $(BASELIBS), duet2.def
  $(RC) duet2.res

duet2.obj: duet2.c duet2.h
  $(CC) $(CFLAGS) duet2.c

duet2.res: duet2.rc duet2.dlg duet2.h duet2.ico
  rc -r duet2.rc

duet2.hlp: duet2.ipf
  ipfc duet2.ipf
