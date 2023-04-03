# SCCSID = @(#)makefile 13.7 92/04/22
#**************************************************************************
#
# MAKE file for Duet Player 1
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

CFLAGS = /G3s /C+ /W3 /Ss+ /Gd- /Ms /Gm+ /DINCL_32
DEBUG      = /Ti+ /O-

LFLAGS  = /map /nod /noe

all: duet1.exe duet1.hlp

duet1.exe: duet1.obj duet1.res duet1.def
  $(LINK) duet1.obj, duet1.exe, $(LFLAGS) /ST:20000, $(BASELIBS), duet1.def
  $(RC) duet1.res

duet1.obj: duet1.c duet1.h
  $(CC) $(CFLAGS) duet1.c

duet1.res: duet1.rc duet1.dlg duet1.h duet1.ico
  rc -r duet1.rc

duet1.hlp: duet1.ipf
  ipfc duet1.ipf
