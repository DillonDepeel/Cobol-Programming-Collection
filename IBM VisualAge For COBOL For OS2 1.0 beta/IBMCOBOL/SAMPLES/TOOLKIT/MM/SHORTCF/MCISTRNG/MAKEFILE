# SCCSID = @(#)makefile 13.7 92/04/22
#**************************************************************************
#
# Makefile for the MCISTRNG sample program.
#
#**************************************************************************

CC          = Icc
LINK        = ilink /nofree

BASELIBS    = CPPOM30.LIB OS2386.LIB MMPM2.LIB


CTARG      = /G3s
COMPILE    = /C+ /W3 /Ss+ /Gd- /DINCL_32 /Ms
DEBUG      = /Ti+ /O-

CFLAGS  = $(COMPILE) $(CTARG)

LFLAGS  = /map /nod /noe

all: mcistrng.exe MCISTRNG.hlp

mcistrng.exe : mcistrng.obj mcistrng.res mcistrng.def
  $(LINK) mcistrng.obj, mcistrng.exe, $(LFLAGS) /ST:36000, $(BASELIBS), \
          mcistrng.def
  rc mcistrng.res

mcistrng.obj : mcistrng.c mcistrng.h
  $(CC) $(CFLAGS) mcistrng.c

mcistrng.res : mcistrng.rc mcistrng.dlg mcistrng.h mcistrng.ico
  rc -r mcistrng.rc

mcistrng.hlp: mcistrng.ipf
  ipfc mcistrng.ipf
