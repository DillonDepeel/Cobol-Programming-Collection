# SCCSID = @(#)makefile 13.5 92/04/22
# SCCSID = @(#)makefile 13.5 92/04/22
#**************************************************************************
#
# Makefile for the AVCINST program.
#
#**************************************************************************

CC          = icc
LINK        = ilink /NOFREE

BASELIBS    = CPPOM30.LIB OS2386.LIB MMPM2.LIB

CTARG      = /G3s
COMPILE    = /C+ /W3 /Ss+ /Gd- /DINCL_32 /Ms
DEBUG      = /Ti+ /O-

CFLAGS  = $(COMPILE) $(CTARG)

#
# Link Switches
#
#    /map     : Creates a listing file containing all pulbic symbols.
#    /nod     : Causes all default libraries to be ignored.
#    /noe     : The linker will not search the extended dictionary.
#

LFLAGS  = /map /nod /noe

all: avcinst.exe avcinst.hlp

avcinst.exe : avcinst.obj avcinst.res avcinst.def
  $(LINK) avcinst.obj, avcinst.exe, $(LFLAGS) /ST:36000, $(BASELIBS), \
          avcinst.def
  $(RC) avcinst.res

avcinst.obj : avcinst.c avcinst.h
  $(CC) $(CFLAGS) avcinst.c

avcinst.res : avcinst.rc avcinst.dlg avcinst.h avcinst.ico
  rc -r avcinst.rc

avcinst.hlp: $*.ipf
  ipfc $*.ipf

