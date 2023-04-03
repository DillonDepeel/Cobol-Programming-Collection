# SCCSID = @(#)makefile 13.7 92/04/22
#**************************************************************************
#
# Makefile for the CLOCK sample program.
#
#**************************************************************************

CC          = icc
LINK        = ilink /NOFREE

BASELIBS    = CPPOM30.LIB OS2386.LIB MMPM2.LIB

CTARG      = /G3s
COMPILE    = /C+ /W3 /Ss+ /Gd- /DINCL_32 /Ms
DEBUG      = /Ti+ /O-
CM_THREAD  = /Gm+

CFLAGS  = $(COMPILE) $(CTARG) $(CM_THREAD)


LFLAGS  = /map /nod /noe

all: clock.exe clock.hlp

BITMAPS = bellc.bmp bellfrlt.bmp bellfrrt.bmp belllt.bmp bellrt.bmp


clock.exe : clock.obj clockdrw.obj clock.res clock.def
 $(LINK) clock.obj clockdrw.obj, clock.exe, $(LFLAGS) /ST:36000, $(BASELIBS), clock.def
  rc clock.res


clock.obj : clock.c clock.h
  $(CC) $(CFLAGS) clock.c

clockdrw.obj : clockdrw.c clock.h
  $(CC) $(CFLAGS) clockdrw.c

clock.res : clock.rc clock.dlg clock.h clock.ico $(BITMAPS)
  rc -r clock.rc

clock.hlp: $*.ipf
  ipfc $*.ipf
