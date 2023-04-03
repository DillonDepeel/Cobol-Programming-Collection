#===================================================================
# Makefile for DRAGDROP Sample
#
#  Copyright (C) 1991, 1994 IBM Corporation
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
#===================================================================
#

include ..\..\ibmsamp.inc

all:    DRAGDROP.EXE            DRAGDROP.HLP

OBJS    =       dragdrop.obj dragdrag.obj dragheap.obj draghelp.obj

# Resources
#
dragdrop.res:      dragdrop.rc dragdrop.ico dragdrop.ptr dragdrop.h \
                                dragdrop.dlg draghelp.rc draghelp.h
        copy ..\..\prodinfo.bmp
        rc -r dragdrop.rc
        del prodinfo.bmp

# C files
#
dragdrop.obj:      dragdrop.c dragdrop.h

draghelp.obj:      draghelp.c dragdrop.h draghelp.h

dragheap.obj:      dragheap.c dragdrop.h

dragdrag.obj:      dragdrag.c dragdrop.h

# Main files:  DRAGDROP.EXE
#
dragdrop.exe:      dragdrop.mak dragdrop.def $(OBJS) dragdrop.res
    $(LINK) $(OBJS), dragdrop, dragdrop, $(LIBS), dragdrop.def
    rc -p -x dragdrop.res dragdrop.exe

dragdrop.hlp:   dragdrop.ipf dragfile.ipf draghelp.ipf dragdlg.ipf \
                dragmenu.ipf dragwnd.ipf

