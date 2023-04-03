#******************************************************************************
#
#  Work Place Shell- Show Description Sample makefile
#
#  This makefile builds the Show Method Description program, SHOWDESC.EXE.
#  This program is used with the WPS Object class WPSTutorial, in WPSTUTOR.DLL.
#
#  Copyright (C) 1993, 1994  IBM Corporation
#
#      DISCLAIMER OF WARRANTIES.  The following [enclosed] code is
#      sample code created by IBM Corporation. This sample code is not
#      part of any standard or IBM product and is provided to you solely
#      for  the purpose of assisting you in the development of your
#      applications.  The code is provided "AS IS", without
#      warranty of any kind.  IBM shall not be liable for any damages
#      arising out of your use of the sample code, even if they have been
#      advised of the possibility of such damages.                                                    *
#
#      NOTE: you will have to modify the environment variables to suit
#            your environment
#
#******************************************************************************

!include ..\..\ibmsamp.inc

CC         = icc
LFLAGS   = /NOFREE /NOE /NOD /ALIGN:16 /EXEPACK /M /BASE:0x10000

#===================================================================
#
#   A list of all of the object files
#
#===================================================================
ALL_OBJS = showdesc.obj

HEADERS  = showdesc.h methodid.h

#===================================================================
#
#   Dependencies
#
#===================================================================

.c.obj:
        $(CC) /c /Gd- /Se /Re /ss /Ms /Gm+ /Ti+ $<

all: showdesc.exe

#
# Resources
#
showdesc.res:   showdesc.h methodid.h showdesc.ico showdesc.rc
        rc -r showdesc.rc

showdesc.lnk: showdesc.mak
    echo $(ALL_OBJS)           > showdesc.lnk
    echo showdesc.exe         >> showdesc.lnk
    echo showdesc.map         >> showdesc.lnk
    echo $(MTLIBS)            >> showdesc.lnk
    echo showdesc.def         >> showdesc.lnk

showdesc.res: showdesc.rc showdesc.h methodid.h

showdesc.obj: showdesc.c $(HEADERS)

showdesc.exe: $(ALL_OBJS) showdesc.def showdesc.lnk showdesc.res
    $(LINK) $(LFLAGS) @showdesc.lnk
    rc -p -x showdesc.res showdesc.exe
