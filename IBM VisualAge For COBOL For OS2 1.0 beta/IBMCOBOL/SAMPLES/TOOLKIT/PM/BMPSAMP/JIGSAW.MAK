#==============================================================================
#
#  Jigsaw Sample Makefile
#
#  Copyright (C) 1992, 1994 IBM Corporation
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
#==============================================================================

include ..\..\ibmsamp.inc

HEADERS =  globals.h jigsaw.h jighelp.h

#===================================================================
#
#   A list of all of the object files
#
#===================================================================
OBJECTS = jigsaw.obj jighelp.obj procs.obj misc.obj globals.obj

#===================================================================
#
#   A list of all of the Help files
#
#===================================================================
IPFS = jigsaw.ipf

#===================================================================
#
#   Dependencies
#
#===================================================================

all: jigsaw.exe jigsaw.hlp

jigsaw.lnk: jigsaw.mak
    echo $(OBJECTS)           > jigsaw.lnk
    echo jigsaw.exe           >> jigsaw.lnk
    echo jigsaw.map           >> jigsaw.lnk
    echo $(MTLIBS)            >> jigsaw.lnk
    echo jigsaw.def           >> jigsaw.lnk

jigsaw.res: jigsaw.rc jigsaw.h jigsaw.ico jighelp.rc jigsaw.dlg
        copy ..\..\prodinfo.bmp
        rc -r jigsaw.rc
        del prodinfo.bmp

jigsaw.obj: jigsaw.c $(HEADERS)

jighelp.obj: jighelp.c $(HEADERS)

procs.obj: procs.c $(HEADERS)

misc.obj: misc.c $(HEADERS)

globals.obj: globals.c $(HEADERS)

jigsaw.hlp: $(IPFS)

jigsaw.exe: $(OBJECTS) jigsaw.def jigsaw.lnk jigsaw.res jigsaw.mak
    $(LINK) @jigsaw.lnk
    rc -p -x jigsaw.res jigsaw.exe
