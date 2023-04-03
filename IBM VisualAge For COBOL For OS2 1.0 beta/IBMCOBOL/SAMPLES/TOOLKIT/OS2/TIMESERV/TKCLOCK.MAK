#===================================================================
#
#   Timer Services Clock Sample Makefile
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
# Comment out or delete the .inc file you don't want
# IBMSAMP.INC - sets up for IBM/TORONTO Compiler
#
include ..\..\ibmsamp.inc
HEADERS = clock.h clkdata.h

#===================================================================
#
#   A list of all of the object files
#
#===================================================================


COBJ = clock.obj dialogs.obj paint.obj wndproc.obj \
       clkdata.obj udtime.obj digital.obj \
       alarmthd.obj  help.obj

ALL_IPF = clock.ipf help.ipf dlg.ipf view.ipf settings.ipf


#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------
all: clock.exe clock.hlp



#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
clock.lnk: tkclock.mak
    echo $(COBJ)                > clock.lnk
    echo clock.exe              >> clock.lnk
    echo clock.map              >> clock.lnk
    echo $(MTLIBS)              >> clock.lnk
    echo clock.def              >> clock.lnk




#===================================================================
#
# Dependencies
#
#   This section lists all object files needed to be built for the
#   application, along with the files it is dependent upon (e.g. its
#   source and any header files).
#
#===================================================================

clock.res: clock.rc help.rc clock.ico clock.dlg product.dlg
         copy ..\..\prodinfo.bmp
         rc -r clock.rc
         del prodinfo.bmp


clock.hlp: $(ALL_IPF)

help.obj : help.c

clock.obj: clock.c

dialogs.obj: dialogs.c


clock.exe: clock.res $(COBJ) clock.def clock.lnk
    $(LINK) @clock.lnk
    rc -p -x clock.res clock.exe

