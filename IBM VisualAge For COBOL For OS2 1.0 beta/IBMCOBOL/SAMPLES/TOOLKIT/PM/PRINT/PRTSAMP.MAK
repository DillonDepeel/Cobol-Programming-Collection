#===================================================================
#
#   Print Sample Make file
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

include ..\..\ibmsamp.inc

#-------------------------------------------------------------------
#   A list of all of the object files
#-------------------------------------------------------------------
OBJS = prtsamp.obj prtcreat.obj prtmenu.obj prtprint.obj prtobj.obj \
prtprof.obj prtpage.obj
INCS = pmassert.h prtsamp.h prtsdlg.h

#-------------------------------------------------------------------
#   This section lists all targets to be built by the make.  The
#   makefile builds the executable as well as its associated help
#   file.
#-------------------------------------------------------------------
all: prtsamp.exe prtsamp.hlp

#-------------------------------------------------------------------
#   Definition file
#-------------------------------------------------------------------
prtsamp.def: prtsamp.mak
        @echo Creating .DEF file -- <<$(@R).def
NAME    prtsamp WINDOWAPI

DESCRIPTION 'PM Print Sample Application'

CODE    MOVEABLE
DATA    MOVEABLE MULTIPLE

HEAPSIZE   18192
STACKSIZE  20384
EXPORTS    WinProc
           ObjectWinProc
<<keep

#===================================================================
#
# Dependencies
#
#   This section lists all object files needed to be built for the
#   application, along with the files it is dependent upon (e.g. its
#   source and any header files).
#
#===================================================================
prtsamp.res: prtsamp.rc prtsamp.ico prtsamp.dlg prtsdlg.h
        copy ..\..\prodinfo.bmp
        rc -r prtsamp.rc
        erase prodinfo.bmp

prtcreat.obj: prtcreat.c $(INCS)

prtmenu.obj: prtmenu.c prtshlp.h $(INCS)

prtobj.obj: prtobj.c $(INCS)

prtpage.obj: prtpage.c $(INCS)

prtprof.obj: prtprof.c $(INCS)

prtprint.obj: prtprint.c $(INCS)

prtsamp.obj: prtsamp.c $(INCS)

#-------------------------------------------------------------------
#   This section creates the help manager file.
#-------------------------------------------------------------------
prtsamp.hlp: prtsamp.ipf

#-------------------------------------------------------------------
#   This section links the object modules into the finished program
#   using the linker command file created earlier.  At the end, the
#   resource file is bound to the application.
#-------------------------------------------------------------------
prtsamp.exe: $(OBJS) prtsamp.def prtsamp.res
    $(LINK) @<<$(@R).lnk
$(OBJS)
$(@R).exe
$(@R).map
$(MTLIBS)
$(@R).def
<<keep
    rc prtsamp.res prtsamp.exe
    rem Done!
