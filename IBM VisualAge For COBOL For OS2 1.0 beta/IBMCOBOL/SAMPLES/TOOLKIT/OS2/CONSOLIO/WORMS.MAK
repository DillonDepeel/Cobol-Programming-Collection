#===================================================================
#
#  Worms Sample makefile
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
# IBMSAMP.INC - sets up for IBM/C2  Compiler
#
include ..\..\ibmsamp.inc
HEADERS = worms.h wrmthrd.h

#===================================================================
#
#   A list of all of the object files
#
#===================================================================


COBJ = worms.obj  wrmthrd.obj init.obj disp.obj wrmhelp.obj wmouse.obj



ALL_IPF =


#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------
all: worms.exe



#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
worms.lnk: worms.mak
    echo $(COBJ)                > worms.lnk
    echo worms.exe              >> worms.lnk
    echo worms.map              >> worms.lnk
    echo $(MTLIBS)               >> worms.lnk
    echo worms.def;             >> worms.lnk




#===================================================================
#
# Dependencies
#
#   This section lists all object files needed to be built for the
#   application, along with the files it is dependent upon (e.g. its
#   source and any header files).
#
#===================================================================


worms.obj   : worms.c

wrmthrd.obj : wrmthrd.c

init.obj    : init.c

disp.obj    : disp.c

wrmhelp.obj : wrmhelp.c

wmouse.obj  : wmouse.c

worms.exe:  $(COBJ) worms.def worms.lnk
    $(LINK) $(LFLAGS) @worms.lnk


