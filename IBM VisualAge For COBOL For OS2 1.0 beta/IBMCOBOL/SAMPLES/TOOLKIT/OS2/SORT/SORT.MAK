#===================================================================
#
#   SORT.MAK - Sort Application Make file
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
#===================================================================

#-------------------------------------------------------------------
#   IBMSAMP.INC - sets up for IBM Compiler
#-------------------------------------------------------------------
include ..\..\ibmsamp.inc

HEADERS = sort.h

#-------------------------------------------------------------------
#
#   A list of all of the object files
#
#-------------------------------------------------------------------
ALL_OBJS = sort.obj
ALL_IPF = sort.ipf

#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------
all: sort.exe sort.hlp

#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
sort.lnk: sort.mak
    echo $(ALL_OBJS)              > sort.lnk
    echo sort.exe                >> sort.lnk
    echo sort.map                >> sort.lnk
    echo $(MTLIBS)               >> sort.lnk
    echo sort.def                >> sort.lnk

#-------------------------------------------------------------------
#   Dependencies
#     This section lists all object files needed to be built for the
#     application, along with the files it is dependent upon (e.g.
#     its source and any header files).
#-------------------------------------------------------------------
sort.res: sort.rc $(HEADERS) sort.ico sort.dlg
         copy ..\..\prodinfo.bmp
         rc -r sort.rc
         del prodinfo.bmp

sort.hlp: $(ALL_IPF)

sort.obj: sort.c $(HEADERS)

sort.exe: $(ALL_OBJS) sort.res sort.def sort.lnk
    $(LINK) @sort.lnk
    rc -p -x sort.res sort.exe

