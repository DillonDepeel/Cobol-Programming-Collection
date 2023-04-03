#===================================================================
#
#   HANOI.MAK - Hanoi Make file
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

HEADERS = hanoi.h
#-------------------------------------------------------------------
#
#   A list of all of the object files
#
#-------------------------------------------------------------------

ALL_OBJS = hanoi.obj
ALL_IPF = hanoi.ipf

#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------
all: hanoi.exe hanoi.hlp

#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
hanoi.lnk: hanoi.mak
    echo $(ALL_OBJS)               > hanoi.lnk
    echo hanoi.exe                >> hanoi.lnk
    echo hanoi.map                >> hanoi.lnk
    echo $(MTLIBS)                >> hanoi.lnk
    echo hanoi.def                >> hanoi.lnk

#-------------------------------------------------------------------
#   Dependencies
#     This section lists all object files needed to be built for the
#     application, along with the files it is dependent upon (e.g.
#     its source and any header files).
#-------------------------------------------------------------------
hanoi.res: hanoi.rc $(HEADERS) hanoi.ico hanoi.dlg
         copy ..\..\prodinfo.bmp
         rc -r hanoi.rc
         del prodinfo.bmp

hanoi.hlp: $(ALL_IPF)

hanoi.obj: hanoi.c $(HEADERS)

hanoi.exe: $(ALL_OBJS) hanoi.def hanoi.res hanoi.lnk
   $(LINK) @hanoi.lnk
   rc -p -x hanoi.res hanoi.exe

