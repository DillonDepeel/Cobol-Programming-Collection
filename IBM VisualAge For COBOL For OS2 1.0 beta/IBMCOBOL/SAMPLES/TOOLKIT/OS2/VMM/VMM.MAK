#===================================================================
#
#   VMM.MAK - VMM Application Makefile
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

HEADERS = vmm.h

#===================================================================
#
#   A list of all of the object files
#
#===================================================================
ALL_OBJS = vmm_main.obj vmm_init.obj vmm_dlg.obj \
           vmm_user.obj vmm_help.obj
ALL_IPF = vmm.ipf

#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------
all: vmm.exe vmm.hlp

#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
vmm.lnk: vmm.mak
    echo $(ALL_OBJS)             > vmm.lnk
    echo vmm.exe                >> vmm.lnk
    echo vmm.map                >> vmm.lnk
    echo $(MTLIBS)              >> vmm.lnk
    echo vmm.def                >> vmm.lnk

#-------------------------------------------------------------------
#   Dependencies
#     This section lists all object files needed to be built for the
#     application, along with the files it is dependent upon (e.g.
#     its source and any header files).
#-------------------------------------------------------------------
vmm.res: vmm.rc $(HEADERS) vmm.dlg vmm.ico
         copy ..\..\prodinfo.bmp
         rc -r vmm.rc
         del prodinfo.bmp

vmm.hlp: $(ALL_IPF)

vmm_main.obj: vmm_main.c $(HEADERS)

vmm_user.obj: vmm_user.c $(HEADERS)

vmm_init.obj: vmm_init.c $(HEADERS)

vmm_dlg.obj: vmm_dlg.c $(HEADERS)

vmm_help.obj: vmm_help.c $(HEADERS)

vmm.exe: $(ALL_OBJS) vmm.def vmm.res vmm.lnk
    $(LINK) @vmm.lnk
    rc -p -x vmm.res vmm.exe

