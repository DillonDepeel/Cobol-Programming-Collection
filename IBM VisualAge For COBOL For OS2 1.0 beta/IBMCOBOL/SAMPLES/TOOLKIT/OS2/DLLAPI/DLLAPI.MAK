#===================================================================
#
#   DLLAPI.MAK - DLL Application Make file
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

HEADERS = dllapi.h

#===================================================================
#
#   A list of all of the object files
#
#===================================================================
ALL_OBJS = dllapi.obj dlapi_in.obj dlapi_hp.obj dlapi_dg.obj

ALL_IPF = dllapi.ipf

#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------
all: dllapi.exe dllapi.hlp dllib.dll

#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
dllapi.lnk: dllapi.mak
    echo $(ALL_OBJS)                > dllapi.lnk
    echo dllapi.exe                >> dllapi.lnk
    echo dllapi.map                >> dllapi.lnk
    echo $(MTLIBS)                 >> dllapi.lnk
    echo dllapi.def                >> dllapi.lnk

#-------------------------------------------------------------------
#   Dependencies
#     This section lists all object files needed to be built for the
#     application, along with the files it is dependent upon (e.g.
#     its source and any header files).
#-------------------------------------------------------------------
dllapi.res: dllapi.rc $(HEADERS) dllapi.dlg dllapi.ico
         copy ..\..\prodinfo.bmp
         rc -r dllapi.rc
         del prodinfo.bmp

dllapi.hlp: $(ALL_IPF)

dllapi.obj: dllapi.c $(HEADERS)

dlapi_dg.obj: dlapi_dg.c $(HEADERS)

dlapi_in.obj: dlapi_in.c $(HEADERS)

dlapi_hp.obj: dlapi_hp.c $(HEADERS)


dllapi.exe: $(ALL_OBJS) dllapi.def dllapi.res dllapi.lnk
    $(LINK) @dllapi.lnk
    rc -p -x dllapi.res dllapi.exe



#===================================================================
#
#   DLLIB.MAK - Dynamic Linking Library Make file
#   (C) Copyright IBM Corporation 1992.
#
#===================================================================
LIBHEADERS  = dllib.h

#===================================================================
#
#   A list of all of the object files
#
#===================================================================
DLL_OBJS = dllib.obj

#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------
dllib.obj : dllib.c $(LIBHEADERS)
  icc /c /Ge- /Gd- /Se /Re /ss /Ms /Gm+ /Kb+ /Ti+ /D__MIG_LIB__ -Fodllib.obj dllib.c

#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
dllib.lnk: dllapi.mak
    echo $(DLL_OBJS)           > dllib.lnk
    echo dllib.dll            >> dllib.lnk
    echo dllib.map            >> dllib.lnk
    echo $(DLLLIBS)           >> dllib.lnk
    echo dllib.def            >> dllib.lnk

#-------------------------------------------------------------------
#   Dependencies
#     This section lists all object files needed to be built for the
#     application, along with the files it is dependent upon (e.g.
#     its source and any header files).
#-------------------------------------------------------------------
dllib.obj : dllib.c $(LIBHEADERS)

dllib.dll: $(DLL_OBJS) dllib.lnk dllib.def
    $(LINK) @dllib.lnk
