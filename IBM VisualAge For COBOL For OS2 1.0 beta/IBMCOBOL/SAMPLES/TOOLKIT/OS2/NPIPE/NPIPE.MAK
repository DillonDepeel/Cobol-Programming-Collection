#===================================================================
#   Npipe.mak - Make file for named pipe sample
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

#
# Comment out or delete the .inc file you don't want
# IBMSAMP.INC - sets up for IBM 32-bit compiler Compiler
#
include ..\..\ibmsamp.inc

SVR_HEADERS = svrpmain.h svrpxtrn.h svrpglbl.h svrphelp.h namepipe.h
CLI_HEADERS = clnpmain.h clnpxtrn.h clnpglbl.h clnphelp.h namepipe.h


#-------------------------------------------------------------------
#   A list of all of the object files
#-------------------------------------------------------------------

SVR_OBJ = svrpmain.obj svrpthrd.obj svrppnt.obj svrpgame.obj svrphelp.obj
CLI_OBJ = clnpmain.obj clnpthrd.obj clnphelp.obj


#-------------------------------------------------------------------
#   A list of all of the Help text files
#-------------------------------------------------------------------

SVR_IPF = svrnpipe.ipf file.ipf svroptn.ipf help.ipf dlg.ipf
CLI_IPF = clinpipe.ipf clifile.ipf clioptn.ipf help.ipf clidlg.ipf


#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------

all: svrnpipe.exe svrnpipe.hlp \
     clinpipe.exe clinpipe.hlp

#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------

svrnpipe.lnk: npipe.mak
    echo $(SVR_OBJ)              > svrnpipe.lnk
    echo svrnpipe.exe           >> svrnpipe.lnk
    echo svrnpipe.map           >> svrnpipe.lnk
    echo $(MTLIBS)              >> svrnpipe.lnk
    echo svrnpipe.def           >> svrnpipe.lnk

clinpipe.lnk: npipe.mak
    echo $(CLI_OBJ)              > clinpipe.lnk
    echo clinpipe.exe           >> clinpipe.lnk
    echo clinpipe.map           >> clinpipe.lnk
    echo $(MTLIBS)              >> clinpipe.lnk
    echo clinpipe.def           >> clinpipe.lnk


#===================================================================
#
# Dependencies
#
#   This section lists all object files needed to be built for the
#   application, along with the files it is dependent upon (e.g. its
#   source and any header files).
#
#===================================================================

svrnpipe.res: svrnpipe.rc namepipe.ico svrpmain.h svrphelp.rc svrphelp.h
         copy ..\..\prodinfo.bmp
         rc -r svrnpipe.rc
         del prodinfo.bmp

svrpmain.obj: svrpmain.c $(SVR_HEADERS)

svrpthrd.obj: svrpthrd.c $(SVR_HEADERS)

svrpgame.obj: svrpgame.c $(SVR_HEADERS)

svrppnt.obj:  svrppnt.c  $(SVR_HEADERS)

svrphelp.obj: svrphelp.c $(SVR_HEADERS)


clinpipe.res: clinpipe.rc namepipe.ico clnpmain.h clnphelp.rc clnphelp.h
         copy ..\..\prodinfo.bmp
         rc -r clinpipe.rc
         del prodinfo.bmp

clnpmain.obj: clnpmain.c $(CLI_HEADERS)

clnpthrd.obj: clnpthrd.c $(CLI_HEADERS)

clnphelp.obj: clnphelp.c $(CLI_HEADERS)



#-------------------------------------------------------------------
#   This section creates the help manager file.  The IPF compiler
#   creates a file called main.hlp which is renamed to template.hlp.
#-------------------------------------------------------------------

svrnpipe.hlp: $(SVR_IPF)
clinpipe.hlp: $(CLI_IPF)



#-------------------------------------------------------------------
#   This section links the object modules into the finished program
#   using the linker command file created earlier.  At the end, the
#   resource file is bound to the application.
#-------------------------------------------------------------------

svrnpipe.exe: $(SVR_OBJ) svrphelp.obj svrnpipe.def svrnpipe.lnk svrnpipe.res
    $(LINK) @svrnpipe.lnk
    rc -p -x svrnpipe.res svrnpipe.exe


clinpipe.exe: $(CLI_OBJ) clnphelp.obj clinpipe.def clinpipe.lnk clinpipe.res
    $(LINK) @clinpipe.lnk
    rc -p -x clinpipe.res clinpipe.exe
