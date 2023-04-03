#===================================================================
#
#  Queues Sample Makefile
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
# IBMSAMP.INC - sets up for IBM 32-bit Compiler
#
include ..\..\ibmsamp.inc

SVR_HEADERS = svrqmain.h svrqxtrn.h svrqglbl.h svrqhelp.h queue.h
CLI_HEADERS = cliqmain.h cliqxtrn.h cliqglbl.h cliqhelp.h queue.h


#-------------------------------------------------------------------
#   A list of all of the object files
#-------------------------------------------------------------------

SVR_OBJ = svrqmain.obj svrqthrd.obj svrqpnt.obj svrqhelp.obj
CLI_OBJ = cliqmain.obj cliqthrd.obj cliqhelp.obj


#-------------------------------------------------------------------
#   A list of all of the Help text files
#-------------------------------------------------------------------

SVR_IPF = svrqueue.ipf file.ipf svroptn.ipf help.ipf dlg.ipf
CLI_IPF = cliqueue.ipf file.ipf clientry.ipf help.ipf dlg.ipf


#-------------------------------------------------------------------
#   This section lists all files to be built by the make.  The
#   makefile builds the executible as well as its associated help
#   file.
#-------------------------------------------------------------------

all: svrqueue.exe svrqueue.hlp cliqueue.exe cliqueue.hlp


#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------

svrqueue.lnk: queues.mak
    echo $(SVR_OBJ)              > svrqueue.lnk
    echo svrqueue.exe           >> svrqueue.lnk
    echo svrqueue.map           >> svrqueue.lnk
    echo $(MTLIBS)              >> svrqueue.lnk
    echo svrqueue.def           >> svrqueue.lnk

cliqueue.lnk: queues.mak
    echo $(CLI_OBJ)              > cliqueue.lnk
    echo cliqueue.exe           >> cliqueue.lnk
    echo cliqueue.map           >> cliqueue.lnk
    echo $(MTLIBS)              >> cliqueue.lnk
    echo cliqueue.def           >> cliqueue.lnk


#===================================================================
#
# Dependencies
#
#   This section lists all object files needed to be built for the
#   application, along with the files it is dependent upon (e.g. its
#   source and any header files).
#
#===================================================================

svrqueue.res: svrqueue.rc queue.ico svrqmain.h svrqhelp.rc svrqhelp.h
         copy ..\..\prodinfo.bmp
         rc -r svrqueue.rc
         del prodinfo.bmp

svrqmain.obj: svrqmain.c $(SVR_HEADERS)

svrqthrd.obj: svrqthrd.c $(SVR_HEADERS)

svrqpnt.obj:  svrqpnt.c  $(SVR_HEADERS)

svrqhelp.obj: svrqhelp.c $(SVR_HEADERS)


cliqueue.res: cliqueue.rc queue.ico cliqmain.h cliqhelp.rc cliqhelp.h
         copy ..\..\prodinfo.bmp
         rc -r cliqueue.rc
         del prodinfo.bmp

cliqmain.obj: cliqmain.c $(CLI_HEADERS)

cliqthrd.obj: cliqthrd.c $(CLI_HEADERS)

cliqhelp.obj: cliqhelp.c $(CLI_HEADERS)



#-------------------------------------------------------------------
#   This section creates the help manager file.  The IPF compiler
#   creates a file called main.hlp which is renamed to template.hlp.
#-------------------------------------------------------------------

svrqueue.hlp: $(SVR_IPF)
cliqueue.hlp: $(CLI_IPF)



#-------------------------------------------------------------------
#   This section links the object modules into the finished program
#   using the linker command file created earlier.  At the end, the
#   resource file is bound to the application.
#-------------------------------------------------------------------

svrqueue.exe: $(SVR_OBJ) svrqhelp.obj svrqueue.def svrqueue.lnk svrqueue.res
    $(LINK) @svrqueue.lnk
    rc -p -x svrqueue.res svrqueue.exe


cliqueue.exe: $(CLI_OBJ) cliqhelp.obj cliqueue.def cliqueue.lnk cliqueue.res
    $(LINK) @cliqueue.lnk
    rc -p -x cliqueue.res cliqueue.exe
