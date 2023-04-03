#===================================================================
#
#  Dialog Sample Makefile
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

include ..\..\ibmsamp.inc

all:    DIALOG.EXE

# Resources
#
dialog.res: dialog.h dialog.ico dialog.rc ..\..\prodinfo.bmp
    copy ..\..\prodinfo.bmp
    rc -r dialog.rc
    del prodinfo.bmp

# C files
#
dialog.obj: dialog.c dialog.h

# Main files: DIALOG.EXE
#
dialog.exe: dialog.mak dialog.def dialog.obj dialog.res
    $(LINK) dialog, dialog, dialog, $(MTLIBS), dialog.def
    rc -p -x dialog.res dialog.exe
