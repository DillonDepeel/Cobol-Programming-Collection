#===================================================================
#
#   Clipbrd Make file
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

HEADERS = clipbrd.h

#-------------------------------------------------------------------
#   A list of all of the object files
#-------------------------------------------------------------------
ALL_OBJ1 = clipbrd.obj


all: clipbrd.exe


clipbrd.l: clipbrd.mak
    echo $(ALL_OBJ1)            > clipbrd.l
    echo clipbrd.exe           >> clipbrd.l
    echo clipbrd.map           >> clipbrd.l
    echo $(LIBS)                >> clipbrd.l
    echo clipbrd.def           >> clipbrd.l


clipbrd.res: clipbrd.rc clipbrd.ico clipbrd.h clipbrd.bmp
        copy ..\..\prodinfo.bmp
        rc -r clipbrd.rc
        del prodinfo.bmp

clipbrd.obj: clipbrd.c $(HEADERS)

clipbrd.exe: $(ALL_OBJ1)  clipbrd.def clipbrd.l clipbrd.res
    $(LINK) @clipbrd.l
    rc -p -x clipbrd.res clipbrd.exe
