#===================================================================
#
#   Hello Make file
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

CC         = icc /Ti /c /Ge /Gd- /Se /Re /ss /Gm+

HEADERS = hello.h

#-------------------------------------------------------------------
#   A list of all of the object files
#-------------------------------------------------------------------
ALL_OBJ1 = hello.obj


all: hello.exe


hello.l: hello.mak
    echo $(ALL_OBJ1)            > hello.l
    echo hello.exe           >> hello.l
    echo hello.map           >> hello.l
    echo $(LIBS)                >> hello.l
    echo hello.def           >> hello.l




hello.res: hello.rc hello.ico hello.h

hello.obj: hello.c $(HEADERS)

hello.exe: $(ALL_OBJ1)  hello.def hello.l hello.res
    $(LINK) @hello.l
    rc -p -x hello.res hello.exe
