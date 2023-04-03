#===================================================================
#
#  Style Sample Makefile
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

#===================================================================
#
#   A list of all of the object files
#
#===================================================================
ALL_OBJ1 = sty_main.obj sty_user.obj sty_init.obj sty_pnt.obj sty_dlg.obj
ALL_OBJ2 = sty_file.obj sty_edit.obj sty_help.obj
HEADERS  = sty_dlg.h sty_help.h sty_main.h sty_xtrn.h
#===================================================================
#
#   A list of all of the Help files
#
#===================================================================
ALL_IPF = style.ipf sty_file.ipf sty_edit.ipf sty_help.ipf sty_menu.ipf \
          sty_dlg.ipf

#===================================================================
#
#   Dependencies
#
#===================================================================

all: style.exe style.hlp

# Resources
#
sty_main.res:      sty_main.h sty_main.ico sty_main.rc
        copy ..\..\prodinfo.bmp
        rc -r sty_main.rc
        del prodinfo.bmp

style.lnk: style.mak
    echo $(ALL_OBJ1) +         > style.lnk
    echo $(ALL_OBJ2)          >> style.lnk
    echo style.exe            >> style.lnk
    echo style.map            >> style.lnk
    echo $(MTLIBS)            >> style.lnk
    echo style.def            >> style.lnk

sty_main.res: sty_main.rc sty_main.h sty_dlg.h sty_main.dlg sty_help.rc sty_help.h

sty_main.obj: sty_main.c $(HEADERS)

sty_file.obj: sty_file.c $(HEADERS)

sty_edit.obj: sty_edit.c $(HEADERS)

sty_user.obj: sty_user.c $(HEADERS)

sty_init.obj: sty_init.c $(HEADERS)

sty_pnt.obj: sty_pnt.c $(HEADERS)

sty_dlg.obj: sty_dlg.c $(HEADERS)

sty_help.obj: sty_help.c $(HEADERS) sty_help.h

style.hlp: $(ALL_IPF)

style.exe: $(ALL_OBJ1) $(ALL_OBJ2) style.def style.lnk sty_main.res
    $(LINK) @style.lnk
    rc -p -x sty_main.res style.exe
