#===================================================================
#
#   Image Make file - Porting Stage 3 : Pure 32 bit Application
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

#-------------------------------------------------------------------
#   .SUFFIXES line helps NMAKE recognize other file suffixes besides
#   .c, .exe, etc.
#-------------------------------------------------------------------
.SUFFIXES: .res .rc .hlp .ipf

#===================================================================
#
# Macro definitions
#
#   These macro definitions set are used to set up the options needed
#   to build the application.  Such options include the command line
#   options for the compiler and the libraries needed for the linker.
#   The flags are created as macros so that they can be easily modified
#   so they need to be changed.
#
#   The macros also allow you to have a different set of options
#   for each environment your application is built for.  The macros
#   below include the flags for compiling the application in the
#   32=bit and 16=bit memory models.  You can also add flags for a
#   a retail version of the product, a version which does not contain
#   any codeview symbols and has the maximum optimization.
#
#===================================================================
include ..\..\ibmsamp.inc
CFLAGS32 = -DLINT_ARGS -DHELP_MANAGER_ENABLED -DPORT_32
CINC32 =
LINK32 = ilink /nofree
LFLAGS = /MAP /CO /NOD
RC = rc
HEADERS = img_main.h img_xtrn.h


#-------------------------------------------------------------------
#   This section is used to set which options macros are to be used
#   in the build.  By changing the macro names on the right, we can
#   change which environment the application will be built for.  This
#   application currently uses the flags for the 32-bit memory model.
#-------------------------------------------------------------------
LINK = $(LINK32)
CFLAGS = $(CFLAGS32)
CINC = $(CINC32)

#===================================================================
#
# Default inference rules
#
#   This section lists the command and flags to build each type of
#   of source file listed.
#
#===================================================================
.c.obj:
    $(CC) $(CFLAGS) $(CINC) $*.c

.rc.res:
    $(RC) -r $*.rc

#-------------------------------------------------------------------
#   A list of all of the object files
#-------------------------------------------------------------------
ALL_OBJ1 = img_ma32.obj img_da32.obj img_in32.obj img_pn32.obj img_vi32.obj
ALL_OBJ2 = img_dl32.obj img_he32.obj img_si32.obj img_fi32.obj img_me32.obj img_ut32.obj

#-------------------------------------------------------------------
#   A list of all of the Help text files
#-------------------------------------------------------------------
ALL_IPF = img_main.ipf img_file.ipf img_view.ipf img_help.ipf

#-------------------------------------------------------------------
#   This section lists all files to be build my the make.  This make
#   file build the executible as well as its associated help file.
#-------------------------------------------------------------------
all: image32.exe image.hlp



#-------------------------------------------------------------------
#   This section creates the command file used by the linker.  This
#   command file is recreated automatically every time you change
#   the object file list, linker flags, or library list.
#-------------------------------------------------------------------
image32.lnk: image32.mak
    echo $(ALL_OBJ1) +           > image32.lnk
    echo $(ALL_OBJ2)            >> image32.lnk
    echo image32.exe            >> image32.lnk
    echo image32.map $(LFLAGS)  >> image32.lnk
    echo $(LIBS)                >> image32.lnk
    echo image32.def            >> image32.lnk



#===================================================================
#
# Dependencies
#
#   This section lists all object files needed to be built for the
#   application, along with the files it is dependent upon (e.g. its
#   source and any header files).
#
#===================================================================
img_ma32.res: img_main.rc img_main.h img_dlg.h image.dlg\
              img_help.rc img_help.h
    copy ..\..\prodinfo.bmp
    $(RC) -r img_main.rc img_ma32.res
    del prodinfo.bmp

img_ma32.obj: img_main.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_ma32.obj img_main.c

img_me32.obj: img_menu.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_me32.obj img_menu.c

img_si32.obj: img_size.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_si32.obj img_size.c

img_fi32.obj: img_file.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_fi32.obj img_file.c

img_vi32.obj: img_view.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_vi32.obj img_view.c

img_da32.obj: img_data.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_da32.obj img_data.c

img_in32.obj: img_init.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_in32.obj img_init.c

img_pn32.obj: img_pnt.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_pn32.obj img_pnt.c

img_dl32.obj: img_dlg.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_dl32.obj img_dlg.c

img_he32.obj: img_help.c $(HEADERS) img_help.h
    $(CC) $(CFLAGS) $(CINC) -Foimg_he32.obj img_help.c

img_ut32.obj: img_util.c $(HEADERS)
    $(CC) $(CFLAGS) $(CINC) -Foimg_ut32.obj img_util.c

#-------------------------------------------------------------------
#   This section creates the help manager file.  The IPF compiler
#   creates a file called main.hlp which is renamed to image.hlp.
#-------------------------------------------------------------------
image.hlp: $(ALL_IPF)
     ipfc img_main.ipf /W3
     if exist image.hlp del image.hlp
     rename img_main.hlp image.hlp


#-------------------------------------------------------------------
#   This section links the object modules into the finished program
#   using the linker command file created earlier.  At the end, the
#   resource file is bound to the application.
#-------------------------------------------------------------------
image32.exe: $(ALL_OBJ1) $(ALL_OBJ2) image32.def image32.lnk img_ma32.res
    $(LINK) @image32.lnk
    $(RC) -p -x img_ma32.res image32.exe
