#===================================================================
#
# Makefile for IPF Sample
#
#  Copyright (C) 1991, 1994  IBM Corporation
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

#===================================================================
#
# Common definitions for the IBM compiler environment
#
#===================================================================

.SUFFIXES:
.SUFFIXES: .c .bmp .def .dll .h .ico .ipf .obj .rc .res

#===================================================================
#
# Default compilation macros for sample programs
#
# Compile switches that are enabled
# /c      compile without linking
# /Ge-    build a DLL file, not an .EXE
# /Gd-    statically link the run-time library
# /Re     generate executable code to run in a run-time environment
# /ss     allow use of double slashes (//) for comments
# /Ms     use SYSTEM linkage for functions and not OPTILINK
# /Gm-    link with single-threading libraries
# /W3     set the warning level to three
#
#===================================================================

CC      = icc /c /Ge- /Gd- /Se /Re /ss /Ms /Gm- /W3

LFLAGS  = /NOFREE /NOE /NOD /NOI /M
LINK    = ilink $(LFLAGS)

#===================================================================
#
# Libraries
#
# CPPON30  - Single tasking, standard library, static binding
# CPPON30O - Single tasking, standard library, dynamic binding
# CPPOS30  - Single tasking, standard library
# CPPOM30  - Multi tasking, standard library
#
#===================================================================

LIBS    = CPPOM30 + OS2386
STLIBS  = CPPOS30 + OS2386
MTLIBS  = CPPOM30 + OS2386
DLLLIBS = CPPON30 + CPPON30O + OS2386
VLIBS   = CPPOS30 + VDH + OS2386

#===================================================================
#
# Rules for generating object, linker response and definition files
#
#===================================================================

.c.obj:
    $(CC) -Fo $*.obj $*.c

.rc.res:
        rc -r $*.rc

.ipf.inf:
        ipfc /inf $*.ipf

#===================================================================

all: ipf.dll ipf.inf

#
# Create .DLL file
#

ipf.dll: ipf.def ipf.obj ipf.res
         $(LINK) ipf.obj, ipf.dll, ipf, $(DLLLIBS), ipf.def
         rc -p -x ipf.res ipf.dll

#
# Create .RES file
#

ipf.res: ipf.rc ipf.ico ipf.h \
         frame1.bmp frame2.bmp frame3.bmp frame4.bmp frame5.bmp

#
# Create .OBJ file
#

ipf.obj: ipf.c ipf.h

#
# Create .INF file
#

ipf.inf: ipf.ipf

