# SCCSID = @(#)makefile 13.5 92/04/23
#******************************************************************************
#
#       Copyright (c) IBM Corporation 1990, 1991, 1992, 1993
#       Copyright (c) Microsoft Corporation 1990, 1991
#
#       All Rights Reserved
#       CONV IOProc
#       Case Converter IO Procedure
#
#
#******************************************************************************


#****************************************************************************
#  Dot directive definition area (usually just suffixes)
#****************************************************************************

.SUFFIXES: .asm .c .def .dlg .dll .inc .exe .lib .lrf .lst .obj .rc .res

#
# Compiler/tools Macros
#

CC386=icc
IMPLIB=implib
LINK386=ilink /nofree
MAPSYM=mapsym
RC=rc

CFLAGS= /Sm /C /Gs-  /W3 /Ss+ /C+ /Ge- /Ms /Gd- /Gm- /DINCL_32 /I.

LFLAGS = /noi /nod /map

Libs = OS2386.LIB CPPOM30.LIB MMPM2.LIB

Objs = convproc.obj convcvsr.obj convconv.obj hhpheap.obj

#****************************************************************************
# Inference Rules
#****************************************************************************

.c.obj:
        $(CC386) $(CFLAGS) $< ;

#****************************************************************************
#   Target Information
#****************************************************************************

all: convproc.lib convproc.dll

#****************************************************************************
# UTOLPROC.LIB - Build in the current directory
#****************************************************************************

convproc.lib:  convproc.dll convproc.def caseconv.mak
   @echo $@ > con
   implib convproc.lib convproc.def

#****************************************************************************
# CONVPROC.LRF
#****************************************************************************

convproc.lrf: caseconv.mak
        @echo $@ > con
        @echo convproc.obj+                           >  $@
        @echo convcvsr.obj+                           >> $@
        @echo convconv.obj+                           >> $@
        @echo hhpheap.obj                             >> $@
        @echo convproc.dll                            >> $@
        @echo convproc $(LFLAGS)                      >> $@
        @echo $(Libs)                                 >> $@
        @echo convproc.def;                           >> $@

#****************************************************************************
# CONVPROC.DEF
#****************************************************************************

convproc.def: caseconv.mak
        @echo $@ > con
        @echo LIBRARY CONVPROC                            >  $@
        @echo DATA SHARED                                 >> $@
        @echo PROTMODE                                    >> $@
        @echo HEAPSIZE 0                                  >> $@
        @echo EXPORTS                                     >> $@
        @echo    CONVProc                        @1       >> $@
        @echo    convhlpGetFormatStringLength    @2       >> $@
        @echo    convhlpGetFormatString          @3       >> $@
        @echo    convhlpGetNLSData               @4       >> $@
        @echo    convhlpToUpper                  @5       >> $@
        @echo    convhlpToLower                  @6       >> $@

#****************************************************************************
# CONVPROC.DLL - Build in the current directory
#****************************************************************************

convproc.dll: convproc.lrf convproc.def convproc.res $(Objs)  caseconv.mak
        @echo $@ > con
        $(LINK386) $(LNK_DEBUG) @convproc.lrf
        $(MAPSYM) convproc
        $(RC) convproc.res $*.dll

#****************************************************************************
# CONVPROC.RES
#****************************************************************************

convproc.res: convproc.rc caseconv.mak
        @echo $@ > con
        $(RC) $(RINC) -r $*.rc $@

