#**************************************************************************
# *
# * File Name        : SHOW.MAK
# *
# * Description      : Makefile for SHOW.EXE
# *
# *
# * Copyright (C) 1992 IBM Corporation
# *
# *     DISCLAIMER OF WARRANTIES.  The following [enclosed] code is
# *     sample code created by IBM Corporation. This sample code is not
# *     part of any standard or IBM product and is provided to you solely
# *     for  the purpose of assisting you in the development of your
# *     applications.  The code is provided "AS IS", without
# *     warranty of any kind.  IBM shall not be liable for any damages
# *     arising out of your use of the sample code, even if they have been
# *     advised of the possibility of such damages.
# *
# ****************************************************************************/

show.exe : show.res show.obj
     ilink /NOFREE /NOE /NOI /DE /MAP show.obj,,,os2386.lib + mmpm2.lib,show.def
     rc show.res show.exe

show.res: show.rc showdlg.dlg show.ico
     rc -r show.rc

show.obj : show.c show.h
     icc /C /Ss /Ti /O- show.c

