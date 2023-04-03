#/*
#********************************************************************
#**                        SOFTWARE INSTALLER
#** 5621-434 (C) COPYRIGHT IBM CORP. 1989, 1995. ALL RIGHTS RESERVED.
#**               LICENSED MATERIALS - PROPERTY OF IBM
#********************************************************************
#*/
# Make file for EPFIHOOK.DLL
#
# Make no changes to this file.
#

epfihook.dll:  .\epfihook.obj \
               .\vars.obj              \
               .\epfihook.def
   link386 @<<
     .\epfihook.obj+
     .\vars.obj /A:16 /EXEPACK /NOI /NOL
     .\epfihook.dll
     NUL
     os2386.lib
     .\epfihook.def
<<
   @echo.
   @echo Check for any errors.
   @pause

.\epfihook.obj:  .\varsfp.h \
                 .\epfihook.mak
     ICC.EXE /O- /Gd- /Sm /Kf /C+ /Ge-m /Ss /Kt-p-i-x-o- /W3 /Sp1 /Q+ -Fo$@ epfihook.c
