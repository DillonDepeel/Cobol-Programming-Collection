#ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
#³                                                                          ³
#³ PROGRAM NAME: PALETTE                                                    ³
#³  A PM program demonstrating vector fonts, palette manager APIs, and IPF  ³
#³  help.                                                                   ³
#³                                                                          ³
#³ COPYRIGHT:                                                               ³
#³ ----------                                                               ³
#³  Copyright (C) 1992, 1994 IBM Corporation                                ³
#³                                                                          ³
#³ DISCLAIMER OF WARRANTIES:                                                ³
#³ -------------------------                                                ³
#³  The following [enclosed] code is sample code created by IBM Corporation.³
#³  This sample code is not part of any standard IBM product and is provided³
#³  to you solely for the purpose of assisting you in the development of    ³
#³  your applications.  The code is provided "AS IS", without warranty of   ³
#³  any kind.  IBM shall not be liable for any damages arising out of your  ³
#³  use of the sample code, even if they have been advised of the           ³
#³  possibility of such damages.                                            ³
#³                                                                          ³
#³ For details on what this program does etc., please see the PALETTE.C     ³
#³ file.                                                                    ³
#³                                                                          ³
#ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

.SUFFIXES:

.SUFFIXES: .c .rc .ipf

ALL: PALETTE.EXE \
     PALETTE.HLP \
     PALETTE.RES

PALETTE.EXE:  \
  PALETTE.OBJ \
  PALETTE.RES \
  PALETTE.DEF \
  PALETTE.MAK
   @REM @<<PALETTE.@0
     /CO /M /PM:PM +
     PALETTE.OBJ
     PALETTE.EXE
     PALETTE.MAP

     PALETTE.DEF;
<<
   ILINK /NOFREE @PALETTE.@0
   RC -p -x PALETTE.RES PALETTE.EXE

palette.res:  palette.rc palette.ico palette.h
    copy ..\..\prodinfo.bmp
    $(RC) -r palette.rc palette.res
    del prodinfo.bmp

{.}.ipf.hlp:
   IPFC .\$*.IPF

{.}.c.obj:
  ICC.EXE /Ss /W2 /Gm /Gs- /C .\$*.c

!include PALETTE.INC
