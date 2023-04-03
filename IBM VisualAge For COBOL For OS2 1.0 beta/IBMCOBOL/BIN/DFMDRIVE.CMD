@echo off
rem ****************************************************************************
rem * Copyright (C) International Business Machines Corporation 1993           *
rem *                                                                          *
rem * DFM/2 (program no. 5648-020)                                             *
rem * Version: 1.0                                                             *
rem * Release: 1.0                                                             *
rem * Level:   0.0                                                             *
rem *                                                                          *
rem ****************************************************************************
rem *                                                                          *
rem *  DFMDRIVE.CMD                                                            *
rem *                                                                          *
rem *  Function: This OS/2 command file is part of the end-user interfaces of  *
rem *            the Distributed FileManager (DFM/2) for OS/2. The interfaces  *
rem *            are used to assign or release a drive letter to a remote      *
rem *            DFM target system.                                            *
rem *            If no argument is given to this command file, the DFM/2       *
rem *            Presentation Manager interface DFMDRVPM.EXE is started.       *
rem *            If arguments are given to this command file, the DFM/2        *
rem *            batch interface program DFMDRVB.EXE is started.               *
rem ****************************************************************************
rem * CHANGE ACTIVITY                                                          *
rem * Flag Reason      Level    Date   Origin    Comments                      *
rem * ------------------------------------------------------------------------ *
rem *                           930402 Mueller  : Initial Release              *
rem * IBM Deutschland Entwicklung GmbH, Boeblingen, Germany      GMU at SDFVM1 *
rem ****************************************************************************

if "%1" == "" goto WITHOUT

:WITH
DFMDRVB %1 %2 %3 %4 %5 %6 %7
goto END

:WITHOUT
DFMDRVPM
goto END

:END
