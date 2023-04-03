#*******************************************************************************
#* SAMPLE PROGRAM: EHNXNMP - Sample Name Mapping Exit for DFM/2                *
#*                                                                             *
#* MODULE NAME:    EHNXNMP.DEF                                                 *
#* DESCRIPTION:    Make File for the Name Mapping Exit EHNXNMP.DLL             *
#*                                                                             *
#*******************************************************************************
#*                                                                             *
#* DFM/2 (program no. 5648-020)                                                *
#* Version: 1.0                                                                *
#* Release: 1.0                                                                *
#*                                                                             *
#* Copyright (C) International Business Machines Corp., 1993.                  *
#*                                                                             *
#* DISCLAIMER OF WARRANTIES:                                                   *
#* The following [enclosed] code is sample code created by IBM                 *
#* Corporation.  This sample code is not part of any standard IBM product      *
#* and is provided to you solely for the purpose of assisting you in the       *
#* development of your applications.  The code is provided "AS IS",            *
#* without warranty of any kind.  IBM shall not be liable for any damages      *
#* arising out of your use of the sample code, even if they have been          *
#* advised of the possibility of such damages.                                 *
#*                                                                             *
#*******************************************************************************
#* CHANGE ACTIVITY                                                             *
#* Flag Reason      Level    Date   Origin    Comments                         *
#* --------------------------------------------------------------------------- *
#*                           930402 Mueller  : Initial Release                 *
#* IBM Deutschland Entwicklung GmbH, Boeblingen, Germany         GMU at SDFVM1 *
#*******************************************************************************

#*******************************************************************************
#* This file is part of the Name Mapping Exit sample program for DFM/2.        *
#* Make File.                                                                  *
#* For further description refer to the header of the file EHNXNMP.C           *
#*******************************************************************************
#*                                                                             *
#* Before using this file do the following                                     *
#* 1. In the section 'file definitions' replace the specification for          *
#*    %EHNDIR% by the name of your RLIO/DFM product directory.                 *
#*       Note: The %..% notation has been used there as in OS/2 .cmd files.    *
#*       However, there is NO automatic replacement of these variables,        *
#*       you have to do it manually.                                           *
#* 2. In the section 'libraries' replace the specification for                 *
#*    COMPDIR by the correct specification for your compiler directory.        *
#*       Note: The %..% notation has been used there as in OS/2 .cmd files.    *
#*       However, there is NO automatic replacement of these variables,        *
#*       you have to do it manually.                                           *
#* 3. In the section 'libraries' replace the specification for                 *
#*    TOOLDIR by the correct specification for your toolkit directory.         *
#*       Note: The %..% notation has been used there as in OS/2 .cmd files.    *
#*       However, there is NO automatic replacement of these variables,        *
#*       you have to do it manually.                                           *
#* 4. Check the occurrences of COMPDIR and TOOLDIR in the rest of the file     *
#*    for correct specifications relating to your directory structures         *
#*    (necessary only when you have renamed subdirectories after installation).*
#* 5. You need                                                                 *
#*    a. The IBM C Set/2 compiler version 1.0 or later                         *
#*    b. The Developer's Toolkit for OS/2 2.0 or later                         *
#*       including the linker LINK386                                          *
#*       and the make file utility NMAKE                                       *
#*                                                                             *
#* To use the make file, do the following:                                     *
#* 1. Set the current directory to the \SAMPLE subdirectory of the DFM/2       *
#*    product directory (%EHNDIR%\SAMPLE).                                     *
#*       If you have the files EHNXNMP.xxx in a different directory,           *
#*       make that directory the current one.                                  *
#*       If you want the resulting files created in different directories,     *
#*       prefix the file names in the File Definitions section (following      *
#*       this header) with the proper path names.                              *
#* 2. In an OS/2 window or full-screen, type                                   *
#*    NMAKE -F EHNXNMP.MAK                                                     *
#*       Then the file EHNXNMP.DLL and some other files will be created        *
#*       in the current directory, or the ones specified in the path           *
#*       prefixed to the file names in the File Definitions section resp.      *
#*                                                                             *
#* To use (activate) the Name Mapping Exit routines, do the following:         *
#*    Copy the resulting file EHNXNMP.DLL into a directory in your LIBPATH.    *
#*******************************************************************************

################################################################################
#       File Definitions                                                       #
################################################################################
# If wanted, prefix the following file names with a complete path specification:
# Source files
CFILE     = EHNXNMP.C
HEADERDIR = %EHNDIR%\H
HEADERS   = $(HEADERDIR)\EHNXNMP.H
MAKEFILE  = EHNXNMP.MAK
DEFFILE   = EHNXNMP.DEF
# Files created
DLLNAME   = EHNXNMP.DLL
OBJNAME   = EHNXNMP.OBJ
MAPNAME   = EHNXNMP.MAP

################################################################################
#       Libraries                                                              #
################################################################################
# Modify the following statements according to your environment:
# Directory containing the C Set/2 Compiler:
COMPDIR  = %tooldisk%\IBMC
# Directory containing the OS/2 2.0 Toolkit:
TOOLDIR  = %tooldisk%\TOOLKT20

################################################################################
#       Suffixes to make use of the inference rules                            #
################################################################################
.SUFFIXES:
.SUFFIXES: .C  .OBJ  .DLL


################################################################################
#       Definitions for C compiler                                             #
################################################################################
# Options for the IBM C Set/2 compiler:
#------------------------------------------------------------------
#Option       Description
#------------------------------------------------------------------
#/C+          Suppress linking
#/Ge-         Build a DLL file
#/Gd+         Dynamically link of the run-time libraries
#/Gs-         Remove stack probes
#/G3          Instruction set 386
#/G4          Instruction set 486
#/Kb+         Produce basic diagnostic messages
#/Kb-         Suppress basic diagnostic messages
#/I dirlist   Specify a search path,
#                dirlist = list of directories separated by ";"
#/O+          Optimize code
#/O-          Do not optimize code
#/Rn          Generate subsystem code with no run-time environm.
#/Se          Allow all C Set/2 language extensions w/o migration
#/Sm          Language level for migration
#/Sp1         Alignment in structures is on byte boundary
#/Ss+         Allow use of // for comments
#/Ti+         Generate debugger information
#/Ti-         Do not generate debugger information
#/V"string"   Version string
#/W3          Warning level 3
#/Xc-         Search paths specified using /I
#Xi+          Do not search the paths specified by the INCLUDE environment variable
#/DINCL_32    Compile for 32-bit OS/2 only
#------------------------------------------------------------------
CC     = $(COMPDIR)\BIN\ICC

#  Compiler options used:
CFLAGS=/C+  \
       /Gd+ \
       /Gs- \
       /G3  \
       /Kb- \
       /O-  \
       /Se  \
       /Sp1 \
       /Ss+ \
       /W3  \
       /Xi+ \
       /DINCL_32  \
       /Ti- \
       /V"V1.00"

# CFLAGS1: Special compiler options for .EXE or .DLL
CFLAGS1 = /Ge-

# Include libraries for the compiler:
# - For OS/2 includes
OS2INCL    = $(TOOLDIR)\c\os2h;
# - For C includes
C386INCL   = $(COMPDIR)\INCLUDE;$(COMPDIR)\INCLUDE\SYS;

# Search path for include files used by the compiler
CINC=/I$(OS2INCL)       \
     /I$(C386INCL)      \
     /I$(HEADERDIR)

# Path to store object modules
OBJDIR=/Fo$(OBJNAME)

################################################################################
#       Definitions for the linker                                             #
################################################################################
LINK   = $(TOOLDIR)\OS2BIN\LINK386

# Options for the linker LINK386:
#------------------------------------------------------------------
#Option       Description
#------------------------------------------------------------------
#/BATCH       Run in batch mode - disable prompting
#/CO          Prepare for debugger
#/MAP         List public symbols
#/NOE         Ignore extended dictionary
#/NOI         Preserve case sensitivity
#/NOD         Ignore default libraries
#/PACKC       Pack contiguous code
#/PACKD       Pack contiguous data
#/STACK:n     Specify program stack size
#----------------------------------------------------------------

# Linker options used:
LFLAGS= /NOI /MAP /NOD /PACKC /NOE

# Link libraries:
LIBS   = $(COMPDIR)\LIB\DDE4MBS.LIB \
         $(TOOLDIR)\OS2LIB\OS2386.LIB

################################################################################
#       Targets of the NMAKE process.                                          #
################################################################################
all: $(DLLNAME)

################################################################################
#       Dependencies                                                           #
################################################################################
$(OBJNAME): $(CFILE)    \
            $(HEADERS)  \
            $(MAKEFILE)
          $(CC) $(CFLAGS) $(CFLAGS1)     \
                $(CINC) $(OBJDIR)        \
                $(CFILE)

$(DLLNAME): $(OBJNAME)   \
            $(LIBS)      \
            $(MAKEFILE)  \
            $(DEFFILE)
          $(LINK) $(LFLAGS)        \
                  $(OBJNAME),      \
                  $(DLLNAME),      \
                  $(MAPNAME),      \
                  $(LIBS),         \
                  $(DEFFILE)       \
                  ;

#### end of file ###############################################################
