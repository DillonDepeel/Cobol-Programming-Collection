# ----------------
# TELDIR make file
# ----------------

CC         = icc /Ti+ /c /Q+ /Gn- /Gd- /Se /Re /ss+ /Gm-
LFLAGS     = /CODEVIEW /NOE /ALIGN:16 /EXEPACK /M /BASE:0x10000
LINK       = ILINK /NOFREE $(LFLAGS)
LIBS       = OS2386 + PMBIDI

IPF_COMPILER      = ipfcbidi

.c.obj:
    $(CC) -Fo$*.obj $*.c


#-------------------------------------------------------------------------
#  ---------------------------------
#  NOTE ABOUT THE USE OF IPFCBIDI!
#  ---------------------------------
#
#  The IPFCBIDI utility (the bidirectional version of IPFC) can be
#  run only on a system with bidirectional support capabilites.
#  As a result, the Arabic .HLP file can be built only on such a system.
#
#  If you are running on a system with bidirectional support capabilites
#  you can build the .HLP files by un-commenting the following line, and
#  commenting the line just after it.
#
#-------------------------------------------------------------------------
#  --> Activate the following line on a system with Bidirectional support:
#all: teldir.exe teldir.hlp teldira.hlp
#
#  --> Activate the following line on a system without Bidirectional support:
all: teldir.exe


#-------------------------------------------------------------------
#   A list of all of the object files
#-------------------------------------------------------------------
teldir.l: teldir.mak
    echo teldir.obj            > teldir.l
    echo teldir.exe           >> teldir.l
    echo teldir.map           >> teldir.l
    echo $(LIBS)              >> teldir.l
    echo teldir.def           >> teldir.l

teldir.obj: teldir.c

teldir.hlp: teldir.ipf
    $(IPF_COMPILER) $*.ipf /W3  /L=ENU

teldira.hlp: teldira.ipf
    $(IPF_COMPILER) $*.ipf /W3  /COUNTRY=785 /CODEPAGE=864 /L=ARA

teldir.res : teldir.rc teldir.h
    rc -r TELDIR.RC

teldir.exe: teldir.obj teldir.l teldir.def teldir.res teldir.h
    $(LINK) @teldir.l
   rc teldir.res
