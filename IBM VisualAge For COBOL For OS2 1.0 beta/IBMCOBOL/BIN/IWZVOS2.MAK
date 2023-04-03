#--------------------------------------------------------------------
# IWZVOS2.MAK
#
#              28H2177 (C) Copyright IBM Corp. 1995.
#              LICENSED MATERIAL - PROGRAM PROPERTY OF IBM     
#
#--------------------------------------------------------------------

.SUFFIXES:

OBJLIST=BACKUP                \
        $(IWZVAPPNAME).cpy    \
        COMPILE               \
        $(IWZVAPPNAME).def    \
        $(IWZVAPPTARGET).dll  \
        $(IWZVAPPTARGET).hlp

# no spaces allowed at the beginning of LFLAGS
LFLAGS=/NOE /NOL /NOI /NOD /PMTYPE:PM /EXEPACK $(LFLAGS) $(LINKOPT)

!ifdef IWZVEXE
!IF "$(IWZVEXE)"=="EXE"
OBJLIST=$(OBJLIST) VEXE
!endif
!endif

!ifdef IWZVMSG
OBJLIST=$(OBJLIST) $(IWZVAPPTARGET).msg
!endif

# definitions to copy executables to TEST directory which is in LIBPATH
!ifdef IWZVTEST
IWZVTESTC1="copy $(IWZVAPPSOURCE).ODX $(ICOBDIR)\TEST\$(IWZVAPPNAME).ODX"
IWZVTESTC2="copy $(ICOBDIR)\BIN\IWZVEXE.EXE $(ICOBDIR)\TEST\$(IWZVAPPNAME).EXE"
IWZVTESTC3="copy $(IWZVAPPSOURCE).DLL $(ICOBDIR)\TEST\$(IWZVAPPNAME).DLL"
!endif

# definitions to backup files before build
!ifdef IWZVBACKUP
IWZVBACKUP1="iwzvmd"
IWZVBACKUP2="iwzvdbk"
!endif

all: $(OBJLIST)

#-------------------------------------------------------------------
#   Dependencies
#-------------------------------------------------------------------

BACKUP:
      $(IWZVBACKUP1)

$(IWZVAPPTARGET).odx: $(IWZVAPPSOURCE).odf
      IWZVgenx $(IWZVAPPNAME) $(IWZVSOURCEDIR) $(IWZVSOURCEDIR) 2>nul 1>&2
      $(IWZVTESTC1)
      copy $(IWZVAPPSOURCE).ODX $(IWZVAPPTARGET).ODX

$(IWZVAPPNAME).cpy:  $(IWZVAPPTARGET).odx
     $(ICOBDIR)\BIN\IWZVCPY $(IWZVAPPSOURCE)

COMPILE:
       -7 $(ICOBDIR)\bin\iwzvcomp -c -q"$(COBOLOPT)" $(OTHEROPT) -I$(ICOBDIR)\INCLUDE -I$(IWZVSOURCEDIR) -pp"$(CICSOPT) $(SQLOPT) $(DBOPT)" $(IWZVAPPSOURCE).cbl

$(IWZVAPPNAME).def:  $(IWZVAPPSOURCE).cbl
     $(ICOBDIR)\BIN\IWZVDEF $(IWZVAPPSOURCE)

$(IWZVAPPTARGET).DLL: $(IWZVAPPSOURCE).OBJ
     COB2 /dll  $(LFLAGS) $(IWZVAPPNAME).OBJ IWZVLANG.OBJ $(IWZVPA) IWZVRT.LIB IWZVAPI.LIB iwzrlib.lib ECICALL.LIB cppooc3i.lib cppom30i.lib os2386.lib $(FILESIN) $(IWZVAPPNAME).DEF
     rc -r $(IWZVAPPNAME).rc
     rc $(IWZVAPPNAME).res $(IWZVAPPSOURCE).dll
     $(IWZVTESTC3)
     del $(IWZVAPPNAME).res
     copy $(IWZVAPPSOURCE).DLL $(IWZVAPPTARGET).DLL

VEXE:
!ifdef IWZVPA
     copy $(ICOBDIR)\BIN\IWZVPA.EXE $(IWZVAPPTARGET).EXE
!else
     copy $(ICOBDIR)\BIN\IWZVEXE.EXE $(IWZVAPPTARGET).EXE
!endif
     $(IWZVTESTC2)
     $(IWZVBACKUP2)

$(IWZVAPPTARGET).msg: $(IWZVAPPSOURCE).txm
     mkmsgf $(@B).txm $(IWZVAPPTARGET).msg $(IWZVMSGOPT)

$(IWZVAPPTARGET).hlp: $(IWZVAPPSOURCE).ipf $(IWZVAPPSOURCE).vpf $(IWZVAPPSOURCE).ipm
     ipfc $(IWZVIPFOPT) $(@B).ipf
     copy $(@B).hlp $(IWZVAPPTARGET).hlp
     del $(@B).hlp

