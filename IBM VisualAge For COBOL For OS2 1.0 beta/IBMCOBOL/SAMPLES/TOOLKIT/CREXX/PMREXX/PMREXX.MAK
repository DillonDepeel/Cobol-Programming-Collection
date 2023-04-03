# NMAKE-compatible MAKE file for the REXX sample program PMREXX.EXE.

all: pmrexx.exe pmrexxio.dll pmrexxio.res pmrexx.res pmrexxio.hlp

pmrexx.obj: pmrexx.c pmrexx.h pmrexxio.h
        icc /c /Gm+ $*.c

rxhafile.obj: rxhafile.c pmrexx.h
        icc /c /Gm+ $*.c

pmrexxio.obj: pmrexxio.c pmrexxio.h rhdtatyp.h pmrxiodt.h
        icc /c /Ge- /Gm+ $*.c

pmrxsubs.obj: pmrxsubs.c pmrexxio.h pmrxiodt.h
        icc /c /Ge- /Gm+ $*.c

pmrexxio.hlp: pmrexxio.ipf
         ipfc pmrexxio.ipf

pmrexx.res: pmrexx.rc pmrxhlp.rc pmrexx.h rxhdluse.h pmrxhlp.h pmrexx.ico
        rc -r pmrexx.rc

pmrexxio.res: pmrexxio.rc rxiohlp.rc pmrexxio.h rxhdluse.h pmrexxio.hlp
        rc -r pmrexxio.rc

pmrexx.exe: rxhafile.obj pmrexx.obj pmrexx.res pmrexx.def
        ilink /nofree  pmrexx+rxhafile,pmrexx.exe,pmrexx,rexx,pmrexx.def
        rc pmrexx.res

pmrexxio.dll: pmrexxio.obj pmrxsubs.obj pmrexxio.res pmrexxio.def pmrexxio.hlp
        ilink /nofree  pmrexxio+pmrxsubs,pmrexxio.dll,pmrexxio,rexx,pmrexxio.def
        rc pmrexxio.res pmrexxio.dll

