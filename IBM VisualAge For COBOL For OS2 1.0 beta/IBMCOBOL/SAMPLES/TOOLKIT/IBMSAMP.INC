#===================================================================
#
#   Sample application makefile, common definitions for the IBM C
#   compiler environment
#===================================================================
.SUFFIXES:
.SUFFIXES: .rc .res .obj .lst .c .asm .hlp .itl .ipf
#===================================================================
# Default compilation macros for sample programs
#
# Compile switches  that are enabled
# /c      compile don't link
# /Gm+    use the multi-threaded libraries
# /ss     allow  "//" for comment lines
# /Ms     use the system calling convention and not _optlink as the default
# /Gd-    disable optimization
# /Se     allow C Set/2  extensions
#
#

CC         = icc  /c /Gd- /Se /Re /ss /Ms /Gm+


AFLAGS  = /Mx -t -z
ASM     = ml /c /Zm
LFLAGS   = /NOFREE /NOE /NOD /ALIGN:16 /EXEPACK /M /BASE:0x10000 
LINK    = ILINK  $(LFLAGS)
LIBS    = CPPOM30 + OS2386
STLIBS  = CPPOS30 + OS2386
MTLIBS  = CPPOM30 + os2386
DLLLIBS = CPPON30 + os2386
VLIBS   = CPPOS30 + vdh + os2386

.c.lst:
    $(CC) -Fc$*.lst -Fo$*.obj $*.c

.c.obj:
    $(CC) -Fo$*.obj $*.c

.asm.obj:
    $(ASM)   $*.asm

.ipf.hlp:
        ipfc $*.ipf /W3

.itl.hlp:
        cc  -P $*.itl
        ipfc $*.i
        del $*.i

.rc.res:
        rc -r $*.rc
