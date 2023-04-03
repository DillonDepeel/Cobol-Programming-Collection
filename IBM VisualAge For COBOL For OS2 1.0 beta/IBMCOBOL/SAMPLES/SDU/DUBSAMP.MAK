#**     Makefile for DUBSAMP.EXE

.SUFFIXES:
.SUFFIXES:      .exe .obj .lst .cod .c .asm .def .lnk .inc .h

#       Definitions for C compiler

CC=icc
CFLAGS= /Ss+ /Se /G3 /Gs- /W3 /Ti+ /O-
CINC=

#       Definitions for assembler

ASM=masm386
AFLAGS= -Mx -t
AINC=

#       Definitions for linker

LINK4=link386
LFLAGS4= /CO

#       Definitions for CodeView

CVPATH=

#       Rules for generating object and linker response and definition files

.asm.obj:
        $(ASM) $(AFLAGS) $(AINC) $(CVPATH)$*.asm;

.asm.lst:
        $(ASM) -l $(AFLAGS) $(AINC) $(CVPATH)$*.asm;

.c.obj:
        $(CC) $(CFLAGS) -Fo$(CVPATH)$*.obj -dos $(CINC) -c $(CVPATH)$*.c

.c.cod:
        $(CC) $(CFLAGS) -Fc$(CVPATH)$*.cod -Fo$(CVPATH)$*.obj -dos $(CINC) -c $(CVPATH)$*.c

.c.lst:
        $(CC) $(CFLAGS) -Fc$(CVPATH)$*.cod -Fo$(CVPATH)$*.obj -Fs$(CVPATH)$*.lst  -dos $(CINC) -c $(CVPATH)$*.c


#       Dependencies

all: dubsamp.exe

dubsamp.exe: dubsamp.obj dubsamp.lnk
           $(LINK4) $(LFLAGS4) @dubsamp.lnk;

dubsamp.obj : dubsamp.c

