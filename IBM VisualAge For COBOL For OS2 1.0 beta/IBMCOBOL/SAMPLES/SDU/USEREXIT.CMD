rem ****************************************************************************
rem * USEREXIT.CMD                                                             *
rem * Compile and Link commands for the USER EXIT sample.                      *
rem ****************************************************************************
ICC      /C+ /Ge- /Gm+ /Sp1 /Ss+ /Ti+ USEREXIT.C
LINK386  /CO /NOI /PM:VIO             USEREXIT.OBJ,USEREXIT.DLL,,,USEREXIT.DEF;
