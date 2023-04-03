/******************************************************************************/
/* IWZVDON.CMD                                                                */
/*                                                                            */
/*            28H2177 (C) Copyright IBM Corp. 1995.                           */
/*            LICENSED MATERIAL - PROGRAM PROPERTY OF IBM                     */
/*                                                                            */
/*      Description: VisualAge COBOL GUI Desiger program to be used by        */
/*                   end users to switch to the trace version of the          */
/*                   executables and dynamic link libraries.                  */
/*                                                                            */
/******************************************************************************/

/* Set the name of the trace file */
tracefile = 'IWZVDBUG.TRC'

/* Get the command line parameters */
Parse Arg parameters

/* call the program to switch to the trace version */
'IWZVTRAC /ON'

If Rc = 0 Then Do
   If parameters = '' Then Do
     Exit 0
   End
   Else Do
     'SET ICLUI TRACETO=STDERR'

     /* execute the parameters as a command, redirecting STDERR to the trace file */
     parameters '2>' tracefile

     /* now that we're done, tell the user about the tracefile and IWZVDOFF */
     Say
     Say 'Trace output can be found in the file' tracefile || '.'
     Say 'To resume normal use, end the debug mode using the command IWZVDOFF.'

     Exit 0
   End
End
Else Do
   /* Exit with the non-zero return code from IWZVTRAC */
   Exit Rc
End
