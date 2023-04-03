/******************************************************************************/
/* SAMPLE PROGRAM: EHNXNMP   - Sample Name Mapping Exit for DFM/2.            */
/*                                                                            */
/* MODULE NAME:    EHNXNMP.C                                                  */
/* DESCRIPTION:    Main File for the Name Mapping Exit EHNXNMP.DLL            */
/*                                                                            */
/*                                                                            */
/* DFM/2 (program no. 5648-020)                                               */
/* Version: 1.0                                                               */
/* Release: 1.0                                                               */
/* Level:   0.0                                                               */
/*                                                                            */
/* Copyright (C) International Business Machines Corporation 1993             */
/*                                                                            */
/* DISCLAIMER OF WARRANTIES:                                                  */
/* The following [enclosed] code is sample code created by IBM                */
/* Corporation.  This sample code is not part of any standard IBM product     */
/* and is provided to you solely for the purpose of assisting you in the      */
/* development of your applications.  The code is provided "AS IS",           */
/* without warranty of any kind.  IBM shall not be liable for any damages     */
/* arising out of your use of the sample code, even if they have been         */
/* advised of the possibility of such damages.                                */
/*                                                                            */
/******************************************************************************/
/* CHANGE ACTIVITY                                                            */
/* Flag Reason      Level    Date   Origin    Comments                        */
/* -------------------------------------------------------------------------- */
/*                           930402 Mueller  : Initial Release                */
/* IBM Deutschland Entwicklung GmbH, Boeblingen, Germany        GMU at SDFVM1 */
/******************************************************************************/

/******************************************************************************/
/*        This is a programming sample showing how to write a                 */
/*                                                                            */
/*                   Name Mapping Exit Program                                */
/*                            for                                             */
/*            IBM Distributed Filemanager (DFM/2) for OS/2.                   */
/*                                                                            */
/*                                                                            */
/* The sample program does the following:                                     */
/* If the file is for a MVS target, some conversions are done. They are       */
/* described below.                                                           */
/* If this module is compiled with PRINTLOG defined, then                     */
/* the input file name and the corresponding output file name are logged      */
/* in a file NAMEMAP.LOG in directory %EHNDIR%.                               */
/* No further processing is done.                                             */
/*                                                                            */
/* Note: If you use the sample as is or with some modifications only,         */
/*       make sure to delete the file NAME.LOG from time to time              */
/*       before it becomes too large.                                         */
/*                                                                            */
/* Files belonging to this sample program:                                    */
/*     EHNXNMP.C    C-language program, this file                             */
/*     EHNXNMP.H    Prototype for the Name Mapping Exit and other definitions */
/*     EHNXNMP.DEF  Module Definition File                                    */
/*     EHNXNMP.MAK  Make File                                                 */
/*                                                                            */
/* You find these files in the following subdirectories of the                */
/* RLIO/DFM product directory:                                                */
/*     %ehndir%\H        contains EHNXNMP.H                                   */
/*     %ehndir%\SAMPLE   contains EHNXNMP.C, .DEF, and .MAK                   */
/*                                                                            */
/* To compile the program, you need                                           */
/*    a. The IBM C Set/2 compiler version 1.0 or later                        */
/*    b. The Developer's Toolkit for OS/2 2.0 or later                        */
/*       including the linker LINK386                                         */
/*       and the make file utility NMAKE                                      */
/*                                                                            */
/******************************************************************************/

/******************************************************************************/
/* OS/2 Include files                                                         */
/******************************************************************************/
#define INCL_BASE
#include <os2.h>                         /* OS/2 base set of C functions      */

/******************************************************************************/
/* C Library Includes                                                         */
/******************************************************************************/
#include <string.h>                      /* Include used for: memcpy, ...     */
#include <stdlib.h>                      /* Include used for: getenv.         */
#include <stdio.h>

#pragma pack()

/******************************************************************************/
/* DFM/2 Includes                                                             */
/******************************************************************************/
#include "ehnxnmp.h"             /* Definitions and prototypes for the
                                    Name Mapping Exit functions */

#define MAX8    255

/******************************************************************************/
/* Name    : DFM_Map_to_Server                                                */
/*                                                                            */
/* Function: Name mapping exit routine on the way from OS/2 to the target.    */
/*                                                                            */
/*           This sample routine does the following conversion                */
/*           when a file is sent to an MVS system:                            */
/*           - A pattern "\(" is replaced by "(".                             */
/*           - A pattern "\" not followed by a "(" is replaced by ".".        */
/*           - The file name is converted to uppercase.                                                                 */
/*           Otherwise no name mapping is done.                               */
/*           Then the exit routine returns to DFM/2.                          */
/*                                                                            */
/* Input   : A pointer to the DFM/2 Name Mapping Exit interface control       */
/*           block (defined in EHNXNMP.H).                                    */
/* Output  : The mapped file name can be written to the OutFileName           */
/*           buffer in the interface control block.                           */
/******************************************************************************/
extern void _System DFM_Map_to_Server(PDFM_NAME_MAP_CB pNameMapCB)
{
   FILE *printer;
   char *ehndir;         /* pointer to the EHNDIR environment variable */
   char pfilename[MAX8+1];  /* filename for printer */
   char *ptr;
   LONG incount, outcount;

#ifdef PRINTLOG
   ehndir = getenv("EHNDIR");                   /* get environment var. EHNDIR */
   if (ehndir != NULL)
           { ptr = strcpy(pfilename, ehndir); } /* and build the file name     */
      else { ptr = strcpy(pfilename, "."   ); }
   ptr = strcat(pfilename, "\\NAMEMAP.LOG" );   /* %EHNDIR%\NAMEMAP.LOG        */

   printer = fopen(pfilename,"a");

   fprintf (printer,"\nDFM_Map_to_Server: ");
   fprintf (printer,"%s",pNameMapCB->InFileName);
#endif

   if(strcmp(pNameMapCB->SrvClsName,SRVCLSNM_MVS) == 0)
                                       // Target is MVS.
   {
      for( incount=0, outcount=0 ;
           incount<MAX8 && pNameMapCB->InFileName[incount];
           incount++, outcount++ )
      {
         if( '\\' == pNameMapCB->InFileName[incount] )
         {
            if( '(' == pNameMapCB->InFileName[incount+1] )
            {
                                       // Convert "\(" to "("
               incount++;
               pNameMapCB->OutFileName[outcount] = '(';
            }
            else
                                       // Convert "\" to "."
               pNameMapCB->OutFileName[outcount] = '.';
         }
         else
                                       // In all the other cases convert
                                       // to uppercase and copy.
            pNameMapCB->OutFileName[outcount] =
                      toupper( pNameMapCB->InFileName[incount] );
      }
      pNameMapCB->OutFileName[outcount] = 0;
   }
   else
   {
     strcpy( pNameMapCB->OutFileName, pNameMapCB->InFileName );
   }

#ifdef PRINTLOG
   fprintf (printer," -> %s",pNameMapCB->OutFileName);
   fprintf (printer," (Target: %s LU: %s)",
                    pNameMapCB->SrvClsName,
                    pNameMapCB->LU_Name );

   fclose(printer);
#endif
   return;
}

/******************************************************************************/
/* Name    : DFM_Map_to_Client                                                */
/*                                                                            */
/* Function: Name mapping exit routine on the way from the target to OS/2.    */
/*                                                                            */
/*           This sample routine does the following conversion                */
/*           when a file is receved from an MVS system:                       */
/*           - A pattern "(" is replaced by "\(".                             */
/*           - A pattern "." is replaced by "\".                              */
/*           Otherwise no name mapping is done.                               */
/*           Then the exit routine returns to DFM/2.                          */
/*                                                                            */
/* Input   : A pointer to the DFM/2 Name Mapping Exit interface control       */
/*           block (defined in EHNXNMP.H).                                    */
/* Output  : The mapped file name can be written to the OutFileName           */
/*           buffer in the interface control block.                           */
/******************************************************************************/
extern void _System DFM_Map_to_Client(PDFM_NAME_MAP_CB pNameMapCB)
{
   FILE *printer;
   char *ehndir;         /* pointer to the EHNDIR environment variable */
   char pfilename[MAX8+1];  /* filename for printer */
   char *ptr;
   LONG incount, outcount;

#ifdef PRINTLOG
   ehndir = getenv("EHNDIR");                   /* get environment var. EHNDIR */
   if (ehndir != NULL)
           { ptr = strcpy(pfilename, ehndir); } /* and build the file name     */
      else { ptr = strcpy(pfilename, "."   ); }
   ptr = strcat(pfilename, "\\NAMEMAP.LOG" );   /* %EHNDIR%\NAMEMAP.LOG        */

   printer = fopen(pfilename,"a");

   fprintf (printer,"\nDFM_Map_to_Client: ");
   fprintf (printer,"%s",pNameMapCB->InFileName);
#endif

   if(strcmp(pNameMapCB->SrvClsName,SRVCLSNM_MVS) == 0)
   {
      for( incount=0, outcount=0 ;
           outcount<MAX8 && pNameMapCB->InFileName[incount];
           incount++, outcount++ )
      {
         switch( pNameMapCB->InFileName[incount] )
         {
         case '(':
                                       // Convert "(" to "\("
            pNameMapCB->OutFileName[outcount] = '\\';
            outcount++;
            pNameMapCB->OutFileName[outcount] = '(';
            break;

         case '.':
                                       // Convert "." to "\"
            pNameMapCB->OutFileName[outcount] = '\\';
            break;

         default:
                                       // In all the other cases copy char.
            pNameMapCB->OutFileName[outcount] =
                      pNameMapCB->InFileName[incount];
            break;
         }
      }
      pNameMapCB->OutFileName[outcount] = 0;
   }
   else
   {
     strcpy( pNameMapCB->OutFileName, pNameMapCB->InFileName );
   }

#ifdef PRINTLOG
   fprintf (printer," -> %s",pNameMapCB->OutFileName);
   fprintf (printer," (Target: %s LU: %s)",
                     pNameMapCB->SrvClsName,
                     pNameMapCB->LU_Name );
   fclose(printer);
#endif
   return;
}
/**** end of file *************************************************************/
