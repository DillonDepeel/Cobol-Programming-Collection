/******************************************************************************/
/* MODULE NAME:    EHNXNMP.H                                                  */
/* DESCRIPTION:    Interface Definitions for the DFM/2 Name Mapping Exit      */
/*                 Include File for the Name Mapping Exit sample EHNXNMP.DLL  */
/*                                                                            */
/*                                                                            */
/* DFM/2 (program no. 5648-020)                                               */
/* Version: 1.0                                                               */
/* Release: 1.0                                                               */
/* Level:   0.0                                                               */
/*                                                                            */
/* Copyright (C) International Business Machines Corporation, 1993            */
/*                                                                            */
/******************************************************************************/
/* CHANGE ACTIVITY                                                            */
/* Flag Reason      Level    Date   Origin    Comments                        */
/* -------------------------------------------------------------------------- */
/*                           930402 Mueller  : Initial Release                */
/* IBM Deutschland Entwicklung GmbH, Boeblingen, Germany        GMU at SDFVM1 */
/******************************************************************************/

/******************************************************************************/
/*        This file contains definitions needed to write a                    */
/*                                                                            */
/*                   Name Mapping Exit Program                                */
/*                            for                                             */
/*            IBM Distributed Filemanager (DFM/2) for OS/2.                   */
/*                                                                            */
/*                                                                            */
/* It is also part of the Name Mapping Exit sample program for DFM/2.         */
/* It contains definitions and prototypes for the Name Mapping Exit.          */
/* For further description refer to the header of the file EHNXNMP.C,         */
/* that you find in the \SAMPLE subdirectory of the RLIO/DFM product directory*/
/******************************************************************************/

#if !defined(EHNXNMP)                    /* If this include hasn't been
                                            included before, include it.      */
 #define EHNXNMP  EHNXNMP                /* Define the term so this include
                                            will not be included again.       */

 /*****************************************************************************/
 /* Interface structure for the DFM/2 Name Mapping Exit                       */
 /*****************************************************************************/
 typedef struct _DFM_NAME_MAP_CB         /* DFM/2 name mapping control block  */
 {
   char           SrvClsName[9];         /* DFM server class of remote system */
   char           LU_Name[9];            /* partner LU alias of target */
   char           InFileName[256];       /* original directory or file name */
   char           OutFileName[256];      /* mapped directory or file name */
 } DFM_NAME_MAP_CB, *PDFM_NAME_MAP_CB;

 /*****************************************************************************/
 /* Function prototypes for the DFM/2 Name Mapping Exits                      */
 /*****************************************************************************/
 void _System DFM_Map_to_Server(PDFM_NAME_MAP_CB);     /* from OS/2 to target */
 void _System DFM_Map_to_Client(PDFM_NAME_MAP_CB);     /* from target to OS/2 */

 /*****************************************************************************/
 /* Definitions for the values in SrvClsName                                  */
 /*****************************************************************************/
 #define SRVCLSNM_36      "Q36"     /* DDM file server on System/36 */
 #define SRVCLSNM_38      "Q38"     /* DDM file server on System/38 */
 #define SRVCLSNM_CICSDDM "QCICS"   /* CICS DDM file server on CICS/MVS 
                                                           and CICS/VSE */
 #define SRVCLSNM_OS400   "QAS"     /* DDM file server on OS/400 */
 #define SRVCLSNM_MVS     "QMVS"    /* DDM file server on MVS */
 #define SRVCLSNM_CMS     "QCMS"    /* DDM file server on VM/CMS (reserved) */
 #define SRVCLSNM_PCS400  "QFS"     /* Enhanced PC Support/400 target server */
 #define SRVCLSNM_OS4680  "Q4680"   /* DDM file server on a 4680 Store System */

#endif 
/**** end of file *************************************************************/
