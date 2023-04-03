/****************************************************************************/
/*                                                                          */
/* Module Name: MCIAPI.H                                                    */
/*                                                                          */
/* This is the multimedia include file that has the typedefs, defines and   */
/* function prototyes for High-Level MCI Interfaces                         */
/*                                                                          */
/* Copyright (c) International Business Machines Corporation 1992, 1993     */
/*                        All Rights Reserved                               */
/*                                                                          */
/****************************************************************************/
/* The folowing symbols are used in this file for conditional sections.     */
/*                                                                          */
/*   #define:                To include:                                    */
/*                                                                          */
/*                                                                          */
/****************************************************************************/

/* High-Level Macro Service API Routines */

#ifndef __MCIAPI_H
#define __MCIAPI_H

#ifndef INCL_32
#define     mciPlayFile        mci16PlayFile
#define     mciPlayResource    mci16PlayResource
#define     mciRecordAudioFile mci16RecordAudioFile
#define     mmioRemoveElement  mmio16RemoveElement
#define     mmioFindElement    mmio16FindElement
#endif


ULONG EXPENTRY mciPlayFile (HWND hwndOwner,        /* Ownerwindow */
                            PSZ  pszFile,          /* File */
                            ULONG ulFlags,         /* Flags */
                            PSZ  pszTitle,         /* Title */
                            HWND hwndViewport);    /* Viewport Window */

ULONG EXPENTRY mciPlayResource (HWND hwndOwner,       /* Owner Window */
                                HMODULE hmod,         /* Module */
                                ULONG resType,        /* Resource Type */
                                ULONG resID,          /* Resource ID */
                                ULONG ulFlags,        /* Flags */
                                PSZ  pszTitle,        /* Title */
                                HWND hwndViewport);   /* Viewport Window */

ULONG EXPENTRY mciRecordAudioFile (HWND hwndOwner, PSZ pszFile, PSZ pszTitle, ULONG ulFlags);

/* Audio Macro Service Routines */

ULONG EXPENTRY mmioRemoveElement (PSZ pszFileElement, ULONG ulFlag); /* c:\path\file+element */

ULONG EXPENTRY mmioFindElement (ULONG ulCode,       /* Find Code */
                               PSZ   pszElement,   /* Element */
                               ULONG ulElementLen, /* Element Buffer Length */
                               PSZ   pszFile,
                               ULONG ulReserved
                               );     /* Compound File */

#define MMIO_FE_FINDFIRST       1
#define MMIO_FE_FINDNEXT        2
#define MMIO_FE_FINDEND         3
#define MMIO_FE_FINDELEMENT     4

#define MMIO_RE_COMPACT         1


/* ulFlags  for mciPlayFile */

#define MCI_OWNERISPARENT   0x0001      /* digital and overlay                 */
#define MCI_STOPACTIVE      0x0002      /* stop playing whatever is playing    */
#define MCI_ASYNCRENDEZVOUS 0x0004      /* play and return immediately         */
#define MCI_RENDEZVOUS      0x0008      /* wait til prev is finished then play */
#define MCI_ASYNC           0x0010      /* no syncup will be done              */

#endif
