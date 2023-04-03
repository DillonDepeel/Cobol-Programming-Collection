/*static char *SCCSID = "@(#)bsememf.h	6.8 92/03/15";*/

 /******************************************************************************\
 *                                                                             *
 * Module Name: BSEMEMF.H                                                      *
 *                                                                             *
 * OS/2 Base MEMory Flags include file                                         *
 *                                                                             *
 * Copyright (c) 1989  - 1992 IBM Corporation                                  *
 *                                                                             *
 *******************************************************************************/

#if __IBMC__ || __IBMCPP__
#pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( none )
   #endif
#pragma info( restore )
#endif
#ifdef __cplusplus
      extern "C" {
#endif

#ifndef __BSEMEMF__
/* NOINC */
#define __BSEMEMF__
/* INC */

 /* Access protection                                                          */

 #define PAG_READ          0x00000001U      /* read access                      */

 #define PAG_WRITE         0x00000002U      /* write access                     */

 #define PAG_EXECUTE       0x00000004U      /* execute access                   */

 #define PAG_GUARD         0x00000008U      /* guard protection                 */

 #define PAG_DEFAULT       0x00000400U      /* default (initial) access         */



 /* Commit                                                                     */

 #define PAG_COMMIT        0x00000010U      /* commit storage                   */

 #define PAG_DECOMMIT      0x00000020U      /* decommit storage                 */



 /* Allocation attributes                                                      */

 #define OBJ_TILE          0x00000040U      /* tile object                      */

 #define OBJ_PROTECTED     0x00000080U      /* protect object

                                            NOTE: This flag is NOT available at

                                            the api level */

 #define OBJ_GETTABLE      0x00000100U      /* gettable by other processes      */

 #define OBJ_GIVEABLE      0x00000200U      /* giveable to other processes      */



 #define fPERM             (PAG_EXECUTE | PAG_READ | PAG_WRITE)

 #define fSHARE            (OBJ_GETTABLE | OBJ_GIVEABLE)



 /* DosAllocMem flags                                                          */

 #define fALLOC            (OBJ_TILE | PAG_COMMIT | fPERM)



 /* DosAllocSharedMem flags                                                    */

 #define fALLOCSHR         (OBJ_TILE | PAG_COMMIT | fSHARE | fPERM)



 /* DosGetNamedSharedMem flags                                                 */

 #define fGETNMSHR         (fPERM)



 /* DosGetSharedMem flags                                                      */

 #define fGETSHR           (fPERM)



 /* DosGiveSharedMem flags                                                     */

 #define fGIVESHR          (fPERM)



 /* DosSetMem flags                                                            */

 #define fSET              (PAG_COMMIT + PAG_DECOMMIT + PAG_DEFAULT + fPERM)



 /* Dos32SubSet flags                                                          */

 #define DOSSUB_INIT       0x01U           /* initialize memory object for     */

                                           /* suballocation                    */

 #define DOSSUB_GROW       0x02U           /* increase size of memory pool     */

                                           /* for suballocation                */

 #define DOSSUB_SPARSE_OBJ 0x04U           /* indicator for DosSub to          */

                                           /* manage the commitment of         */

                                           /* pages spanned by the memory      */

                                           /* pool                             */

 #define DOSSUB_SERIALIZE  0x08U           /* indicates that access to the     */

                                           /* memory pool is to be             */

                                           /* serialized by DosSub             */



 /* Allocation type (returned from DosQueryMem)                                */

 #define PAG_SHARED        0x00002000U     /* shared object                    */

 #define PAG_FREE          0x00004000U     /* pages are free                   */

 #define PAG_BASE          0x00010000U     /* first page in object             */

#endif /* __BSEMEMF__ */

#ifdef __cplusplus
        }
#endif
#if __IBMC__ || __IBMCPP__
#pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( restore )
   #endif
#pragma info( restore )
#endif
