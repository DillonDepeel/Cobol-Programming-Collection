#include <os2def.h>

#pragma info( none )
#ifndef __CHKHDR__
   #pragma info( none )
#endif
#pragma info( restore )

/* These three functions can be called by the user through CBLPERF   */
#ifdef __cplusplus
   extern "C" {
#endif

   #ifndef  _LNK_CONV
      #ifdef __LE2__
         #define  _LNK_CONV   _System
      #else
         #define  _LNK_CONV   _Optlink
      #endif
   #endif

/************************************************************************/
/*  <cblperf.h> header file                                             */
/*                                                                      */
/*  IBM(R) VisualAge(TM) C++ for OS/2(R), Version 3                     */
/*  (C) Copyright IBM Corp. 1991, 1995.                                 */
/*  - Licensed Material - Program-Property of IBM - All Rights Reserved.*/
/*                                                                      */
/*                                                                      */
/* These three functions can be called by the user through PERF         */
/* to stop the trace        void   _LNK_CONV    PERFSTOP  ();           */
/* to start the trace       void   _LNK_CONV    PERFSTART ();           */
/* to log a user event within the trace file                            */
/*                          void   _LNK_CONV    PERF (PSZ STRING);      */
/************************************************************************/

    void   _LNK_CONV    PERFSTOP  ();
    void   _LNK_CONV    PERFSTART ();
    void   _LNK_CONV    PERF      (PSZ STRING);

#ifdef __cplusplus
   }
#endif

#pragma info( none )
#ifndef __CHKHDR__
   #pragma info( restore )
#endif
#pragma info( restore )
