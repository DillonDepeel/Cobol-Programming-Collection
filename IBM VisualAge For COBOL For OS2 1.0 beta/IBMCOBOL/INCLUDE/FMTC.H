 #if !defined(FMTC)                     /* If this include hasn't been
                                           included before, include it.       */
 #define FMTC FMTC                      /* Define the term so this include
                                           will not be included again.        */
#pragma pack(1)
/******************************************************************************/
// $Header:   /df/vc/0/1/6/2/s.58   1.2   30 Mar 1995 16:08:40   df  $
/******************************************************************************/
/*                                                                            */
/* PRODUCT   = Data Description and Conversion for OS/2                       */
/*                                                                            */
/*             IBM Confidential                                               */
/*             (IBM Confidential-Restricted                                   */
/*             when combined with the aggregated OCO                          */
/*             source modules for this program.)                              */
/*                                                                            */
/*             OCO SOURCE MATERIALS                                           */
/*                                                                            */
/* COPYRIGHT = 5622-75101 (c) IBM Corp. 1994                                  */
/*                                                                            */
/* SOURCE FILE NAME = FMTC.H                                                  */
/*                                                                            */
/* DESCRIPTIVE NAME = External interface of the Conversion Plan Builder.      */
/*                                                                            */
/* STATUS = VERSION 1, RELEASE 1, MODIFICATION 0                              */
/*                                                                            */
/* CONTENT = prototypes: - FMTCRCP                                            */
/*                                                                            */
/******************************************************************************/
//
// Change Activity
// ===============
//
//   Revision  Date        Time       Responsible
//   ========  ====        ====       ===========
//Reason
// $Log:   /df/vc/0/1/6/2/s.58  $
//
//   Rev 1.8   19 Jan 1994 12:07:22   BMB
//CPB_ERROR_INTERRUPT inserted
//
//   Rev 1.7   13 Jan 1994 11:04:26   BMB
//Returncode CPB_ID_CONTAINED_IN_MULTIPLE_PLANS inserted
//
//   Rev 1.6   10 Dec 1993 14:34:00   BMB
//
//
//   Rev 1.5   01 Dec 1993 09:21:12   BMB
//Implementation of parameter ulFlagList
//
//   Rev 1.4   24 Sep 1993 10:04:04   BMB
//Add returncodes CPB_PP_NOT_FOUND and
//CPB_PP_AMBIGUOUS
//
//   Rev 1.3   02 Sep 1993 17:34:08   BMB
//
//
//   Rev 1.2   02 Sep 1993 10:22:44   BMB
//
//
//   Rev 1.1   02 Sep 1993 10:07:40   BMB
//First compilable revision
//
//   Rev 1.0   19 Aug 1993 14:50:58   NEH
//Ensemble generation code.
/******************************************************************************/

extern void APIENTRY FMTCRCP(          /* function prototype of Conversion    */
                                       /* Plan Builder                        */
       ULONG ulAdlDclSpcCount,         /* Number of ADLDCLSPC addresses       */
       PBYTE *ppAdlDclSpcList,         /* Address of an array of ADLDCLSPC    */
                                       /* addresses                           */
       ULONG ulUserAdlPlnSpcCount,     /* Number of user ADLPLNSPC addresses  */
       PBYTE *ppUserAdlPlnSpcList,     /* Address of an array of user         */
                                       /* ADLPLNSPC addresses                 */
       ULONG ulDefaultAdlPlnSpcCount,  /* Number of default ADLPLNSPC         */
                                       /* addresses                           */
       PBYTE *ppDefaultAdlPlnSpcList,  /* Address of an array of default      */
                                       /* ADLPLNSPC addresses                 */
       ULONG ulCnvPlnSpcLength,        /* Size of supplied conversion plan    */
                                       /* plan space                          */
       PVOID pCnvPlnSpc,               /* Address of conversion plan space    */
       ULONG ulFlagList,               /* Flags for Cpb                       */
       PFMTCTOK pFeedback);            /* Address of the condition token      */



ULONG _System _DLL_InitTerm( ULONG hModule, ULONG ulFlag);

/******************************************************************************/
/* Defines of return codes                                                    */
/******************************************************************************/

/******************************************************************************/
/* Returncodes with Severity 0    ( Information )                             */
/******************************************************************************/
#define CPB_NO_ERROR                             (ULONG)0

/******************************************************************************/
/* Returncodes with Severity 1    ( Warning )                                 */
/******************************************************************************/
#define CPB_NO_EQ_DEFAULT_PLAN_NAME              (ULONG)1

/******************************************************************************/
/* Returncodes with Severity 2    ( Error )                                   */
/******************************************************************************/
#define CPB_ADL_EXCEPTION_SEV2                   (ULONG)2

/******************************************************************************/
/* Returncodes with Severity 3    ( Severe Error )                            */
/******************************************************************************/
#define CPB_ERROR_PARAMETERS                     (ULONG)3
#define CPB_ERROR_NO_MEMORY                      (ULONG)4
#define CPB_INVALID_ADLDCLSPC                    (ULONG)5
#define CPB_INVALID_ADLPLNSPC                    (ULONG)6
#define CPB_ERROR_SAME_DECLARE                   (ULONG)7
#define CPB_ERROR_BUILD_PLAN                     (ULONG)8
#define CPB_CNVPLNSPC_NO_MEMORY                  (ULONG)9
#define CPB_INVALID_ATTR_VALUE                   (ULONG)10
#define CPB_STMT_ID_NOT_FOUND                    (ULONG)11
#define CPB_ID_NOT_FOUND                         (ULONG)12
#define CPB_PP_NOT_FOUND                         (ULONG)13
#define CPB_STMT_ID_AMBIGUOUS                    (ULONG)14
#define CPB_ID_AMBIGUOUS                         (ULONG)15
#define CPB_PP_AMBIGUOUS                         (ULONG)16
#define CPB_ERROR_DCL_REFERENCED                 (ULONG)17
#define CPB_ADL_EXCEPTION_SEV3                   (ULONG)18
#define CPB_ID_CONTAINED_IN_MULTIPLE_PARAMETERS  (ULONG)19
#define CPB_ERROR_INTERRUPT                      (ULONG)20

#pragma pack()
 #endif                                 /* End of FMTC include.               */
