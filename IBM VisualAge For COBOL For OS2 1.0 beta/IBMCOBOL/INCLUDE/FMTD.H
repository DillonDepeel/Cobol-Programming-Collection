 #if !defined(FMTD)                    /* If this include hasn't been
                                          included before, include it.       */
 #define FMTD FMTD                     /* Define the term so this include
                                          will not be included again.        */
/******************************************************************************/
// $Header:   /df/vc/0/1/6/3/s.15   1.2   31 Mar 1995 10:45:54   df  $
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
/* SOURCE FILE NAME = FMTD.H                                                  */
/*                                                                            */
/* DESCRIPTIVE NAME = global type definitions for the CPEX                    */
/*                                                                            */
/* STATUS = VERSION 1, RELEASE 1, MODIFICATION 0                              */
/*                                                                            */
/* CONTENT = included definitions (types, constants, prototypes etc.)         */
/*                                                                            */
/******************************************************************************/
//
// Change Activity
// ===============
//
//   Revision  Date        Time       Responsible
//   ========  ====        ====       ===========
//Reason
// $Log:   /df/vc/0/1/6/3/s.15  $
//
//   Rev 1.18   13 Apr 1994 13:54:16   MQT
//rename apInput and apOutput to ppxxx (KDC0091). (@MC1)
//
//   Rev 1.17   16 Mar 1994 16:25:52   MQT
//
//
//   Rev 1.16   16 Mar 1994 14:09:46   MQT
//rename error messages _INV_  to _INVALID_
//
//   Rev 1.15   22 Feb 1994 13:15:02   MQT
//remove unused return codes
//
//   Rev 1.14   09 Feb 1994 13:47:50   MQT
//Change ADL_EXEPTION to ADL_EXCEPTION
//
//   Rev 1.13   03 Feb 1994 14:27:26   VIELFOR
//new return code CPX_MIXED_BYTE_TRUNC added
//
//   Rev 1.12   31 Jan 1994 16:36:02   MQT
//Last comments for CUT end.
//
//   Rev 1.11   25 Jan 1994 13:40:28   MQT
//
//
//   Rev 1.10   12 Jan 1994 18:35:20   MQT
//Unit testest.
//
//   Rev 1.9   05 Jan 1994 07:52:48   MQT
//Add CPX_LOADMODULE_ERROR to FMTD.H.
//
//   Rev 1.8   10 Dec 1993 13:15:30   MQT
//Add pragma pack(1) statement
//
//   Rev 1.7   08 Dec 1993 16:53:42   MQT
//Add new return code CPX_ERROR_INTERRUPT and
//move internal errors from FMTD.H for FMTDDEFN.H
//
//   Rev 1.6   26 Nov 1993 12:33:10   MQT
//First BIN to BIN works. Changed CPXC API.
//
//   Rev 1.5   26 Oct 1993 10:42:58   MQT
//Init/Term works.
//
//   Rev 1.4   11 Oct 1993 17:33:56   MQT
//Remove shared memory access.
//
//   Rev 1.3   06 Sep 1993 09:25:02   MQT
//
//
//   Rev 1.2   25 Aug 1993 08:00:30   MQT
//
//
//   Rev 1.1   13 Aug 1993 14:17:36   MQT
//First compilable version
//
//   Rev 1.0   08 Jul 1993 16:10:22   MQT
//Initial Revision
/******************************************************************************/

#pragma pack(1)

 extern void APIENTRY FMTCPXI(         /* function prototype for conversion
                                          plan executor - initialize function */
             PVOID    pCnvPlnSpc,      /*Pointer to the Conversion Plan Space */
             PULONG   pulCnvPlnSpcHdl, /*pulCnvPlnSpcHdl                      */
             PFMTCTOK pFeedback);      /*pFeedback                            */

 extern void APIENTRY FMTCPXC(         /* function prototype for conversion
                                          plan executor - convert function    */
            ULONG    ulCnvPlnSpcHdl,   /* conversion plan space handle        */
            ULONG    ulPlnNamLength,   /* ADL plan name length                */
            PCHAR    pchAdl_Plan_Name, /* ADL PlanName to execute             */
            ULONG    ulInputParmNum,   /* number of input parameter           */
            PBYTE    *ppInputData,     /* pointer to plan execution input
                                          parameter                      @MC1 */
            ULONG    ulOutputParmNum,  /* number of output parameter          */
            PBYTE    *ppOutputData,    /* pointer to the execution output
                                          parameter                      @MC1 */
            PFMTCTOK pFeedback);       /* pointer to the user provided
                                          condition token buffer              */

 extern void APIENTRY FMTCPXT(         /* execute the external CPEX Terminate
                                          function                            */
             ULONG     ulCnvPlnSpcHdl, /* conversion plan space handle        */
             PFMTCTOK  pFeedback);     /* pointer to the user provided
                                          feedback buffer                     */

/******************************************************************************/
/* Define Reason Codes for CPEX functions (FMTCPXI, FMTCPXC  and FMTCPXT)     */
/******************************************************************************/

#define  CPX_NO_ERROR            (ULONG)0
#define  CPX_ERROR_INTERRUPT     (ULONG)1
#define  CPX_INTERNAL_ERR        (ULONG)2
#define  CPX_INVALID_CODE        (ULONG)3
#define  CPX_INVALID_HANDLE      (ULONG)4
#define  CPX_INVALID_ADDR        (ULONG)5
#define  CPX_INVALID_ADLCA       (ULONG)6
#define  CPX_INVALID_CPS         (ULONG)7
#define  CPX_INVALID_PLNNM       (ULONG)8
#define  CPX_NO_MEMORY           (ULONG)9
#define  CPX_PARMNBR_DIFFERS     (ULONG)10
#define  CPX_PLAN_NOT_FOUND      (ULONG)11
#define  CPX_RESOURCE_LIM        (ULONG)12
#define  CPX_USEREXIT_WARNING    (ULONG)13
#define  CPX_USEREXIT_ERROR      (ULONG)14
#define  CPX_CDRA_RESOURCE_ERROR (ULONG)15
#define  CPX_LOADMODUL_ERROR     (ULONG)16
#define  CPX_MIXED_BYTE_TRUNC    (ULONG)17


#define  CPX_ADL_EXCEPTION_SEV1   (ULONG)101
#define  CPX_ADL_EXCEPTION_SEV2   (ULONG)102
#define  CPX_ADL_EXCEPTION_SEV3   (ULONG)103

#pragma pack()
#endif                                  /* End of FMTD include.               */
