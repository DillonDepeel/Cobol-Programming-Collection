 #if !defined(FMT)                     /* If this include hasn't been
                                          included before, include it.       */
 #define FMT FMT                       /* Define the term so this include
                                          will not be included again.        */
/******************************************************************************/
// $Header:   /df/vc/0/1/6/2/s.49   1.2   31 Mar 1995 10:44:38   df  $
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
/* SOURCE FILE NAME = FMT.H                                                   */
/*                                                                            */
/* DESCRIPTIVE NAME = DDC/2 main include file                                 */
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
// $Log:   /df/vc/0/1/6/2/s.49  $
//
//   Rev 1.17   13 Apr 1994 10:57:46   MQT
//remove the enum within the ADLCA.
//
//   Rev 1.16   16 Feb 1994 12:45:20   MQT
//
//
//   Rev 1.15   16 Feb 1994 11:26:42   MQT
//Change CTOK structure.
//
//   Rev 1.14   19 Jan 1994 13:25:46   MQT
//
//
//   Rev 1.13   19 Jan 1994 10:45:14   MQT
//Remove pI_S_Info union.
//
//   Rev 1.12   10 Dec 1993 13:16:12   MQT
//Add pragma pack(1) statement.
//
//   Rev 1.11   07 Dec 1993 10:49:40   VIELFOR
//
//
//   Rev 1.10   07 Dec 1993 10:37:08   VIELFOR
//new ADL Exceptions of DCR02 inserted
//
//   Rev 1.9   01 Dec 1993 08:31:36   MQT
//Add reduced_source flag define.
//
//   Rev 1.8   05 Nov 1993 09:28:02   VIELFOR
//
//
//   Rev 1.7   29 Sep 1993 16:38:14   BMB
//
//
//   Rev 1.6   29 Sep 1993 10:53:38   BMB
//ADL Exceptions included
//
//   Rev 1.5   06 Sep 1993 09:19:36   MQT
//
//
//   Rev 1.4   03 Sep 1993 11:37:24   MQT
//
//
//   Rev 1.3   13 Aug 1993 15:36:18   MQT
//First compilable version
//
//   Rev 1.2   26 Jul 1993 17:32:08   MQT
//First compilable version.
//
//   Rev 1.1   20 Jul 1993 13:03:28   MQT
//remove compile error.
//
//   Rev 1.0   08 Jul 1993 16:35:42   MQT
//Initial Revision<
//$A1=          950512,SJPLAC, Disabled Utility function
/******************************************************************************/

#pragma pack(1)

typedef struct _CHARPRE
{
  USHORT usLength;
  UCHAR  uchData[255];
} CHARPRE;

enum { FMT_INFORMATION,
       FMT_WARNING,
       FMT_ERROR,
       FMT_SEVERE,
       FMT_CRITICAL };


typedef struct _FMTADLCA
{
   LONG          lLength;
   LONG          lExId;
   USHORT        usSevCod;

// enum { INFORMATION,
//        WARNING,
//        ERROR,
//        SEVERE,
//        CRITICAL} SevCod;
   CHARPRE       PlanId;
   LONG          lPlanStmt;
   CHARPRE       InpErrDta;
   CHARPRE       SrcFldId;
   CHARPRE       TrgFldId;
} FMTADLCA, *PFMTADLCA;

typedef struct _FMTCTOK
{
   struct
   {
      USHORT usMsgSev;
      USHORT usMsgNo;
   }  Condition_ID;
   UINT   fCase    :2;
   UINT   fSeverity:3;
   UINT   fControl :3;
   UCHAR  uchFacility_ID[3];
   union
   {
     ULONG             ulAdlExId;
     FMTADLCA          *pAdlCommArea;
     struct _FMTCTOK   *pUserExitCtok;
   } pI_S_Info;
} FMTCTOK, *PFMTCTOK;

/******************************************************************************/
/* ADL Exceptions                                                             */
/******************************************************************************/
#define ADL_CONV_NOT_SUPPORTED        (LONG)1
#define ADL_CCSID_NOT_SUPPORTED       (LONG)2
#define ADL_INVALID_CCSID_PAIR        (LONG)3
#define ADL_UNDEFINED_CCSID           (LONG)4
#define ADL_FLOAT_OVERFLOW            (LONG)5
#define ADL_CASE_FAILURE              (LONG)6
#define ADL_INVALID_WHEN_CLAUSE       (LONG)7
#define ADL_NONCONFORM_ARRAYS         (LONG)8
#define ADL_ENUM_MISMATCH             (LONG)9
#define ADL_INVALID_ENUM_VALUE        (LONG)10
#define ADL_FIX_OVERFLOW              (LONG)11
#define ADL_NEG_TO_UNSIGNED           (LONG)12
#define ADL_FLOAT_UNDERFLOW           (LONG)13
#define ADL_NOT_A_NUMBER              (LONG)14
#define ADL_INFINITY                  (LONG)15
#define ADL_INPUT_AREA_TOO_SHORT      (LONG)16
#define ADL_OUTPUT_AREA_TOO_SHORT     (LONG)17
#define ADL_COMPLEX_TO_SCALAR         (LONG)18
#define ADL_FLOAT_FIT_VIOLATION       (LONG)19
#define ADL_CASE_REJECTED             (LONG)20
#define ADL_FIX_CONSTRAINED_VIOLATION (LONG)21
#define ADL_FIX_FIT_VIOLATION         (LONG)22
#define ADL_SEQ_ELEMENT_NOT_FOUND     (LONG)23
#define ADL_TARGET_CASE_MISMATCH      (LONG)24
#define ADL_NEG_ARRAY_SIZE            (LONG)25
#define ADL_INVALID_ARRAY_SIZE        (LONG)26
#define ADL_INV_IN_LENGTH             (LONG)27
#define ADL_INV_IN_HIGH_LOW           (LONG)28
#define ADL_INV_OUT_HIGH_LOW          (LONG)29
#define ADL_IN_NO_SFX                 (LONG)30
#define ADL_OUT_MULT_SFX              (LONG)31
#define ADL_IN_ORPHAN_BYTE            (LONG)32
#define ADL_OUT_ORPHAN_BYTE           (LONG)33
#define ADL_INV_IN_STRING             (LONG)34
#define ADL_INV_PK_DIGIT              (LONG)35
#define ADL_INV_PK_SIGN               (LONG)36
#define ADL_INV_ZN_DIGIT              (LONG)37
#define ADL_INV_ZN_SIGN               (LONG)38
#define ADL_INV_ZN_ZONE               (LONG)39

/******************************************************************************/
/* Common API Parameter Flag define                                           */
/******************************************************************************/
#define DDC_REDUCED_SOURCE             0x0001L




#pragma pack()
/******************************************************************************/
/* include files for the Declaration Translator and Generate function         */
/******************************************************************************/
#if !defined(FMT_NO_DCLXLRIFC)
#include "FMTB.H"
#endif

/******************************************************************************/
/* include files for the Conversion Plan Builder                              */
/******************************************************************************/
#if !defined(FMT_NO_CPB)
#include "FMTC.H"
#endif

/******************************************************************************/
/* include files for the Conversion Plan Executor                             */
/******************************************************************************/
#if !defined(FMT_NO_CPEX)
#include "FMTD.H"
#endif

/******************************************************************************/
/* include files for the Low Level Conversion Functions                       */
/******************************************************************************/
#if !defined(FMT_NO_LCF)
#include "FMTF.H"
#endif





                                                                                                 /* 3@A1D */

/******************************************************************************/

#endif                                 /* End of FMT include.                 */
