#ifndef __FMTF_H                       /* If this include hasn't been
                                           included before, include it.       */
#define __FMTF_H                       /* Define the term so this include
                                           will not be included again.        */
/******************************************************************************/
// $Header:   /df/vc/0/1/6/3/s.43   1.2   31 Mar 1995 11:17:02   df  $
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
/* SOURCE FILE NAME = FMTF.H                                                  */
/*                                                                            */
/* DESCRIPTIVE NAME = definitions for numeric conversion routines             */
/*                                                                            */
/* STATUS = VERSION 1, RELEASE 1, MODIFICATION 0                              */
/*                                                                            */
/* CONTENT = ADL defined constants,                                           */
/*           message numbers,                                                 */
/*           attribute structures,                                            */
/*           function prototypes of numeric conversions                       */
/*                                                                            */
/******************************************************************************/
//
// Change Activity
// ===============
//
//   Revision  Date        Time       Responsible
//   ========  ====        ====       ===========
//Reason
// $Log:   /df/vc/0/1/6/3/s.43  $
//
//   Rev 1.4   21 Mar 1994 16:13:48   VIELFOR
//start Message Numbers for numeric conversion routines at 50
//
//   Rev 1.3   14 Dec 1993 14:35:22   VIELFOR
//
//
//   Rev 1.2   14 Dec 1993 13:19:44   VIELFOR
//message numbers changed
//
//   Rev 1.1   07 Dec 1993 13:06:18   VIELFOR
//
//
//   Rev 1.0   27 Oct 1993 15:47:36   VIELFOR
//
//
//   Rev 1.1   08 Oct 1993 12:37:32   VIELFOR
//
//
//   Rev 1.0   27 Jul 1993 16:03:32   VIELFOR
//
/******************************************************************************/

/******************************************************************************/
/* ADL defined constants                                                      */
/******************************************************************************/

enum { LSTBIT };                       /* constant for BLNENC                 */

enum { ROUND,                          /* constants for FIT attribute         */
       TRUNCATE,
       EXACT };

enum { LEFT,                           /* constants for JUSTIFY attribute     */
       RIGHT };

enum { DGTLSTBYT,                      /* constants for SGNLOC attribute      */
       ZONFRSBYT,
       ZONLSTBYT,
       FRSBYT,
       LSTBYT };

enum { FB32,                           /* constants for FORM attribute        */
       FB64,
       FB80,
       FH32,
       FH64,
       FH128,
       FI128 };

enum { ALGEBRAIC,                      /* constants for SGNCNV attribute      */
       LOGICAL };

enum { ASTERISK=-1 };


/******************************************************************************/
/* Mnemonics for message numbers                                              */
/******************************************************************************/
enum {
   LCF_NO_ERROR = 0,                   /* no error                            */
   LCF_INV_IN_FORM = 50,               /* invalid attributes of input field   */
   LCF_INV_IN_LENGTH,
   LCF_INV_IN_PRECISION,
   LCF_INV_IN_RADIX,
   LCF_INV_IN_SCALE,
   LCF_INV_IN_SGNLOC,
   LCF_INV_IN_ZONENC,
   LCF_INV_IN_LENGTH_BYTRVS,
   LCF_INV_OUT_FIT,                    /* invalid attributes of output field  */
   LCF_INV_OUT_FORM,
   LCF_INV_OUT_LENGTH,
   LCF_INV_OUT_PRECISION,
   LCF_INV_OUT_RADIX,
   LCF_INV_OUT_SCALE,
   LCF_INV_OUT_SGNCNV,
   LCF_INV_OUT_SGNLOC,
   LCF_INV_OUT_ZONENC,
   LCF_INV_OUT_LENGTH_BYTRVS,
   LCF_ADL_EXCEPTION_SEV1 = 101,       /* ADL exceptions severity 1           */
   LCF_ADL_EXCEPTION_SEV2,             /* ADL exceptions severity 2           */
   LCF_ADL_EXCEPTION_SEV3              /* ADL exceptions severity 3           */
};


#ifndef FMT_ADL_ATTR_STRUCT
/******************************************************************************/
/* ADL Attribute Unions                                                       */
/******************************************************************************/

typedef _Packed union                  /* attribute union for BINARY          */
{
   LONG BNAttrVector[10];
   struct
   {
      LONG lBYTRVS;
      LONG lCOMPLEX;
      LONG lCONSTRAINED;
      LONG lFIT;
      LONG lLENGTH;
      LONG lPRECISION;
      LONG lRADIX;
      LONG lSCALE;
      LONG lSGNCNV;
      LONG lSIGNED;
   } BNAttrRecord;
} BNATTR, *PBNATTR;

typedef _Packed union                  /* attribute union for FLOAT           */
{
   LONG FLAttrVector[6];
   struct
   {
      LONG lBYTRVS;
      LONG lCOMPLEX;
      LONG lFIT;
      LONG lFORM;
      LONG lPRECISION;
      LONG lRADIX;
   } FLAttrRecord;
} FLATTR, *PFLATTR;

typedef _Packed union                  /* attribute union for PACKED          */
{
   LONG PKAttrVector[10];
   struct
   {
      LONG lCOMPLEX;
      LONG lCONSTRAINED;
      LONG lFIT;
      LONG lPRECISION;
      LONG lSCALE;
      LONG lSGNLOC;
      LONG lSGNMNS;
      LONG lSGNPLS;
      LONG lSGNUNS;
      LONG lSIGNED;
   } PKAttrRecord;
} PKATTR, *PPKATTR;

typedef _Packed union                  /* attribute union for ZONED           */
{
   LONG ZNAttrVector[11];
   struct
   {
      LONG lCCSID;
      LONG lCOMPLEX;
      LONG lCONSTRAINED;
      LONG lFIT;
      LONG lPRECISION;
      LONG lSCALE;
      LONG lSGNLOC;
      LONG lSGNMNS;
      LONG lSGNPLS;
      LONG lSIGNED;
      LONG lZONENC;
   } ZNAttrRecord;
} ZNATTR, *PZNATTR;

#endif


/******************************************************************************/
/* function prototypes for numeric conversions                                */
/******************************************************************************/

VOID APIENTRY FMTBNBN( PBNATTR pInBNAttr,
                       PVOID pInBNFld,
                       PBNATTR pOutBNAttr,
                       PVOID pOutBNFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTBNFL( PBNATTR pInBNAttr,
                       PVOID pInBNFld,
                       PFLATTR pOutFLAttr,
                       PVOID pOutFLFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTBNPK( PBNATTR pInBNAttr,
                       PVOID pInBNFld,
                       PPKATTR pOutPKAttr,
                       PVOID pOutPKFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTBNZN( PBNATTR pInBNAttr,
                       PVOID pInBNFld,
                       PZNATTR pOutZNAttr,
                       PVOID pOutZNFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTFLBN( PFLATTR pInFLAttr,
                       PVOID pInFLFld,
                       PBNATTR pOutBNAttr,
                       PVOID pOutBNFld,
                       PFMTCTOK pFeedback);


VOID APIENTRY FMTFLFL( PFLATTR pInFLAttr,
                       PVOID pInFLFld,
                       PFLATTR pOutFLAttr,
                       PVOID pOutFLFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTFLPK( PFLATTR pInFLAttr,
                       PVOID pInFLFld,
                       PPKATTR pOutPKAttr,
                       PVOID pOutPKFld,
                       PFMTCTOK pFeedback);


VOID APIENTRY FMTFLZN( PFLATTR pInFLAttr,
                       PVOID pInFLFld,
                       PZNATTR pOutZNAttr,
                       PVOID pOutZNFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTPKBN( PPKATTR pInPKAttr,
                       PVOID pInPKFld,
                       PBNATTR pOutBNAttr,
                       PVOID pOutBNFld,
                       PFMTCTOK pFeedback);


VOID APIENTRY FMTPKFL( PPKATTR pInPKAttr,
                       PVOID pInPKFld,
                       PFLATTR pOutFLAttr,
                       PVOID pOutFLFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTPKPK( PPKATTR pInPKAttr,
                       PVOID pInPKFld,
                       PPKATTR pOutPKAttr,
                       PVOID pOutPKFld,
                       PFMTCTOK pFeedback);


VOID APIENTRY FMTPKZN( PPKATTR pInPKAttr,
                       PVOID pInPKFld,
                       PZNATTR pOutZNAttr,
                       PVOID pOutZNFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTZNBN( PZNATTR pInZNAttr,
                       PVOID pInZNFld,
                       PBNATTR pOutBNAttr,
                       PVOID pOutBNFld,
                       PFMTCTOK pFeedback);


VOID APIENTRY FMTZNFL( PZNATTR pInZNAttr,
                       PVOID pInZNFld,
                       PFLATTR pOutFLAttr,
                       PVOID pOutFLFld,
                       PFMTCTOK pFeedback);

VOID APIENTRY FMTZNPK( PZNATTR pInZNAttr,
                       PVOID pInZNFld,
                       PPKATTR pOutPKAttr,
                       PVOID pOutPKFld,
                       PFMTCTOK pFeedback);


VOID APIENTRY FMTZNZN( PZNATTR pInZNAttr,
                       PVOID pInZNFld,
                       PZNATTR pOutZNAttr,
                       PVOID pOutZNFld,
                       PFMTCTOK pFeedback);

#endif
