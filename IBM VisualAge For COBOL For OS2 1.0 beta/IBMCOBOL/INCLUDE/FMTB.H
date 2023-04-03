 #if !defined(FMTB)                     /* If this include hasn't been
                                           included before, include it.       */
 #define FMTB FMTB                      /* Define the term so this include
                                           will not be included again.        */
/******************************************************************************/
// $Header:   /df/vc/0/1/6/2/s.50   1.2   31 Mar 1995 11:08:08   df  $
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
/* SOURCE FILE NAME = FMTB.H                                                  */
/*                                                                            */
/* DESCRIPTIVE NAME = Function Prototypes for Parse and Generate functions.   */
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
// $Log:   /df/vc/0/1/6/2/s.50  $
//
//   Rev 1.11   07 Apr 1994 09:28:38   WIC
//Fields of CNSTKN structure renamed.
//
//   Rev 1.10   04 Feb 1994 14:23:58   YSTEFAND
//new return codes for FMTGEN, list option handling changed
//
//   Rev 1.9   10 Dec 1993 13:03:34   WIC
//Pragma pack statements added.
//
//   Rev 1.8   03 Dec 1993 10:28:04   YSTEFAND
//new generate return values added
//
//   Rev 1.7   24 Nov 1993 11:30:02   WIC
//Define for CCSID replaced by FMTCCSID.
//
//   Rev 1.6   10 Nov 1993 11:27:50   WIC
//CNSTKN parameter for parse changed, new error codes for parse.
//
//   Rev 1.5   05 Nov 1993 14:54:48   WIC
//PRS_* return codes added and changed.
//
//   Rev 1.4   02 Nov 1993 14:12:40   WIC
//parser defines now PRS_* instead of XLR_*.
//
//   Rev 1.3   02 Nov 1993 14:10:26   YSTEFAND
//return code defines for generate function added
//
//   Rev 1.2   22 Oct 1993 15:00:34   WIC
//Return Codes for parser changed.
//
//   Rev 1.1   20 Oct 1993 14:34:42   WIC
//Return codes for parser added.
//
//   Rev 1.0   11 Oct 1993 16:15:48   WIC
//Initial Revision.
/******************************************************************************/


#pragma pack(1)

#define   FMTCCSID         LONG

typedef struct _FMTCNSTKN  {
   ULONG                   ulLength;
   USHORT                  usClass;
   BYTE                    abValue[16];
} FMTCNSTKN, * PFMTCNSTKN;

#define LENGTH_FMTCNSTKN            (sizeof ( FMTCNSTKN ))

#define ADLDECLTRANSLATOR   "\x2B\x12\x00\x03\x01\x88\x6D\x01"
#define LENGTH_ADLDECLTR    8


/*    Generate Function Prototype API */

VOID APIENTRY
FMTGEN( PBYTE        pbDclXlrId,
        FMTCCSID     lParameterCCSID,
        LONG         lDclXlrOptLength,
        PCHAR        pchDclXlrOpt,
        PBYTE        pbAdlSpc,
        FMTCCSID     lAdlSpcCCSID,
        LONG         lSrcFilNamLength,
        PCHAR        pchSrcFilNam,
        FMTCCSID     lSrcFilCCSID,
        LONG         lLstOptLength,
        PCHAR        pchLstOpt,
        LONG         lLstFilNamLength,
        PCHAR        pchLstFilNam,
        FMTCCSID     lLstFilCCSID,
        PFMTCTOK     pFeedBack );


/*    ADL Parse Function Prototype API */

VOID APIENTRY
FMTPRS( PBYTE        pbDclXlrId,
        FMTCCSID     lParameterCCSID,
        LONG         lSrcFilNamLength,
        PCHAR        pchSrcFilNam,
        FMTCCSID     lSrcFilCCSID,
        LONG         lDclXlrOptLength,
        PCHAR        pchDclXlrOpt,
        LONG         lLstOptLength,
        PCHAR        pchLstOpt,
        LONG         lLstFilNamLength,
        PCHAR        pchLstFilNam,
        LONG         lADLDclSpcLength,
        PBYTE        pbADLDclSpc,
        FMTCCSID     lADLDclSpcCCSID,
        PFMTCNSTKN   pADLDclSpcCNSTKN,
        LONG         lADLPlnSpcLength,
        PBYTE        pbADLPlnSpc,
        PFMTCTOK     pFeedBack );


/* return codes parser */

#define     PRS_NO_ERROR                     0
#define     PRS_ERROR                        1
#define     PRS_ERR_INTERNAL                 2
#define     PRS_ERR_LIST_OPEN                3
#define     PRS_ERR_LIST_WRITE               4
#define     PRS_ERR_DCLSPC                   5
#define     PRS_ERR_PLNSPC                   6
#define     PRS_ERR_DCLPLNSPC                7
#define     PRS_ERR_XLRID                    8
#define     PRS_ERR_CNSTKN                   9
#define     PRS_ERR_LSTOPT                   10
#define     PRS_ERR_XLROPT                   11
#define     PRS_ERR_SOURCE                   12
#define     PRS_ERR_INV_LSTOPT               13
#define     PRS_ERR_INV_XLROPT               14

/* return codes generator */

#define     GEN_NO_ERROR                     0
#define     GEN_WARNING                      1
#define     GEN_ERROR                        2
#define     GEN_SEV_ERROR                    3
#define     GEN_ERR_INTERNAL                 4
#define     GEN_ERR_MEMORY                   5
#define     GEN_ERR_LSTOPT                   6
#define     GEN_ERR_LISTFIL                  7
#define     GEN_ERR_LIST_NLEN                8
#define     GEN_ERR_LIST_OPEN                9
#define     GEN_ERR_LIST_WRITE              10
#define     GEN_ERR_SRCFIL                  11
#define     GEN_ERR_SRC_NLEN                12
#define     GEN_ERR_SRC_OPEN                13
#define     GEN_ERR_SRC_WRITE               14
#define     GEN_ERR_INV_XLRID               15
#define     GEN_ERR_PTR_ADLSPC              16
#define     GEN_ERR_PTR_LSTOPT              17
#define     GEN_ERR_LSTOPT_LEN              18

#pragma pack()

#endif                                 /* End of FMTB include.           */
