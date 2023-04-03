/*  $Header:   /df/vc/0/2/1/5/s.24   1.1   26 Aug 1994 16:54:38   df  $ */
/*  $Log:   /df/vc/0/2/1/5/s.24  $ */
/*  */
/*    Rev 1.2   26 May 1994 14:13:14   MTN */
/* $I= I0000307   MTN   : Changes made for port to MVS. */
/*  */
/*    Rev 1.1   11 Aug 1993 13:43:26   MTN */
/* $I= I0000265   MTN   : CDRASRV enabling to allow different naming of the */
/*                        DLL and config. file and resources. The names of these */
/*                        files are derived from the name of the DLL. */
/*                        CONFIG:  dllname.CFG */
/*                        CCSID:   dllnameC */
/*                        GCCST:   dllnameG */
/*  */
/*    Rev 1.0   28 Jul 1993 14:13:36   MTN */
/* Initial revision. */
/*  $Workfile:   CDRASRV.H, cdra/2_cdrasrv, cdra2_release1  $ */

#ifndef CDRASRV_INCLUDED
#define CDRASRV_INCLUDED

#if defined(__OS2__) && defined(__32BIT__) && !defined(OS2)
#define OS2
#endif

typedef long CDRASRV_CCSID_T;
typedef long CDRASRV_CPGID_T;
typedef long CDRASRV_ESIDA_T;
typedef long CDRASRV_GCSGID_T;

typedef struct {         /* structure used to return a return code */
  short Status;
  short Reason;
  long reserved1;
  long reserved2;
} CDRASRV_FeedBack_T;


typedef struct {
  CDRASRV_ESIDA_T ESID;  /* entire Encoding Scheme ID */
  long BasicEncoding;    /* parts                     */
  long NumberOfBytes;    /* of                        */
  long CodeExtension;    /* the ESID                  */
} CDRASRV_ESID_T;

typedef struct {
  CDRASRV_GCSGID_T GCSGID;  /* Character Set ID          */
  CDRASRV_CPGID_T  CPGID;   /* Code Page ID              */
} CDRASRV_CSCPpair_T;       /* CGCSGID - Coded Graphic
                               Character Set Global
                               Identifiers               */


typedef struct {
  long CodePoint;
  long Width;
  long State;
} CDRASRV_CTLFDF_T;

#define CDRASRV_ESID_BasEncStrEBCDIC              1
#define CDRASRV_ESID_BasEncStrIBM_PC_Data         2
#define CDRASRV_ESID_BasEncStrIBM_PC_Display      3
#define CDRASRV_ESID_BasEncStrISO_8               4
#define CDRASRV_ESID_BasEncStrISO_7               5
#define CDRASRV_ESID_NrOfBytes_SB                 1
#define CDRASRV_ESID_NrOfBytes_DB                 2
#define CDRASRV_ESID_NrOfBytes_MixedSBDB          3
#define CDRASRV_ESID_ExtMtd_None               0X00
#define CDRASRV_ESID_ExtMtd_SOSI               0X01


#define CDRASRV_ST_NoError             0
#define CDRASRV_ST_NotFound            1
#define CDRASRV_ST_Value0              2
#define CDRASRV_ST_SpecialValue        3
#define CDRASRV_ST_Overflow            4
#define CDRASRV_ST_Syntax              5
#define CDRASRV_ST_CantProceed         6
#define CDRASRV_ST_ResourceDamaged     7
#define CDRASRV_ST_ParameterRangeError 8
#define CDRASRV_ST_ConvertSpecific     0x100
#define CDRASRV_ST_OS2Specific         0x800

#define CDRASRV_RSN_EntryExit          1
#define CDRASRV_RSN_Init               2
#define CDRASRV_RSN_TraceError         3
        

/* Name of Configuration File */

#ifdef OS2
#define    CDRASRV_Entry  _Optlink
#endif

#ifdef AIX
#define    CDRASRV_Entry  
#endif

#ifdef MVS
#define    CDRASRV_Entry  
#endif

#ifdef OS400
#define    CDRASRV_Entry  
#endif

typedef long CDRASRV_MSC_Token_T[8];

void CDRASRV_Entry CDRXSRF(const CDRASRV_FeedBack_T * InputFeedBack,
                           long                     * Status,
                           long                     * Reason,
                           CDRASRV_FeedBack_T       * FeedBack);

void CDRASRV_Entry CDRGESE(const CDRASRV_CCSID_T * ccsid,
                           CDRASRV_ESID_T        * esel,
                           CDRASRV_FeedBack_T    * FeedBack);

void CDRASRV_Entry CDRGCSP(const CDRASRV_CCSID_T * ccsid,
                           const long            * n1,
                           long                  * n2,
                           long                  * cscpl,
                           CDRASRV_FeedBack_T    * FeedBack );
 
void CDRASRV_Entry CDRGESP(const CDRASRV_CCSID_T * ccsid,
                           const long            * n1,
                           long                  * n2,
                           CDRASRV_ESIDA_T       * esid,
                           long                  * cscpl,
                           CDRASRV_FeedBack_T    * FeedBack);

void CDRASRV_Entry CDRGCTL(const CDRASRV_CCSID_T * ccsid,
                           const long            * Sel,
                           const long            * n1,
                           long                  * n2,
                           long                  * CtlFdF,
                           CDRASRV_FeedBack_T    * FeedBack);

void CDRASRV_Entry CDRGSTE(const CDRASRV_CCSID_T * ccsid1,
                           const long            * Selection,
                           const CDRASRV_CCSID_T * ccsid2,
                           const long            * n1,
                           long                  * n2,
                           CDRASRV_CCSID_T       * ccsid3,
                           char                  * String,
                           CDRASRV_FeedBack_T    * FeedBack);

void CDRASRV_Entry CDRSMXC(const CDRASRV_CPGID_T * cp,
                           const CDRASRV_ESIDA_T * es,
                           CDRASRV_CCSID_T       * ccsid,
                           CDRASRV_ESIDA_T       * esr,
                           CDRASRV_FeedBack_T    * FeedBack);

void CDRASRV_Entry CDRSCSP(const long               * cscpl,
                           const long               * n1,
                           const CDRASRV_ESIDA_T    * esin,
                           CDRASRV_CCSID_T          * ccsidr,
                           CDRASRV_ESIDA_T          * esr,
                           CDRASRV_FeedBack_T       * FeedBack);

void CDRASRV_Entry CDRCVRT(const CDRASRV_CCSID_T * ccsid1,
                           const long            * StringType1,
                           const char            * String1,
                           const long            * Length1,
                           const CDRASRV_CCSID_T * ccsid2,
                           const long            * StringType2,
                           const long            * gccasn,
                           const long            * Length2,
                           char                  * String2,
                           long                  * OutLength,
                           long                  * ErrorByteNr,
                           CDRASRV_FeedBack_T    * FeedBack);

void CDRASRV_Entry CDRMSCI(const CDRASRV_CCSID_T  * ccsid1,
                           const long             * StringType1,
                           const CDRASRV_CCSID_T  * ccsid2,
                           const long             * StringType2,
                           const long             * gccasn,
                           long                   * Token,
                           CDRASRV_FeedBack_T     * FeedBack);

void CDRASRV_Entry CDRMSCP(const long                * Token,
                           const char                * String1,
                           const long                * Length1,
                           const long                * Length2,
                           char                      * String2,
                           long                      * OutLength,
                           long                      * ErrorByteNr,
                           CDRASRV_FeedBack_T        * FeedBack);

void CDRASRV_Entry CDRMSCC( long              * Token,
                           CDRASRV_FeedBack_T * FeedBack);
 
#endif
