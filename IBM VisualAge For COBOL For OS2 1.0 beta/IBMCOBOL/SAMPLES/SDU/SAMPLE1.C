#pragma title ("SAMPLE1")
/******************************************************************************/
/*                                                                            */
/* PRODUCT   = Data Description and Conversion for OS/2                       */
/*                                                                            */
/* SOURCE FILE NAME = Sample1.C                                               */
/*                                                                            */
/* DESCRIPTIVE NAME = ADL Declaration Translator and CPB sample               */
/*                                                                            */
/* FUNCTION =  This sample program calls the parse function of the ADL        */
/*             declaration translator to compile ADL source text SAMPLE.ADL   */
/*             into the appropriate ADL declare and plan spaces, calls        */
/*             the generate function of the ADL declaration translator        */
/*             to reproduce the ADL source file SAMPLE.GEN. The               */
/*             parse function's output is also used to call                   */
/*             the conversion plan builder to create conversion plans         */
/*             from the encoded descriptions.                                 */
/*             The conversion plan builder output generated conversion plan   */
/*             space is stored in the file SAMPLE.SPC.                        */
/*                                                                            */
/* NOTES =                                                                    */
/*                                                                            */
/*   DEPENDENCIES = OS/2 Release 2.0 or later                                 */
/*                                                                            */
/*   RESTRICTIONS = None                                                      */
/*                                                                            */
/* ENTRY POINTS = main()                                                      */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
#pragma page ()
/******************************************************************************/
/* Header Files.                                                              */
/******************************************************************************/

/*---------------- OS/2 Base Header ------------------------------------------*/

#define INCL_BASE                      /* all of OS/2 Base                    */
#define INCL_NOPMAPI                   /* no presentation manager functions   */
#include <os2.h>

/*---------------- C Library Header ------------------------------------------*/

#include <stdio.h>
#include <memory.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#pragma page ()
/******************************************************************************/
/* DDC/2 global header file                                                   */
/******************************************************************************/
#define FMT_NO_LCF   FMT_NO_LCF        /* exclude the Low Level Conversion
                                          Functions function prototypes and
                                          their declarations                  */
#define FMT_NO_CPEX  FMT_NO_CPEX       /* exclude the Conversion Plan
                                          Executor function prototypes and
                                          their declarations                  */

#include  "FMT.H"

/******************************************************************************/
/* Define ADLDCLSPC and ADLPLNSPC buffer length                               */
/******************************************************************************/
#define  BUFLEN_ADLDCLSPC  64000
#define  BUFLEN_ADLPLNSPC  64000
#define  BUFLEN_CNVPLNSPC   5000


/******************************************************************************/
/*  MAIN function                                                             */
/******************************************************************************/
int main( )
{
  FMTCTOK     FeedBack;
  PBYTE       pAdlDclSpc;
  PBYTE       pAdlPlnSpc;
  PBYTE       *ppAdlDclSpcList = 0;
  PBYTE       *ppDefaultAdlPlnSpcList = 0;
  PVOID       pCnvPlnSpc = 0;
  FMTCNSTKN   Cnstkn;
  FMTADLCA    MyIsInfo;
  FILE        *CnvPlnSpcHandle;
  ULONG       ulSpcLen;

  /****************************************************************************/
  /* Get space for ADLDCLSPC and ADLPLNSPC.                                   */
  /****************************************************************************/
  pAdlDclSpc     = (PBYTE) malloc(BUFLEN_ADLDCLSPC);
  pAdlPlnSpc     = (PBYTE) malloc(BUFLEN_ADLPLNSPC);

  /****************************************************************************/
  /* Call PARSE function of ADL Declaration Translator for ADL source text    */
  /* to get ADLDCLSPC and ADLPLNSPC.                                          */
  /* Type Manager id is set to ADL.                                           */
  /* Note: Currently all CCSID's should be zero.                              */
  /****************************************************************************/

  FMTPRS (  ADLDECLTRANSLATOR,             // PBYTE    pbDclXlrId
            0,                             // FMTCCSID lParameterCCSID
            10,                            // LONG     lSrcFilNamLength
            "SAMPLE.ADL",                  // PCHAR    pchSrcFilNam
            0,                             // FMTCCSID lSrcFilCCSID
            8,                             // LONG     lDclXlrOptLength
            "AUTOSKIP",                    // PCHAR    pchDclXlrOpt
            4,                             // LONG     lLstOptLength
            "LIST",                        // PCHAR    pchLstOpt
            12,                            // LONG     lLstFilNamLength
            "SAMPLE_P.LST",                // PCHAR    pchLstFilNam
            BUFLEN_ADLDCLSPC,              // LONG     lADLDclSpcLength
            pAdlDclSpc,                    // PBYTE    pbADLDclSpc
            0,                             // FMTCCSID lADLDclSpcCCSID
            &Cnstkn,                       // PFMTCNSTKN pbADLDclSpcCNSTKN
            BUFLEN_ADLPLNSPC,              // LONG     lADLPlnSpcLength
            pAdlPlnSpc,                    // PBYTE    pbADLPlnSpc
            &FeedBack );                   // PFMTCTOK pFeedBack

  /****************************************************************************/
  /* Check the Condition Token                                                */
  /****************************************************************************/

  if ( FeedBack.Condition_ID.usMsgNo != PRS_NO_ERROR )
   {
     printf("Error in PARSE function.\n");
     printf("The Condition Token has the following contents:\n");

     printf("Message Severity %d Number %d\n", FeedBack.Condition_ID.usMsgSev,
                                               FeedBack.Condition_ID.usMsgNo);
     printf("Service Condition Case %d\n",     FeedBack.fCase);
     printf("Condition Severity     %d\n",     FeedBack.fSeverity);
     printf("Control                %d\n",     FeedBack.fControl);
     printf("Facility ID            %c%c%c\n", FeedBack.uchFacility_ID[0],
                                               FeedBack.uchFacility_ID[1],
                                               FeedBack.uchFacility_ID[2]);
     printf("Instance Specific      %d\n\n",   FeedBack.pI_S_Info.ulAdlExId);

   } /* endif */
  else
   {
     /*************************************************************************/
     /* Call GENERATE function of ADL Declaration Translator for ADLDCLSPC    */
     /* to get ADL Source text.                                               */
     /* This call is not necessary to create a conversion plan, it is mainly  */
     /* done for debugging of the PARSE function.                             */
     /* Type Manager id is set to ADL.                                        */
     /* Note: Currently all CCSID's should be zero.                           */
     /*************************************************************************/
     FMTGEN (  ADLDECLTRANSLATOR,             // PBYTE    pbDclXlrId
               0,                             // FMTCCSID lParameterCCSID
               0,                             // LONG     lDclXlrOptLength
               "",                            // PCHAR    pchDclXlrOpt
               pAdlDclSpc,                    // PBYTE    pbAdlSpc
               0,                             // FMTCCSID lAdlSpcCCSID
               10,                            // LONG     lSrcFilNamLength
               "SAMPLE.GEN",                  // PCHAR    pchSrcFilNam
               0,                             // FMTCCSID lSrcFilCCSID
               12,                            // LONG     lLstOptLength
               "LIST FLAG(I)",                // PCHAR    pchLstOpt
               12,                            // LONG     lLstFilNamLength
               "SAMPLE_G.LST",                // PCHAR    pchLstFilNam
               0,                             // FMTCCSID lLstFilCCSID
               &FeedBack);                    // PFMTCTOK pFeedback

     /*************************************************************************/
     /* Check the Condition Token                                             */
     /*************************************************************************/

     if ( FeedBack.Condition_ID.usMsgNo != GEN_NO_ERROR )
      {
        printf("Error in GENERATE function.\n");
        printf("The Condition Token has the following contents:\n");

        printf("Message Severity %d Number %d\n",FeedBack.Condition_ID.usMsgSev,
                                                 FeedBack.Condition_ID.usMsgNo);
        printf("Service Condition Case %d\n",    FeedBack.fCase);
        printf("Condition Severity     %d\n",    FeedBack.fSeverity);
        printf("Control                %d\n",    FeedBack.fControl);
        printf("Facility ID            %c%c%c\n",FeedBack.uchFacility_ID[0],
                                                 FeedBack.uchFacility_ID[1],
                                                 FeedBack.uchFacility_ID[2]);
        printf("Instance Specific      %d\n\n",  FeedBack.pI_S_Info.ulAdlExId);

      } /* endif */

     /*************************************************************************/
     /* Get space for conversion plan space (CNVPLNSPC )                      */
     /*************************************************************************/
     pCnvPlnSpc     = malloc( BUFLEN_CNVPLNSPC );
     memset( pCnvPlnSpc, '0', BUFLEN_CNVPLNSPC );

     /*************************************************************************/
     /* Initialize ADL Communication Area                                     */
     /*************************************************************************/
     FeedBack.pI_S_Info.pAdlCommArea = &MyIsInfo;

     /*************************************************************************/
     /* Call Conversion Plan Builder                                          */
     /*************************************************************************/
     ppAdlDclSpcList = malloc( sizeof( PBYTE ) );
     ppAdlDclSpcList[0] = pAdlDclSpc;

     ppDefaultAdlPlnSpcList = malloc( sizeof(PBYTE) );
     ppDefaultAdlPlnSpcList[0] = pAdlPlnSpc;

     FMTCRCP(
              1,                       // ULONG    ulAdlDclSpcCount
              ppAdlDclSpcList,         // PBYTE    *ppAdlDclSpcList
              0,                       // ULONG    ulUserAdlPlnSpcCount
              NULL,                    // PBYTE    *ppUserAdlPlnSpcList
              1,                       // ULONG    ulDefaultAdlPlnSpcCount
              ppDefaultAdlPlnSpcList,  // PBYTE    *ppDefaultAdlPlnSpcList
              BUFLEN_CNVPLNSPC,        // ULONG    ulCnvPlnSpcLength
              pCnvPlnSpc,              // PVOID    pCnvPlnSpc
              0,                       // ULONG    ulFlagList
              &FeedBack );             // PFMTCTOK pFeedback

     /*************************************************************************/
     /* Check the Condition Token                                             */
     /*************************************************************************/

     if ( FeedBack.Condition_ID.usMsgNo != CPB_NO_ERROR )
      {
        printf("Error in Conversion Plan Builder.\n");
        printf("The Condition Token has the following contents:\n");

        printf("Message Severity %d Number %d\n",FeedBack.Condition_ID.usMsgSev,
                                                 FeedBack.Condition_ID.usMsgNo);
        printf("Service Condition Case %d\n",    FeedBack.fCase);
        printf("Condition Severity     %d\n",    FeedBack.fSeverity);
        printf("Control                %d\n",    FeedBack.fControl);
        printf("Facility ID            %c%c%c\n",FeedBack.uchFacility_ID[0],
                                                 FeedBack.uchFacility_ID[1],
                                                 FeedBack.uchFacility_ID[2]);

        /**********************************************************************/
        /* Check whether an ADL exception occurred. If ADL exception the ADL  */
        /* communication area is filled.                                      */
        /**********************************************************************/
        if ( FeedBack.Condition_ID.usMsgNo == CPB_ADL_EXCEPTION_SEV2 ||
             FeedBack.Condition_ID.usMsgNo == CPB_ADL_EXCEPTION_SEV3 )
         {
           printf("The ADL communication area has the following contents:\n" );

           printf("ADL exception:             %d\n", MyIsInfo.lExId );
           printf("Severity of ADL exception: %d\n", MyIsInfo.usSevCod );
                                          /* The Severity of the ADL          */
                                          /* exception has the same value as  */
                                          /* the message severity             */
                                          /* ( Feedback.Condition_ID.usMsgSev */

           printf("Name of processed plan:    %.255s\n",
                                                     MyIsInfo.PlanId.uchData );
           printf("Number of processed PLAN statement: %d\n",
                                                     MyIsInfo.lPlanStmt );
           printf("Source identifier of processed assignment statement: %.255s\n"
                  , MyIsInfo.SrcFldId.uchData );
           printf("Target identifier of processed assignment statement: %.255s\n"
                  , MyIsInfo.TrgFldId.uchData );


         } /* endif */

      } /* endif */
     else
      {
        /**********************************************************************/
        /* Write conversion plan space into file                              */
        /**********************************************************************/
        CnvPlnSpcHandle = fopen( "SAMPLE.SPC","wb");

        ulSpcLen = *((PULONG)pCnvPlnSpc);  /* Get length of the space out of  */
                                           /* the first 4 Byte                */

        fwrite( pCnvPlnSpc, sizeof(CHAR), ulSpcLen , CnvPlnSpcHandle );
        fclose( CnvPlnSpcHandle );

      } /* endelse */

   } /* endelse */

  /****************************************************************************/
  /* Free allocated resources                                                 */
  /****************************************************************************/
  if ( pAdlDclSpc != NULL ) {
    free( pAdlDclSpc );
  } /* endif */

  if ( pAdlPlnSpc != NULL ) {
    free( pAdlPlnSpc );
  } /* endif */

  if ( pCnvPlnSpc != NULL ) {
    free( pCnvPlnSpc );
  } /* endif */

  if ( ppAdlDclSpcList != NULL) {
    free( ppAdlDclSpcList );
  } /* endif */

  if ( ppDefaultAdlPlnSpcList != NULL ) {
    free( ppDefaultAdlPlnSpcList );
  } /* endif */

  return 0;
}
