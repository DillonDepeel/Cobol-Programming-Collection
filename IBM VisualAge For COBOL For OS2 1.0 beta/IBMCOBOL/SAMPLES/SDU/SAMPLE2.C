#pragma title ("SAMPLE2")
/******************************************************************************/
/*                                                                            */
/* PRODUCT   = Data Description and Conversion for OS/2                       */
/*                                                                            */
/* SOURCE FILE NAME = Sample2.C                                               */
/*                                                                            */
/* DESCRIPTIVE NAME = Conversion Plan Executor sample                         */
/*                                                                            */
/* FUNCTION =  This sample program calls the functions of the                 */
/*             conversion plan executor to convert data based on the          */
/*             conversion plans created by the conversion plan builder        */
/*             in program SAMPLE1.                                            */
/*             In this sample the hex string C1C2C301234C will be converted   */
/*             with specified plan COBOL_TO_C into the hex string             */
/*             414243000D204. Then the hex string 414243000D204 will be       */
/*             converted with the specified plan C_TO_COBOL into the hex      */
/*             string C1C2C301234C. The result will be printed on screen.     */
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
#include <io.h>

#pragma page ()
/******************************************************************************/
/* DDC/2 global header file                                                   */
/******************************************************************************/
#define FMT_NO_DCLXLRIFC FMT_NO_DCLXLRIFC
                                       /* exclude the Declaration Translator
                                          and Generate function prototypes and
                                          their declarations                  */
#define FMT_NO_CPB   FMT_NO_CPB        /* exclude the Conversion Plan Builder
                                          function prototypes and
                                          their declarations                  */
#define FMT_NO_LCF   FMT_NO_LCF        /* exclude the Low Level Conversion
                                          Functions function prototypes and
                                          their declarations                  */

#include  "FMT.H"

/******************************************************************************/
/* Define length of input and output buffer                                   */
/******************************************************************************/
#define  BUFFER_LENGTH  6

/******************************************************************************/
/* Enumeration to identify the appropriate CPEX function                      */
/******************************************************************************/
enum Execute { INIT,
               CONVERT,
               TERM
};

/******************************************************************************/
/*                                                                            */
/* The function PrintCtok prints the condition token and the ADL              */
/* communication area after an error occured in a conversion plan executor    */
/* function.                                                                  */
/*                                                                            */
/******************************************************************************/
void PrintCtok( PFMTCTOK  pFeedBack, PFMTADLCA pMyIsInfo, enum Execute Type )
{

  switch ( Type )
    {
      case INIT    : printf("Error in Conversion Plan Executor Init.\n");
        break;
      case CONVERT : printf("Error in Conversion Plan Executor Convert.\n");
        break;
      case TERM    : printf("Error in Conversion Plan Executor Term.\n");
        break;
    } /* endswitch */

  printf("The Condition Token has the following contents:\n");

  printf("Message Severity %d Number %d\n",pFeedBack->Condition_ID.usMsgSev,
                                           pFeedBack->Condition_ID.usMsgNo);
  printf("Service Condition Case %d\n",    pFeedBack->fCase);
  printf("Condition Severity     %d\n",    pFeedBack->fSeverity);
  printf("Control                %d\n",    pFeedBack->fControl);
  printf("Facility ID            %c%c%c\n",pFeedBack->uchFacility_ID[0],
                                           pFeedBack->uchFacility_ID[1],
                                           pFeedBack->uchFacility_ID[2]);

  /****************************************************************************/
  /* Check whether an ADL exception occurred.                                 */
  /****************************************************************************/
  if ( pFeedBack->Condition_ID.usMsgNo == CPX_ADL_EXCEPTION_SEV2 ||
       pFeedBack->Condition_ID.usMsgNo == CPX_ADL_EXCEPTION_SEV3 )
   {
     if ( Type == INIT )
      {
        printf("ADL exception         %d\n", pFeedBack->pI_S_Info.ulAdlExId );
      } /* endif */
     else
      {
        printf("The ADL communication area has the following contents:\n" );

        printf("ADL exception:             %d\n", pMyIsInfo->lExId );
        printf("Severity of ADL exception: %d\n", pMyIsInfo->usSevCod );
                                       /* The Severity of the ADL          */
                                       /* exception has the same value as  */
                                       /* the message severity             */
                                       /* ( Feedback.Condition_ID.usMsgSev */

        printf("Name of processed plan:    %.255s\n",
                                                  pMyIsInfo->PlanId.uchData );
        printf("Number of processed PLAN statement: %d\n",
                                                  pMyIsInfo->lPlanStmt );
        printf("Input data portion that caused the error: %.255s\n"
               , pMyIsInfo->InpErrDta.uchData );
        printf("Source identifier of processed assignment statement: %.255s\n"
               , pMyIsInfo->SrcFldId.uchData );
        printf("Target identifier of processed assignment statement: %.255s\n"
               , pMyIsInfo->TrgFldId.uchData );

      } /* endelse */
   } /* endif */

  return;

}



/******************************************************************************/
/*  MAIN function                                                             */
/******************************************************************************/
int main( )
{
  FMTCTOK     FeedBack;
  FMTADLCA    MyIsInfo;
  PVOID       pCnvPlnSpc;
  FILE        *CnvPlnSpcHandle;
  ULONG       ulLength;
  ULONG       ulCnvPlnSpcHdl;
  PBYTE       *ppInputData = 0;
  PBYTE       *ppOutputData = 0;
  PBYTE       pInValue = 0;
  PBYTE       pOutValue = 0;
  CHAR        Buffer[ BUFFER_LENGTH * 2 ];
  CHAR        EBCDIC[] = { 0xC1,0xC2,0xC3,0x01,0x23,0x4C };
  CHAR        ASCII[] = { 0x41,0x42,0x43,0x00,0xD2,0x04 };
  int         k;

  /****************************************************************************/
  /* Read the conversion plan space from file SAMPLE.SPC                      */
  /****************************************************************************/

  CnvPlnSpcHandle = fopen( "SAMPLE.SPC","rb");
  ulLength = _filelength( _fileno( CnvPlnSpcHandle ));
  pCnvPlnSpc = (PVOID)calloc( ulLength, sizeof(CHAR));

  fread( pCnvPlnSpc, sizeof(CHAR), ulLength, CnvPlnSpcHandle );

  fclose( CnvPlnSpcHandle );


  /****************************************************************************/
  /* Call Conversion Plan Executor Initialisation                             */
  /****************************************************************************/

  FMTCPXI( pCnvPlnSpc,                    // PBYTE    pCnvPlnSpc
           &ulCnvPlnSpcHdl,               // PULONG   pulCnvPlnSpcHdl
           &FeedBack );                   // PFMTCTOK pFeedback


  /****************************************************************************/
  /* Check the Condition Token                                                */
  /****************************************************************************/
  if ( FeedBack.Condition_ID.usMsgNo != CPX_NO_ERROR )
   {
     PrintCtok( &FeedBack, NULL, INIT );

   } /* endif */
  else
   {
     /*************************************************************************/
     /* Initialize ADL Communication Area                                     */
     /*************************************************************************/
     FeedBack.pI_S_Info.pAdlCommArea = &MyIsInfo;

     /*************************************************************************/
     /* Alloc input and output buffer                                         */
     /*************************************************************************/
     pInValue     = malloc( BUFFER_LENGTH );
     pOutValue    = malloc( BUFFER_LENGTH );
     ppInputData  = malloc( sizeof( PBYTE ) );
     ppOutputData = malloc( sizeof(PBYTE) );

     /*************************************************************************/
     /* Call Conversion Plan Executor Convert                                 */
     /* In this conversion the plan COBOL_TO_C is executed. The input data    */
     /* are :                                                                 */
     /*    "ABC"  in international EBCDIC format  -> '0xC1C2C3'               */
     /*    1234   in PACKED PRECISION(5) format   -> '0x01234C'               */
     /*                                                                       */
     /* The output data after conversion should be:                           */
     /*    "ABC"  in Latin PC Data format + suffix  -> '0x41424300'           */
     /*    1234   in BINARY bytereversed format     -> '0xD204'               */
     /*************************************************************************/

     memcpy( pInValue, EBCDIC, sizeof(EBCDIC));
     memset( pOutValue, 0, BUFFER_LENGTH );


     ppInputData[0] = pInValue;
     ppOutputData[0] = pOutValue;


     FMTCPXC(
               ulCnvPlnSpcHdl,              // ULONG    ulCnvPlnSpcHdl
               10,                          // ULONG    ulPlnNamLength
               "COBOL_TO_C",                // PCHAR    pPlnNam
               1,                           // ULONG    ulInputParmNum
               ppInputData,                 // PBYTE    *ppInputData
               1,                           // ULONG    ulOutputParmNum
               ppOutputData,                // PBYTE    *ppInputData
               &FeedBack );                 // PFMTCTOK pFeedBack

     if ( FeedBack.Condition_ID.usMsgNo != CPX_NO_ERROR )
      {
        /**********************************************************************/
        /* An error occured. Print the condition token                        */
        /**********************************************************************/
        PrintCtok( &FeedBack, &MyIsInfo, CONVERT );

      } /* endif */
     else
      {
        /**********************************************************************/
        /* Print the converted value                                          */
        /**********************************************************************/
        printf(" Converted value for plan COBOL_TO_C:\n" );
        for( k=0; k < BUFFER_LENGTH ;k++)
        {
          printf("%02x",pOutValue[k]);
        }

      } /* endelse */

     /*************************************************************************/
     /* Call Conversion Plan Executor Convert                                 */
     /* In this conversion the plan C_TO_COBOL is executed. The input data    */
     /* are :                                                                 */
     /*    "ABC"  in Latin PC Data format + suffix  -> '0x41424300'           */
     /*    1234   in BINARY bytereversed format     -> '0xD204'               */
     /*                                                                       */
     /* The output data after conversion should be:                           */
     /*    "ABC"  in international EBCDIC format  -> '0xC1C2C3'               */
     /*    1234   in PACKED PRECISION(5) format   -> '0x01234C'               */
     /*************************************************************************/

     memcpy( pInValue, ASCII, sizeof(ASCII));
     memset( pOutValue, 0, BUFFER_LENGTH );

     ppInputData[0] = pInValue;
     ppOutputData[0] = pOutValue;


     FMTCPXC(
               ulCnvPlnSpcHdl,              // ULONG    ulCnvPlnSpcHdl
               10,                          // ULONG    ulPlnNamLength
               "C_TO_COBOL",                // PCHAR    pPlnNam
               1,                           // ULONG    ulInputParmNum
               ppInputData,                 // PBYTE   *apInputData
               1,                           // ULONG    ulOutputParmNum
               ppOutputData,                // PBYTE   *apInputData
               &FeedBack );                 // PFMTCTOK pFeedBack

     if ( FeedBack.Condition_ID.usMsgNo != CPX_NO_ERROR )
      {
        /**********************************************************************/
        /* An error occured. Print the condition token                        */
        /**********************************************************************/
        PrintCtok( &FeedBack, &MyIsInfo, CONVERT );

      } /* endif */
     else
      {
        /**********************************************************************/
        /* Print the converted value                                          */
        /**********************************************************************/
        printf("\n Converted value for plan C_TO_COBOL:\n" );
        for( k=0; k < BUFFER_LENGTH ;k++)
        {
          printf("%02x",pOutValue[k]);
        }
      } /* endelse */



     /*************************************************************************/
     /* Call Conversion Plan Executor Termination                             */
     /*************************************************************************/

     FMTCPXT(
              ulCnvPlnSpcHdl,               // ULONG    ulCnvPlnSpcHdl
              &FeedBack );                  // PFMTCTOK pFeedBack


     /*************************************************************************/
     /* Check the Condition Token                                             */
     /*************************************************************************/
     if ( FeedBack.Condition_ID.usMsgNo != CPX_NO_ERROR )
      {
        PrintCtok( &FeedBack, NULL, TERM );
      } /* endif */


   } /* endelse */

  /****************************************************************************/
  /* Free allocated resources                                                 */
  /****************************************************************************/

  if ( pCnvPlnSpc != NULL ) {
    free( pCnvPlnSpc );
  } /* endif */

  if ( pInValue != NULL ) {
    free( pInValue );
  } /* endif */

  if ( pOutValue != NULL ) {
    free( pOutValue );
  } /* endif */

  if ( ppInputData != NULL) {
    free( ppInputData );
  } /* endif */

  if ( ppOutputData != NULL) {
    free( ppOutputData );
  } /* endif */

  return 0;
}
