#pragma title ("USEREXIT")
/******************************************************************************/
/*                                                                            */
/* PRODUCT   = Data Description and Conversion for OS/2                       */
/*                                                                            */
/* SOURCE FILE NAME = USEREXIT.C                                              */
/*                                                                            */
/* DESCRIPTIVE NAME = User Exit Sample                                        */
/*                                                                            */
/* FUNCTION = This user exit sample program can be called via the             */
/*            DDC/2 user exit facility.                                       */
/*            It converts any character string from upper case to lower case  */
/*            characters or from lower case to upper case characters,         */
/*            depending on a boolean parameter.                               */
/* NOTES =                                                                    */
/*                                                                            */
/*   DEPENDENCIES = OS/2 Release 2.0 or later                                 */
/*                                                                            */
/*   RESTRICTIONS = None                                                      */
/*                                                                            */
/* ENTRY POINTS = ConvertToUpperOrLowerCaseChar                               */
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
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

/*---------------- DDC/2 Global Header ---------------------------------------*/
#define  FMT_NO_DCLXLRIFC FMT_NO_DCLXLRIFC    /* Exclude FMTB.H (DCLXLRIFC)   */
#define  FMT_NO_CPB       FMT_NO_CPB          /* Exclude FMTC.H (CPB)         */
#define  FMT_NO_CPEX      FMT_NO_CPEX         /* Exclude FMTD.H (CPEX)        */
#define  FMT_NO_LCF       FMT_NO_LCF          /* Exclude FMTF.H (LCF)         */
#define  FMT_NO_UTL       FMT_NO_UTL          /* Exclude FMTU.H (UTL)         */
#include "FMT.H"


VOID APIENTRY ConvertToUpperOrLowerCaseChar( LONG, VOID *[], PFMTCTOK );


/******************************************************************************/
/* User Exit Function (API)                                                   */
/******************************************************************************/
VOID APIENTRY ConvertToUpperOrLowerCaseChar( LONG lParamCount,
                                             VOID *pParameter[],
                                             PFMTCTOK pFeedBack )

/******************************************************************************/
/* Expected parameters:                                                       */
/*    lParamCount:   5                                                        */
/*    pParameter[0]: Reference to the input charater field.                   */
/*    pParameter[1]: Reference to the length of the input character field.    */
/*    pParameter[2]: Reference to a boolen condition.                         */
/*    pParameter[3]: Reference to output character field.                     */
/*    pParameter[4]: Reference to the length of the output character field.   */
/*    pFeedBack:     Reference to feedback area (NO ERROR: usMsgSev == 0).    */
/******************************************************************************/

{
   /***************************************************************************/
   /* Declaration and initialization of variables.                            */
   /***************************************************************************/
   INT   i;
   INT   CharToConvert;
   PCHAR pImputCharField = (PCHAR)(pParameter[0]);
   LONG  lByteSizeOfInputChar = *((PLONG)pParameter[1]);
   BOOL  fToUpperCaseLetter = *((PBOOL)pParameter[2]);
   PCHAR pOutputCharField = (PCHAR)(pParameter[3]);
   LONG  lByteSizeOfOutputChar = *((PLONG)pParameter[4]);

   pFeedBack->Condition_ID.usMsgSev = 0;
   pFeedBack->Condition_ID.usMsgNo = 0;


   /***************************************************************************/
   /* Check buffer sizes and parameter count                                  */
   /***************************************************************************/
   if ( ( lByteSizeOfInputChar <= lByteSizeOfOutputChar ) &&
        ( lParamCount == 5 ) )
   {

      if (fToUpperCaseLetter == TRUE)
      {
         /*********************************************************************/
         /* Convert all lower case character to upper case character          */
         /*********************************************************************/
         for (i = 0; i < lByteSizeOfInputChar; i++)
         {
            CharToConvert = (INT)(*pImputCharField++);
            CharToConvert = toupper( CharToConvert );
            *pOutputCharField++ = (CHAR)CharToConvert;

         } /* endfor */
      }
      else
      {
         /*********************************************************************/
         /* Convert all upper case character to lower case character          */
         /*********************************************************************/
         for (i = 0; i < lByteSizeOfInputChar; i++)
         {
            CharToConvert = (INT)(*pImputCharField++);
            CharToConvert = tolower( CharToConvert );
            *pOutputCharField++ = (CHAR)CharToConvert;

         } /* endfor */
      } /* endif */
   }
   else
   {
      /************************************************************************/
      /* Set error conditions                                                 */
      /************************************************************************/
      pFeedBack->Condition_ID.usMsgSev = 3;
      pFeedBack->Condition_ID.usMsgNo = 2647;
   } /* endif */

}
