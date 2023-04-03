/*
********************************************************************
**                        SOFTWARE INSTALLER
** 5621-434 (C) COPYRIGHT IBM CORP. 1989, 1995. ALL RIGHTS RESERVED.
**               LICENSED MATERIALS - PROPERTY OF IBM
********************************************************************
*/
/******************************************************************/
/* Define constants for includes                                  */
/******************************************************************/
#define  INCL_BASE


/******************************************************************/
/* OS/2 and C includes                                            */
/******************************************************************/
#include <os2.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "varsfp.h"


/******************************************************************/
/* Function prototypes                                            */
/******************************************************************/
VOID  ChangeRoot(
         CHAR *,
         CHAR *,
         CHAR *);

VOID  EXPENTRY EPFIHOOK1(
                  BOOL,
                  ULONG,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *,
                  CHAR *);


USHORT EXPENTRY EPFIHOOK2(void);

/*******************************************************************
**
** Function Name:  EPFIHOOK1
**
** Description:    This hook is called each time the directories
**                 in the Install-directories dialog changes or
**                 when the opportunity for a change has occurred.
**
** input:          BOOL bReset
**                 - bool that indicates if a reset action is to
**                 be performed; this will set the static storage
**                 back to its original state
**
**                 CHAR *szFilePath
**                 - Ptr to file path
**
**                 CHAR *szWorkPath
**                 - Ptr to work path
**
**                 CHAR *szAuxNPath
**                 - Ptr to AuxN path, where 1<=N<=18
**
** output:         Return (VOID)
**
** Author:         Greenlee, K L
**
** Date Written:   02/15/93
**
** DETAIL =
**               1. Allocate temp storage
**               2. If FILE path has changed, cascade the value
**
*******************************************************************/
VOID  EXPENTRY EPFIHOOK1(
                  BOOL  bReset,
                  ULONG ulMaxPathLength,
                  CHAR  *szFilePath,
                  CHAR  *szWorkPath,
                  CHAR  *szAux1Path,
                  CHAR  *szAux2Path,
                  CHAR  *szAux3Path,
                  CHAR  *szAux4Path,
                  CHAR  *szAux5Path,
                  CHAR  *szAux6Path,
                  CHAR  *szAux7Path,
                  CHAR  *szAux8Path,
                  CHAR  *szAux9Path,
                  CHAR  *szAux10Path,
                  CHAR  *szAux11Path,
                  CHAR  *szAux12Path,
                  CHAR  *szAux13Path,
                  CHAR  *szAux14Path,
                  CHAR  *szAux15Path,
                  CHAR  *szAux16Path,
                  CHAR  *szAux17Path,
                  CHAR  *szAux18Path)
{  // begin EPFIHOOK1()
CHAR  *pTemp;
CHAR  *pBuffer;
ULONG ulBufferSize;

/****************************************************/
/* Delete the saved values when a reset is received */
/****************************************************/
if (bReset)
   {
   putvar("EPFIHOOKROOT1", "");
   }  // end then

pTemp   = (CHAR *) malloc((ulMaxPathLength + 1) * sizeof(CHAR));

ulBufferSize = ulMaxPathLength;
pBuffer = (CHAR *) malloc((ulMaxPathLength + 1) * sizeof(CHAR));

                                 // retrieve the main root path
                                 // all others change to this value
getvar("EPFIHOOKROOT1", pBuffer, &ulBufferSize);

if ((pTemp) &&
    (pBuffer) &&
    (strcmp(pBuffer, szFilePath) != 0))
   {
   /********************************************/
   /* File path was updated; cascade the value */
   /********************************************/
                                 // save the main root path
   putvar("EPFIHOOKROOT1", szFilePath);

   ChangeRoot(szWorkPath, szFilePath, pTemp);
   ChangeRoot(szAux1Path, szFilePath, pTemp);
   ChangeRoot(szAux2Path, szFilePath, pTemp);
   ChangeRoot(szAux3Path, szFilePath, pTemp);
   ChangeRoot(szAux4Path, szFilePath, pTemp);
   ChangeRoot(szAux5Path, szFilePath, pTemp);
   ChangeRoot(szAux6Path, szFilePath, pTemp);
   ChangeRoot(szAux7Path, szFilePath, pTemp);
   ChangeRoot(szAux8Path, szFilePath, pTemp);
   ChangeRoot(szAux9Path, szFilePath, pTemp);
   ChangeRoot(szAux10Path, szFilePath, pTemp);
   ChangeRoot(szAux11Path, szFilePath, pTemp);
   ChangeRoot(szAux12Path, szFilePath, pTemp);
   ChangeRoot(szAux13Path, szFilePath, pTemp);
   ChangeRoot(szAux14Path, szFilePath, pTemp);
   ChangeRoot(szAux15Path, szFilePath, pTemp);
   ChangeRoot(szAux16Path, szFilePath, pTemp);
   ChangeRoot(szAux17Path, szFilePath, pTemp);
   ChangeRoot(szAux18Path, szFilePath, pTemp);
   }  // end then

free(pTemp);
free(pBuffer);

}  // end EPFIHOOK1()



/*******************************************************************
**
** Function Name:  ChangeRoot
**
** Description:    Change a path's root value to the specified
**                 value.
**
** input:          CHAR *pszPath
**                 - Ptr to a path
**
**                 CHAR *pszNewRootValue
**                 - Ptr to new root value
**
**                 CHAR *pTemp
**                 - Ptr to temp storage
**
** output:         Return (VOID)
**
** Author:         Greenlee, K L
**
** Date Written:   02/15/93
**
** DETAIL =
**               1. Allocate temp storage
**               2. If FILE path has changed, cascade the value
**
*******************************************************************/
VOID  ChangeRoot(
         CHAR  *pszPath,
         CHAR  *pszNewRootValue,
         CHAR  *pTemp)

{  // begin InsertFilePath()
CHAR     *pBSlash;

/*****************************************/
/* Only work with paths that are defined */
/*****************************************/
if (strlen(pszPath) > 0)
   {
   /************************************************/
   /* Search for the end of the root; if the input */
   /* is "C:\INSTALL\DOC", pBSlash will point to   */
   /* "\DOC" ignore all paths that are only of     */
   /* roots (e.g. "C:\INSTALL")                    */
   /************************************************/
   pBSlash = strstr(pszPath, "\\");
   pBSlash = strstr(pszPath+(pBSlash-pszPath)+1, "\\");

   if (pBSlash)
      {
      /**********************************/
      /* Input path has a subdirectory; */
      /* change the value of the root   */
      /**********************************/
      strcpy(pTemp, pBSlash);             // save subdirs
      strcpy(pszPath, pszNewRootValue);   // copy new root
      strcat(pszPath, pTemp);             // add subdirs
      }  // end then
   }  // end then

return;

}  // end InsertFilePath()



/*******************************************************************
**
** Function Name:  EPFIHOOK2
**
** Description:    This hook is called when the Install button 
**                 on the Install-directories dialog is
**                 selected.
**
** output:         0
**                  - Dismiss the Install-directories dialog
**                 1
**                  - Do not dismiss the Install-directories
**                  dialog
**
*******************************************************************/
USHORT  EXPENTRY EPFIHOOK2()
{  // begin EPFIHOOK2()
  return 0;  // dismiss the install directories dialog
}  // end EPFIHOOK2()
