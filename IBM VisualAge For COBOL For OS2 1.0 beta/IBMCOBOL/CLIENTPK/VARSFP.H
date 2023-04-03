/*
**                      SOFTWARE INSTALLER
** 5621-434 (C) COPYRIGHT IBM CORP. 1989, 1995. ALL RIGHTS RESERVED.
**             LICENSED MATERIALS - PROPERTY OF IBM
*/
/****************************************************************************/
/*                                                                          */
/* This file contains :                                                     */
/*    constants                                                             */
/*    data structures                                                       */
/*    entry points                                                          */
/* for the programming interfaces of the Software Installer.                */
/*                                                                          */
/****************************************************************************/
#define PATH_DIR_LEN         128   /* max for paths */
#define MAX_AUX_PATHS         18   /* max number of AUX paths supported */

/**********************************************************/
/* This structure can be used to obtain the installation  */
/* path information for a product installed with Software */
/* Installer.  This structure would be used with the      */
/* getproductdir() function to obtain the product         */
/* directory information.                                 */
/**********************************************************/
typedef struct 
{
  char szFilePath[PATH_DIR_LEN+1];                 
  char szWorkPath[PATH_DIR_LEN+1];                 
  char szAuxPath[MAX_AUX_PATHS][PATH_DIR_LEN+1];   
}PRODUCTDIR;


unsigned long _System getvar (char *pszVarName, char *pszVarBuffer, unsigned long *pulBufferSize);
unsigned long _System putvar (char *pszVarName, char *pszVarVal);
unsigned long _System getowner(void);
unsigned long _System disableentry(char *pDisabled);
unsigned long _System getproductdir(char *pszProdName, char *pszProdNumber, char *pszProdFeature, PRODUCTDIR *pProductDir);


/************************************************************
** From a user exit, to simulate having pressed the Stop
** button on the Progress dialog, use the following PM call:
**    WinPostMsg(
**       hWndProgress,
**       WM_COMMAND,
**       (MPARAM) ID_PROGRESS_STOP_BUTTON,
**       (MPARAM) NULL);
** Use the getowner() function to get the handle of the
** progress dialog (hWndProgress).
**
** By doing this call, an exit will be able to stop the
** installation.
************************************************************/
#define  ID_PROGRESS_STOP_BUTTON  602  /* Window ID of Stop button on */
                                       /* the Progress dialog         */
