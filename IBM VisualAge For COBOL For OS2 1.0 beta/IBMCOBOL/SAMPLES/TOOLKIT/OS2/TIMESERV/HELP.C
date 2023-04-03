/*static char *SCCSID = "@(#)help.c	6.7 92/02/18";*/
/*static char *SCCSID = "@(#)help.c	6.7 92/02/18";*/
/*==============================================================*\
 *  Help.c - routines for the help manager interface
 *--------------------------------------------------------------
 *
 *  This module contains all the routines for interfacing with
 *  the IPF help manager.
 *
 *--------------------------------------------------------------
 *
 *      Copyright 1992  IBM Corp.
 *
 *      DISCLAIMER OF WARRANTIES.  The following [enclosed] code is        *
 *      sample code created by IBM Corporation. This sample code is not    *
 *      part of any standard or IBM product and is provided to you solely  *
 *      for  the purpose of assisting you in the development of your       *
 *      applications.  The code is provided "AS IS", without               *
 *      warranty of any kind.  IBM shall not be liable for any damages     *
 *      arising out of your use of the sample code, even if they have been *
 *      advised of the possibility of   such damages.                      *
 *
 *--------------------------------------------------------------
 *
 *  This source file contains the following functions:
 *
 *          InitHelp()
 *          HelpHelpForHelp(mp2)
 *          HelpExtended(mp2)
 *          HelpKeys(mp2)
 *          HelpIndex(mp2)
 *          HelpTutorial(mp2)
 *          HelpAbout(mp2)
 *          DisplayHelpPanel(idPanel)
 *          DestroyHelpInstance()
 *
\*==============================================================*/

/*--------------------------------------------------------------*\
 *  Include files, macros, defined constants, and externs
\*--------------------------------------------------------------*/


#define INCL_GPITRANSFORMS
#define INCL_WINHELP
#define INCL_DOSSEMAPHORES
#define INCL_DOSDATETIME


#include <os2.h>
#include <string.h>
#include "clock.h"
#include "dialogs.h"
#include "clkdata.h"
#include "help.h"
#include "res.h"

#define HELPLIBRARYNAMELEN  20

/* If DEBUG is defined, then the help panels will display their
 *  id values on their title bar.  This is useful for determining
 *  which help panels are being shown for each dialog item.  When
 *  the DEBUG directive is not defined, then the panel ids are not
 *  displayed.
 */

/* #define  DEBUG */


/*--------------------------------------------------------------*\
 *  Global variables
\*--------------------------------------------------------------*/
HWND hwndHelpInstance;
static CHAR szLibName[HELPLIBRARYNAMELEN];
static CHAR szWindowTitle[HELPLIBRARYNAMELEN];


/*--------------------------------------------------------------*\
 *  Entry point declarations
\*--------------------------------------------------------------*/


/****************************************************************\
 *  Routine for initializing the help manager
 *--------------------------------------------------------------
 *
 *  Name:   InitHelp()
 *
 *  Purpose: Initializes the IPF help facility
 *
 *  Usage:  Called once during initialization of the program
 *
 *  Method: Initializes the HELPINIT structure and creates the
 *          help instance.  If successful, the help instance
 *          is associated with the main window
 *
 *  Returns:
 *
\****************************************************************/
VOID InitHelp(VOID)
{
    HELPINIT hini;

    /* if we return because of an error, Help will be disabled */

    /* initialize help init structure */
    hini.cb = sizeof(HELPINIT);
    hini.ulReturnCode = 0;

    hini.pszTutorialName = (PSZ)NULL;   /* if tutorial added, add name here */

    hini.phtHelpTable = (PHELPTABLE)MAKELONG(CLOCK_HELP_TABLE, 0xFFFF);
    hini.hmodHelpTableModule = 0L;
    hini.hmodAccelActionBarModule = 0L;
    hini.idAccelTable = 0L;
    hini.idActionBar = 0L;

    if(!WinLoadString(hab,
                      0,
                      IDS_HELPWINDOWTITLE,
                      HELPLIBRARYNAMELEN,
                      (PSZ)szWindowTitle) )
    {

        MessageBox(hwndFrame, IDMSG_CANNOTLOADSTRING,
                     MB_OK | MB_ERROR, FALSE);
        return;
    }

    hini.pszHelpWindowTitle = (PSZ)szWindowTitle;

    /* if debugging, show panel ids, else don't */
#ifdef DEBUG
    hini.fShowPanelId = CMIC_SHOW_PANEL_ID;
#else
    hini.fShowPanelId = CMIC_HIDE_PANEL_ID;
#endif

    if(!WinLoadString(hab,
                      0,
                      IDS_HELPLIBRARYNAME,
                      HELPLIBRARYNAMELEN,
                      (PSZ)szLibName))
    {
        MessageBox(hwndFrame, IDMSG_CANNOTLOADSTRING,
                         MB_OK | MB_ERROR, FALSE);
        return;
    }

    hini.pszHelpLibraryName = (PSZ)szLibName;

    /* creating help instance */
    hwndHelpInstance = WinCreateHelpInstance(hab, &hini);

    if(hwndHelpInstance == NULLHANDLE || hini.ulReturnCode)
    {
        MessageBox(hwndFrame, IDMSG_HELPLOADERROR, MB_OK | MB_ERROR, TRUE);
        return;
    }

    /* associate help instance with main frame */
    if(!WinAssociateHelpInstance(hwndHelpInstance, hwndFrame))
    {
        MessageBox(hwndFrame, IDMSG_HELPLOADERROR, MB_OK | MB_ERROR, TRUE);
        return;
    }

    /* help manager is successfully initialized so set flag to TRUE */
    fHelpEnabled = TRUE;

}   /* InitHelp() */


/****************************************************************\
 *  Processes the Help for Help command from the menu bar
 *--------------------------------------------------------------
 *
 *  Name:   HelpHelpForHelp(mp2)
 *
 *  Purpose: Processes the WM_COMMAND message posted by the
 *            Help for Help item of the Help menu
 *
 *  Usage:  Called from MainCommand when the Help for Help
 *          menu item is selected
 *
 *  Method: Sends an HM_DISPLAY_HELP message to the help
 *          instance so that the default Help For Help is
 *          displayed.
 *
 *  Returns:
 *
\****************************************************************/
VOID  HelpHelpForHelp(MPARAM mp2)
{

    /* this just displays the system help for help panel */
    if(fHelpEnabled)
        if(WinSendMsg(hwndHelpInstance,
                HM_DISPLAY_HELP,
                MPVOID,
                MPVOID ) )
        {
            MessageBox(hwndFrame,
                       IDMSG_HELPDISPLAYERROR,
                       MB_OK | MB_ERROR,
                       FALSE);
        }
}   /* HelpHelpForHelp() */


/****************************************************************\
 *  Processes the Extended Help command from the menu bar
 *--------------------------------------------------------------
 *
 *  Name:   HelpExtended(mp2)
 *
 *  Purpose: Processes the WM_COMMAND message posted by the
 *            Extended Help item of the Help menu
 *
 *  Usage:  Called from MainCommand when the Extended Help
 *          menu item is selected
 *
 *  Method: Sends an HM_EXT_HELP message to the help
 *          instance so that the default Extended Help is
 *          displayed.
 *
 *  Returns:
 *
\****************************************************************/
VOID  HelpExtended(MPARAM mp2) /* second parameter of WM_COMMAND message */
{

    /* this just displays the system extended help panel */
    if(fHelpEnabled)
        if(WinSendMsg(hwndHelpInstance, HM_EXT_HELP,MPVOID,
                       MPVOID) )
            MessageBox(hwndFrame,
                       IDMSG_HELPDISPLAYERROR,
                       MB_OK | MB_ERROR,
                       FALSE);
}   /* HelpExtended() */


/****************************************************************\
 *  Processes the Keys Help command from the menu bar
 *--------------------------------------------------------------
 *
 *  Name:   HelpKeys(mp2)
 *
 *  Purpose: Processes the WM_COMMAND message posted by the
 *            Keys Help item of the Help menu
 *
 *  Usage:  Called from MainCommand when the Keys Help
 *          menu item is selected
 *
 *  Method: Sends an HM_KEYS_HELP message to the help
 *          instance so that the default Keys Help is
 *          displayed.
 *
 *  Returns:
 *
\****************************************************************/
VOID  HelpKeys(MPARAM mp2)
{

    /* this just displays the system keys help panel */
    if(fHelpEnabled)
        if(WinSendMsg(hwndHelpInstance, HM_KEYS_HELP,
                   MPVOID,
                   MPVOID) )
            MessageBox(hwndFrame,
                       IDMSG_HELPDISPLAYERROR,
                       MB_OK | MB_ERROR,
                       FALSE);
}   /* HelpKeys() */


/****************************************************************\
 *  Processes the Index Help command from the menu bar
 *--------------------------------------------------------------
 *
 *  Name:   HelpIndex(mp2)
 *
 *  Purpose: Processes the WM_COMMAND message posted by the
 *            Index Help item of the Help menu
 *
 *  Usage:  Called from MainCommand when the Index Help
 *          menu item is selected
 *
 *  Method: Sends an HM_INDEX_HELP message to the help
 *          instance so that the default Index Help is
 *          displayed.
 *
 *  Returns:
 *
\****************************************************************/
VOID  HelpIndex(MPARAM mp2)
{

    /* this just displays the system help index panel */
    if(fHelpEnabled)
    {
        if(WinSendMsg(hwndHelpInstance, HM_HELP_INDEX,MPVOID,
                       MPVOID))
            MessageBox(hwndFrame,
                       IDMSG_HELPDISPLAYERROR,
                       MB_OK | MB_ERROR,
                       FALSE);
    }
}   /* HelpIndex() */


/****************************************************************\
 *  Processes the Tutorial Help command from the menu bar
 *--------------------------------------------------------------
 *
 *  Name:   HelpTutorial(mp2)
 *
 *  Purpose: Processes the WM_COMMAND message posted by the
 *            Tutorial Help item of the Help menu.  While the
 *            standard template application does not include a
 *            Tutorial menu item, you can add one if your
 *            application has a tutorial.
 *
 *  Usage:  Called from MainCommand when the Tutorial Help
 *          menu item is selected
 *
 *  Method:
 *
 *  Returns:
 *
\****************************************************************/
VOID  HelpTutorial(MPARAM mp2)
{

   /*--------------------------------------------------------------*\
    *  Insert code for any tutorial here
   \*--------------------------------------------------------------*/

}


/****************************************************************\
 *  Processes the About command from the Help menu
 *--------------------------------------------------------------
 *
 *  Name:   HelpAbout(mp2)
 *
 *  Purpose: Processes the WM_COMMAND message posted by the
 *            About item of the Help menu
 *
 *  Usage:  Called from MainCommand when the About
 *          menu item is selected
 *
 *  Method: Calls WinDlgBox to display the about box dialog.
 *
 *  Returns:
 *
\****************************************************************/
VOID  HelpAbout(MPARAM mp2)
{

    /* display the AboutBox dialog */
    WinDlgBox(HWND_DESKTOP,
              hwndFrame,
              AboutBoxDlgProc,
              0,
              IDD_ABOUTBOX,
              (PVOID)NULL);
}   /* HelpAbout() */


/****************************************************************\
 *  Displays the help panel indicated
 *--------------------------------------------------------------
 *
 *  Name:   DisplayHelpPanel(idPanel)
 *
 *  Purpose: Displays the help panel whose id is given
 *
 *  Usage:  Called whenever a help panel is desired to be
 *          displayed, usually from the WM_HELP processing
 *          of the dialog boxes
 *
 *  Method: Sends HM_DISPLAY_HELP message to the help instance
 *
 *  Returns:
 *
\****************************************************************/
VOID DisplayHelpPanel(SHORT idPanel)
{

    if(fHelpEnabled)
    {
        if(WinSendMsg(hwndHelpInstance,
               HM_DISPLAY_HELP,
               MPFROMLONG(MAKELONG(idPanel, NULL)),
               MPFROMSHORT(HM_RESOURCEID)))
        {

            MessageBox(hwndFrame,
                       IDMSG_HELPDISPLAYERROR,
                       MB_OK | MB_ERROR,
                       TRUE);
        }
   }
}   /* DisplayHelpPanel() */



/****************************************************************\
 *  Destroys the help instance
 *--------------------------------------------------------------
 *
 *  Name:   DestroyHelpInstance(VOID)
 *
 *  Purpose: Destroys the help instance for the application
 *
 *  Usage:  Called after exit from message loop
 *
 *  Method: Calls WinDestroyHelpInstance() to destroy the
 *          help instance
 *
 *  Returns:
 *
\****************************************************************/
VOID DestroyHelpInstance(VOID)
{

    if(hwndHelpInstance != NULLHANDLE)  {
        WinDestroyHelpInstance(hwndHelpInstance);
    }
}   /* DestroyHelpInstance() */
