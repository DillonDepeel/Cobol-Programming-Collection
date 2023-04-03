/*                                                                       *
 *-----------------------------------------------------------------------*
 * DISCLAIMER OF WARRANTIES:                                             *
 *                                                                       *
 *   The following enclosed code is sample code created by IBM           *
 *   Corporation.  This sample code is not part of any standard IBM      *
 *   product and is provided to you solely for the purpose of assisting  *
 *   you in the development of your applications.  The code is provided  *
 *   "AS IS", without warranty of any kind.  IBM shall not be liable     *
 *   for any damages arising out of your use of the sample code, even    *
 *   if they have been advised of the possibility of such damages.       *
 *                                                                       *
 *-----------------------------------------------------------------------*
 *                                                                       */
/*REXX*/

/* Get the path of the VisualAge C++ installation */
env='OS2ENVIRONMENT';               /* Default target directory */
BasePath = VALUE('CPPWORK',,env);  /* on CPPWORK variable dir   */
if BasePath="" then call Abort("Error: CPPWORK environment variable not set.");

/* Get project path name. CHANGE THIS FOR YOUR OWN PROJECT! */
PrjName = "LXMIN2"
PrjPath = '"'BasePath'\samples\projects\editor\'PrjName'"'

/* Call IWFBUILD utility */
iwfbuild '/Project 'PrjPath' /NoPrompt'
SAY 'Build completed with rc='rc
call Done(rc);

Abort:
parse arg abortMessage
SAY abortMessage
call Done(100)

Done:
arg exitRc
exit(exitRc)


