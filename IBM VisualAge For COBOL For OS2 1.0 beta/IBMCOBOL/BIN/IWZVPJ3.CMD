/******************************************************************************/
/*  Licensed Material - Property of IBM                                       */
/*  28H2177 (C) Copyright IBM Corp., 1995                                     */
/*  All rights reserved.                                                      */
/*  Use Government Users Restricted Rights - Use, duplication or disclosure   */
/*  restricted by GSA ADP Schedule Contract with IBM Corp.                    */
/******************************************************************************/
/******************************************************************************/
/* Get arguments                                                              */
/*   CREATE:     create the smarts catalog                                    */
/*   ADDGUI:     add the gui entries                                          */
/*   ADDNONGUI:  add the non-gui etnries                                      */
/* If no arguments are present, default is CREATE ADDGUI ADDNONGUI            */
/******************************************************************************/
/******************************************************************************/
/* Installation Exit for COBOL Project Smarts                                 */
/******************************************************************************/
call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
call SysLoadFuncs

/******************************************************************************/
/* Arguments                                                                  */
/******************************************************************************/
arg arguments

if words(arguments) == 0 then do
   create = 1
   addgui = 1
   addnongui = 1
end
else do
   if wordpos('CREATE', arguments) == 0 then
      create = 0
   else
      create = 1

   if wordpos('ADDGUI', arguments) == 0 then
      addgui = 0
   else
      addgui = 1

   if wordpos('ADDNONGUI', arguments) == 0 then
      addnongui = 0
   else
      addnongui = 1
end

/******************************************************************************/
/*  BEGINNING OF TEXT STRINGS                                                 */
/******************************************************************************/
/*  The following lines are the description strings for the catalog.          */
/*  These strings need to be translated for NLS.                              */
/******************************************************************************/
/* Catalog title                                                              */
title    = "COBOL Project Smarts"
/* COBOL GUI Designer Project strings                                         */
namea    = "COBOL GUI Designer Project"
linea1   = "Select this entry to invoke the VisualAge COBOL GUI Designer "
linea2   = "to create a new project that will have a "
linea3   = "graphical user interface."
/* COBOL GUI Designer Project with prompt strings                             */
nameb    = "COBOL GUI Designer Project with prompt"
lineb1   = "Select this entry to invoke the VisualAge COBOL GUI Designer for a "
lineb2   = "project that has a graphical user interface.  The project can be a "
lineb3   = "new project or an existing project created using the GUI Designer."
/* Empty COBOL GUI Designer Project strings                                   */
namec    = "Empty COBOL GUI Designer Project"
linec1   = "Select this entry to create a new GUI project "
linec2   = "that does not contain any files.  The GUI Designer is not "
linec3   = "started."
/* Default COBOL Project strings                                              */
named    = "Default COBOL Project"
lined1   = "Select this entry to create a new project for "
lined2   = "COBOL programs that do not require a VisualAge COBOL "
lined3   = "graphical user interface."
/* Messages                                                                   */
abortstr = "Aborting!"
abstr1   = "Could not create Project Smarts catalog."
abstr2   = "Unable to open the Project Smarts catalog."
okstr1   = "catalog created."
okstr2   = "catalog processing completed successfully."
/******************************************************************************/
/*  END OF TEXT STRINGS                                                       */
/******************************************************************************/
/******************************************************************************/
/* Register IWFQuickStart class                                               */
/******************************************************************************/
if create == 1 then do
   class = 'IWFQuickStart'
   dll = 'IWFSTART'
/* Say 'Registering class' class'...'                                        */
   ok? = SysRegisterObjectClass( class, dll )
   if ok? then do
      Say 'Register' class 'successful'
   end
   else
   do
      Say 'Register' class 'failed'
      exit 1
   End

   /******************************************************************************/
   /* Create Project Smarts catalog                                              */
   /******************************************************************************/
   /* Say '<<< Create Project Smarts catalog'                                    */
   /* Say 'Creating instance of' class 'on your desktop, as' title               */
   /******************************************************************************/
   rc = SysCreateObject('IWFQuickStart',,
                        title,,
                        '<IWZWORKS>',,
                        'OBJECTID=<IWZVPSMT>;',,
                        'ReplaceIfExists');
   if (rc <> 1) then call Abort (abstr1)
        else Say title okstr1
end  /* create new catalog */

/******************************************************************************/
/* Initialize - use a global stem variable.                                   */
stem = "stem"
RC_OK     = 0
RC_CANCEL = 95

/******************************************************************************/
/* Load the Project Smarts utility functions.                                 */
rc = RxFuncAdd('iwfLoadFuncs', 'iwfRexx', 'iwfLoadFuncs');
rc = iwfLoadFuncs();

/******************************************************************************/
/* Open the catalog.                                                          */
/*Say "Populating Project Smarts catalog with entries..."                     */
stem.pszCatalog = "<IWZVPSMT>"
rc = iwfOpenCatalog(stem);
if (rc <> 0) then
   call Abort(abstr2);

/******************************************************************************/
/* Add catalog entries.                                                       */

/******************************************************************************/
/* VDE catalog entries.                                                       */
env = 'OS2ENVIRONMENT'
cobpath = value('ICOBDIR',,env);
vdeexe = '\bin\iwzvguib.exe'
filespec = cobpath || vdeexe
result = stream(filespec, c, 'query exists')
if (result <> "") & (addgui == 1) then
do
   stem.pszName              = namea
   stem.pszDescription       = linea1||linea2||linea3
   stem.pszSourceLocation    = ""
   stem.pszInstallScript     = "iwzvtst1.cmd"
   stem.pszInstallParameters = ""

   rc = iwfUpdateCatalog(stem);
   if (rc <> 0) then
      call Abort(abstr2);

   stem.pszName              = nameb
   stem.pszDescription       = lineb1||lineb2||lineb3
   stem.pszSourceLocation    = ""
   stem.pszInstallScript     = "iwzvtst2.cmd"
   stem.pszInstallParameters = ""

   rc = iwfUpdateCatalog(stem);
   if (rc <> 0) then
      call Abort(abstr2);

   stem.pszName              = namec
   stem.pszDescription       = linec1||linec2||linec3
   stem.pszSourceLocation    = ""
   stem.pszInstallScript     = "iwzvtst3.cmd"
   stem.pszInstallParameters = ""

   rc = iwfUpdateCatalog(stem);
   if (rc <> 0) then
      call Abort(abstr2);
end /* do for VDE entries */

/******************************************************************************/
/* Non-VDE catalog entries.                                                   */
if addnongui == 1 then do
   stem.pszName              = named
   stem.pszDescription       = lined1||lined2||lined3
   stem.pszSourceLocation    = ""
   stem.pszInstallScript     = "iwzvtst4.cmd"
   stem.pszInstallParameters = ""

   rc = iwfUpdateCatalog(stem);
   if (rc <> 0) then
      call Abort(abstr2);
end
/******************************************************************************/
/* We are done.                                                               */
/* Say "OK."                                                                  */
Say title okstr2
call Done(0);

exit

/******************************************************************************/
/*  Abort routine                                                             */
/******************************************************************************/
Abort:
arg abortMessage
Say abortstr
Say abortMessage
call Done(16)

/******************************************************************************/
/*  Done routine                                                              */
/******************************************************************************/
Done:
arg doneRc
rc = iwfDropFuncs();
exit(doneRc)
