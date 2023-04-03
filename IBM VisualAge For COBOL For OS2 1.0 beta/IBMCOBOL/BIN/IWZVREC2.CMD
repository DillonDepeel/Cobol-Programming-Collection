/*REXX*/

/******************************************************************************/
/* Licensed Material - Property of IBM                                        */
/* 28H2177 (C) Copyright IBM Corp., 1995                                      */
/* All Rights Reserved                                                        */
/* Use Government Users Restricted Rights - Use, dulplication or              */
/* disclosure resticted by GSA ADP Schedule Contract with IBM Corp.           */
/******************************************************************************/

/*   Based on IWFSMART.CMD - Project Smarts default install script            */


/* Initialize - use a global stem variable. */
stem = "stem"
RC_OK     = 0
RC_CANCEL = 95


/* Open the installation console. */
rc = IwfOpenConsole(stem);


/* Load the REXX utility functions. */
rc = RxFuncAdd('SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs');
rc = SysLoadFuncs();


/* Extract the passed parameters. */
if (Arg() <> 1) Then
   call Abort("Error in parameter list.");
Parse Arg Proj;


/* Initialize the progress, status, and log. */
stem.usPercent     = 30
stem.pszStatusText = "Initializing..."
rc = IwfUpdateConsoleProgress(stem);
rc = IwfUpdateConsoleStatus(stem);
SAY "Initializing the Project Smarts install..."


/* Query the HOST target location for the install. */
stem.usPercent     = 50
stem.pszStatusText = "Query host location..."
rc = IwfUpdateConsoleProgress(stem);
rc = IwfUpdateConsoleStatus(stem);
SAY "Enter the host server and source PDS"
stem.usVariableCount=1
stem.pszVariableName.1='Host_Source'
hostlocn='You may enter one Host Server and Host PDS name per line below:'
hostlocn=hostlocn||'\n \n<SERVER>full_pds_name(*)'
stem.pszVariableDescription.1 = hostlocn
stem.pszVariableValue.1='<COBOLVS>userid.iwz.cobol(*)'
do until (rc = RC_OK)
   rc = IwfQueryVariables(stem);
   if (rc  = RC_CANCEL) then call Cancel
   else
   if (rc <> RC_OK) then call Abort("Error ("RC") querying Host source information.");
end
hostlocn=stem.pszVariableValue.1


/* Query the OS/2 target location for the install. */
stem.usPercent     = 70
stem.pszStatusText = "Querying the install location..."
rc = IwfUpdateConsoleProgress(stem);
rc = IwfUpdateConsoleStatus(stem);
SAY "Enter the project name, OS/2 directory, and folder for the installation."
/* Set defaults for Location dialog */
env='OS2ENVIRONMENT';                  /* Default target directory */
tmploc = VALUE('tmp',,env)             /* is on TMP variable dir   */
tmploc = STRIP(tmploc,Trailing,'\');   /* Strip any backslashes at end of path */
/* what happens if TMP is HPFS and ends in a quote?  or ends with a semi-colon?? */
Locn = tmploc||'\mvsprj';
stem.pszTargetProject   = Proj
stem.pszTargetDirectory = locn
stem.pszTargetFolder    = "Desktop"
do until (rc = RC_OK)
   rc = IwfQueryLocation(stem);
   if (rc  = RC_CANCEL) then call Cancel
   else
   if (rc <> RC_OK) then call Abort("Error ("RC") querying OS/2 target information.");
end


/* Create the MVS project */
stem.usPercent     = 85;
stem.pszStatusText = "Creating the Workframe/2 project.";
rc = IwfUpdateConsoleProgress(stem);
rc = IwfUpdateConsoleStatus(stem);
SAY "The project will be created in "stem.pszTargetFolder;
setup='PAMLOCATION:IWFBPAM='||stem.pszTargetDirectory
setup=setup||';PAMLOCATION:IWFPMVS='||hostlocn
setup=setup||';INHERITLIST=<IWZMVSPRJ>;PAMORDER=IWFBPAM'
setup=setup||x2c('D')||'IWFPMVS'
rc=sysCreateObject('IWFProject',,
                    stem.pszTargetProject,,
                    stem.pszTargetFolder,,
                    setup,,
                   'FAILIFEXISTS')
if (rc <> 1) then call Abort("Error("rc") creating project.  Maybe it already exists?");


/* Done! */
stem.usPercent     = 100
stem.pszStatusText = "Project Creation Successful"
rc = IwfUpdateConsoleProgress(stem);
rc = IwfUpdateConsoleStatus(stem);
SAY "Done!"
SAY "Project Successfully Created."
rc = RxMessageBox("The project has been created.", "Done!", "OK", "Information");


/* We are done. */
call Done(0);


Cancel:
rcCancel = RxMessageBox("Do you really want to cancel?", , "YesNo", "Query")
if (rcCancel = 6) Then
   call Done(8)
return

Abort:
parse arg abortMessage
rcAbort = RxMessageBox(abortMessage, , "OK", "Error")
call Done(16)

Done:
arg exitRc
/* Close the installation console. */
rcDone = IwfCloseConsole(stem);
exit(exitRc)





                                                     