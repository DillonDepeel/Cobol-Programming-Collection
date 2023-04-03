/********************************************************************/
/* Licensed material - Property of IBM                              */
/* 28H2177 (C) Copyright IBM Corp., 1995                            */
/* All rights reserved.                                             */
/* Use Government Users Restricted Rights - Use, duplication or     */
/* diclosure restriced by GSA ADP Schedule Contract with IBM Corp.  */
/********************************************************************/

/* add an entry for COBOL MVS Project */
/*name of file changed from mvssmart.cmd to IWZVREC1.cmd   8/03/95 */
/* to Project Smarts */
  /* Initialize - use a global stem variable. */
  stem = "stem"
  RC_OK     = 0
  RC_CANCEL = 95

  /* Load the Project Smarts utility functions. */
  rc = RxFuncAdd('iwfLoadFuncs', 'iwfRexx', 'iwfLoadFuncs');
  rc = iwfLoadFuncs();

  /* Open the catalog. */
  SAY "Populating Smarts catalog with entries..."
  stem.pszCatalog = "<IWZVPSMT>"
  rc = iwfOpenCatalog(stem);
  if (rc <> 0) then
     call Abort("Unable to open the Project Smarts catalog.");


  /* Add a catalog entry. */
  stem.pszName              = "COBOL MVS Project"
  line1                     = "This Project Smarts entry will "
  line2                     = "create a new project that inherits from "
  line3                     = "the COBOL MVS Master Project. "
  stem.pszDescription       = line1||line2||line3
  stem.pszSourceLocation    = "-none-"
  stem.pszInstallScript     = "IWZVREC2.cmd"
  stem.pszInstallParameters = "%project%"

  rc = iwfUpdateCatalog(stem);
  if (rc <> 0) then
     call Abort("Unable to update the QuickStart catalog.");
  rc = iwfDropFuncs();
Exit
