/*
***********************************************************************
**                        SOFTWARE INSTALLER
** 5621-434 (C) COPYRIGHT IBM CORP. 1989, 1995. ALL RIGHTS RESERVED.
**               LICENSED MATERIALS - PROPERTY OF IBM
***********************************************************************
*/
/*
** DO NOT MAKE ANY CHANGES TO THIS FILE OTHER THAN TO CUSTOMIZE THE
** RC COMPILE STATEMENT FOR NATIONAL LANGUAGE VERSIONS!
*/
'@echo off'

Prefix = 'EPFPREFIX:EPF'

/* Query the new prefix */
if pos(':', Prefix) <> 0
then do
   /* Prefix found */
   parse value Prefix with . ':' RenamedPrefix
end   /* end then */
else do
   /* Prefix not found; use default */
   RenamedPrefix = 'EPF'
end   /* end else */

   intro.1  = "   --------------------------------------------------------"
   intro.2  = "                                                           "
   intro.3  = "   This command file builds the customized IIRC.RC file and"
   intro.4  = "   binds it to the" RenamedPrefix"IDLDS.EXE executable.  In order to   "
   intro.5  = "   successfully perform this build, the following files    "
   intro.6  = "   must be present in the current directory:               "
   intro.7  = "                                                           "
   intro.8  = "            IIRC.RC             IIRCH.H                    "
   intro.9  = "           " RenamedPrefix"IIIRS.RES       " RenamedPrefix"IDLDS.EXE               "
   intro.10 = "            Any bitmap file that IIRC.RC references        "
   intro.11 = "                                                           "
   intro.12 = "   --------------------------------------------------------"
   intro.13 = "   Press <Enter> to start the build.                       "

   compiling.1 = "--------------------------------------------------"
   compiling.2 = "- Compiling your customized resource file. . .   -"
   compiling.3 = "--------------------------------------------------"

   creating.1 = "--------------------------------------------------"
   creating.2 = "- Creating the bindable resource file . . .      -"
   creating.3 = "--------------------------------------------------"

   binding.1 =  "--------------------------------------------------"
   binding.2 =  "- Binding resources to" RenamedPrefix"IDLDS.EXE . . .        -"
   binding.3 =  "--------------------------------------------------"

   complete.1 = "--------------------------------------------------"
   complete.2 = "- Build completed successfully.  You can now run -"
   complete.3 = "-" RenamedPrefix"IDLDS.EXE to test your changes.             -"
   complete.4 = "--------------------------------------------------"

   error.1 = "**************************************************"
   error.2 = "* Error occurred.  The build was not successful. *"
   error.3 = "* Eliminate the errors and rerun BLDRC.CMD.      *"
   error.4 = "**************************************************"

   'cls'
   say intro.1
   say intro.2
   say intro.3
   say intro.4
   say intro.5
   say intro.6
   say intro.7
   say intro.8
   say intro.9
   say intro.10
   say intro.11
   say intro.12
   say intro.13
   pull .


   /**************************************************************/
   /* Compile IIRC.RC.                                           */
   /**************************************************************/
   /* If compiling translated national language versions of the  */
   /* IIRC.RC file and you have a RC.EXE resource compiler which */
   /* supports the -cc and -cp parameters, then you can modify   */
   /* the 'rc -r iirc.rc iirc.res' line below which performs the */
   /* compile so that your system will not need to be rebooted   */
   /* with the country code and codepage for the language.       */
   /* Example replacment lines for several languages are:        */
   /*                                                            */
   /* Japanese       : 'rc -cc 81 -cp 942 -r iirc.rc iirc.res'   */
   /* Korean         : 'rc -cc 82 -cp 949 -r iirc.rc iirc.res'   */
   /* Trad. Chinese  : 'rc -cc 88 -cp 948 -r iirc.rc iirc.res'   */
   /**************************************************************/
   'cls'
   say " "
   say compiling.1
   say compiling.2
   say compiling.3
   'rc -r iirc.rc iirc.res'
   if \(rc = 0) then signal error;

   /**************************************************************/
   /* Concatenate the EPFIIIRS.RES file and the IIRC.RES file    */
   /* together into ALL.RES.                                     */
   /**************************************************************/
   'cls'
   say " "
   say creating.1
   say creating.2
   say creating.3
   'copy' RenamedPrefix'iiirs.res /B + iirc.res /B all.res'
   if \(rc = 0) then signal error;

   /**************************************************************/
   /* Bind ALL.RES to EPFIDLDS.EXE.                              */
   /**************************************************************/
   'cls'
   say " "
   say binding.1
   say binding.2
   say binding.3
   'rc all.res' RenamedPrefix'idlds.exe'
   if \(rc = 0) then signal error;

   say complete.1
   say complete.2
   say complete.3
   say complete.4
   call beep  1000,500
   'erase all.res'

   say
   say 'Check for any errors.'
   'pause'

   exit 0

error:
   say " "
   say error.1
   say error.2
   say error.3
   say error.4
   exit rc
