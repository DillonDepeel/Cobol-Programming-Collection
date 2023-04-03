/******************************************************************************
*
*  Program Number 5763-CD1 (C) Copyright IBM Corp. 1992, 1994.
*  Program Number 5688-194 (C) Copyright IBM Corp. 1992, 1994.
*  All rights reserved.
*
******************************************************************************/
trace 'o'

parse upper arg bootdrive host action wfdesk seconds .

call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
call SysLoadFuncs

/* Verify the last required parameter */
if action == '' then do
   call Message 14
   exit 1
   end

if length(bootdrive) > 1 then bootdrive = left(bootdrive, 1)

if datatype(bootdrive) <> 'CHAR' then do
   call Message 15
   exit 1
   end

if host <> '400' & host <> 'MVS' & host <> 'VM' then do
   call Message 15
   exit 1
   end

if action <> 'INSTALL' & action <> 'UPDATE' then do
   call Message 15
   exit 1
   end

/* if 'seconds' is not specified or is non-integer then override to 8 */
if (seconds = '' | datatype(seconds, 'W') = 0) then
   seconds = 10

platform = host
if host = 'MVS' | host = 'VM' then platform = '370'

epfisini = bootdrive||':\OS2\SYSTEM\EPFIS.INI'

if stream(epfisini, 'c', 'query exists') = '' then do
   call Message 15
   exit 1
   end

/* Find where CODE is installed */
if platform = '400' then
   key = 'EPFINST_ADTS CS/400_5763-CL1_0600'
else
   key = 'EPFINST_CODE/370_5688-194_0000'

codedir = Remove0x(SysIni(epfisini, key, 'FilePath'))
if codedir = 'ERROR:' | codedir = '' then do
   call Message 8
   exit 1
   end

codeini = codedir||'\EVFCCODE.INI'
if stream(codeini, 'c', 'query exists') = '' then do
   call Message 15
   exit 1
   end

code400 = Remove0x(SysIni(codeini, 'INSTALL', 'CODE400'))
if code400 = 'CODE400' then
   code_installed = 1
else
   code_installed = 0

if platform = '400' then
   profile_folder = 'CODE_400_PROFILES'
else
   profile_folder = 'CODE_370_PROFILES'

rc = SysSetObjectData('<'profile_folder'>', "TEMPLATE=NO")
if rc = 0 then do
   if platform = '400' then
      profile_folder = 'ADTSCS'
   else
      profile_folder = 'CODE/370'
   rc = SysSetObjectData('<'profile_folder'>', "TEMPLATE=NO")
   if rc = 0 then do
      if platform = '400' then
          '@'codedir'\BIN\TPUPDATE'
      exit 2
      end
   end

vrpg = Remove0x(SysIni(codeini, 'INSTALL', 'VRPG'))
if vrpg = 'VRPG' then do
   vrpg_installed = 1
   vrpg_samp = Remove0x(SysIni(codeini, 'INSTALL', 'VRPG_SAMPLES'))
   if vrpg_samp = 'VRPG_SAMPLES' then do
      vrpg_samples = 1
      sample_folder = 'VRPG_SAMPLES'
      rc = SysSetObjectData('<'sample_folder'>', "TEMPLATE=NO")
      if rc = 0 then do
         sample_folder = 'VRPG'
         rc = SysSetObjectData('<'sample_folder'>', "TEMPLATE=NO")
         if rc = 0 then do
            '@'codedir'\BIN\TPUPDATE'
            exit 2
            end
         end
      end
   else do
      vrpg_samples = 0
      sample_folder = ''
      end
   end
else
   vrpg_installed = 0

/* Find out if WorkFrame/2 was installed by CODE, and where */
wfdir0x = SysIni(epfisini, 'EPFINST_WorkFrame/2_5622-016_0000', 'FilePath')
wfdir = Remove0x(wfdir0x)

/* Find out if WorkFrame phase 2 has already been successfully run */
phase = Remove0x(SysIni( , 'IBMWFV2', 'PHASE'))

if wfdir <> 'ERROR:' & wfdir <> '' & phase <> 2 then do
   /* Get set to run WorkFrame/2 Phase 2 */
   rc = SysIni( , 'IBMWFV2', 'DIR', wfdir0x)
   rc = SysIni( , 'IBMWFV2', 'OPTIONS', '000090'X)
   rc = SysIni( , 'IBMWFV2', 'PHASE', '1')

   rc = SysIni(bootdrive':\OS2\IBMWFV2.INI', 'IBMWFV2', 'DIR', wfdir0x)
   rc = SysIni(bootdrive':\OS2\IBMWFV2.INI', 'IBMWFV2', 'OPTIONS', '000090'X)
   rc = SysIni(bootdrive':\OS2\IBMWFV2.INI', 'IBMWFV2', 'PHASE', '1')

   '@call' wfdir'\WFDESK.CMD' bootdrive':' wfdir seconds

   /* Check the completion code */
   phase = Remove0x(SysIni( , 'IBMWFV2', 'PHASE'))

   end /* if wfdir <> 'ERROR:' & phase <> 2 */

if vrpg_installed then do
   '@call' codedir'\BIN\FVDIPHS2.CMD' bootdrive codedir wfdir action profile_folder sample_folder
   end

/* Will only continue with this setup if WF 2.5 is installed correctly */
if wfdir <> 'ERROR:' & wfdir <> '' & phase = 2 then do

   if (platform <> '400' | (platform = '400' & code_installed)) then do
      /* Create actions profiles */
      call Message 35
      if platform = '400' then
         '@'codedir'\BIN\EVFPPGEN /'host' /FOLDERID='profile_folder
      else
         '@'codedir'\BIN\EVFPPGEN /MVS /VM /FOLDERID='profile_folder

      select
         when rc = -100 then do
            call Message 25 /* Invalid parameter */
            if platform = '400' then
               '@'codedir'\BIN\TPUPDATE'
            exit 4
            end
         when rc = -200 then do
            call Message 26 /* Unknown PM Error */
            if platform = '400' then
               '@'codedir'\BIN\TPUPDATE'
            exit 4
            end
         when rc = -300 then do
            call Message 27 /* Actions profiles already exist */
            call Message 30 /* Prompt */
            yes = SubstText(31)
            parse upper pull response
            if (left(response, 1) = '''' | left(response, 1) = '"') then response = substr(response, 2)
            if left(response, 1) = left(yes, 1) then do
               if platform = '400' then
                  '@'codedir'\BIN\EVFPPGEN /'host' /REPLACE /FOLDERID='profile_folder
               else
                  '@'codedir'\BIN\EVFPPGEN /MVS /VM /REPLACE /FOLDERID='profile_folder
               if rc <> 0 then do
                  if platform = '400' then
                     '@'codedir'\BIN\TPUPDATE'
                  exit 4
                  end
               end
            end
         when rc = -400 then do
            call Message 28 /* Required parameter not specified */
            if platform = '400' then
               '@'codedir'\BIN\TPUPDATE'
            exit 4
            end
         otherwise do
            nop /* Actions Profiles successfully created */
            end
         end /* select */

      /* register PAM class */
      call SysDeRegisterObjectClass "EVFPPrjFldr"

      rc = SysRegisterObjectClass('EVFPPrjFldr', codedir'\DLL\EVFPFLDR.DLL')
      if rc <> 0 then do
         /* call Message 16 SubstText(18) success */
         nop
         end
      else do
         call Message 17 SubstText(18) /* failure */
         if platform = '400' then
            '@'codedir'\BIN\TPUPDATE'
         exit 5
         end

      lf='0a'x

      /* now create WF/2 projects */

      if platform = '400' then
         folder = '<CODE_400_PROJECTS>'
      else
         folder = '<CODE_370_PROJECTS>'
      rc = SysSetObjectData(folder, "TEMPLATE=NO")
      if rc = 0 then do
         if platform = '400' then
            folder = '<ADTSCS>'
         else
            folder = '<CODE>'
         rc = SysSetObjectData(folder, "TEMPLATE=NO")
         if rc = 0 then folder = ''
         end

      if folder <> '' then do
         if platform = '400' then do
            /* CODE/400 specific projects */
            /* 21: "OS/400 Project" */
            rc = SysCreateObject("EVFPPrjFldr", SubstText(21), folder,,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_400_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFP400;MASK=<*>;", "replace")
            if rc <> 0 then
               call Message 20 SubstText(21) /* success */
            else
               call Message 19 SubstText(21) /* failure */

            rc = SysCreateObject("EVFPPrjFldr", SubstText(21), "<WP_TEMPS>",,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_400_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFP400;TEMPLATE=YES;MASK=<*>;", "replace")
            if rc <> 0 then
               /* call Message 20 SubstText(21) success */
               nop
            else
               call Message 19 SubstText(21) /* failure */


            /* 22: "ADM/400 Project" */
            rc = SysCreateObject("EVFPPrjFldr", SubstText(22), folder,,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_ADM_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFPADM;MASK=<*>;", "replace")
            if rc <> 0 then
               call Message 20 SubstText(22) /* success */
            else
               call Message 19 SubstText(22) /* failure */

            rc = SysCreateObject("EVFPPrjFldr", SubstText(22), "<WP_TEMPS>",,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_ADM_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFPADM;TEMPLATE=YES;MASK=<*>;", "replace")
            if rc <> 0 then
               /* call Message 20 SubstText(22) success */
               nop
            else
               call Message 19 SubstText(22) /* failure */

            /* 23: "OS/400 CS Project" */
            rc = SysCreateObject("EVFPPrjFldr", SubstText(23), folder,,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_400OS2_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFP400;MASK=<LOCAL>*;", "replace")
            if rc <> 0 then
               call Message 20 SubstText(23) /* success */
            else
               call Message 19 SubstText(23) /* failure */

            rc = SysCreateObject("EVFPPrjFldr", SubstText(23), "<WP_TEMPS>",,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_400OS2_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFP400;TEMPLATE=YES;MASK=<LOCAL>*;", "replace")
            if rc <> 0 then
               /* call Message 20 SubstText(23) success */
               nop
            else
               call Message 19 SubstText(23) /* failure */

            /* 24: "Sample Project" */
            rc = SysCreateObject("EVFPPrjFldr", SubstText(24), folder,,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_400_PROFILE>;DEFVIEW=ICON;DIR="codedir";PAM=EVFP400;MASK=<>CODELIB/QRPGSRC(*)"lf"<>CODELIB/QDDSSRC(*);", "replace")
            if rc <> 0 then
               call Message 20 SubstText(24) /* success */
            else
               call Message 19 SubstText(24) /* failure */
            end /* if platform = '400' */
         else do
            /* CODE/370 specific projects */


            /* 33: "MVS Project" */

            rc = SysCreateObject("EVFPPrjFldr", SubstText(33), folder,,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_MVS_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFPMVS;MASK=<*>;", "replace")
            if rc <> 0 then
               call Message 20 SubstText(33) /* success */
            else
               call Message 19 SubstText(33) /* failure */

            rc = SysCreateObject("EVFPPrjFldr", SubstText(33), "<WP_TEMPS>",,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_MVS_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFPMVS;TEMPLATE=YES;MASK=<*>;", "replace")
            if rc <> 0 then
               /* call Message 20 SubstText(33) success */
               nop
            else
               call Message 19 SubstText(33) /* failure */


            /* 34: "VM Project" */

            rc = SysCreateObject("EVFPPrjFldr", SubstText(34), folder,,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_VM_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFPVM;MASK=<*>;", "replace")
            if rc <> 0 then
               call Message 20 SubstText(34) /* success */
            else
               call Message 19 SubstText(34) /* failure */

            rc = SysCreateObject("EVFPPrjFldr", SubstText(34), "<WP_TEMPS>",,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_VM_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFPVM;TEMPLATE=YES;MASK=<*>;", "replace")
            if rc <> 0 then
               /* call Message 20 SubstText(34) success */
               nop
            else
               call Message 19 SubstText(34) /* failure */


            /* 23: "CODE/370 OS/2 Project" */
            rc = SysCreateObject("EVFPPrjFldr", SubstText(23), folder,,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_"host"OS2_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFP"host";MASK=<LOCAL>*;", "replace")
            if rc <> 0 then
               call Message 20 SubstText(23) /* success */
            else
               call Message 19 SubstText(23) /* failure */

            rc = SysCreateObject("EVFPPrjFldr", SubstText(23), "<WP_TEMPS>",,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_"host"OS2_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFP"host";TEMPLATE=YES;MASK=<LOCAL>*;", "replace")
            if rc <> 0 then
               /* call Message 20 SubstText(23) success */
               nop
            else
               call Message 19 SubstText(23) /* failure */


            /* 24: "Sample Project" - always MVS flavour */
            rc = SysCreateObject("EVFPPrjFldr", SubstText(24), folder,,
                               "OPENACTION=~EDIT;EXECACTION=R~UN;PROFILE=<CODE_MVS_PROFILE>;DEFVIEW=ICON;DIR="codedir"\TMP;PAM=EVFPMVS;MASK=<>EQAW.V1R2M0.SEQACLIS(*)"lf"<>EQAW.V1R2M0.SEQASAMP(*);", "replace")
            if rc <> 0 then
               call Message 20 SubstText(24) /* success */
            else
               call Message 19 SubstText(24) /* failure */

            end /* else of : if platform = '400' */
         end /* if folder <> '' */
      end /* if code_installed */

   end /* wfdir <> 'ERROR:' & if phase = 2 */
else if wfdir <> 'ERROR:' & wfdir <> '' then do
   call Message 29 /* WF/2 Phase 2 failed */
   if platform = '400' then
      '@'codedir'\BIN\TPUPDATE'
   exit 3
   end

if platform = '400' then
   '@'codedir'\BIN\TPUPDATE'

call SysDestroyObject "<EVFICPAM>"

call SysDropFuncs

exit


/* Retrieve substitution text from message file */
SubstText:
Parse Arg MsgNo

/* Retrieve the message text with substitution */
String = SysGetMessage(MsgNo, 'EVFIRSTM.MSG')

/* Remove the CR/LF at the end */
String = Remove0x(String)

return String


/* Issue a message with given substitution text */
Message:
Parse Arg MsgNo Subst

/* Retrieve the message text with substitution */
String = SysGetMessage(MsgNo, 'EVFIRSTM.MSG', Subst)

/* Remove the CR/LF at the end */
String = Remove0x(String)

say String
return

/* Get rid of the null termination that C uses for a string     */
/* i.e. extract everything up to, but not including, the '0'x.  */
/* Also removes CR/LF                                           */
Remove0x:
Parse Arg String

End_String = Pos('0'x, String)
If End_String = 0 Then
   End_String = Pos('0d0a'x, String)
If End_String > 0 Then
   String = Left(String, End_String - 1)

return String
