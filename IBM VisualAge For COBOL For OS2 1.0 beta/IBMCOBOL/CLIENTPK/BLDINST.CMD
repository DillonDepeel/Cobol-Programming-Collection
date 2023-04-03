/*
***********************************************************************
**                        SOFTWARE INSTALLER
** 5621-434 (C) COPYRIGHT IBM CORP. 1989, 1995. ALL RIGHTS RESERVED.
**               LICENSED MATERIALS - PROPERTY OF IBM
***********************************************************************
*/
/*
**
** BLDINST.CMD -- Creates INSTALL.IN_ from the renamed copy of Install.
**
** DO NOT MAKE ANY CHANGES TO THIS FILE!
**
*/

'@echo off'
trace 'o'
parse arg ExtraFiles

/* Load the RexxUtil functions */
call RxFuncAdd 'SysFileTree', 'RexxUtil', 'SysFileTree'

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

/* Query the files in the current directory */
CurrentFiles. = ''
SysFTrc = SysFileTree('.\*.*', 'CurrentFiles', 'FO')

/* Read the IIRC.RC to get the included files */
IIRCFile    = "IIRC.RC"
IIRCLines.0 = 0

do while lines(IIRCFile) > 0
   IIRCLines.0 = IIRCLines.0 + 1
   Temp        = IIRCLines.0

   IIRCLines.Temp = linein(IIRCFile)
end   /* end do */

rc = lineout(IIRCFile)       /* close the file */

/* Parse the IIRC.RC */
IncludedFiles.  = ''
IncludedFiles.0 = 0

do I = 1 to IIRCLines.0
   Found = FALSE
   parse value IIRCLines.I with Tag ','  '"'Value'"'
   parse upper var Tag Tag          /* upper case Tag */

   UValue = Value
   parse upper var UValue UValue    /* upper case UValue */

   if Tag = 'INFO1_FILE' |,
      Tag = 'INFO2_FILE' |,
      Tag = 'STARTPARM_RESPFILE'
   then do
      Found = TRUE
   end   /* end then */

   parse value IIRCLines.I with Tag '"'Value'"'
   parse upper var Tag Tag          /* upper case Tag */

   UValue = Value
   parse upper var UValue UValue    /* upper case UValue */

   if Tag = 'INFO1_FILE' |,
      Tag = 'INFO2_FILE' |,
      Tag = 'STARTPARM_RESPFILE'
   then do
      Found = TRUE
   end   /* end then */

   if Found = TRUE
   then do
      /* Add the file to the included list */
      /* but only if exists on the disk    */
      do J = 1 to CurrentFiles.0
         CurrentFiles.J = strip(CurrentFiles.J)
         CurrentFileName = right(CurrentFiles.J, length(UValue))
         parse upper var CurrentFileName CurrentFileName

         if CurrentFileName = UValue
         then do
            IncludedFiles.0 = IncludedFiles.0 + 1
            Temp = IncludedFiles.0
            IncludedFiles.Temp = Value
            leave
         end   /* end then */
      end   /* end do */
   end   /* end then */
end   /* end do */

/* Create the list of files included in INSTALL.IN_ */
'echo' RenamedPrefix'insts.exe > temp.lst'
'echo' RenamedPrefix'iprcs.exe >> temp.lst'
'echo' RenamedPrefix'ipii.dll  >> temp.lst'
'echo' RenamedPrefix'iexts.dll >> temp.lst'
'echo' RenamedPrefix'ihplb.hlp >> temp.lst'
'echo' RenamedPrefix'imsg.msg  >> temp.lst'
'echo' RenamedPrefix'idlds.exe >> temp.lst'
'echo' RenamedPrefix'iupk2.exe >> temp.lst'
'echo' RenamedPrefix'iicis.ico >> temp.lst'

/* Add the files referenced in the IIRC.RC */
do I = 1 to IncludedFiles.0
   'echo' IncludedFiles.I '>> temp.lst'
end   /* end do */

/* Check if xxxIHOOK.DLL is present */
HookDLLName = RenamedPrefix'ihook.dll'
UHookDLLName = HookDLLName
parse upper var UHookDLLName UHookDLLName

do I = 1 to CurrentFiles.0
   CurrentFiles.I = strip(CurrentFiles.I)
   CurrentFileName = right(CurrentFiles.I, length(UHookDLLName))
   parse upper var CurrentFileName CurrentFileName

   if CurrentFileName = UHookDLLName
   then do
      'echo' HookDLLName '>> temp.lst'
      leave
   end   /* end then */
end   /* end do */

/* Parse the input filenames */
ExtraFiles = strip(ExtraFiles)
do while length(ExtraFiles) > 0
   parse value ExtraFiles with ExtraFile ExtraFiles
   'echo' ExtraFile '>> temp.lst'
end   /* end do */

/* Create INSTALL.IN_ */
RenamedPrefix'ipak2 temp.lst install.in_ /L'
'del temp.lst'

/* Drop the RexxUtil functions */
call RxFuncDrop 'SysFileTree'

say
say 'Check for any errors.'
'pause'

exit
