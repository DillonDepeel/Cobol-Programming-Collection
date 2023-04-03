/******************************************************************************/ 
/*  Licensed Material - Property of IBM                                       */ 
/*  28H2177 (C) Copyright IBM Corp., 1995                                     */ 
/*  All rights reserved.                                                      */ 
/*  Use Government Users Restricted Rights - Use, duplication or disclosure   */ 
/*  restricted by GSA ADP Schedule Contract with IBM Corp.                    */ 
/******************************************************************************/ 

call RxFuncAdd 'SysFileTree', 'RexxUtil', 'SysFileTree'
call RxFuncAdd 'SysFileDelete', 'RexxUtil', 'SysFileDelete'

env  = 'OS2ENVIRONMENT'

appname = VALUE('IWZVAPPNAME',, env)
if appname = '' then do
   say 'ERROR: IWZVAPPNAME environment variable not set'
   return 1;
   end;

srcdir = VALUE('IWZVSOURCEDIR',, env)
if srcdir = '' then do
   say 'ERROR: IWZVSOURCEDIR environment variable not set'
   return 1;
   end;

dirrc = directory(srcdir)
if TRANSLATE(dirrc) \= TRANSLATE(srcdir) then do
   say 'ERROR: Missing directory:',
       '"' || srcdir ||'"';
   return 1;
   end;

call SysFileTree 'IWZVTEMP', 'stem', 'D'
if STEM.0 = 0 then return 1

/* delete files   */
call SysFileTree 'IWZVTEMP\*.*', 'file', 'F'
delcount = 0
if file.0 \= 0 then
   do i=1 to file.0
      delfile = word(file.i,words(file.i))
      rc = SysFileDelete(delfile)
      if rc \= 0 then return rc;
      delcount = delcount + 1;
   end  /* Do */

if delcount \= 0  then
say delcount 'backup File(s) deleted after successful build'

return 0;
