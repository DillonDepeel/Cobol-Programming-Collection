/******************************************************************************/ 
/*  Licensed Material - Property of IBM                                       */ 
/*  28H2177 (C) Copyright IBM Corp., 1995                                     */ 
/*  All rights reserved.                                                      */ 
/*  Use Government Users Restricted Rights - Use, duplication or disclosure   */ 
/*  restricted by GSA ADP Schedule Contract with IBM Corp.                    */ 
/******************************************************************************/ 

call RxFuncAdd 'SysFileTree', 'RexxUtil', 'SysFileTree'
call RxFuncAdd 'SysFileDelete', 'RexxUtil', 'SysFileDelete'
call RxFuncAdd 'SysMkDir', 'RexxUtil', 'SysMkDir'

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
if STEM.0 = 0 then
         DO
            rc = SysMkDir('IWZVTEMP');
            if rc \= 0 then return rc;
         end

/* copy current files to temp directory  */
copycount = 0
call SysFileTree '.\RT_OS2\' || appname || '.odx', 'file', 'F'
if file.0 \= 0 then
   do
      '@copy .\RT_OS2\' || appname || '.odx' 'IWZVTEMP'
      if rc \= 0 then exit rc;
      copycount = copycount + 1;
   end;

call SysFileTree appname || '.def', 'file', 'F'
if file.0 \= 0 then
   do
      '@copy ' appname||'.def' 'IWZVTEMP'
      if rc \= 0 then exit rc;
      copycount = copycount + 1;
   end;

call SysFileTree appname || '.cpy', 'file', 'F'
if file.0 \= 0 then
   do
      '@copy ' appname||'.cpy' 'IWZVTEMP'
      if rc \= 0 then exit rc;
      copycount = copycount + 1;
   end;

call SysFileTree appname || '.dll', 'file', 'F'
if file.0 \= 0 then
   do
      '@copy ' appname||'.dll' 'IWZVTEMP'
      if rc \= 0 then exit rc;
      copycount = copycount + 1;
   end;

if copycount \= 0  then
say copycount 'File(s) copied to IWZVTEMP directory as backup'

return 0;
