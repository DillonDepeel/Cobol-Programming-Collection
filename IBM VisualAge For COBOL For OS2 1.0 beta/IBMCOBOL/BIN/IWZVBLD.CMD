/******************************************************************************/
/*  Licensed Material - Property of IBM                                       */
/*  28H2177 (C) Copyright IBM Corp., 1995                                     */
/*  All rights reserved.                                                      */
/*  Use Government Users Restricted Rights - Use, duplication or disclosure   */
/*  restricted by GSA ADP Schedule Contract with IBM Corp.                    */
/******************************************************************************/

/* set initial values   */
COBOLOPT  = ''
OTHEROPT  = ''
LINKOPT   = ''
MAKEOPT   = '/NOLOGO '
BUILDOPT  = ''
CICSOPT   = ''
SQLOPT    = ''
DBOPT   = ''
FILESIN   = ''
CICSFLAG = 0
SQLFLAG = 0
MSGOPT = ''
IPFOPT = ''

n = SETLOCAL()

env = 'OS2ENVIRONMENT'
call RxFuncAdd 'SysFileTree', 'RexxUtil', 'SysFileTree'
call RxFuncAdd 'RxMessageBox', 'RexxUtil', 'RxMessageBox'

parse arg argc

/* check for " in first position                                */
/* HPFS file names with embedded spaces are passed in quotes    */
argc = strip(argc)
if pos('"',argc) <> 1 then                        /* no quotes  */
   do
      parse value argc with projectname options
      options = strip(options)
   end
else                                      /* filename in quotes */
   do
      /* look for closing quote  */
      endpos = pos('"',argc,2)
      if endpos = 0 then
         do
            say "Error: No closing quote found"
            exit 8
         end
      projectname = substr(argc,2,endpos-2)
      options     = strip(substr(argc,endpos+1))
   end

if (projectname='' | projectname='?') then
   do
      call Help
      exit 8
   end

/* check if project exists    */
call SysFileTree projectname, 'STEM', 'F'
if STEM.0 = 0 then
   do
      SAY 'Cannot find workframe project file: ' projectname
      exit 8
   end

/* parse all options  */
do while options <> ''
   /* look for COBOL options */
   /* COBOL options begin with - */
   select
      /* check for special -pp option  */
      when translate(substr(options,1,4)) == '-PP"' then
         do
            options = substr(options,5)          /* remove -pp"   */
            qpos = pos('"',options)              /* find ending " */
            if qpos == 0  then
            do
               say 'Error: Unmatched "'
               exit 8
            end /* end do  */
            ppopt   = substr(options,1,qpos-1)   /* get pp options */
            options = strip(substr(options,qpos+1))
            do while ppopt <> ''
               lprenpos   = pos("(",ppopt)
               if lprenpos == 0 then      /* no left pren  */
                  do
                     say 'Error: Missing "(" '
                     exit 8
                  end /* end do  */

               /* find keyword */
               keyword = translate(substr(ppopt,1,lprenpos-1))

               /* find closing pren  */
               rprenpos = pos(')',ppopt)  /* first right pren  */
               if rprenpos == 0 then      /* no right pren  */
                  do
                     say 'Error: Unmatched ()'
                     exit 8
                  end /* end do  */

               /* find next left pren  */
               nlprenpos = pos('(',ppopt,lprenpos+1)

               /* look for other () sets */
               do while (nlprenpos < rprenpos) & (nlprenpos <> 0)
                  rprenpos = pos(')',ppopt,rprenpos+1)
                  if rprenpos == 0 then      /* missing right pren  */
                     do
                        say 'Error: Unmatched ()'
                        exit 8
                     end /* end do  */
                  nlprenpos = pos('(',ppopt,nlprenpos+1)
               end /* do */
               opt   = substr(ppopt,lprenpos+1,rprenpos-lprenpos-1)
               ppopt = strip(substr(ppopt,rprenpos+1))

               select
                  when keyword = 'BUILD'
                       then BUILDOPT = strip(BUILDOPT opt)
                  when keyword = 'CICS' then
                      DO
                         CICSFLAG = 1
                         CICSOPT = strip(CICSOPT opt)
                      END
                  when keyword = 'SQL' then
                     do
                        SQLFLAG = 1
                        SQLOPT = strip(SQLOPT opt)
                     end
                  when keyword = 'DB'
                       then DBOPT = strip(DBOPT opt)
                  otherwise
                       say 'Error: Unrecognized preprocessor option' keyword
                       say '      Option: ' keyword ' was discarded'
               end  /* select */
            end  /* end of -pp loop     */
         end  /* end slect for -pp processing  */

      /* parse -q" option  */
      when translate(substr(options,1,3)) == '-Q"' then
         do
            options = substr(options,4)          /* remove -q"   */
            qpos = pos('"',options)              /* find ending " */
            if qpos == 0  then
            do
               say 'Error: Unmatched "'
               exit 8
            end /* end do  */
            opt      = substr(options,1,qpos-1)   /* get options */
            options  = strip(substr(options,qpos+1))
            COBOLOPT = strip(COBOLOPT opt)
         end

      /* parse -b" option  */
      when translate(substr(options,1,3)) == '-B"' then
         do
            options = substr(options,4)          /* remove -B"   */
            qpos = pos('"',options)              /* find ending " */
            if qpos == 0  then
            do
               say 'Error: Unmatched "'
               exit 8
            end /* end do  */
            opt      = substr(options,1,qpos-1)   /* get options */
            options  = strip(substr(options,qpos+1))
            LINKOPT = strip(LINKOPT opt)
         end

      /* parse -g option  */
      when translate(substr(options,1,3)) == '-G ' then
         do
            parse var options opt options
            BUILDOPT = strip(BUILDOPT 'DEBUG')
         end

      /* parse -p option  */
      when translate(substr(options,1,3)) == '-P ' then
         do
            parse var options opt options
            OTHEROPT = strip(OTHEROPT opt)
            BUILDOPT = strip(BUILDOPT 'DEBUG PROFILE')
         end

      when translate(substr(options,1,1)) == '-' then
         do
            parse var options opt options
            OTHEROPT = strip(OTHEROPT opt)
         end
      otherwise
         do
            /* check for hpfs  */
            if pos('"',options) <> 1 then                 /* no quotes  */
               do
                  parse var options opt options
                  options = strip(options)
               end
            else                                      /* filename in quotes */
               do
                 /* look for closing quote  */
                  endpos = pos('"',options,2)
                  if endpos = 0 then
                     do
                       say "Error: No closing quote found"
                        exit 8
                     end
                  opt     = substr(options,1,endpos)     /* keep quotes  */
                  options = strip(substr(options,endpos+1))
               end

            select
               when pos('.CBL',translate(opt)) <> 0  then
                  FILESIN = strip(FILESIN opt)

               when pos('.EXE',translate(opt)) <> 0  then
                  FILESIN = strip(FILESIN opt)

               when pos('.DLL',translate(opt)) <> 0  then
                  FILESIN = strip(FILESIN opt)

               when pos('.MAP',translate(opt)) <> 0  then
                  FILESIN = strip(FILESIN opt)

               when pos('.DEF',translate(opt)) <> 0  then
                  FILESIN = strip(FILESIN opt)

               when pos('.OBJ',translate(opt)) <> 0  then
                  FILESIN = strip(FILESIN opt)

               when pos('.LIB',translate(opt)) <> 0  then
                  FILESIN = strip(FILESIN opt)

               when pos('.ODF',translate(opt)) <> 0  then
                  nop

               otherwise
               do
                  say 'Error: Unrecognized option or file discarded"' opt '"'
               end /* end do  */
            end  /* end select  */
         end   /* end do  */
   end /* end select  */
end  /* while  */

/* split buildopt into make, build, msg, and ipf  */
bopt = strip(BUILDOPT)
BUILDOPT = ''
do while bopt <> ''
   /* parse var bopt opt bopt */
   select
      when substr(bopt,1,1) = '/' then     /* all / options are for make  */
         do
            parse var bopt opt bopt
            MAKEOPT = strip(MAKEOPT opt)
         end

      when (substr(translate(bopt),1,4) = 'MSG(') then
         do
            bopt = substr(bopt,5)          /* remove msg(   */
            /* find closing pren  */
            rprenpos = pos(')',bopt)  /* first right pren  */
            if rprenpos == 0 then      /* no right pren  */
               do
                  say 'Error: Unmatched ()'
                  exit 8
               end /* end do  */

            /* find next left pren  */
            nlprenpos = pos('(',bopt,1)

            /* look for other () sets */
            do while (nlprenpos < rprenpos) & (nlprenpos <> 0)
               rprenpos = pos(')',bopt,rprenpos+1)
               if rprenpos == 0 then      /* missing right pren  */
                  do
                     say 'Error: Unmatched ()'
                     exit 8
                  end   /* end do  */
               nlprenpos = pos('(',bopt,nlprenpos+1)
            end /* do */
            MSGOPT  = substr(bopt,1,rprenpos-1)
            bopt = strip(substr(bopt,rprenpos+1))
         end  /* when MSG  */

      when (substr(translate(bopt),1,4) = 'IPF(') then
         do
            bopt = substr(bopt,5)          /* remove msg(   */
            /* find closing pren  */
            rprenpos = pos(')',bopt)  /* first right pren  */
            if rprenpos == 0 then      /* no right pren  */
               do
                  say 'Error: Unmatched ()'
                  exit 8
               end /* end do  */

            /* find next left pren  */
            nlprenpos = pos('(',bopt,1)

            /* look for other () sets */
            do while (nlprenpos < rprenpos) & (nlprenpos <> 0)
               rprenpos = pos(')',bopt,rprenpos+1)
               if rprenpos == 0 then      /* missing right pren  */
                  do
                     say 'Error: Unmatched ()'
                     exit 8
                  end   /* end do  */
               nlprenpos = pos('(',bopt,nlprenpos+1)
            end /* do */
            IPFOPT  = substr(bopt,1,rprenpos-1)
            bopt = strip(substr(bopt,rprenpos+1))
         end  /* when IPF  */

      when (substr(translate(bopt),1,5) = 'DEBUG') then
         do
            parse var bopt opt bopt
          /* delete next line PWS  */
          /*  BUILDOPT = strip(BUILDOPT 'IWZVDEBUG=Y')  */
         end

      when (substr(translate(bopt),1,7) = 'PROFILE') then
         do
            parse var bopt opt bopt
            BUILDOPT = strip(BUILDOPT 'IWZVPA=IWZPA.OBJ')
         end

      when (substr(translate(bopt),1,7) = 'COPYDLL') then
         do
            parse var bopt opt bopt
            BUILDOPT = strip(BUILDOPT 'IWZVTEST=Y')
         end

      when (substr(translate(bopt),1,6) = 'BACKUP') then
         do
            parse var bopt opt bopt
            BUILDOPT = strip(BUILDOPT 'IWZVBACKUP=Y')
         end

      otherwise
         parse var bopt opt bopt
         say 'Error: Unrecognized option ignored "' || opt || '"'

   end  /* select */
end /* while */

/* when translate(substr(options,1,1)) == '-' then  */
/* invoke program to retrieve workframe info */
'IWZVBLDE.EXE '||'"'||projectname||'"'

/* if current directory is Target: RT_OS2, then backup one directory  */
/* get current directory  */
curdir = directory()
dirname = Substr(curdir,Lastpos('\',curdir)+1)
if dirname = 'RT_OS2' then '@cd ..'

/* check if COBOL source exists    */
call SysFileTree '*.CBL', 'STEM', 'F'
if STEM.0 = 0 then DO
                      SAY 'COBOL source is missing'
                      exit 8
                   end

/* check is cobsenv.cmd exists    */
call SysFileTree 'COBSENV.CMD', 'STEM', 'F'
if STEM.0 = 0 then
   do
      SAY 'Error: IWZVBLDE.EXE did not create COBSENV.CMD'
      exit 8
   end

/* add variable to COBSENV.CMD  */
call LINEOUT 'COBSENV.CMD',"SET COBOLOPT=" || COBOLOPT
if RESULT = 1 then call cwrite
call LINEOUT 'COBSENV.CMD',"SET OTHEROPT=" || OTHEROPT
if RESULT = 1 then call cwrite
call LINEOUT 'COBSENV.CMD',"SET LINKOPT=" || LINKOPT
if RESULT = 1 then call cwrite
call LINEOUT 'COBSENV.CMD',"SET FILESIN=" || FILESIN
if RESULT = 1 then call cwrite
if CICSFLAG = 1 then
   do
      call LINEOUT 'COBSENV.CMD',"SET CICSOPT=CICS(" || CICSOPT || ")"
      if RESULT = 1 then call cwrite
   end

if SQLFLAG = 1 then
   do
      call LINEOUT 'COBSENV.CMD',"SET SQLOPT=SQL(" || SQLOPT || ")"
      if RESULT = 1 then call cwrite
      call LINEOUT 'COBSENV.CMD',"SET DBOPT=DB(" || DBOPT || ")"
      if RESULT = 1 then call cwrite
   end

do while buildopt <> ''
   parse var BUILDOPT opt BUILDOPT
   call LINEOUT 'COBSENV.CMD',"SET" opt
   if RESULT = 1 then call cwrite
end

call LINEOUT 'COBSENV.CMD',"SET IWZVMSGOPT=" || MSGOPT
if RESULT = 1 then call cwrite


call LINEOUT 'COBSENV.CMD',"SET IWZVIPFOPT=" || IPFOPT
if RESULT = 1 then call cwrite

call LINEOUT 'COBSENV.CMD'

"CALL COBSENV"

makefile = value('ICOBDIR', , env) || '\bin\IWZVOS2.mak'
'nmake' makeopt ' /f' makefile
makerc = rc
say "make complete. Return code: " makerc
n = ENDLOCAL()
return (makerc)

cwrite:
      say "Error: Unsuccessful write to file: COBSENV.CMD"
      exit 8
return

Help:
   say
   say 'This command file is called from the GUI builder'
   say 'It initiates the build process.'
   say
   say 'IWZVBLD c:\desktop\WorkFrameProgect options '
   say
return

