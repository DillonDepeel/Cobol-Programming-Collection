/*** IWZVCOMP.CMD                                                                     */
/***                                                                                  */
/***            28H2177 (C) Copyright IBM Corp. 1995.                                 */
/***            LICENSED MATERIAL - PROGRAM PROPERTY OF IBM                           */
/***                                                                                  */
/*** Program Requirement: REXX installed:                                             */
/***                                       OS/2 1.x, 2.x Installed                    */
/***                                       Interpretive Command Execution             */
/***                                       using REXX                                 */
/*** Language Used:  REXX                                                             */
/*** Program Invocation:  IWZVCOMP cob2_options -PP"SQL(sql_options)"                 */
/***                               -PP"DB(database-name)"                             */
/***                               -PP"CICS(cics_options)" filename1.CBL...           */
/***                               linkfile1...                                       */
/***                                                                                  */
/*** Function:                                                                        */
/***   Determines preprocessing (DB2/2 or CICS) required by a COBOL source program,   */
/***   based on whether -PP"SQL()" or -PP"CICS()" were specified.                     */
/***   Does the preprocessing, followed by compile.  Uses temporary files to handle   */
/***   file extension requirements of preprocessors.  Sets APOST/QUOTE option for     */
/***   CICS preprocessor and compiler.                                                */
/***                                                                                                     */
/*** INPUT: cob2_options (can be -Q"xxx", -I"xxx", -c, B"xxx" etc.)                                      */
/***        sql_options (DB2/2 precompiler options)                                                      */
/***        database_name (DB2 database to connect to for precompile)                                    */
/***        cics_options (CICS for OS/2 preprocessor options)                                            */
/***        filename1.CBL... (names of the files to compile)                                             */
/***        linkfile1... (names of files to use in the link)                                             */
/***                                                                                                     */
/***        The cob2_options, sql_options, and cics_options must not contain double quotes (") and       */
/***        any parens in them must be matched.                                                          */
/***                                                                                                     */
/*** OUTPUT:  filename.PPR (output from preprocessing, input to compiler)                                */
/***          filename.TRL (CICS preprocessor listing)                                                   */
/***          filename.OBJ and filename.LST (compiler listing)                                           */
/***                                                                                                     */
/*** Assumptions:                                                                                        */
/***   - File to be preprocessed and compiled is either in current directory or is fully qualified       */
/***   - If COPY statement is used by program, the -I"pathname" compiler option has been specified       */
/***   - For SQL preprocessing, DB2 database has already been started                                    */
/***   - Linking CICS and DB2 applications requires the user to name all necessary libs and special      */
/***     link options themselves -- no defaults are set up for them.                                     */
/***   - Name of database to precompile with is in DB2DB environment variable (my invention)             */
/***     Note:  need more general solution?                                                              */
/***                                                                                                     */
/*** Limitations:                                                                                        */
/***   - Addresses only case where SQL package is generated as part of precompile.  Can easily be        */
/***     extended.                                                                                       */
/***   - Need to add code to check whether already connected to database                                 */
'@ECHO OFF'
trace o
mytrace = 0

   parse arg parm

   /* set variables to be used if link is requested */
   keepparm = parm /* keep a copy of the input argument - will be used as input to COB2 to do the link */
   objfiles = '' /* list of .obj files that were output from compile */

   call RxFuncAdd 'SysFileDelete' , 'RexxUtil', 'SysFileDelete'
   call RxFuncAdd 'SysFileTree','RexxUtil','SysFileTree'
   call RxFuncAdd 'SysFileSearch','RexxUtil','SysFileSearch'

   maxgoodrc = 4

   returncode = 0

   /* SYSLIB may need to be changed, then reset */
   new_syslib = ''
   reset_syslib = 'no'
   did_connect = 'no'
   sqldone = 'no'
   cicsdone = 'no'

   /* make sure the temporary directory name is in a usable form:                              */
   /* - if there are multiple directories specified, use the first one                         */
   /* - if the directory ends in \ and\or ;, strip it off                                      */
   /* - make sure hpfs directory names are handled properly                                    */
   environ  = 'OS2ENVIRONMENT'
   tempdir  = strip(VALUE('TMP',,environ))
   tempdirhpfs = 'n'
   if length(tempdir) >0 then /* if temp directory env var set up */
     if substr(tempdir,1,1) = '"' then /* if hpfs name */
       do
         endpos = pos('"',tempdir,2) /* look for closing quote */
         if endpos = 0 then /* no closing quote */
           do
             say 'Error:  no closing quote found in TMP environment variable'
             sqlrc = maxgoodrc + 4
             return sqlrc
           end
         else /* closing quote found in hpfs name */
           do
             tempdir = substr(tempdir,1,endpos-1) /* temporarily remove ending quote */
             if lastpos('\',tempdir) = length(tempdir) then  /* if temp directory name ends in backslash */
               tempdir = substr(tempdir,1,length(tempdir)-1) /* remove backslash */
             tempdir = tempdir || '"' /* put closing quote back on */
             tempdirhpfs = 'y'
           end
       end
     else /* not an hpfs name */
       do
         if pos(';',tempdir) > 0 then  /* could be multiple paths strung together */
           tempdir = substr(tempdir,1,pos(';',tempdir)-1)  /* pick up first path */
         if lastpos('\',tempdir) = length(tempdir) then  /* if temp directory name ends in backslash */
           tempdir = substr(tempdir,1,length(tempdir)-1) /* remove backslash */
       end
   else /* no temp directory specified */
     do
       say 'Error: the TMP environment variable is null.  It must be set to a temporary directory '
       say '       name in order to compile.'
       returncode = maxgoodrc + 4
       call end_program
     end

   /* parse the input parameter to get the compiler options, preprocessor options and   */
   /* name of file to preprocess/compile                                                */
   call parseparm
   returncode = max(RESULT,returncode)
   if returncode > maxgoodrc then
     do
       call end_program
     end

   do while infiles <> ''

     prepdone = 'no'

     infiles = strip(infiles)
     if pos('"',infiles) <> 1 then                        /* no quotes  */
       do
         parse var infiles infile infiles
         HPFS=0
       end
     else /* hpfs file in quotes */
       do
         endpos = pos('"',infiles,2) /* look for closing quote */
         if endpos = 0 then
           do
             say 'Error: No closing quote found'
             exit maxgoodrc+4
           end
         else /* closing quote found */
           do
             infile = substr(infiles,2,endpos-2)
             infiles = strip(substr(infiles,endpos+1))
             HPFS=1
           end
       end

     currdir = directory()
     currdrv  = filespec('drive',currdir)

     /* Get file's path and verify file exists */
     filedrv = filespec('drive',infile)
     filepath = filespec('path',infile)
     if filepath \= '' then filepath = substr(filepath,1,length(filepath)-1)
     parse value filespec('name',infile) with filename '.' fileext

     filedir = filedrv || filepath
     if filedir = '' then filedir = currdir

     /* Construct fully qualified filename */
     file = filedir || '\' || filename
     if fileext \= '' then file = file || '.' || fileext

     /* construct name of .obj file and add it to the list of .obj files that are output from compile */
     objfiles = objfiles filename || '.obj'

     if mytrace == 1 then
       do
         say 'infile=' infile
         say 'filedrv=' filedrv
         say 'filepath=' filepath
         say 'filedir=' filedir
         say 'filename=' filename
         say 'file=' file
         say 'objfiles=' objfiles
       end

     call SysFileTree file, 'found', 'F'
     IF  RESULT = '0' THEN
       DO
         IF found.0 = 0 THEN
           do
             say 'ERROR:  File' file 'not found'
             exit maxgoodrc+4
           END
       end

     /* Construct the name of the intermediate file (output from preprocessing, input to compiler */
     ppfile = currdir || '\' || filename || '.ppr'

     /* Find a tempname that won't conflict with anything else in the current directory */
     tempname = date(d)||time(s)
     if tempdirhpfs = 'n' then tempcbl = tempdir || '\' || tempname || '.CBL'
     else tempcbl = substr(tempdir,1,length(tempdir)-1) || '\' tempname || '.CBL"'

     /* Find out if source code requires SQL or CICS preprocessing */
     /* call iwzvpprs file */
     /* sourcetype = RESULT */

     /* Let the user know what we're about to do */
     if sourcetype \= '' then
       do
         if run = 'yes' then
           do
             message = 'The following preprocessing will be performed on'
             message = message 'file' infile || ':'  sourcetype
             say message
           end
       end

     /* Call CICS preprocessor script based on source type */
     if pos('CICS',sourcetype) \=0 then
       do
         call cicsprep
         returncode = max(RESULT,returncode)
         if RESULT > maxgoodrc then
           do
             call end_program
           end
         prepdone = 'yes'
       end

     /* Call DB2 preprocessor script based on source type */
     if pos('SQL',sourcetype) \=0 then
       do
         call db2prep
         returncode = max(RESULT,returncode)
         if RESULT > maxgoodrc then
           do
             call end_program
           end
         prepdone = 'yes'
       end

     /* Call COBOL compiler */
     call cobcomp
     returncode = max(RESULT,returncode)
     if returncode > maxgoodrc then
       do
         call end_program
       end
   end /* while infiles <> '' */

   /* Call routine to do link */
   if link = 'yes' then
     do
       call linkit
       returncode = max(RESULT,returncode)
       if returncode > maxgoodrc then
         do
           call end_program
         end
       end

   exit returncode


   /* DB2/2 Preprocessing */
   db2prep:

     /* may add some SQL options -- keep original around in case there are multiple files to process */
     keepsqlopt = sqlopt

     /* Get environment variables */
     environ  = 'OS2ENVIRONMENT'
     db2path  = strip(VALUE('DB2PATH',,environ))

     /* make sure the db2 path name is in a usable form:                                         */
     /* - if there are multiple directories specified, use the first one                         */
     /* - if the directory ends in \ and\or ;, strip it off                                      */
     /* - make sure hpfs directory names are handled properly                                    */
     db2pathhpfs = 'n'
     if length(db2path) >0 then /* if db2/2 path env var set up */
       if substr(db2path,1,1) = '"' then /* if hpfs name */
         do
           endpos = pos('"',db2path,2) /* look for closing quote */
           if endpos = 0 then /* no closing quote */
             do
               say 'Error:  no closing quote found in DB2PATH environment variable'
               sqlrc = maxgoodrc + 4
               return sqlrc
             end
           else /* closing quote found in hpfs name */
             do
               db2path = substr(db2path,1,endpos-1) /* temporarily remove ending quote */
               if lastpos('\',db2path) = length(db2path) then  /* if db2path directory name ends in backslash */
                 db2path = substr(db2path,1,length(db2path)-1) /* remove backslash */
               db2path = db2path || '"' /* put closing quote back on */
               db2pathhpfs = 'y'
             end
         end
       else /* not an hpfs name */
         do
           if pos(';',db2path) > 0 then  /* could be multiple paths strung together */
             db2path = substr(db2path,1,pos(';',db2path)-1)  /* pick up first path */
           if lastpos('\',db2path) = length(db2path) then  /* if db2path name ends in backslash */
             db2path = substr(db2path,1,length(db2path)-1) /* remove backslash */
         end
     else /* db2path env variable not specified */
       do
         say 'Error: the DB2PATH environment variable is null.  It must be set to the name of the DB2'
         say '       path in order to do an SQL precompile.'
         sqlrc = maxgoodrc + 4
         return sqlrc
       end

     if tempdirhpfs = 'n' then tempsqb = tempdir || '\' || tempname || '.SQB'
     else tempsqb = substr(tempdir,1,length(tempdir)-1) || '\' tempname || '.SQB"'
     sqlrc = 0

     call SysFileDelete tempsqb
     /* If some preprocessing has already been done, use tempname.CBL as input to             */
     /* the DB2 preprocessor.  Otherwise, use the original .CBL. In either case, copy it to   */
     /* tempname.SQB first                                                                    */
     if prepdone = 'no' then copy file tempsqb
     else
       do
         copy tempcbl tempsqb
         call SysFileDelete tempcbl
       end

     command = 'db2 prep' tempsqb sqlopt 'TARGET IBMCOB'
     if show = 'yes' then say command
     if run = 'yes' then
       do

         if dbname \= '' then /* if a db name was specified (user may prefer to connect to db himself) */
           do
             /* check to see whether we're already connected to the database.  Involves setting */
             /* up file in temp directory where the connection information will go.             */
             if tempdirhpfs = 'y' then /* temp directory has an hpfs name */
               dbstatefile = substr(tempdir,1,length(tempdir)-1) || '\' || date(d)||time(s) || '.con"' /* file were the connection state will go */
             else /* temp directory does not have an hpfs name */
               dbstatefile = tempdir || '\' || date(d)||time(s) || '.con' /* file were the connection state will go */
             call SysFileDelete dbstatefile /* get rid of old one if it exists */
             address cmd 'db2 -o- -r' || dbstatefile 'get connection state'
             sqlrc = max(rc,sqlrc)
             if sqlrc >= 4 then
               do
                 say 'Error:  Return code' sqlrc 'returned from DB2 get connection state command.'
                 sqlrc = maxgoodrc + 4
               end
             else /* no error from DB2 get connection state command */
               do

                 /* check output from get connection state to see if database already connected */
                 call SysFileSearch dbname, dbstatefile, hits
                 if hits.0 = 0 then /* if database not connected */
                   do
                     say 'Connecting to database' dbname '...'
                     address cmd 'db2 connect to' dbname
                     sqlrc = max(rc,sqlrc)
                     did_connect = 'yes'
                   end /* end database not connected */
               end /* end check output from get connection state */
             call SysFileDelete dbstatefile /* get rid of file containing connection info */
           end /* end database name was specified */

         if sqlrc <= maxgoodrc then /* if the connect logic above was all ok */
           do

             say 'Starting SQL precompile...'
             address cmd command

             /* set the returncode from preprocessing.  4 or greater is an error for DB2/2... if that's */
             /* what we got, bump it up so we know to quit                                              */
             sqlrc = max(rc,sqlrc)

             if sqlrc >= 4 then
               do
                 say 'Error:  Return code' sqlrc 'returned from DB2 preprocessing.'
                 sqlrc = maxgoodrc + 4
                 call SysFileDelete tempcbl
               end
             else say 'DB2 preprocessing complete, return code =' sqlrc

           end
       end

     /* if this is the first file for which SQL preprocessing has been done, then */
     /* set up stuff for the compile and link steps.                              */
     if sqldone = 'no' then
       do
         /* add compiler options */
         compopt = compopt || ' -q"pgmname(mixed) lib" '

         /* add db2 copyfile path to SYSLIB for the compile step.  may need to add ";" to separate */
         if db2pathhpfs = 'y' then /* if db2 path is an hpfs name */
           db2incl = substr(db2path,1,length(db2path)-1) || '\include\cobol_a"'
         else /* db2 path is not an hpfs name */
           db2incl = db2path || '\include\cobol_a'
         new_syslib = strip(new_syslib)
         if new_syslib \= '' & lastpos(';',new_syslib) \= length(new_syslib) then
             new_syslib = new_syslib || ';' || db2incl || ';'
         else
           new_syslib = new_syslib || db2incl || ';'

         /* add link options */
         linkopt = linkopt '-b"/noi"'

         /* add DB2 library to list of link files */
         linkfiles = linkfiles 'DB2API.LIB'
       end

     if mytrace = 1 then
       do
         'dir' currdir '*.sqb'
         'dir' tempdir '*.cbl'
         'dir' currdir '*.bnd'
       end

     /* cleanup */
     sqldone = 'yes'
     call SysFileDelete tempsqb
     sqlopt = keepsqlopt

   return sqlrc

   cicsprep:

     environ  = 'OS2ENVIRONMENT'
     cicsset  = VALUE('CICSSET',,environ)

     tempccp = currdir || '\' || tempname||'.CCP'
     temptrl = currdir || '\' || tempname||'.TRL'

     cicsrc = 0

     call SysFileDelete tempccp
     /* If some preprocessing has already been done, use tempname.CBL as input to              */
     /* the CICS preprocessor.  Otherwise, use the original .CBL. In either case, copy it to   */
     /* tempname.CCP first                                                                     */
     if prepdone = 'no' then copy file tempccp
     else
       do
         copy tempcbl tempccp
         call SysFileDelete tempcbl
       end

     command = 'faaprpdc' tempccp cicsopt

     if show = 'yes' then say command
     if run = 'yes' then
       do

         /* set up CICS environment variables */
         if cicsset = '' then call 'cicsenv'

         /* set up name for file that CICS translator return code will be passed back in */
         if tempdirhpfs = 'y' then /* temp directory has an hpfs name */
           rcfile = substr(tempdir,1,length(tempdir)-1) || '\' || date(d)||time(s) || '.ret"'
         else /* temp directory does not have an hpfs name */
           rcfile = tempdir || '\' || date(d)||time(s) || '.ret'

         'iwzvcics' tempccp rcfile cicsopt

         /* set the returncode from preprocessing */
         cicsrc = strip(linein(rcfile))
         call lineout rcfile /* close the return code file */
         if cicsrc >= 8 | cicsrc < 0 | cicsrc = '' then
           do
             if cicsrc = '' | datatype(cicsrc) \= 'NUM' then  /* returncode not set! */
               do
                 say 'Error:  Null return code returned from CICS preprocessing.'
                 cicsrc = 16
               end
             else /* return code was set */
               do
                 say 'Error:  Return code' cicsrc 'returned from CICS preprocessing.'
                 cicsrc = max(cicsrc,maxgoodrc+4)
               end
             call SysFileDelete tempcbl
           end
         else say 'CICS preprocessing complete, return code =' cicsrc

         if mytrace = 1 then
           do
             'dir' currdir '*.ccp'
             'dir' tempdir '*.cbl'
             'dir' currdir '*.trl'
           end

         /* cleanup */
         'copy' temptrl filename || '.TRL'
         call SysFileDelete tempccp
         call SysFileDelete temptrl
         call SysFileDelete rcfile /* get rid of return code file */

       end

     /* if this is the first file for which CICS preprocessing has been done, then*/
     /* set up stuff for the compile and link steps.                              */
     if cicsdone = 'no' then
       do
         /* add compiler options and files for the link */
         compopt = compopt '-q"nosequence"'
         linkfiles = linkfiles 'FAACBID.LIB FAASR32.LIB FAAOTSLK.LIB FAAOTSCB.LIB'
       end
     cicsdone = 'yes' /* flag CICS preprocessing as having been done now */

   return cicsrc

   /* COBOL Compile */
   cobcomp:
     comprc = 0

     /* get SYSLIB as currently set in the environment, then add new stuff to it.  Anything       */
     /* automatically added will be put on the end (e.g. db2 include path).  Any copyfile include */
     /* paths entered via the compiler options dialog will be put in front.  Add a semi-colon to  */
     /* separate if necessary                                                                     */

     environ  = 'OS2ENVIRONMENT'
     orig_syslib  = strip(VALUE('SYSLIB',,environ))
     if orig_syslib \= '' & lastpos(';',orig_syslib) \= length(orig_syslib) then
         new_syslib = ipath || orig_syslib || ';' || new_syslib
     else
       new_syslib = ipath || orig_syslib || new_syslib

     /* if no preprocessing has been done, use the original .CBL.  Otherwise, use the .CBL with   */
     /* the temp name -- this is the output from the preprocessor(s).                             */
     if prepdone = 'no' then
       compfile = infile
     else /* preprocessing done */
       compfile = ppfile

       /* set up the command and run it */
       command = 'igyccob2' compopt compfile
       if showcompmsgs = 'no' then
         command = command '> nul'
       if show = 'yes' then say command
       if run = 'yes' then
         do
           if new_syslib \= orig_syslib then /* must reset SYSLIB to include new paths */
             do
               address cmd 'SET SYSLIB=' || new_syslib
               reset_syslib = 'yes'
               if mytrace = 1 then say 'syslib reset to' new_syslib
             end

           /* if preprocessing was done, rename the preprocessed file from the ugly temporary filename */
           /* to one that matches the original source file name, but with the .ppr extension           */
           if prepdone = 'yes' then
             do
               call SysFileDelete ppfile
              'copy' tempcbl ppfile
               call SysFileDelete tempcbl
             end

           /* The user can control whether the compiler creates an .obj file, using the */
           /* NOCOMPILE(rc) compiler option (which could be specified within the source */
           /* code).  If he does this, we want to make very sure that the link doesn't  */
           /* proceed with an old .obj.  So if an .obj exists before the compile, erase */
           /* it.                                                                       */
           call SysFileTree fileobj, 'found', 'F'
           IF  RESULT = '0' THEN
               IF found.0 \= 0 THEN
                  call SysFileDelete fileobj

           say 'Starting COBOL compile of ' infile
           address cmd command
           comprc = rc
           say 'COBOL compile complete, return code =' comprc
         end

       /* the compiler can give negative return codes.  Fatal error when this happens */
       if comprc < 0 then comprc = 16

       /* cleanup */
       if reset_syslib = 'yes' then address cmd 'SET SYSLIB=' || orig_syslib

   return comprc


   /* Link */
   linkit:
     linkrc = 0
     command = 'cob2' linkopt objfiles linkfiles 'SOMTK.LIB'
     if run = 'yes' then say 'Starting link...'
     address cmd command
     linkrc = rc
     if run = 'yes' then say 'Link complete, return code =' linkrc

   return linkrc


   /* parse input parameter */
   parseparm:

   compopt   = ''
   CICSOPT   = ''
   SQLOPT    = ''
   IPATH     = ''
   SHOW      = 'no'
   RUN       = 'yes'
   LINK      = 'yes'
   infiles   = ''
   linkfiles = ''
   linkopt = ''
   sourcetype = ''
   dbname = ''
   showcompmsgs = 'no'

   optrc = 0

   /* process the options.  they begin with '-' */
   parm = strip(parm)
   do while parm <> '' & translate(substr(parm,1,1)) == '-'

      /* figure out what kind of option it is */
      select
      /* look for COBOL options (they begin with -) */
      /* check for special -pp option  */
      when translate(substr(parm,1,4)) == '-PP"' then
         do
            qpos = pos('"',parm,5)              /* find ending " */
            if qpos == 0  then
            do
               say 'Error: Unmatched "'
               exit maxgoodrc+4
            end /* end do  */

            ppopt = substr(parm,1,qpos) /* -pp option, including -pp and quotes */

            /* This is a special preprocessing option not understood by COB2 */
            /* or the linker... remove it from  keepparm (keepparm is only   */
            ppstart = pos(translate(ppopt),translate(keepparm))
            ppend = ppstart + length(ppopt) - 1
            keepparm = substr(keepparm,1,ppstart-1) || substr(keepparm,ppend+1)

            parm = strip(substr(parm,qpos+1))

            ppopt = substr(ppopt,5,qpos-5) /* remove surrounding -pp and quotes */
            do while ppopt <> ''
               lprenpos   = pos("(",ppopt)
               if lprenpos == 0 then      /* no left pren  */
                  do
                     say 'Error: Missing "(" '
                     exit maxgoodrc+4
                  end /* end do  */

               /* find keyword */
               keyword = translate(substr(ppopt,1,lprenpos-1))

               /* find closing pren  */
               rprenpos = pos(')',ppopt)  /* first right pren  */
               if rprenpos == 0 then      /* no right pren  */
                  do
                     say 'Error: Unmatched ()'
                     exit maxgoodrc+4
                  end /* end do  */

               /* find next left pren  */
               nlprenpos = pos('(',ppopt,lprenpos+1)

               /* look for other () sets */
               do while (nlprenpos < rprenpos) & (nlprenpos <> 0)
                  rprenpos = pos(')',ppopt,rprenpos+1)
                  if rprenpos == 0 then      /* missing right pren  */
                     do
                        say 'Error: Unmatched ()'
                        exit maxgoodrc+4
                     end /* end do  */
                  nlprenpos = pos('(',ppopt,nlprenpos+1)
               end /* do */
               opt   = substr(ppopt,lprenpos+1,rprenpos-lprenpos-1)
               ppopt = strip(substr(ppopt,rprenpos+1))

               select
                  when keyword = 'CICS' then
                       do
                         sourcetype = sourcetype 'CICS'
                         CICSOPT = strip(CICSOPT opt)
                       end
                  when keyword = 'SQL' then
                       do
                         sourcetype = sourcetype 'SQL'
                         SQLOPT = strip(SQLOPT opt)
                       end
                  when keyword = 'DB' then
                       dbname = strip(opt)
                  otherwise
                       do
                         say 'Error: Unrecognized ppreprocessor option' keyword
                         say '      Option: ' keyword 'was discarded'
                       end
               end  /* select */
            end  /* end of -pp loop     */
         end  /* end of -pp processing  */

      /* check for -q option  */
      when translate(substr(parm,1,3)) == '-Q"' then
                do
                  qpos = pos('"',parm,4)      /* find ending " */
                  if qpos == 0  then
                     do
                        say 'Error: Unmatched "'
                        exit maxgoodrc+4
                     end /* end do  */
                  opt     = substr(parm,1,qpos)   /* get parm */
                  parm = strip(substr(parm,qpos+1))
                  compopt = strip(compopt opt)
               end

      when translate(substr(parm,1,2)) == '-Q' then
                do
                  parse var parm opt parm
                  compopt = strip(compopt opt)
               end

      /* check for -b (link) option  */
      when translate(substr(parm,1,3)) == '-B"' then
                do
                  qpos = pos('"',parm,4)      /* find ending " */
                  if qpos == 0  then
                     do
                        say 'Error: Unmatched "'
                        exit maxgoodrc+4
                     end /* end do  */

                  parm = strip(substr(parm,qpos+1))
               end


      /* check for -i option  */
      /* pws changed to look for -I instead of -I"  */
      when translate(substr(parm,1,2)) == '-I' then
        do
          parse var parm opt parm
          opt = substr(opt,3)          /* remove -i   */
          ipath   = ipath || opt        /* add to copyfile path */
          if lastpos(';',ipath) \= length(ipath) then ipath = ipath || ';'
          parm = strip(parm)
        end

      /* check for -g or -p option  */
      when translate(substr(parm,1,2)) == '-G' |,
           translate(substr(parm,1,2)) == '-P' then
        do
          compopt = compopt '-qtest'
          if translate(substr(parm,1,2)) == '-P' then
            compopt = compopt '-qprofile'
          parse var parm opt parm
        end

      /* check for -# option  */
      when translate(substr(parm,1,2)) == '-#' then
        do
          show = 'yes'
          run = 'no'
          parse var parm opt parm
        end

      /* check for -v option  */
      when translate(substr(parm,1,2)) == '-V' then
        do
          show = 'yes'
          run = 'yes'
          parse var parm opt parm
        end

      /* check for -c option  */
      when translate(substr(parm,1,2)) == '-C' then
        do
          link = 'no'
          parse var parm opt parm
        end

      /* ignore any other options for compile  */
      otherwise;

      end /* select */
   end /* while loop to process options */

   /* assume that the rest of the parm has the name(s) of the file(s) to preprocess/compile and link */
   /* pws - assume multiple .cbl files; other file type are assumed to be link files.                */
   parm = strip(parm)

   /* if a link is requested, start setting up the link command */
   if link = 'yes' then
     do
        optend = lastpos(parm,keepparm) - 1 /* find out where the options end in the original parm */
        linkopt = substr(keepparm,1,max(0,optend)) /* pluck out the options, if any */
     end

   /* loop through the rest of the parm to identify the files to compile vs. link */
   do while parm <> ''

     if pos('"',parm) <> 1 then                        /* no quotes  */
       do
         parse var parm opt parm
       end
     else /* hpfs file in quotes */
       do
         endpos = pos('"',parm,2) /* look for closing quote */
         if endpos = 0 then
           do
             say 'Error: No closing quote found'
             exit maxgoodrc+4
           end
         else /* closing quote found */
           do
             opt  = substr(parm,1,endpos)
             parm = strip(substr(parm,endpos+1))
           end
       end

     select
        when pos('.CBL',translate(opt)) <> 0  then
           infiles = strip(infiles opt)

        otherwise /* assume a link file */
           linkfiles = strip(linkfiles opt)

     end  /* end select  */

   end  /* while loop to process file names */

   /* Make sure some .cbl files were specified.  This action is not intended to do a link only */
   if strip(infiles) = '' then
     do
       say 'Error: No source file with a valid extension (.CBL) was found'
       exit maxgoodrc+4
     end

   /* Check the compiler options to make sure everything is there to enable       */
   /* compiler error message feedback (done via adata).  If not make sure we      */
   /* show the vanilla compiler messages.                                         */
   if pos(' NOADATA ',translate(compopt))>0 | pos('ADEXIT(IWZRMGUX)',translate(compopt))=0 then
       showcompmsgs = 'yes'

   if mytrace ==1  then
   do
     say 'compopt   = ' compopt
     say 'CICSOPT  = ' CICSOPT
     say 'SQLOPT   = ' SQLOPT
     say 'IPATH    = ' IPATH
     say 'SHOW     = ' SHOW
     say 'RUN      = ' RUN
     say 'LINK     = ' LINK
     say 'INFILES   = ' infiles
     say 'sourcetype = ' sourcetype
     say 'dbname = ' dbname
     say 'showcompmsgs =' showcompmsgs
     say 'Trace complete'
  /*    exit 0   */
  end  /* end do */

   return optrc

   END_PROGRAM:

       if did_connect = 'yes' then /* if connected to a DB2 database */
         'db2 connect reset' /* cleanup */

       say 'Processing halted.'
       EXIT returncode
   RETURN returncode


