/*
*
* Copyright:   OCO SOURCE MATERIALS - IBM CONFIDENTIAL
*              (IBM CONFIDENTIAL-RESTRICTED WHEN COMBINED WITH THE
*              AGGREGATED OCO SOURCE MODULES FOR THIS PROGRAM)
*
*  PID 5688-194, 5733-073 (C) COPYRIGHT IBM CORP. 1992, 1993
*              All rights reserved.
*
*/
trace 'o'

/*
*  We use the following utility functions.
*/
call RxFuncAdd 'SysSearchPath', 'RexxUtil', 'SysSearchPath'

/*
* The following strings contain the format of event file records. Each string
* interpreted to make the overall code simpler and more maintainable.
* All that is needed is to parse the input lines into the variables presented
* in the idividual records.
* When the statement is interpreted, the values will be substitued.
*/
TIMESTAMPREC = 'TIMESTAMP version eventtimestamp'
PROCESSORREC = 'PROCESSOR version outputid lineclass'
FILEIDREC    = 'FILEID    version srcid srcline filenamelength filename filetimestamp'
ERRORREC     = 'ERROR     version srcid annotclass stmtline starterrline tokenstart enderrline tokenend msgid sevchar sevnum length msg'
FILEENDREC   = 'FILEEND   version srcid expansion'

/*
*  Setup the defaults for the variables.
*/
version = 1
parse value time() with hh':'mm':'ss
eventtimestamp = date('S')||hh||mm||ss
outputid = 0
lineclass = 1
srcid = 0
srcline = 1
filenamelength = 0
filename = ''
filetimestamp = ''
annotclass = 1
stmtline = 0
starterrline = 0
tokenstart = 0
enderrline = 0
tokenend = 0
msgid = ""
sevchar = ""
sevnum = 0
length = 0
msg = ""
expansion = 0

/*
*  The filenames string is a list of filenames open for an event file.
*  The wordindex of the filename in the list is used as the event file number.
*/
filenames = ''

  /*
  *  get trace parms if any
  */
  parse arg before'-t'tracearg after
  if tracearg <> '' then trace(tracearg)
  parms = before||after

  /*
  *  Get the input file name. If the name is not specified then input will
  *  come from stdin during the linein().
  */
  parse value parms with infile .

  /*
  *  Since we are processing a file we need a timestamp and processor record.
  */
  interpret say TIMESTAMPREC
  interpret say PROCESSORREC

  /*
  *  Go thru all the lines in the source
  */
  do while lines(infile)
    line = linein(infile)

    linetype = ""
    /*
    *  Check for the type of error it is. This could be a compiler, linker
    *  resource compiler, etc error.
    *
    */
    parse value line with filename'('stmtline':'tokenstart')'.':'severity msgid':'msg
    if wordpos(severity, "warning informational error") > 0 then do
      linetype = "ICC"
      starterrline = stmtline
      enderrline = stmtline
      tokenend = tokenstart
      sevchar = translate(left(severity, 1))
      length = length(msg)
    end
    else do
      parse value line with filename'('stmtline')' 'RC:' severity '-' msg
      severity = strip(severity)
      if wordpos(severity, "error") > 0 then do
        linetype = "RC"
        starterrline = stmtline
        enderrline = stmtline
        tokenstart = 0
        tokenend = tokenstart
        msgid = severity
        sevchar = translate(left(severity, 1))
        length = length(msg)
      end
      else do
        parse value line with '<'filename':'stmtline'>' msgid':' msg
        severity = strip(severity)
        if filename <> '' & stmtline <> '' then do
          linetype = "IPFC"
          starterrline = stmtline
          enderrline = stmtline
          tokenstart = 0
          tokenend = tokenstart
          severity = 'Error'
          sevchar = translate(left(severity, 1))
          length = length(msg)
        end
      end
    end

    /*
    *  If we found a line we process then get the fileid and put an error
    *  record.
    */
    if linetype <> "" then do
      /*
      *  Lookup the filename to get the fileid. The index into the filenames
      *  string is the fileid used in the event output.
      */
      if pos(':', filename) = 0 then do           /* no path specified */
        newfilename = syssearchpath('.', filename)   /* add current path  */
        if newfilename <> '' then do
          filename = newfilename
        end
      end
      srcid = wordpos(filename, filenames)
      if srcid = 0 then do

        /*
        *  Set-up some variables for the file record.
        */
        filenames = filenames filename
        srcid = wordpos(filename, filenames)
        filenamelength = length(filename)

        /*
        *  Put the fileid record.
        */
        interpret say FILEIDREC
      end

      /*
      *  output an error line
      */
      interpret say ERRORREC
    end
  end
exit 0
