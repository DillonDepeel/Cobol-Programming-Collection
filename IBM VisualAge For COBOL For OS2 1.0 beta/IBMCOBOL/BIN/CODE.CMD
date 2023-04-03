/******************************************************************************
*
* Copyright:   OCO SOURCE MATERIALS - IBM CONFIDENTIAL
*              (IBM CONFIDENTIAL-RESTRICTED WHEN COMBINED WITH THE
*              AGGREGATED OCO SOURCE MODULES FOR THIS PROGRAM)
*
*  PID 5688-194, 5733-073 (C) COPYRIGHT IBM CORP. 1992, 1993
*              All rights reserved.
*
******************************************************************************/
trace '?o'
parse arg cmdline

editcmd = 'START EVFXLXPM /CODE'                /* editor invocation cmd */
debugcmd = 'START CODEDT'                       /* debugger invocation cmd */
defserver = 'LOCAL'                             /* default server */

debugflag = ''                                  /* either /p or /c  */

true = 1
false = 0

newcmdline = ''                                 /* output command line */
server = defserver                              /* working server name */
filename = ''                                   /* working filename */
editselected = false                            /* edit invocation selected */
debugselected = false                           /* debug invocation selected */

do i = 1 to words(cmdline)                      /* go thru command line */
  word = translate(word(cmdline, i))            /* get a word from cmdline */
  select                                        /* what's the word */
    when left(word, 2) = '/E' then do           /* edit requested */
      editselected = true                       /* we've got an edit command */
    end
    when left(word, 2) = '/D' then do           /* debug requested */
      debugselected = true                      /* we've got a debug command */
    end
    when left(word, 2) = '/S' then do           /* server name specification */
      if abbrev("/SEQNUM", word, 4) = 0 then do /* it's not seqnum */
        server = word(cmdline, i + 1)           /* set server from next parm */
        if left(server, 1) = '/' |,             /* next parm wasn't a server */
          server = '' then do                   /* or it is blank */
          server = defserver                    /* reset to default server */
        end
        else do                                 /* was a server */
          i = i + 1                             /* setup for next parm */
        end
      end
      else do                                   /* it is /SEQNUM */
        newcmdline = newcmdline '/N'
      end
    end
    when left(word, 2) = '/F' then do           /* filename */
      filename = filename word(cmdline, i + 1)  /* set fn from next parm */
      if left(strip(filename), 1) = '/' then do /* next parm wasn't filename */
        filename = ''                           /* reset to defualt filename */
      end
      else do                                   /* was a filename */
        i = i + 1                               /* setup for next parm */
      end
    end
    when left(word, 2) = '/P' then do           /* pgm */
      debugflag = left(word, 2)
      filename = filename word(cmdline, i + 1)  /* get the file name */
      newcmdline = newcmdline subword(cmdline, i + 2)  /* get rest of parms */
      i = words(cmdline)                        /* all done */
    end
    when left(word, 2) = '/C' then do           /* proc */
      debugflag = left(word, 2)
      filename = filename word(cmdline, i + 1)  /* get the file name */
      tmp = word(cmdline, i + 2)                /* get the pgm name */
      newcmdline = newcmdline subword(cmdline, i + 3)  /* get rest of parms */
      newcmdline = newcmdline tmp
      i = words(cmdline)                        /* all done */
    end
    when left(word, 1) = '?' then do            /* help requested */
      newcmdline = newcmdline word              /* get rest of parms */
    end
    when left(word, 1) <> '/' then do           /* not a parm */
      filename = filename word(cmdline, i)      /* must be a filename */
    end
    otherwise do                                /* all other parms passed */
      newcmdline = newcmdline word              /* as they came in */
      if left(word(cmdline, i+1), 1) <> '/' then do /* grab word after parm */
        i = i + 1                               /* if its not another trigger */
        newcmdline = newcmdline word(cmdline, i)
      end
    end
  end
end

newcmdline = '"<'||server||'>'||strip(filename)'"' newcmdline /* finish cmdline */

if editselected = true then do
  newcmdline = editcmd newcmdline               /* setup for edit default */
end
else if debugselected = true then do
  newcmdline = debugcmd debugflag newcmdline    /* setup for edit default */
  parse value newcmdline with before 'TEST('testparms')' after
  if testparms <> '' then do
    newcmdline = before after 'TEST('testparms')'
  end
end
else do
  newcmdline = editcmd newcmdline               /* setup for edit default */
end

'@'newcmdline                                   /* invoke command */

exit                                            /* all done */
