/******************************************************************************/ 
/*  Licensed Material - Property of IBM                                       */ 
/*  28H2177 (C) Copyright IBM Corp., 1995                                     */ 
/*  All rights reserved.                                                      */ 
/*  Use Government Users Restricted Rights - Use, duplication or disclosure   */ 
/*  restricted by GSA ADP Schedule Contract with IBM Corp.                    */ 
/******************************************************************************/ 

'@ECHO OFF'
trace o
     Arg infile rcfile options 

     cicsrc = 0
     maxgoodrc = 4
     environ  = 'OS2ENVIRONMENT'
     cicsset  = VALUE('CICSSET',,environ)
     tempdir = VALUE('TMP',,environ)

     command = 'faaprpdc' infile options        

     /* set up CICS environment variables */
     if cicsset = '' then call 'cicsenv'
     address cmd 'SET COBWRK=' || tempdir 

     say 'Starting CICS preprocessing...'
     address cmd command 
     cicsrc = rc

     /* set the returncode from preprocessing.  since iwzvcprp is executed in a */
     /* different process than iwzvcomp, the returncode must be passed back to  */
     /* iwzvcomp in a file.  The calling command file (iwzvcomp) has provided   */ 
     /* the name of the file to put the return code into.                       */ 
     call lineout rcfile,cicsrc
     call lineout rcfile

exit 
