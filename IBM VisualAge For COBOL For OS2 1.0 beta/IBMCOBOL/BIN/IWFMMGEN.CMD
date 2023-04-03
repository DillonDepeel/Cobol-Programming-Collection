/*---------------------------------------------------------------------------*/
/*      Licensed Materials - Property of IBM                                 */
/*                                                                           */
/*      "Restricted Materials of IBM"                                        */
/*                                                                           */
/*      IBM WorkFrame                                                        */
/*                                                                           */
/*      (C) Copyright IBM Corp. 1991, 1995. All Rights Reserved.             */
/*                                                                           */
/*      US Government Users Restricted Rights - Use, duplication or          */
/*      disclosure restricted by GSA ADP Schedule Contract with IBM Corp.    */
/*---------------------------------------------------------------------------*/
/*REXX*/

/**************************************************************************
*  iwfmmgen.cmd  WorkFrame sample MAKEMAKE makefile generation script.    *
*                                                                         *
*  (c) Copyright International Business Machines Corporation 1994.        *
*  All rights reserved.                                                   *
*                                                                         *
**************************************************************************/

/* Initialize. */
RC.OK     = 0;
RC.CANCEL = 8;
RC.ERROR  = 8;
RC.FATAL  = 16;
NEWLINE   = '0d'x;

Trace 'A'

/* Load the REXX utility functions. */
rc = RxFuncAdd('RxMessageBox', 'RexxUtil', 'RxMessageBox');

/* Extract the passed parameters. */
/*if (Arg() <> 1) then*/
/*   call Abort("Usage is: IWFMake IntermediateFile MakeFile DependencyFile");*/
parse arg IntermediateFile,MakeFile,DependencyFile;

/* Parse the intermediate file to generate the make and dependency files. */
rc = Parse(IntermediateFile);
rc = Generate(Makefile, DependencyFile);

/* We are done. */
call Done(RC_OK);

/**************************************************************************
*  Parse(Stem, IntermediateFile);                                         *
*  Parse the passed intermediate file into a REXX stem variable.          *
*                                                                         *
*  The intermediate file contains entries of the form:                    *
*      :TAG.<VALUE1>...<VALUEn>                                           *
*                                                                         *
*  The stem variable represents the intermediate file as follows:         *
*      STEM.TAG.0                                                         *
*      STEM.TAG.NAME                                                      *
***************************************************************************/

Parse: procedure expose RC. NEWLINE  Stem.
arg IntermediateFile;
say "Parsing intermediate file.";
Stem. = 0;
LastTag = "";
LastTarget = "";
rc = RC_OK;

/* Process while not end of file. */
do while (lines(IntermediateFile) > 0)
   /* Get the current line. */
   Line = linein(IntermediateFile);
   
   /* Parse the line into a (TAG,VALUE) pair. */
   if (substr(Line,1,1) = ":") then do;
      parse var Line ":" Tag "." Value
   end;
   else do;
      Tag   = LastTag;
      Value = Line;
   end;
   if (Tag = "") then call Abort("Error parsing intermediate file.");
   LastTag = Tag;
   if (Value = "") then iterate;

   /* Add the (TAG,VALUE) pair to the stem variable. */
   /* All values are kept subordinate to the current RULE/TARGET tag. */
   if ((Tag = "TARGET") | (Tag = "RULE")) then do;
      Count = Stem.Tag.0;               /* Get the number of values for the tag. */
      Count = Count + 1;                /* We now have one more value. */
      Stem.Tag.0     = Count;           /* Set the new count. */
      Stem.Tag.Count = Value;           /* Set the new value. */
      LastTarget     = Tag"."Count;     /* Set the tag for the last target. */
   end;
   else do;
      Count = Stem.LastTarget.Tag.0;    /* Get the number of values for the tag. */
      Count = Count + 1;                /* We now have one more value. */
      Stem.LastTarget.Tag.0     = Count;/* Set the new count. */
      Stem.LastTarget.Tag.Count = Value;/* Set the new value. */
   end;
end 
return(rc);

/**************************************************************************
*  Generate(Stem, MakeFile, DependencyFile).                              *
*  Generate a makefile and a separate dependency file (if requested)      *
*  from the specified stem variable.                                      *
***************************************************************************/

Generate: procedure expose RC. NEWLINE  Stem.
arg MakeFile, DependencyFile;
say "Generating make file.";
rc = RC_OK;

/* Determine if the make and dependency files are one and the same. */
if (DependencyFile = "") then
   DependencyFile = MakeFile;

/* Generate the make file. */
call GenerateTargets;
return(rc);

/**************************************************************************
*  Generation utilities:                                                  *
*  (a) GenerateTargets.                                                    *
*  (a) GenerateTarget.                                                     *
*  (a) GenerateTrailer.                                                   *
**************************************************************************/

GenerateTargets: 
/*if (Stem.TARGET.0 < 2) then
return(RC.ERROR);*/

rc = GeneratePreTargets("TARGET."1, "TARGET."2);

do ix=1 to Stem.RULE.0;
   rc = GenerateTarget("RULE."ix);
end 
if (Stem.TARGET.0 > 2) then
do ix=3 to Stem.TARGET.0;
   rc = GenerateTarget("TARGET."ix);
end 
else
return(RC.OK);
return(RC.OK);

GenerateTarget:
arg ThisTarget;
if (Stem.ThisTarget.COMMAND.0 <> 0) then
   File = MakeFile;
else
   File = DependencyFile;
rc = lineout(File, NEWLINE);
if Stem.ThisTarget.DEPENDENCY.0 > 0 then
   rc = lineout(File, Stem.ThisTarget": \");
   else
   rc = lineout(File, Stem.ThisTarget":");
do xix=1 to Stem.ThisTarget.DEPENDENCY.0;
   if xix < Stem.ThisTarget.DEPENDENCY.0 then
   rc = lineout(File, "    "Stem.ThisTarget.DEPENDENCY.xix" \");
   else
   rc = lineout(File, "    "Stem.ThisTarget.DEPENDENCY.xix);
end 
do xix=1 to Stem.ThisTarget.ACTION.0;
   rc = lineout(File, "    @echo """ Stem.ThisTarget.ACTION.xix """");
end 
do xix=1 to Stem.ThisTarget.COMMAND.0;
   if Stem.ThisTarget.COMMAND.xix = "<<" then
   rc = lineout(File, Stem.ThisTarget.COMMAND.xix);
   else
   rc = lineout(File, "    "Stem.ThisTarget.COMMAND.xix);
end 
return(RC.OK);

GeneratePreTargets:
arg SuffixTarget, AllTarget
File = MakeFile;
rc = lineout(File, NEWLINE);
rc = charout(File, "."Stem.SuffixTarget":");

do xix=1 to Stem.SuffixTarget.DEPENDENCY.0;
   rc = charout(File, " "Stem.SuffixTarget.DEPENDENCY.xix);
end 
rc = lineout(File, NEWLINE);
rc = lineout(File, NEWLINE);

File = MakeFile;

if Stem.AllTarget.DEPENDENCY.0 > 0 then
   rc = lineout(File, "."Stem.AllTarget": \");
   else
   rc = lineout(File, "."Stem.AllTarget":");
do xix=1 to Stem.AllTarget.DEPENDENCY.0;
   if xix < Stem.AllTarget.DEPENDENCY.0 then
   rc = lineout(File, "    "Stem.AllTarget.DEPENDENCY.xix" \");
   else
   rc = lineout(File, "    "Stem.AllTarget.DEPENDENCY.xix);
end 
do xix=1 to Stem.AllTarget.ACTION.0;
   rc = lineout(File, "    @echo """ Stem.AllTarget.ACTION.xix """");
end 
do xix=1 to Stem.AllTarget.COMMAND.0;
   if Stem.AllTarget.COMMAND.xix = "<<" then
   rc = lineout(File, Stem.AllTarget.COMMAND.xix);
   else
   rc = lineout(File, "    "Stem.AllTarget.COMMAND.xix);
end 
return(RC.OK);

/**************************************************************************
*  General purpose utilities:                                             *
*  (a) cancel():        ask the user if processing should be cancelled.   *
*  (b) abort(message):  abort processing (with the given message).        *
*  (c) done(rc):        terminate processing (with the given return code).*
***************************************************************************/

Cancel: procedure expose RC. NEWLINE 
rc = RxMessageBox("Do you really want to cancel?", , "YesNo", "Query");
if (rc = 6) then
   call Done(RC_CANCEL);
return(RC_OK);

Abort: procedure expose RC. NEWLINE 
arg abortMessage;
rc =  RxMessageBox(abortMessage, , "Ok", "Error");
call Done(RC_FATAL);

Done: procedure expose RC. NEWLINE 
arg exitRc;
if (exitRc = RC_OK) then 
   say "The make file was generated successfully.";
else                     
   say "The make file was not generated due to errors.";
exit(exitRc);
