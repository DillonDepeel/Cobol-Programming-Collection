/********************************************************************************
**                      SOFTWARE INSTALLER
** 5621-434 (C) COPYRIGHT IBM CORP. 1991, 1993. ALL RIGHTS RESERVED.
**             LICENSED MATERIALS - PROPERTY OF IBM
*********************************************************************************
**
** ZAPINI.CMD -- Delete history information saved by Software Installer for OS/2.
**
********************************************************************************/

trace 'o'
parse arg INIDir',' Name',' Number',' Feature

/* Remove leading/trailing spaces */
INIDir  = strip(INIDir)
Name    = strip(Name)
Number  = strip(Number)
Feature = strip(Feature)

/* Check for required parms */
if INIDir  = '' |,
   Name    = '' |,
   Number  = '' |,
   Feature = ''
then do
   say
   say 'Descr:    Deletes history information saved by Software Installer for OS/2'
   say
   say 'Syntax:   ZAPINI INIDir, Name, Number, Feature'
   say 'Where:    INIDir : drive and directory where EPFIS.INI is located'
   say '          Name   : value of PACKAGE.Name'
   say '          Number : value of PACKAGE.Number'
   say '          Feature: value of PACKAGE.Feature'
   say 'All parameters are comma delimited.'
   say
   say 'Example:  ZAPINI c:\os2\system, Software Installer, 5621-434, 0000'
   exit
end   /* then */

/* Load the SysIni function */
call rxfuncadd sysini, rexxutil, sysini

/* Build the fully-qualified INI name */
if right(INIDir, 1) = '\'
then INI = INIDir||'epfis.ini'
else INI = INIDir||'\epfis.ini'

/* Create the application key */
App = 'EPFINST_'Name'_'Number'_'Feature

/* Delete all application keys            */
/* The return code is purposely ignored   */
/* as SysINI() doesn't return it properly */
rc = SysIni(INI, App, 'DELETE:')

say
say 'All records have been deleted:'
say '   From:   ' INI
say '   Name:   ' Name
say '   Number: ' Number
say '   Feature:' Feature
say

/* Drop the SysIni function */
call rxfuncdrop sysini

exit
