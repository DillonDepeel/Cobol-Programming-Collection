/******************************************************************************/
/* Copyright (c) IBM Corporation 1993,1995 All Rights Reserved                */
/*                                                                            */
/*                                                                            */
/*             IBM Distributed FileManager (DFM/2) for OS/2                   */
/*             ============================================                   */
/*                                                                            */
/*                       SAMPLE STARTUP FILE                                  */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/* DFM/2 (program no. 5648-02012)                                             */
/* Version: 1.0                                                               */
/* Release: 1.01                                                              */
/*                                                                            */
/* DISCLAIMER OF WARRANTIES:                                                  */
/* The following [enclosed] code is sample code created by IBM                */
/* Corporation.  This sample code is not part of any standard IBM product     */
/* and is provided to you solely for the purpose of assisting you in the      */
/* development of your applications.  The code is provided "AS IS",           */
/* without warranty of any kind.  IBM shall not be liable for any damages     */
/* arising out of your use of the sample code, even if they have been         */
/* advised of the possibility of such damages.                                */
/******************************************************************************/
/* CHANGE ACTIVITY                                                            */
/* -------------------------------------------------------------------------- */
/*                           930402 Mueller  : Initial Release                */
/*                           950802 W. Murphy: Release 1.011                  */
/******************************************************************************/
/******************************************************************************/
/*                                                                            */
/* Call: startdfm  [/?] | [configuration file]                                */
/*                  configuration file: DFM/2 configuration file, optional    */
/*                     if not specified,                                      */
/*                     first the current directory is searched for config.dfm */
/*                     then it is tried to use %EHNDIR%\config.dfm            */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*                   Modify this file as needed.                              */
/*                                                                            */
/******************************************************************************/
/* Setup  */
'echo off'
UsageMsg = 'startdfm.cmd:  USAGE:  startdfm [/?] | [configuration file]'

/* Verify command line arguments */
parse arg  configfile otherargs .
if otherargs \= '' | configfile = '/?' | configfile = '?' | configfile = '-?' then
do
  say UsageMsg
  exit 1
end

/* Determine configuration file name. */
if configfile = '' then  /* if configuration file not specified */
do
    /* check if configuration file exists in current directory */
    xconfig = stream( '.\config.dfm' , 'Command' , 'Query Size' )
    if xconfig \= '' then
    do    /* configuration file is in current directory */
        configfile = '.\config.dfm'
    end

    /* If configuration file is not in current directory then
       retrieve it from the DFM/2 product directory.          */
    else
    do
        /* get specification of EHNDIR - DFM/2 product directory */
        env = 'OS2ENVIRONMENT'
        ehndir = value('EHNDIR',,env)
        if ehndir = '' then
        do
            say 'startdfm.cmd:  Error: EHNDIR not defined - DFM/2 not started'
            exit 1
        end
        configfile = ehndir'\config.dfm'
    end
end

/* Verify that configfile was found.    */
'dir ' configfile '> nul 2>nul'
if rc \= '0' then
do
     say 'startdfm.cmd:  Error:  Configuration file not found.'
     say UsageMsg
     exit 1
end
else
     say 'startdfm.cmd:  Current configuration file: ' configfile


/* Start DFM/2 Communication Environment and Remote Stream Access Support */
'STRTDFMC' configfile
if rc \= '0' then
do
   say 'startdfm:  Error starting DFM Stream Support.  The most likely error'
   say '           is that the configuration file has not been customized'
   say '           correctly or that the Communication Manager has not been'
   say '           setup and started to support APPC communication.  Also'
   say '           verify that you entered the correct user ID and password'
   say '           and that DFM is available on the target system.'
   say ' '
   say '           Refer to the DFM/2 documentation and read.me file.'

   exit 1
end

/* Start DFM/2 Remote Record Access Support */
'STRTDFMR' configfile
if rc \= '0' then
do
   say 'startdfm:  Error starting DFM Record Support.  The most likely problem'
   say '           is a syntax error in the configuration file.'
   say ' '
   say '           Refer to the DFM/2 documentation and read.me file.'
   exit 1
end

/* Assign drive letter(s) to the remote system(s) */
/* The names behind // refer to the remote_lu names in config.dfm */
say 'startdfm:  DFM Stream and Record Support has been started.  You can'
say '           now assign a drive letter to the target system using the '
say '           dfmdrive command or you can customize this file to assign '
say '           drives automatically'
/* Customize below dfmdrive command below to automatically assign drives. */
/* 'CALL DFMDRIVE ASSIGN W: //MVS'      */      /* MVS System     */
/* 'CALL DFMDRIVE ASSIGN X: //OS400M1'  */      /* AS/400 System M1 */
/* 'CALL DFMDRIVE ASSIGN Y: //DDMCICS1' */      /* CICS/DDM System DDMCICS1 */
/* 'CALL DFMDRIVE ASSIGN Z: //OS4680'   */      /* 4680 Store System */

/* Normal exit. */
exit 0
/**** end of file ***************************************************/
