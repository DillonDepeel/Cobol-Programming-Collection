/* REXX */
/*************************************************************/
/*                                                           */
/*    Licensed Materials - Property of IBM                   */
/*                                                           */
/*    28H2177 (C) Copyright IBM Corp. 1995                   */
/*    All rights reserved                                    */
/*                                                           */
/*    US Government Users Restricted Rights - Use,           */
/*    duplication, or disclosure restricted by GSA ADP       */
/*    Schedule Contract with IBM Corp.                       */
/*                                                           */
/*************************************************************/

/*************************************************************/
/* Name: IWZZWRA.CMD                                         */
/*                                                           */
/* Function: Perform a project level "Rebuild all" action    */
/*           for WorkFrame projects based on the COBOL       */
/*           MVS Master Project.  The supplied version of    */
/*           file issue a message informing the user that    */
/*           they need to manually perform the needed        */
/*           actions to perform the "Rebuild all".  The      */
/*           user can modify this REXX procedure to perform  */
/*           the required actions.                           */
/*                                                           */
/* Last Update: 07/14/95                                     */
/*************************************************************/
signal on novalue;
parse upper arg project_name rest; /* get the project name   */
say 'A "Rebuild all" action has been requested for the WorkFrame';
say 'project named' project_name'.';
say;
say 'The tasks required to perform the "Rebuild all" action for';
say 'MVS parts is not supported.  You must perform these tasks';
say 'manually.';
exit 0;
