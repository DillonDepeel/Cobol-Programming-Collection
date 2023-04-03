/*****************************************************************************/
/* IBM(R) VisualAge(TM) C++ for OS/2(R), Version 3                           */
/* (C)Copyright IBM Corp. 1995.                                              */
/* - Licensed Material - Program-Property of IBM - All Rights Reserved       */
/*****************************************************************************/
/*********************************************************************/
/* This routine deregisters WorkFrame V3.0                           */
/*********************************************************************/

/* Call REXX utility functions */
Call RxFuncAdd 'SysLoadFuncs', 'REXXUTIL', 'SysLoadFuncs';
Call SysLoadFuncs;

/* Deregister WF classes */

rc = SysDeregisterObjectClass('IWFQuickStart');
rc = SysDeregisterObjectClass('IWFProject');
rc = SysDeregisterObjectClass('IWFRouter');
