/***********************************************************************
****************************  DDMCALLS.H  ******************************
************************************************************************
*
*                              Record Level I/O
*
* Module Name: DDMCALLS.H
*
* DDM Function Prototypes
*
* RLIO (program no. 5648-02011)
*
* Version: 1.0
* Release: 1.0
*
*
* ADSTAR Distributed Storage Manager Data Access Services:
* Version 1.0, Release 1.0, Level 0.0
* Licensed Materials - Property of IBM
* 5648-02011 (C) Copyright IBM Corporation, 1992, 1993, All Rights Reserved
* U.S. Government Users Restricted Rights - Use, duplicate or disclosure
* restricted by GSA ADP Schedule Contract with IBM Corporation.
*
* FLAG REASON  LEVEL   DATE    PGMR       COMMENTS
* ---- ------  -----   ----    ----       --------
* $A0= ENHCVT  Rel1.2  042195  SSWang     : Add a conversion function
**********************************************************************/

/*--------------------------------------------------------------------
-- AccessFlags for all APIs
---------------------------------------------------------------------*/
#define DDM_UPDINT       0x0001L  /* Update Intent                        */
#define DDM_RECNBRFB     0x0002L  /* Record Number Feedback               */
#define DDM_KEYVALFB     0x0004L  /* Key Value Feedback                   */
#define DDM_RTNINA       0x0008L  /* Return Inactive Record               */
#define DDM_ALLREC       0x0010L  /* All Records, Active and Inactive     */
#define DDM_NODATA       0x0020L  /* No Record Data Returned              */
#define DDM_BYPDMG       0x0040L  /* Bypass Damaged Record                */
#define DDM_HLDCSR       0x0080L  /* Hold Cursor Position                 */
#define DDM_ALWINA       0x0100L  /* Allow Cursor on Inactive Record      */
#define DDM_INHMODKY     0x0200L  /* Inhibit Modified Key                 */
#define DDM_UPDCSR       0x0400L  /* Update Cursor                        */
#define DDM_HLDUPD       0x0800L  /* Hold Update Intent                   */

/*---------------------------------------------------------------------
-- DDMClose - Close a record file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMClose(
    HDDMFILE);                    /* record file handle                   */

/*---------------------------------------------------------------------
-- DDMConvert - Convert a key file
-----------------------------------------------2@A0A---------------------*/
extern APIRET APIENTRY DDMConvert(
    PSZ);                         /* Key file name                        */

/*--------------------------------------------------------------------
-- DDMCopyFile - Copy a record file
--------------------------------------------------------------------*/
extern APIRET APIENTRY DDMCopyFile(
    PSZ,                          /* from record file name                */
    PSZ,                          /* to record file name                  */
    ULONG,                        /* copy flags                           */
    PBYTE,                        /* subset definition buffer             */
    CODEPOINT,                    /* to file old                          */
    CODEPOINT);                   /* to file new                          */

/*-- Copy Flags --*/
#define DDM_BYPINA       0x0010L  /* Bypass Inactive Records              */
#define DDM_ACCORD       0x1000L  /* Key Order Processing                 */

/*--------------------------------------------------------------------
-- DDMCreateAltIndex - Create an alternate index file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMCreateAltIndex(
    PSZ,                          /* record file name                     */
    PSZ,                          /* base record file name                */
    ULONG,                        /* create flags                         */
    PKEYDEFBUF,                   /* key definition buffer                */
    CODEPOINT,                    /* duplicate file option                */
    PEAOP2);                      /* attribute buffer                     */

/*--------------------------------------------------------------------
-- DDMCreateRecFile - Create a record-oriented file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMCreateRecFile(
    PSZ,                          /* record file name                     */
    ULONG,                        /* create flags                         */
    ULONG,                        /* maximum record length                */
    CODEPOINT,                    /* record length class                  */
    PKEYDEFBUF,                   /* key definition buffer                */
    ULONG,                        /* initial file size                    */
    LONG,                         /* max file size                        */
    CODEPOINT,                    /* duplicate file option                */
    CODEPOINT,                    /* default record option                */
    ULONG,                        /* record count                         */
    PEAOP2,                       /* attribute buffer                     */
    CODEPOINT,                    /* file class                           */
    PDDMDFTREC);                  /* default record initialization buffer */

/*-- Create Flags --*/
#define DDM_ALDUPKEY     0x0001L  /* Allow Duplicate Keys                 */
#define DDM_TMPFIL       0x0002L  /* Temporary File                       */
#define DDM_DELCP        0x0004L  /* Allow Record Deletion Capability     */
#define DDM_INIEX        0x0008L  /* Inhibit Initial Extent               */
#define DDM_GETCP        0x0010L  /* Allow Get Record Capability          */
#define DDM_INSCP        0x0020L  /* Allow Insert Record Capability       */
#define DDM_MODCP        0x0040L  /* Allow Modify Record Capability       */
#define DDM_FILHDD       0x0080L  /* Hidden File                          */
#define DDM_FILSYS       0x0100L  /* System File                          */
#define DDM_FILPRT       0x0200L  /* Protected File                       */

/*--------------------------------------------------------------------
-- DDMDelete - Delete a record file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMDelete(
    PSZ,                          /* record file name                     */
    ULONG);                       /* delete flags                         */

/*-- Delete Flags --*/
#define DDM_OVRDTA       0x0001L  /* Overwrite Data                       */

/*--------------------------------------------------------------------
-- DDMDeleteRec - Delete a record
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMDeleteRec(
    HDDMFILE,                     /* record file handle                   */
    ULONG);                       /* delete flags                         */

/*---------------------------------------------------------------------
-- DDMForceBuffer - Commit a file's cached information
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMForceBuffer(
    HDDMFILE);                    /* record file handle                   */

/*---------------------------------------------------------------------
-- DDMQueryFileInfo- Get a record file's information
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMQueryFileInfo(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* file information level required      */
    PBYTE,                        /* file information (returned)          */
    ULONG);                       /* file information buffer size         */

/*---------------------------------------------------------------------
-- DDMQueryPathInfo- Get a record file's or directory's information
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMQueryPathInfo(
    PSZ,                          /* path name of file                    */
    ULONG,                        /* path information level required      */
    PBYTE,                        /* path information (returned)          */
    ULONG);                       /* path information buffer size         */

/*---------------------------------------------------------------------
-- DDMGetRec - Get a record
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMGetRec(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*---------------------------------------------------------------------
-- DDMGetReplyMessage - Get a reply message
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMGetReplyMessage(
    PBYTE,                        /* reply message buffer (returned)      */
    ULONG,                        /* message buffer length                */
    ULONG);                       /* flag bit for action                  */

/*-- Replies --*/
#define ALL_MESSAGES_RECEIVED                   0x0000
#define MORE_REPLY_MESSAGES_TO_RECEIVE          0x0004
#define ERROR_REPLY_BUFFER_TOO_SMALL            0x0008
#define WARNING_NO_REPLY_MSG_AVAILABLE          0x0010
#define ERROR_INVALID_BUFFER_ADDRESS            0x0020
#define ERROR_UNARCHITECTED_REPLY_MSG           0x0040
#define ERROR_INVALID_REPLY_MSG_FLAG            0x0080

/*---------------------------------------------------------------------
-- DDMInsertRecEOF - Insert record at EOF
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMInsertRecEOF(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer                        */
    ULONG,                        /* record count                         */
    PDDMOBJECT,                   /* feedback buffer (returned)           */
    ULONG);                       /* feedback buffer length               */

/*---------------------------------------------------------------------
-- DDMInsertRecKey - Insert record by key value
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMInsertRecKey(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer                        */
    PRECNUM,                      /* record number (returned)             */
    ULONG);                       /* record count                         */

/*---------------------------------------------------------------------
-- DDMInsertRecNum - Insert record by record number
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMInsertRecNum(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer                        */
    PDDMOBJECT,                   /* key feed back (returned)             */
    ULONG,                        /* key feed back length                 */
    RECNUM,                       /* record number                        */
    ULONG);                       /* record count                         */

/*---------------------------------------------------------------------
-- DDMLoadFileFirst - Load records into file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMLoadFileFirst(
    PSZ,                          /* record file name                     */
    PHDDMLOAD,                    /* load handle (returned)               */
    ULONG,                        /* load flags                           */
    PDDMRECORD,                   /* record buffer                        */
    ULONG);                       /* record count                         */

/*-- Load Flags --*/
#define DDM_CHAIN        0x0001L  /* Chain LODRECF requests               */

/*---------------------------------------------------------------------
-- DDMLoadFileNext - Load records into file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMLoadFileNext(
    HDDMLOAD,                     /* load handle                          */
    ULONG,                        /* load flags                           */
    PDDMRECORD,                   /* record buffer                        */
    ULONG);                       /* record count                         */

/*-- Load Flags --*/
#define DDM_CLOSE        0x0001L  /* Close LODRECF requests               */

/*---------------------------------------------------------------------
-- DDMModifyRec - Modify record
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMModifyRec(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD);                  /* record buffer                        */

/*---------------------------------------------------------------------
-- DDMOpen - Open a record file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMOpen(
    PSZ,                          /* record file name                     */
    PHDDMFILE,                    /* record file handle (returned)        */
    CODEPOINT,                    /* access method                        */
    ULONG,                        /* access intent flags                  */
    USHORT,                       /* file share                           */
    PBYTE,                        /* attribute buffer                     */
    PBYTE);                       /* type id buffer                       */

/*-- Access Intent Flags --*/
#define DDM_GETAI        0x0001L  /* Get Record                           */
#define DDM_INSAI        0x0002L  /* Insert Record                        */
#define DDM_MODAI        0x0004L  /* Modify Record                        */
#define DDM_DELAI        0x0008L  /* Delete Record                        */
#define DDM_WRITETHRU    0x0010L  /* File Write Through                   */
#define DDM_NOINHERIT    0x0020L  /* File Inheritance                     */
#define DDM_FAILONERROR  0x0040L  /* Fail Error Handling                  */

/*-- FileShare --*/
#define DDM_NOSHARE           1   /* No concurrent users                  */
#define DDM_READERS           2   /* Share with concurrent readers        */
#define DDM_UPDATERS          3   /* Share with concurrent updaters       */

/*---------------------------------------------------------------------
-- DDMRename - Rename a record file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMRename(
    PSZ,                          /* record file name                     */
    PSZ);                         /* new record file name                 */

/*---------------------------------------------------------------------
-- DDMSetBOF - Set cursor to beginning of file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetBOF(
    HDDMFILE);                    /* record file handle                   */

/*---------------------------------------------------------------------
-- DDMSetEOF - Set cursor to end of file
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetEOF(
    HDDMFILE);                    /* record file handle                   */

/*---------------------------------------------------------------------
-- DDMSetFileInfo- Set a record file's information
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetFileInfo(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* file information level required      */
    PBYTE,                        /* file information buffer              */
    ULONG);                       /* file information buffer size         */

/*---------------------------------------------------------------------
-- DDMSetFirst - Set cursor to first record
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetFirst(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*---------------------------------------------------------------------
-- DDMSetKey - Set cursor by key
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetKey(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMOBJECT,                   /* key value buffer                     */
    CODEPOINT,                    /* relational operator                  */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*---------------------------------------------------------------------
-- DDMSetKeyFirst - Set cursor to first record in key sequence
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetKeyFirst(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*---------------------------------------------------------------------
-- DDMSetKeyLimits - Set key limits
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetKeyLimits(
    HDDMFILE,                     /* record file handle                   */
    PDDMOBJECT,                   /* Low Key limit buffer                 */
    PDDMOBJECT);                  /* High Key limit buffer                */

/*-------------------------------------------------------------------------
-- DDMSetKeyLast - Set cursor to last record in key sequence
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetKeyLast(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*-------------------------------------------------------------------------
-- DDMSetKeyNext - Set cursor to next record in key sequence
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetKeyNext(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG,                        /* record buffer length                 */
    ULONG,                        /* record count                         */
    PULONG);                      /* record return count (returned)       */

/*-------------------------------------------------------------------------
-- DDMSetKeyPrevious - Set cursor to previous record in key sequence
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetKeyPrevious(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG,                        /* record buffer length                 */
    ULONG,                        /* record count                         */
    PULONG);                      /* record return count (returned)       */

/*-------------------------------------------------------------------------
-- DDMSetLast - Set cursor to last record
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetLast(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*-------------------------------------------------------------------------
-- DDMSetMinus - Set cursor minus
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetMinus(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    ULONG,                        /* cursor displacement                  */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*-------------------------------------------------------------------------
-- DDMSetRecNum - Set cursor to record number
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetRecNum(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    RECNUM,                       /* record number                        */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*-------------------------------------------------------------------------
-- DDMSetNextRec - Set cursor to next record
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetNextRec(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG,                        /* record buffer length                 */
    ULONG,                        /* record count                         */
    PULONG);                      /* record return count (returned)       */

/*-------------------------------------------------------------------------
-- DDMSetNextKeyEqual - Set cursor to next record with equal key
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetNextKeyEqual(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMOBJECT,                   /* key value buffer                     */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*-------------------------------------------------------------------------
-- DDMSetPathInfo- Set a record file's or directory's information
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetPathInfo(
    PSZ,                          /* record file handle                   */
    ULONG,                        /* path information level to set        */
    PBYTE,                        /* path information buffer              */
    ULONG);                       /* path information buffer size         */

/*-------------------------------------------------------------------------
-- DDMSetPlus - Set cursor plus
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetPlus(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    ULONG,                        /* cursor displacement                  */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*-------------------------------------------------------------------------
-- DDMSetPrevious - Set cursor to previous record
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetPrevious(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG,                        /* record buffer length                 */
    ULONG,                        /* record count                         */
    PULONG);                      /* record return count (returned)       */

/*-------------------------------------------------------------------------
-- DDMSetUpdateKey - Set update intent by key value
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetUpdateKey(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    PDDMOBJECT,                   /* key value buffer                     */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*-------------------------------------------------------------------------
-- DDMSetUpdateNum - Set update intent by record number
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMSetUpdateNum(
    HDDMFILE,                     /* record file handle                   */
    ULONG,                        /* access flags                         */
    RECNUM,                       /* record number                        */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG);                       /* record buffer length                 */

/*---------------------------------------------------------------------
-- DDMTruncFile Truncate File
---------------------------------------------------------------------*/
extern APIRET APIENTRY DDMTruncFile(
    HDDMFILE);                    /* record file handle                   */

/*-------------------------------------------------------------------------
-- DDMUnloadFileFirst - Unload records from file
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMUnLoadFileFirst(
    PSZ,                          /* record file name                     */
    PHDDMLOAD,                    /* uload handle (returned)              */
    ULONG,                        /* access flags                         */
    PULONG,                       /* unload flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG,                        /* record buffer length                 */
    CODEPOINT,                    /* unload order                         */
    PULONG);                      /* record count (returned)              */

/*-- UnLoad Flags --*/
#define DDM_MOREDATA  0x0001L     /* More Data Unload Flag                */

/*-------------------------------------------------------------------------
-- DDMUnloadFileNext - Unload records from file
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMUnLoadFileNext(
    HDDMLOAD,                     /* unload handle                        */
    ULONG,                        /* access flags                         */
    PULONG,                       /* unload flags                         */
    PDDMRECORD,                   /* record buffer (returned)             */
    ULONG,                        /* record buffer length                 */
    PULONG);                      /* record count (returned)              */

/*-- UnLoad Flags --*/
#define DDM_CLOSEUNLOAD 0x0002L   /* Close Unload file Flag               */

/*-------------------------------------------------------------------------
-- DDMUnlockRec - Release implicit record locks
--------------------------------------------------------------------------*/
extern APIRET APIENTRY DDMUnLockRec(
    HDDMFILE);                    /* record file handle                   */

