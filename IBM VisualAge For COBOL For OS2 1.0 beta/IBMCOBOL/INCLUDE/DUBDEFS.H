/**************************************************************************
*******************************  DUBDEFS.H  *******************************
***************************************************************************
*
*                              Record Level I/O
*
* Module Name: DUBDEFS.H
*
* DDM Common Definitions File
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
***************************************************************************/

/*-------------------------------------------------------------------------
-- DDM Type Definitions
--------------------------------------------------------------------------*/
typedef ULONG            RECNUM;     /* Record Number                     */
typedef RECNUM *         PRECNUM;
typedef LONG             RECLENGTH;  /* DDM Record Length                 */
typedef RECLENGTH *      PRECLENGTH;
typedef USHORT           CODEPOINT;  /* DDM Codepoint                     */
typedef CODEPOINT *      PCODEPOINT;
typedef ULONG            HDDMFILE;   /* DDM Record File Handle            */
typedef HDDMFILE *       PHDDMFILE;
typedef ULONG            HDDMLOAD;   /* DDM Load/Unload Handle            */
typedef HDDMFILE *       PHDDMLOAD;
typedef LONG             HDDMMEMORY; /* DDM Memory Handle                 */
typedef HDDMMEMORY *     PHDDMMEMORY;
typedef ULONG            OBJLENGTH;  /* DDM Object Length                 */

/*-------------------------------------------------------------------------
-- DDM Severity Codes
--------------------------------------------------------------------------*/
#define SC_NO_ERROR      0x0000      /* No Error                          */
#define SC_WARNING       0x0004      /* Warning                           */
#define SC_ERROR         0x0008      /* Error                             */
#define SC_SEVERE        0x0010      /* Severe error                      */
#define SC_ACCESSDAMAGE  0x0020      /* Access Damage error               */
#define SC_PERMDAMAGE    0x0040      /* Permanent Damage                  */
#define SC_SESSIONDAMAGE 0x0080      /* Session Damage                    */

/*-------------------------------------------------------------------------
-- DDM Server Diagnostic Information for FILATHRM
--------------------------------------------------------------------------*/
#define SD_NOTDDM        (ULONG)1    /* The file not a DDM File           */

/*-------------------------------------------------------------------------
-- DDM Server Diagnostic Information for LENGTHRM
--------------------------------------------------------------------------*/
#define SD_EABUFSMALL    (ULONG)4    /* EA Reply Buffer too small         */
#define SD_EABUFLENGERR  (ULONG)5    /* EA Input Buffer Length Error      */

/*-------------------------------------------------------------------------
-- DDM Server Diagnostic Information for ADDRRM
--------------------------------------------------------------------------*/
#define SD_RECBUF        (ULONG)1    /* Record Buffer                     */
#define SD_KEYBUF        (ULONG)2    /* Key Buffer                        */
#define SD_GETATT        (ULONG)3    /* List File Attribute Buffer        */
#define SD_RECNUMBUF     (ULONG)4    /* Record Number Buffer              */
#define SD_SETATT        (ULONG)5    /* Attribute Reply or Request Buffer */
#define SD_RECCNTBUF     (ULONG)6    /* Record Count Buffer or Returned   */
                                     /*    Record Count Buffer            */
#define SD_FILENAME      (ULONG)7    /* File Name or Title                */
#define SD_FILEHANDLE    (ULONG)8    /* File Handle                       */
#define SD_FLAGS         (ULONG)9    /* Flags buffer                      */
#define SD_DFTRECBUF     (ULONG)10   /* Default Record Buffer             */
#define SD_FDBKBUF       (ULONG)11   /* Feedback Buffer                   */

/*-------------------------------------------------------------------------
-- DDM Key Definition Error Codes (BUGBUG: get better names)
--------------------------------------------------------------------------*/
#define KD_OUTOFBOUNDS   (BYTE)1     /* Key outside of record boundaries  */
#define KD_COMPOSITE     (BYTE)2     /* Composite keys not supported      */
#define KD_MAXKEY        (BYTE)3     /* Key length too long               */
#define KD_OVERLAP       (BYTE)4     /* Overlapping keys not supported    */
#define KD_MULTRECFIELDS (BYTE)5     /* Key defined on multiple rec fields*/
#define KD_DEFONREC      (BYTE)6     /* Key defined over record field     */
#define KD_KEYONNONCHAR  (BYTE)7     /* Key defined on non-character      */
#define KD_KEYSEQUENCE   (BYTE)8     /* Key sequence not supported        */
#define KD_KEYDATACLASS  (BYTE)9     /* Key data class not supported      */

/*-------------------------------------------------------------------------
-- Invalid request type definition
--------------------------------------------------------------------------*/
#define IV_FILPRT  (ULONG)15         /* File is protected. FILPRT EA      */

/*-------------------------------------------------------------------------
-- DDM server diagnostic information for CMDCHKRM
--------------------------------------------------------------------------*/
#define CK_LAN     (ULONG)1          /* FileShare change to none for LAN  */

/*-------------------------------------------------------------------------
-- Force word alignment all DDM structures (see end of defs)
--------------------------------------------------------------------------*/
#pragma pack(1)  /* need to put DDMRECORD on 1 byte because code is doing */
                 /* a sizeof function                                     */

/*-------------------------------------------------------------------------
-- Active Record
--------------------------------------------------------------------------*/
typedef struct _DDMRECORD {
    RECLENGTH      cbRecord;         /* Length of DDMRECORD               */
    CODEPOINT      cpRecord;         /* Active Record Codepoint           */
    BYTE           pRecord[1];       /* Record Data                       */
} DDMRECORD, *PDDMRECORD;

/*-------------------------------------------------------------------------
-- OEM Buffer
--------------------------------------------------------------------------*/
typedef struct _DDMBUFFER {
    BYTE           pBuffer[1];       /* Buffer Data                       */
} DDMBUFFER, *PDDMBUFFER;

#pragma pack(2)
/*-------------------------------------------------------------------------
-- Inactive Record
--------------------------------------------------------------------------*/
typedef struct _DDMRECINA {
    RECLENGTH      cbRecIna;         /* Length of DDMRECINA               */
    CODEPOINT      cpRecIna;         /* Inactive Record Codepoint         */
    RECLENGTH      cbRecord;         /* Record Length                     */
} DDMRECINA, *PDDMRECINA;

/*-------------------------------------------------------------------------
-- Record Attribute List for record number
--------------------------------------------------------------------------*/
typedef struct _DDMRECAL {
    RECLENGTH      cbRecAL;          /* Record Attribute List Length      */
    CODEPOINT      cpRecAL;          /* Record Attribte List Codepoint    */
    RECLENGTH      cbRecNum;         /* Record Number Data Length         */
    CODEPOINT      cpRecNum;         /* Record Number Codepoint           */
    RECNUM         RecNum;           /* Record Number                     */
} DDMRECAL, *PDDMRECAL;

/*-------------------------------------------------------------------------
-- Record Attribute List for key value
--------------------------------------------------------------------------*/
typedef struct _DDMRECALK {
    RECLENGTH      cbRecAL;          /* Record Attribute List Length      */
    CODEPOINT      cpRecAL;          /* Record Attribute List Codepoint   */
    RECLENGTH      cbKeyVal;         /* Key Value Data Length             */
    CODEPOINT      cpKeyVal;         /* Key Value Codepoint               */
    BYTE           pKeyVal[1];       /* Key Value                         */
} DDMRECALK, *PDDMRECALK;

/*-------------------------------------------------------------------------
-- DDM Object
--------------------------------------------------------------------------*/
typedef struct _DDMOBJECT {
    OBJLENGTH      cbObject;         /* Length of DDMOBJECT               */
    CODEPOINT      cpObject;         /* Object's Codepoint                */
    BYTE           pData[1];         /* Object Data                       */
} DDMOBJECT, *PDDMOBJECT;

/*-------------------------------------------------------------------------
-- Default Initialization Record
--------------------------------------------------------------------------*/
typedef struct _DDMDFTREC {
    RECLENGTH      cbDftRec;         /* Length of default record          */
    CODEPOINT      cpDftRec;         /* DftRec Codepoint                  */
    BYTE           pData[1];         /* Default Initialization Data       */
} DDMDFTREC, *PDDMDFTREC;

/*-------------------------------------------------------------------------
-- DDM Key Objects
--------------------------------------------------------------------------*/
typedef DDMOBJECT  DDMKEY,    *PDDMKEY;
typedef DDMOBJECT  KEYDEFBUF, *PKEYDEFBUF;

/*-------------------------------------------------------------------------
-- DDM Key Field Definition
--------------------------------------------------------------------------*/
typedef struct _KEYFLDDEF {
    ULONG          cbKeyFldDef;      /* length of keyflddf structure      */
    CODEPOINT      cpKeyFldDef;      /* codepoint for Keyflddf (x'140f')  */
    CODEPOINT      cpSequence;       /* Key sequence (SEQASC or SEQDSC)   */
    CODEPOINT      cpKeyClass;       /* Key class (should be BYTSTRDR)    */
    USHORT         cbKeyField;       /* Length of this key field          */
    ULONG          oKeyField;        /* Key field displacement            */
} KEYFLDDEF, *PKEYFLDDEF;

/*-------------------------------------------------------------------------
-- DDM Date and Time Format
--------------------------------------------------------------------------*/
typedef struct _DDMDATE {
    ULONG          cbDate;           /* Length of Date Data               */
    CODEPOINT      cpDate;           /* Date and Time Codepoint           */
    UCHAR          year[4];          /* Year                              */
    UCHAR          month[2];         /* Month                             */
    UCHAR          day[2];           /* Day                               */
    UCHAR          hour[2];          /* Hour                              */
    UCHAR          mins[2];          /* Minutes                           */
    UCHAR          secs[2];          /* Seconds                           */
} DDMDATE, *PDDMDATE;

/*-----------------------------------------------------------------------------
-- DDM Heap Manager Defines
------------------------------------------------------------------------------*/
#define NEWMEMHANDLEREQUEST  -1

/*-------------------------------------------------------------------------
-- Resume user requested alignment (see the Zp compile option)
--------------------------------------------------------------------------*/
#pragma pack()
