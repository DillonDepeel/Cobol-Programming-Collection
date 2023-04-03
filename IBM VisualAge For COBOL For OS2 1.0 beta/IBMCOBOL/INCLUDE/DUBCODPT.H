/**************************************************************************
******************************  DDMCODPT.H  *******************************
***************************************************************************
*
*                              Record Level I/O
*
* Module Name: DDMCODPT.H
*
* DDM Code Points
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
* Note: All code points that begin with 0xfxxx are OS/2 specific
*
***************************************************************************/

/****************************************************************************
 *
 * CHANGE ACTIVITY:
 *
 * FLAG REASON     LEVEL   DATE   PGMR            COMMENTS
 * ---- ---------- ------ ------  --------- ---------------------------------
 * $A3= 0000611    R020   940115  KNUDSON  : add GSDL-requested codepoints
 * $A5= 0000655    R020   940128  KNUDSON  : add DFM-req'd message support
 * $A6                    950413  SSW        add warning msg
 * $A7= KD70024     Rel1.2   950531  ACLINTON : Added CHGFATRM reply message
 ****************************************************************************/

/*-------------------------------------------------------------------------
-- Reply Message Code Points
--------------------------------------------------------------------------*/
#define ACCATHRM         0x1230   /* Not Authorized to Use Access Method  */
#define ACCINTRM         0x1266   /* Access Intent List Error             */
#define ACCMTHRM         0x1231   /* Invalid Access Method                */
#define ADDRRM           0xf212   /* Address Error                        */
#define AGNPRMRM         0x1232   /* Permanent Agent Error                */
#define BASNAMRM         0x1234   /* Invalid Base File Name               */
#define CHGFATRM         0x1261   /* Change file attribute error   @A7A */
#define CLSDMGRM         0x125e   /* File Closed with Damage              */
#define CMDCHKRM         0x1254   /* Command Check                        */
#define COMMRM           0xf207   /* Communications Error                 */
#define CONTRM           0xf214   /* Function Continuation Error          */
#define CSRNSARM         0x1205   /* Cursor Not Selecting a Record        */
                                  /*    Position                          */
#define CVTNFNRM         0xF202   /* Conversion Table Not Found           */ /*@A3C*/
#define DDFNFNRM         0xF201   /* Data Description File Not Found      */ /*@A3C*/
#define DFTRECRM         0x1204   /* Default Record Error                 */
#define DRCATHRM         0x1237   /* Not Authorized to Directory          */
#define DRCFULRM         0x1258   /* Directory Full                       */
#define DTAMAPRM         0x126C   /* Record Mapping Error                 */ /*@A3C*/
#define DTANFNRM         0x122C   /* Data Class Not Found                 */ /*@A3C*/
#define DTARECRM         0x1206   /* Invalid Data Record                  */
#define DUPFILRM         0x1207   /* Duplicate File Name                  */
#define DUPKDIRM         0x1208   /* Duplicate Key Different Index        */
#define DUPKSIRM         0x1209   /* Duplicate Key Same Index             */
#define DUPRNBRM         0x120a   /* Duplicate Record Number              */
#define ENDFILRM         0x120b   /* End of File Condition                */
#define EXSCNDRM         0x123a   /* Existing Condition                   */
#define FILATHRM         0x123b   /* Not Authorized to File               */
#define FILDMGRM         0x125a   /* File Damaged                         */
#define FILERRRM         0xf216   /* File Error                           */
#define FILFULRM         0x120c   /* File Is Full                         */
#define FILIUSRM         0x120d   /* File In Use                          */
#define FILNAMRM         0x1212   /* Invalid File Name                    */
#define FILNFNRM         0x120e   /* File Not Found                       */
#define FILOLORM         0x121D   /* File Open Lock Option Changed        */ /*@A3C*/
#define FILSNARM         0x120f   /* File Space Not Available             */
#define FILTNARM         0x121e   /* File Temporarily Not Available       */
#define FUNATHRM         0x121c   /* Not Authorized to Function           */
#define FUNNSPRM         0x1250   /* Function Not Supported               */
#define HDLNFNRM         0x1257   /* File Handle Not Found                */
#define INTATHRM         0x125c   /* Not Authorized to Open Intent for    */
                                  /*    Named File                        */
#define INVFLGRM         0xf205   /* Invalid Flag                         */
#define INVRQSRM         0x123c   /* Invalid Request                      */
#define KEYDEFRM         0x123d   /* Invalid Key Definition               */
#define KEYLENRM         0x122d   /* Invalid Key Length                   */
#define KEYMODRM         0x1260   /* Key Value Modified After Cursor Was  */
                                  /*    Last Set                          */
#define KEYUDIRM         0x1201   /* Key Update Not Allowed by Different  */
                                  /*    Index                             */
#define KEYUSIRM         0x123f   /* Key Update Not Allowed by Same Index */
#define KEYVALRM         0x1240   /* Invalid Key Value                    */
#define LENGTHRM         0xf211   /* Field Length Error                   */
#define MGMATHRM         0x121B   /* Not Authorized to Management Class   */ /*@A3C*/
/* LODRECRM renamed to RECCNTRM                                           */ /*@A3C*/
#define MGMCNFRM         0x1238   /* Management Class Conflict            */
#define MGMNFNRM         0x1217   /* Management Class Not Found           */ /*@A3C*/
#define NEWNAMRM         0x124f   /* Invalid New File Name                */
#define OBJNSPRM         0x1253   /* Object Not Supported                 */
#define OPNEXCRM         0x1243   /* Open Exclusive By Same User          */
#define OPNMAXRM         0x1244   /* Concurrent Opens Exceeds Maximum     */
#define OS2ERRRM         0xf000   /* OS/2 Error                           */
#define PRCCNVRM         0x1245   /* Conversational Protocol Error        */
#define PRMNSPRM         0x1251   /* Parameter Not Supported              */
#define RECCNTRM         0x125b   /* Load Records Count Mismatch          */ /*@A3C*/
#define RECDMGRM         0x1249   /* Record Damaged                       */
#define RECINARM         0x1259   /* Record Inactive                      */
#define RECIUSRM         0x124a   /* Record In Use                        */
#define RECLENRM         0x1215   /* Record Length Mismatch               */
#define RECNAVRM         0x126f   /* Record Not Available                 */
#define RECNBRRM         0x1224   /* Record Number Out Of Bounds          */
#define RECNFNRM         0x1225   /* Record Not Found                     */
#define RSCLMTRM         0x1233   /* Resource Limits Reached              */
#define SRCLMTRM         0xf210   /* Resource Limits Reached in Source    */
                                  /*    System                            */
#define STGATHRM         0x121A   /* Not Authorized to Storage Class      */ /*@A3C*/
#define STGCNFRM         0x1239   /* Storage Class Conflict               */
#define STGNFNRM         0x1216   /* Storage Class Not Found              */ /*@A3C*/
#define SYNTAXRM         0x124c   /* Data Stream Syntax Error             */
#define TRGNSPRM         0x125f   /* Target Not Supported                 */
#define UPDCSRRM         0x124d   /* Update Cursor Error                  */
#define UPDINTRM         0x124e   /* No Update Intent on Record           */
#define VALNSPRM         0x1252   /* Parameter Value Not Supported        */
#define XLATERM          0xF203   /* Translation Error                    */ /*@A3C*/
#define WARNING          0xF217   /* Message warning                      */ /*@A6A*/
/*-------------------------------------------------------------------------
-- Attribute Code Points
--------------------------------------------------------------------------*/
#define ACCINTLS         0x1134      /* Access Intent List                */
#define ACCMTHLS         0x1402      /* Access Method List                */
#define ALCINIEX         0x1154      /* Allocate Initial Extent           */
#define ALTINDLS         0x144e      /* Alternate Index List              */
#define BASFILNM         0x1103      /* Base File                         */
#define DCLNAM           0x1136      /* Declared Name               @A5A  */
#define DELCP            0x111b      /* Record Deletion Capability        */
#define DTACLSNM         0x1121      /* Data Class Name                   */
#define EOFNBR           0x110b      /* End of File Record Number         */
#define FILBYTCN         0x1139      /* File Byte Count                   */
#define FILCHGDT         0x113a      /* File Change Date                  */
#define FILCLS           0x1110      /* File Class                        */
#define FILCRTDT         0x1108      /* File Creation Date                */
#define FILHDD           0x1132      /* File Hidden                       */
#define FILOPNLO         0x1106      /* File Open Lock Option       @A5A  */
#define FILPRT           0x112a      /* File Protected                    */
#define FILINISZ         0x113c      /* Initial File Size                 */
#define FILSIZ           0x110f      /* File Size                         */
#define FILSYS           0x1133      /* System File                       */
#define GETCP            0x1191      /* File Get Capability               */
#define INSCP            0x1192      /* File Insert Capability            */
#define KEYDEF           0x1114      /* Key Definition                    */
#define KEYDUPCP         0x113d      /* Duplicate Keys Capability         */
#define KEYVAL           0x1115      /* Key Value                         */
#define LSTACCDT         0x1113      /* Last Access Date                  */
#define LSTARCDT         0x118a      /* Last Archive Date                 */
#define MAXARNB          0x1159      /* Maximum Active Record Number      */
#define MGMCLSNM         0x1140      /* Management Class Name             */
#define MODCP            0x1166      /* File Modify Capability            */
#define RECLEN           0x111c      /* Record Length                     */
#define RECLENCL         0x1142      /* Record Length Class               */
#define RTNCLS           0x1148      /* File Retention Class              */
#define STGCLSNM         0x1141      /* Storage Class Name                */
#define TITLE            0x0045      /* A Brief Description               */

/*-------------------------------------------------------------------------
-- CopyFile Code Points
--------------------------------------------------------------------------*/
#define CPYAPP           0x1482   /* Copy Append Option                   */
#define CPYERR           0x1483   /* Return Dup File Name Reply Msg       */
#define CPYRPL           0x1484   /* Copy Replace Option                  */
#define CPYNDT           0x1467   /* Copy with No Data Option             */
#define CPYDTA           0x1466   /* Copy with Data Option                */
#define CPYNCR           0x1462   /* Copy with No Creation Option         */

/*-------------------------------------------------------------------------
-- Access Intent Code Points
--------------------------------------------------------------------------*/
#define DELAI            0x140b   /* Delete Record Access Intent          */
#define GETAI            0x1416   /* Get Record Access Intent             */
#define INSAI            0x1417   /* Insert Record Access Intent          */
#define MODAI            0x1428   /* Modify Record Access Intent          */

/*-------------------------------------------------------------------------
-- Access Method Code Points
--------------------------------------------------------------------------*/
#define RELRNBAM         0x1433   /* Relative by Record Number            */
#define RNDRNBAM         0x1435   /* Random by record Number              */
#define CMBRNBAM         0x1407   /* Combined Record Number               */
#define RELKEYAM         0x1432   /* Relative by Key                      */
#define RNDKEYAM         0x1434   /* Random by Key                        */
#define CMBKEYAM         0x1406   /* Combined Keyed                       */
#define CMBACCAM         0x1405   /* Combined Access                      */

/*-------------------------------------------------------------------------
-- Other DDM Code Points
--------------------------------------------------------------------------*/
#define SEQASC           0x1420   /* Ascending Key Sequence Indicator     */
#define SEQDSC           0x1421   /* Descending key sequence Indicator    */
#define BYTSTRDR         0x0044   /* Byte String Key Class                */
#define DIRFIL           0x140c   /* Direct File                          */
#define KEYFIL           0x141e   /* Keyed File                           */
#define SEQFIL           0x143b   /* Sequential File                      */
#define ALTINDF          0x1423   /* Alternate Index File                 */
#define DUPFILDO         0x1459   /* Duplicate File Reply Message         */
                                  /*    Duplicate Option                  */
#define BYTEFIL          0x1465   /* Sequential File                      */
#define EXSCNDDO         0x145a   /* Existing Condition Reply Message     */
                                  /*    Duplicate Option                  */
#define CLRFILDO         0x145c   /* Clear File Duplicate Option          */
#define RECFIX           0x142e   /* Fixed Length Record                  */
#define RECIVL           0x142f   /* Initially Varying Length Record      */
#define RECVAR           0x1431   /* Varying Length Record                */
#define KEYFLDDF         0x140f   /* Key Field Definition                 */
#define KEYAE            0x1445   /* Key greater than or equal to         */
#define KEYAF            0x1446   /* Key greater than                     */
#define KEYBE            0x144b   /* Key less than or equal to            */
#define KEYBF            0x144c   /* Key less than                        */
#define KEYEQ            0x1447   /* Key equal                            */
#define KEYHLM           0x112f   /* High Key Limit                       */
#define KEYLLM           0x1130   /* Low Key Limit                        */
#define DFTREC           0x142b   /* Default Record                       */
#define NIL              0x002a   /* Nil                                  */
#define DFTINAIN         0x1460   /* Default Inactive Record              */
                                  /*    Initialization                    */
#define DFTTRGIN         0x145f   /* Default Target Initialization        */
#define DFTSRCIN         0x1449   /* Default Source Initialization        */
#define RECORD           0x144a   /* Record                               */
#define ACCORD           0x1162   /* Access Order                         */

#define DDM_FALSE    (BYTE)0xf0
#define DDM_TRUE     (BYTE)0xf1

#define SVRCOD           0x1149   /* Severity Code                        */
#define SRVDGN           0x1153   /* Server Diagnostic Information        */
#define CSRPOSST         0x115b   /* Cursor Position Status               */
#define DTALCKST         0x115c   /* Data Lock Status                     */
#define FILNAM           0x110e   /* File Name                            */
#define RECCNT           0x111a   /* Record Count                         */
#define ACCMTHCL         0x114e   /* Access Method Class                  */
#define CODPNT           0x000c   /* Codepoint                            */
#define ERRFILNM         0x1126   /* Error File Name                      */
#define KEYDEFCD         0x1164   /* Key Definition Error Code            */
#define MAXOPN           0x1157   /* Maximum Number of Files Opened       */
#define NEWFILNM         0x114f   /* New File Name                        */
#define PRCCNVCD         0x113f   /* Conversion Protocol Error            */
#define RECNBR           0x111d   /* Record Number                        */
#define SYNERRCD         0x114a   /* Syntax Error Code                    */
#define DATE             0x000f   /* Date                                 */
#define BASMGMNM         0x11D3   /* Base Management Class Name           */
#define BASSTGNM         0x11D4   /* Base Storage Class Name              */
#define DUPFILOPT        0x1137   /* Duplicate File Option value          */

/*-------------------------------------------------------------------------
-- Misc. Code Points
--------------------------------------------------------------------------*/
#define KEYVAL           0x1115   /* Key Value Object                     */
#define PRMFIL           0x142a   /* Protected File                       */
#define TMPFIL           0x143e   /* Temporary File                       */
#define RECINA           0x142d   /* Inactive Record                      */
#define DUPFILOPT        0x1137   /* Duplicate File Option                */

#define RECAL            0x1430   /* Record Attribute List Codepoint      */
#define RECSEQ           0x145e   /* Record Sequence                      */
#define KEYSEQ           0x145d   /* Key Sequence                         */
#define DFTRECOP         0x1111   /* Default Record Option                */
