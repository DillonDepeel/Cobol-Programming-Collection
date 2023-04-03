/**************************************************************************
******************************  DUBFCN.H  *********************************
***************************************************************************
*
*                              Record Level I/O
*
* Module Name: DDMFCN.H
*
* DDM Function Codes
*
* RLIO (program no. 5648-02011)
*
* Version: 1.0
* Release: 1.0
* Level:   0.0
*
*
* Copyright (C)
* International Business Machines Corporation 1992
*
* CHANGE ACTIVITY:
* FLAG REASON  LEVEL   DATE    PGMR       COMMENTS
* ---- ------  -----   ----    ----       --------
* $A0= ENHCVT  Rel1.2  042195  SSWang     : Add a convertion function
***************************************************************************/

#define FCN_CLOSE             0     /* Close a record file                    */
#define FCN_CLEARFILE         1     /* Clear a record file                    */
#define FCN_COPYFILE          2     /* Copy a record file                     */
#define FCN_CREATEALTINDEX    3     /* Create an alternate index file         */
#define FCN_CREATERECFILE     4     /* Create a record-oriented file          */
#define FCN_DELETE            5     /* Delete a record file                   */
#define FCN_DELETEREC         6     /* Delete a record                        */
#define FCN_QUERYFILEINFO     7     /* Get a record file's information        */
#define FCN_QUERYPATHINFO     8     /* Get a record file's information        */
#define FCN_GETREC            9     /* Get a record                           */
#define FCN_INSERTRECEOF      10    /* Insert record at EOF                   */
#define FCN_INSERTRECKEY      11    /* Insert record by key value             */
#define FCN_INSERTRECNUM      12    /* Insert record by record number         */
#define FCN_LOADFILEFIRST     13    /* Load records into file                 */
#define FCN_LOADFILENEXT      14    /* Load records into file                 */
#define FCN_MODIFYREC         15    /* Modify record                          */
#define FCN_OPEN              16    /* Open a record file                     */
#define FCN_RENAME            17    /* Rename a record file                   */
#define FCN_SETBOF            18    /* Set cursor to beginning of file        */
#define FCN_SETEOF            19    /* Set cursor to end of file              */
#define FCN_SETFILEINFO       20    /* Set a record file's information        */
#define FCN_SETFIRST          21    /* Set cursor to first record             */
#define FCN_SETKEY            22    /* Set cursor by key                      */
#define FCN_SETKEYFIRST       23    /* Set cursor to first rec in key sequence*/
#define FCN_SETKEYLIMITS      24    /* Set key limits                         */
#define FCN_SETKEYLAST        25    /* Set cursor to last rec in key sequence */
#define FCN_SETKEYNEXT        26    /* Set cursor to next rec in key sequence */
#define FCN_SETKEYPREVIOUS    27    /* Set cursor to previous rec in key seq  */
#define FCN_SETLAST           28    /* Set cursor to last record              */
#define FCN_SETMINUS          29    /* Set cursor minus                       */
#define FCN_SETRECNUM         30    /* Set cursor to record number            */
#define FCN_SETNEXTREC        31    /* Set cursor to next record              */
#define FCN_SETNEXTKEYEQUAL   32    /* Set cursor to next record w/ equal key */
#define FCN_SETPATHINFO       33    /* Set a record file's information        */
#define FCN_SETPLUS           34    /* Set cursor plus                        */
#define FCN_SETPREVIOUS       35    /* Set cursor to previous record          */
#define FCN_SETUPDATEKEY      36    /* Set update intent by key value         */
#define FCN_SETUPDATENUM      37    /* Set update intent by record number     */
#define FCN_UNLOADFILEFIRST   38    /* Unload records from file               */
#define FCN_UNLOADFILENEXT    39    /* Unload records from file               */
#define FCN_UNLOCKREC         40    /* Release implicit record locks          */

#define FCN_DUPHANDLE         41    /* Duplicate a Record File Handle         */
#define FCN_EXECPGM           42    /* Execute a Program as a Child Process   */
#define FCN_FORCEBUFFER       43    /* Commit a File's Cached Information     */
#define FCN_FSATTACH          44    /* Attach a Device                        */
#define FCN_QUERYFHSTATE      45    /* Query File Handle State                */
#define FCN_QUERYHTYPE        46    /* Query Handle Type                      */
#define FCN_SETFHSTATE        47    /* Set File Handle State                  */
#define FCN_TRUNCFILE         48    /* Truncate File                          */
#define FCN_OEM               49    /* OEM call: For User DLLs only           */
#define FCN_CONVERT           50    /* 1.0 key file convert to 1.1 format @A0A*/
