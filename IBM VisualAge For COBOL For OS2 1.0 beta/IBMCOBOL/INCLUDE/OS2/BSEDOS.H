/****************************** Module Header ******************************\
*
* Module Name: BSEDOS.H
*
* OS/2 Base include file.
*
* Copyright (c) International Business Machines Corporation 1981, 1988-1992
*
* ===========================================================================
*
* The folowing symbols are used in this file for conditional sections.
*
* If INCL_BSE is defined, all subcomponents are included.
*
* Subcomponents marked with "+" are partially included by default:
*
*   #define:                To include:
*
* + INCL_DOSPROCESS         Process and thread support
* + INCL_DOSFILEMGR         File Management
* + INCL_DOSMEMMGR          Memory Management
* + INCL_DOSSEMAPHORES      Semaphore support
* + INCL_DOSDATETIME        Date/Time and Timer support
*   INCL_DOSMODULEMGR       Module manager
* + INCL_DOSRESOURCES       Resource support
*   INCL_DOSNLS             National Language Support
*   INCL_DOSEXCEPTIONS      Exception Management Support
*   INCL_DOSMISC            Miscellaneous
*   INCL_DOSMONITORS        Monitors
*   INCL_DOSQUEUES          Queues
*   INCL_DOSSESMGR          Session Manager Support
*   INCL_DOSDEVICES         Device specific, ring 2 support
*   INCL_DOSNMPIPES         Named Pipes Support
*   INCL_DOSPROFILE         DosProfile API
*   INCL_DOSMVDM            MVDM support
*
* ===========================================================================
*
* Comments at the end of each typedef line give the name tags used in
* the assembler include version of this file.
*
* The assembler include version of this file excludes lines between NOINC
* and INC comments.
*
\***************************************************************************/

/* NOINC */
#if __IBMC__ || __IBMCPP__
   #pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( none )
   #endif
   #pragma info( restore )
#endif
#ifdef __cplusplus
      extern "C" {
#endif
/* INC */

#ifndef __BSEDOS__

/* NOINC */
#define __BSEDOS__
/* INC */

#define INCL_DOSINCLUDED

#ifdef INCL_DOS
   #define INCL_DOSPROCESS
   #define INCL_DOSFILEMGR
   #define INCL_DOSMEMMGR
   #define INCL_DOSSEMAPHORES
   #define INCL_DOSDATETIME
   #define INCL_DOSMODULEMGR
   #define INCL_DOSRESOURCES
   #define INCL_DOSNLS
   #define INCL_DOSEXCEPTIONS
   #define INCL_DOSMISC
   #define INCL_DOSMONITORS
   #define INCL_DOSQUEUES
   #define INCL_DOSSESMGR
   #define INCL_DOSDEVICES
   #define INCL_DOSNMPIPES
   #define INCL_DOSPROFILE
   #define INCL_DOSMVDM
#endif /* INCL_DOS */

#ifdef INCL_ERRORS
   #define INCL_DOSERRORS
#endif /* INCL_ERRORS */

#if (defined(INCL_DOSPROCESS) || !defined(INCL_NOCOMMON))
   /*** General services */
   APIRET APIENTRY DosBeep(ULONG freq,
                           ULONG dur);

   /*** Process and Thread support */
   VOID APIENTRY DosExit(ULONG action,
                         ULONG result);

   /* DosExit codes */
   #define EXIT_THREAD        0
   #define EXIT_PROCESS       1
#endif /* common INCL_DOSPROCESS stuff */

#ifdef INCL_DOSPROCESS
/* NOINC */
   #define DosCwait           DosWaitChild
   #define DosSetPrty         DosSetPriority
/* INC */

   #include <bsetib.h>

   typedef  VOID APIENTRY FNTHREAD(ULONG);
   typedef FNTHREAD *PFNTHREAD;

   APIRET APIENTRY DosCreateThread(PTID ptid,
                                   PFNTHREAD pfn,
                                   ULONG param,
                                   ULONG flag,
                                   ULONG cbStack);

   APIRET APIENTRY DosResumeThread(TID tid);

   APIRET APIENTRY DosSuspendThread(TID tid);

   APIRET APIENTRY DosGetInfoBlocks(PTIB *pptib,
                                    PPIB *pppib);

   APIRET APIENTRY DosKillThread(TID tid);

   APIRET APIENTRY DosAllocThreadLocalMemory(ULONG cb, PULONG *p);

   APIRET APIENTRY DosFreeThreadLocalMemory(ULONG *p);

   /* Action code values */

   #define DCWA_PROCESS       0
   #define DCWA_PROCESSTREE   1

   /* Wait option values */

   #define DCWW_WAIT          0
   #define DCWW_NOWAIT        1

/* Thread Flags for DosCreateThread options 59468 */

#define CREATE_READY        0                   /* defect 65437  */
#define CREATE_SUSPENDED    1
#define STACK_SPARSE        0
#define STACK_COMMITTED     2

   typedef struct _RESULTCODES       /* resc */
   {
      ULONG codeTerminate;
      ULONG codeResult;
   } RESULTCODES;
   typedef RESULTCODES *PRESULTCODES;

   APIRET APIENTRY DosWaitChild(ULONG action,
                                ULONG option,
                                PRESULTCODES pres,
                                PPID ppid,
                                PID pid);

   APIRET APIENTRY DosWaitThread(PTID ptid,
                                 ULONG option);

   APIRET APIENTRY DosSleep(ULONG msec);

#ifndef DBG_INCL_DOSDEBUG

#define DBG_INCL_DOSDEBUG


/***            User's Debug Buffer structure
 *
 *      A pointer to a uDB is the sole parameter to DosDebug.  It
 *      contains all the information required for each DosDebug
 *      command.
 *
 */

typedef struct _uDB {           /* uDB */
        unsigned long   Pid;            /* Debuggee Process id          */
        unsigned long   Tid;            /* Debuggee Thread id           */
        long            Cmd;            /* Command or Notification      */
        long            Value;          /* Generic Data Value           */
        unsigned long   Addr;           /* Debuggee Address             */
        unsigned long   Buffer;         /* Debugger Buffer Address      */
        unsigned long   Len;            /* Length of Range              */
        unsigned long   Index;          /* Generic Identifier Index     */
        unsigned long   MTE;            /* Module Table Entry Handle    */
        unsigned long   EAX;            /* Register Set                 */
        unsigned long   ECX;
        unsigned long   EDX;
        unsigned long   EBX;
        unsigned long   ESP;
        unsigned long   EBP;
        unsigned long   ESI;
        unsigned long   EDI;
        unsigned long   EFlags;
        unsigned long   EIP;
        unsigned long   CSLim;
        unsigned long   CSBase;
        unsigned char   CSAcc;
        unsigned char   CSAtr;
        unsigned short  CS;
        unsigned long   DSLim;
        unsigned long   DSBase;
        unsigned char   DSAcc;
        unsigned char   DSAtr;
        unsigned short  DS;
        unsigned long   ESLim;
        unsigned long   ESBase;
        unsigned char   ESAcc;
        unsigned char   ESAtr;
        unsigned short  ES;
        unsigned long   FSLim;
        unsigned long   FSBase;
        unsigned char   FSAcc;
        unsigned char   FSAtr;
        unsigned short  FS;
        unsigned long   GSLim;
        unsigned long   GSBase;
        unsigned char   GSAcc;
        unsigned char   GSAtr;
        unsigned short  GS;
        unsigned long   SSLim;
        unsigned long   SSBase;
        unsigned char   SSAcc;
        unsigned char   SSAtr;
        unsigned short  SS;
} uDB_t;

/***            DosDebug Command Numbers
 *
 *      These numbers are placed in the Cmd field of the uDB on
 *      entry to DosDebug.
 *
 *      These numbers identify which command DosDebug is requested
 *      to perform.
 *
 */

#define DBG_C_Null              0       /* Null                         */
#define DBG_C_ReadMem           1       /* Read Word                    */
#define DBG_C_ReadMem_I         1       /* Read Word                    */
#define DBG_C_ReadMem_D         2       /* Read Word (same as 1)        */
#define DBG_C_ReadReg           3       /* Read Register Set            */
#define DBG_C_WriteMem          4       /* Write Word                   */
#define DBG_C_WriteMem_I        4       /* Write Word                   */
#define DBG_C_WriteMem_D        5       /* Write Word (same as 4)       */
#define DBG_C_WriteReg          6       /* Write Register Set           */
#define DBG_C_Go                7       /* Go                           */
#define DBG_C_Term              8       /* Terminate                    */
#define DBG_C_SStep             9       /* Single Step                  */
#define DBG_C_Stop              10      /* Stop                         */
#define DBG_C_Freeze            11      /* Freeze Thread                */
#define DBG_C_Resume            12      /* Resume Thread                */
#define DBG_C_NumToAddr         13      /* Object Number to Address     */
#define DBG_C_ReadCoRegs        14      /* Read Coprocessor Registers   */
#define DBG_C_WriteCoRegs       15      /* Write Coprocessor Registers  */
                                        /* 16 is reserved               */
#define DBG_C_ThrdStat          17      /* Get Thread Status            */
#define DBG_C_MapROAlias        18      /* Map read-only alias          */
#define DBG_C_MapRWAlias        19      /* Map read-write alias         */
#define DBG_C_UnMapAlias        20      /* Unmap Alias                  */
#define DBG_C_Connect           21      /* Connect to Debuggee          */
#define DBG_C_ReadMemBuf        22      /* Read Memory Buffer           */
#define DBG_C_WriteMemBuf       23      /* Write Memory Buffer          */
#define DBG_C_SetWatch          24      /* Set Watchpoint               */
#define DBG_C_ClearWatch        25      /* Clear Watchpoint             */
#define DBG_C_RangeStep         26      /* Range Step                   */
#define DBG_C_Continue          27      /* Continue after an Exception  */
#define DBG_C_AddrToObject      28      /* Address to Object            */
#define DBG_C_XchgOpcode        29      /* Exchange opcode and go       */
#define DBG_C_LinToSel          30      /* 32 to 16 conversion      A001*/
#define DBG_C_SelToLin          31      /* 16 to 32 conversion      A001*/


/***            DosDebug Notification Numbers
 *
 *      These numbers are placed in the Cmd field of the uDB upon
 *      exit from DosDebug.
 *
 *      These numbers identify which DosDebug Event just occured,
 *      or whether a particular command succeeded or failed.
 *
 */

#define DBG_N_Success           0L      /* Command completed successfully  */
#define DBG_N_Error             -1L     /* Error detected during command   */
#define DBG_N_ProcTerm          -6L     /* Process exiting - ExitList done */
#define DBG_N_Exception         -7L     /* Exception detected              */
#define DBG_N_ModuleLoad        -8L     /* Module loaded                   */
#define DBG_N_CoError           -9L     /* Coprocessor not in use error    */
#define DBG_N_ThreadTerm        -10L    /* Thread exiting - Exitlist soon  */
#define DBG_N_AsyncStop         -11L    /* Async Stop detected             */
#define DBG_N_NewProc           -12L    /* New Process started             */
#define DBG_N_AliasFree         -13L    /* Alias needs to be freed         */
#define DBG_N_Watchpoint        -14L    /* Watchpoint hit                  */
#define DBG_N_ThreadCreate      -15L    /* New thread created              */
#define DBG_N_ModuleFree        -16L    /* Module freed                    */
#define DBG_N_RangeStep         -17L    /* Range Step completed            */

/***          - Thread Status Buffer structure
 *
 *      A pointer to a TStat structure is required for the
 *      DBG_C_ThrdStat command.
 *
 *      The TStat structure returns information about a thread.
 *
 *      DbgState in the Thread Status buffer contains info about the
 *      current state of debugging, and will have one of the following
 *      values upon return :
 *
 *      DBG_D_Thawed, DBG_D_Frozen
 *
 *      TState in the Thread Status buffer contains info about the
 *      scheduling state of the thread, and will have one of the
 *      following values upon return.
 *
 *      DBG_T_Runnable, DBG_T_Suspended, DBG_T_Blocked, DBG_T_CritSec
 *
 *      TPriority in the Thread Status buffer contains the thread's
 *      base scheduling priority.  This priority will be expressed as
 *      scheduling class and delta values upon return.
 *
 */

typedef struct _TStat {         /* TS */
        unsigned char   DbgState;       /* Thread's Debugging State      */
        unsigned char   TState;         /* Thread's Scheduler State     */
        unsigned short  TPriority;      /* Thread's Scheduler Priority  */
} TStat_t;

/***            DbgState values
 *
 *      These are the possible values which can be returned
 *      in the DbgState field of the TStat structure. These
 *      bits identify debugging information.
 *
 */

#define DBG_D_Thawed    0
#define DBG_D_Frozen    1

/***            TState values
 *
 *      These are the possible values which can be returned in
 *      the TState field of the TStat structure.  These values
 *      identify scheduler state information.
 *
 */

#define DBG_T_Runnable  0
#define DBG_T_Suspended 1
#define DBG_T_Blocked   2
#define DBG_T_CritSec   3

/***             Coprocessor Type Parameters
 *
 *      These are the possible values identifying the coprocessor
 *      types supported by DosDebug used when accessing the
 *      coprocessor register set.
 *
 */

#define DBG_CO_387      1

/***EK+ DBG_LEN - Coprocessor Buffer Lengths
 *
 *      These are the possible values identifying the length of
 *      the coprocessor buffer when accessing the coprocessor
 *      register set.
 *
 */

#define DBG_LEN_387     108

/***            Debugging Level Parameter
 *
 *      This is the only possible value for the DBG_C_Connect command,
 *      and identifies that debugging on the 386 envoronment is desired.
 *
 */

#define DBG_L_386       1

/***            Watchpoint Scope and Type Parameters
 *
 *      These are the possible Watchpoint Scope values, as used by
 *      the DBG_C_SetWatch command.     The DBG_C_SetWatch command
 *      expects a  combination (using either an ADD or an OR) of
 *      the Scope and Type parameters to be passed as a single value.
 *
 */

/*      Watchpoint Scope Parameters     */

#define DBG_W_Global    0x00000001
#define DBG_W_Local     0x00000002

/*      Watchpoint Type Parameters      */

#define DBG_W_Execute   0x00010000
#define DBG_W_Write     0x00020000
#define DBG_W_ReadWrite 0x00030000

/***            Object flags
 *
 *      This flag indicates whether the MTE field contains the object's
 *      MTE after a DBG_C_AddrToObject call.
 *
 */

#define DBG_O_OBJMTE    0x10000000

/***            Exception chances
 *
 *      The three scenarios under which a debug exception is reported
 *      are pre-1st, 1st, and last chance. The value field of the
 *      user debug buffer will indicate which chance a given call is
 *      for. Additional parameter values will be:
 *
 *         For pre-1st chance (XCPT_BREAKPOINT):
 *              Addr   = Linear address of breakpoint
 *              Buffer = XCPT_BREAKPOINT
 *
 *         For pre-1st change (XCPT_SINGLE_STEP):
 *              Addr   = Linear address of instruction after Single Step
 *              Buffer = XCPT_SINGLE_STEP
 *
 *         For 1st chance (all exceptions):
 *              Addr   = Linear address of exception
 *              Buffer = Pointer to Exception Report Record in
 *                       Debuggee's context
 *              Len    = Pointer to Exception Context Record in
 *                       Debuggee's context
 *
 *         For Last chance (all exceptions):
 *              Addr   = Linear address of exception
 *              Buffer = Pointer to Exception Report Record in
 *                       Debuggee's context
 *              Len    = Pointer to Exception Context Record in
 *                       Debuggee's context
 *
 *         For Invalid stack notification (all exceptions)
 *              Addr   = Linear address of exception
 *              Buffer = Exception number
 */

#define DBG_X_PRE_FIRST_CHANCE  0x00000000
#define DBG_X_FIRST_CHANCE      0x00000001
#define DBG_X_LAST_CHANCE       0x00000002
#define DBG_X_STACK_INVALID     0x00000003

#endif /* DBG_INCL_DOSDEBUG */

   APIRET APIENTRY DosDebug(PVOID pdbgbuf);


   /* codeTerminate values (also passed to ExitList routines) */

   #define TC_EXIT            0
   #define TC_HARDERROR       1
   #define TC_TRAP            2
   #define TC_KILLPROCESS     3
   #define TC_EXCEPTION       4

   typedef VOID APIENTRY FNEXITLIST(ULONG);
   typedef FNEXITLIST *PFNEXITLIST;

   APIRET APIENTRY DosEnterCritSec(VOID);

   APIRET APIENTRY DosExitCritSec(VOID);

   APIRET APIENTRY DosExitList(ULONG ordercode,
                               PFNEXITLIST pfn);

   /* DosExitList functions */

   #define EXLST_ADD          1
   #define EXLST_REMOVE       2
   #define EXLST_EXIT         3

#ifdef __cplusplus
   APIRET APIENTRY DosExecPgm(PCHAR pObjname,
                              LONG cbObjname,
                              ULONG execFlag,
                              PCSZ  pArg,
                              PCSZ  pEnv,
                              PRESULTCODES pRes,
                              PCSZ  pName);
#else
   APIRET APIENTRY DosExecPgm(PCHAR pObjname,
                              LONG cbObjname,
                              ULONG execFlag,
                              PSZ pArg,
                              PSZ pEnv,
                              PRESULTCODES pRes,
                              PSZ pName);
#endif

   /* DosExecPgm functions */

   #define EXEC_SYNC          0
   #define EXEC_ASYNC         1
   #define EXEC_ASYNCRESULT   2
   #define EXEC_TRACE         3
   #define EXEC_BACKGROUND    4
   #define EXEC_LOAD          5
   #define EXEC_ASYNCRESULTDB 6


   APIRET APIENTRY  DosSetPriority(ULONG scope,
                                   ULONG ulClass,
                                   LONG  delta,
                                   ULONG PorTid);

   /* Priority scopes */

   #define PRTYS_PROCESS      0
   #define PRTYS_PROCESSTREE  1
   #define PRTYS_THREAD       2

   /* Priority classes */

   #define PRTYC_NOCHANGE     0
   #define PRTYC_IDLETIME     1
   #define PRTYC_REGULAR      2
   #define PRTYC_TIMECRITICAL 3
   #define PRTYC_FOREGROUNDSERVER 4

   /* Priority deltas */

   #define PRTYD_MINIMUM     -31
   #define PRTYD_MAXIMUM      31

   APIRET APIENTRY DosKillProcess(ULONG action,
                                  PID pid);

   #define DKP_PROCESSTREE    0
   #define DKP_PROCESS        1
#endif /* INCL_DOSPROCESS */

#ifndef INCL_SAADEFS
   /*************************************************************************\
   * CCHMAXPATH is the maximum fully qualified path name length including  *
   * the drive letter, colon, backslashes and terminating NULL.            *
   \*************************************************************************/
   #define CCHMAXPATH         260

   /*************************************************************************\
   * CCHMAXPATHCOMP is the maximum individual path component name length   *
   * including a terminating NULL.                                         *
   \*************************************************************************/
   #define CCHMAXPATHCOMP     256
#endif  /* !INCL_SAADEFS */

#if (defined(INCL_DOSFILEMGR) || !defined(INCL_NOCOMMON))
   /*** File manager */
   /* DosSetFilePtr() file position codes */

   #define FILE_BEGIN      0x0000   /* Move relative to beginning of file */
   #define FILE_CURRENT    0x0001   /* Move relative to current fptr position */
   #define FILE_END        0x0002   /* Move relative to end of file */

   /* DosFindFirst/Next Directory handle types */
   #define HDIR_SYSTEM        1     /* Use system handle (1) */
   #define HDIR_CREATE      (-1)    /* Allocate a new, unused handle */

   /* DosCopy control bits; may be or'ed together */
   #define DCPY_EXISTING   0x0001   /* Copy even if target exists */
   #define DCPY_APPEND     0x0002   /* Append to existing file, do not replace */
   #define DCPY_FAILEAS    0x0004   /* Fail if EAs not supported on target*/

   /* DosOpen/DosQFHandState/DosQueryFileInfo et al file attributes; also */
   /* known as Dos File Mode bits... */
   #define FILE_NORMAL     0x0000
   #define FILE_READONLY   0x0001
   #define FILE_HIDDEN     0x0002
   #define FILE_SYSTEM     0x0004
   #define FILE_DIRECTORY  0x0010
   #define FILE_ARCHIVED   0x0020

   #define FILE_IGNORE     0x10000     /* ignore file attribute in */
   /* DosSetPath/File Info if */
   /* this bit is set*/

   #define MUST_HAVE_READONLY      ( (FILE_READONLY  << 8) | FILE_READONLY  )
   #define MUST_HAVE_HIDDEN        ( (FILE_HIDDEN    << 8) | FILE_HIDDEN    )
   #define MUST_HAVE_SYSTEM        ( (FILE_SYSTEM    << 8) | FILE_SYSTEM    )
   #define MUST_HAVE_DIRECTORY     ( (FILE_DIRECTORY << 8) | FILE_DIRECTORY )
   #define MUST_HAVE_ARCHIVED      ( (FILE_ARCHIVED  << 8) | FILE_ARCHIVED  )

   /* DosOpen() actions */
   #define FILE_EXISTED    0x0001
   #define FILE_CREATED    0x0002
   #define FILE_TRUNCATED  0x0003

   /* DosOpen() open flags */
   #define FILE_OPEN       0x0001
   #define FILE_TRUNCATE   0x0002
   #define FILE_CREATE     0x0010

   /*     this nibble applies if file already exists                xxxx */
   #define OPEN_ACTION_FAIL_IF_EXISTS     0x0000  /* ---- ---- ---- 0000 */
   #define OPEN_ACTION_OPEN_IF_EXISTS     0x0001  /* ---- ---- ---- 0001 */
   #define OPEN_ACTION_REPLACE_IF_EXISTS  0x0002  /* ---- ---- ---- 0010 */

   /*     this nibble applies if file does not exist           xxxx      */
   #define OPEN_ACTION_FAIL_IF_NEW        0x0000  /* ---- ---- 0000 ---- */
   #define OPEN_ACTION_CREATE_IF_NEW      0x0010  /* ---- ---- 0001 ---- */

   /* DosOpen/DosSetFHandState flags */
   #define OPEN_ACCESS_READONLY           0x0000  /* ---- ---- ---- -000 */
   #define OPEN_ACCESS_WRITEONLY          0x0001  /* ---- ---- ---- -001 */
   #define OPEN_ACCESS_READWRITE          0x0002  /* ---- ---- ---- -010 */
   #define OPEN_SHARE_DENYREADWRITE       0x0010  /* ---- ---- -001 ---- */
   #define OPEN_SHARE_DENYWRITE           0x0020  /* ---- ---- -010 ---- */
   #define OPEN_SHARE_DENYREAD            0x0030  /* ---- ---- -011 ---- */
   #define OPEN_SHARE_DENYNONE            0x0040  /* ---- ---- -100 ---- */
   #define OPEN_FLAGS_NOINHERIT           0x0080  /* ---- ---- 1--- ---- */
   #define OPEN_FLAGS_NO_LOCALITY         0x0000  /* ---- -000 ---- ---- */
   #define OPEN_FLAGS_SEQUENTIAL          0x0100  /* ---- -001 ---- ---- */
   #define OPEN_FLAGS_RANDOM              0x0200  /* ---- -010 ---- ---- */
   #define OPEN_FLAGS_RANDOMSEQUENTIAL    0x0300  /* ---- -011 ---- ---- */
   #define OPEN_FLAGS_NO_CACHE            0x1000  /* ---1 ---- ---- ---- */
   #define OPEN_FLAGS_FAIL_ON_ERROR       0x2000  /* --1- ---- ---- ---- */
   #define OPEN_FLAGS_WRITE_THROUGH       0x4000  /* -1-- ---- ---- ---- */
   #define OPEN_FLAGS_DASD                0x8000  /* 1--- ---- ---- ---- */
   #define OPEN_FLAGS_NONSPOOLED          0x00040000
   #define OPEN_FLAGS_PROTECTED_HANDLE 0x40000000


   /* DosSearchPath() constants */
   #define SEARCH_PATH           0x0000
   #define SEARCH_CUR_DIRECTORY  0x0001
   #define SEARCH_ENVIRONMENT    0x0002
   #define SEARCH_IGNORENETERRS  0x0004


   /************************************************************
   EA Info Levels & Find First/Next
   =========================================
   API's: DosFindFirst, DosQueryFileInfo, DosQueryPathInfo, DosSetFileInfo,
   DosSetPathInfo
   ************************************************************/

   /* File info levels&gml All listed API's */
   #define FIL_STANDARD          1     /* Info level 1, standard file info */
   #define FIL_QUERYEASIZE       2     /* Level 2, return Full EA size */
   #define FIL_QUERYEASFROMLIST  3     /* Level 3, return requested EA's */

   /* File info levels: Dos...PathInfo only */
   #define FIL_QUERYFULLNAME     5     /* Level 5, return fully qualified
   name of file */


   /* DosFsAttach() */
   /* Attact or detach */
   #define FS_ATTACH             0     /* Attach file server */
   #define FS_DETACH             1     /* Detach file server */
   #define FS_SPOOLATTACH        2     /* Register a spooler device */
   #define FS_SPOOLDETACH        3     /* De-register a spooler device */


   /* DosFsCtl() */
   /* Routing type */
   #define FSCTL_HANDLE          1     /* File Handle directs req routing */
   #define FSCTL_PATHNAME        2     /* Path Name directs req routing */
   #define FSCTL_FSDNAME         3     /* FSD Name directs req routing */

   /* defined FSCTL functions */
   #define FSCTL_ERROR_INFO      1     /* return error info from FSD */
   #define FSCTL_MAX_EASIZE      2     /* Max ea size for the FSD */

   typedef struct  _EASIZEBUF       /* struct for FSCTL fn 2 - max ea size */
   {
      USHORT  cbMaxEASize;        /* max. size of one EA */
      ULONG   cbMaxEAListSize;    /* max size of the full EA List */
   } EASIZEBUF;
   typedef EASIZEBUF  *PEASIZEBUF;



   /* DosQueryFSAttach() */
   /* Information level types (defines method of query) */
   #define FSAIL_QUERYNAME       1     /* Return data for a Drive or Device */
   #define FSAIL_DEVNUMBER       2     /* Return data for Ordinal Device # */
   #define FSAIL_DRVNUMBER       3     /* Return data for Ordinal Drive # */

   /* Item types (from data structure item "iType") */
   #define FSAT_CHARDEV          1     /* Resident character device */
   #define FSAT_PSEUDODEV        2     /* Pusedu-character device */
   #define FSAT_LOCALDRV         3     /* Local drive */
   #define FSAT_REMOTEDRV        4     /* Remote drive attached to FSD */

   typedef struct  _FSQBUFFER       /* fsqbuf Data structure for QFSAttach*/
   {
      USHORT  iType;              /* Item type */
      USHORT  cbName;             /* Length of item name, sans NULL */
      UCHAR   szName[1];          /* ASCIIZ item name */
      USHORT  cbFSDName;          /* Length of FSD name, sans NULL */
      UCHAR   szFSDName[1];       /* ASCIIZ FSD name */
      USHORT  cbFSAData;          /* Length of FSD Attach data returned */
      UCHAR   rgFSAData[1];       /* FSD Attach data from FSD */
   } FSQBUFFER;
   typedef FSQBUFFER  *PFSQBUFFER;


   typedef struct _FSQBUFFER2       /* fsqbuf Data structure for QFSAttach*/
   {
      USHORT  iType;
      USHORT  cbName;
      USHORT  cbFSDName;
      USHORT  cbFSAData;
      UCHAR   szName[1];
      UCHAR   szFSDName[1];
      UCHAR   rgFSAData[1];
   } FSQBUFFER2;
   typedef FSQBUFFER2 *PFSQBUFFER2;

   typedef struct _SPOOLATTACH      /* Data structure for spooler operations */
   {
      USHORT  hNmPipe;            /* Named pipe handle */
      ULONG   ulKey;              /* Attached key */
   } SPOOLATTACH;
   typedef SPOOLATTACH  *PSPOOLATTACH;


   /*****************************************************************************
   * File System Drive Information&gml DosQueryFSInfo DosSetFSInfo              *
   *****************************************************************************/

   /* FS Drive Info Levels */
   #define FSIL_ALLOC            1     /* Drive allocation info (Query only) */
   #define FSIL_VOLSER           2     /* Drive Volum/Serial info */

   /* DosQueryFHType() */
   /* Handle classes (low 8 bits of Handle Type) */
   #define FHT_DISKFILE          0x0000   /* Disk file handle */
   #define FHT_CHRDEV            0x0001   /* Character device handle */
   #define FHT_PIPE              0x0002   /* Pipe handle */

   /* Handle bits (high 8 bits of Handle Type) */
   #define FHB_DSKREMOTE         0x8000   /* Remote disk */
   #define FHB_CHRDEVREMOTE      0x8000   /* Remote character device */
   #define FHB_PIPEREMOTE        0x8000   /* Remote pipe */



   #ifndef INCL_SAADEFS
      /* File time and date types */
#if __IBMC__ || __IBMCPP__
         typedef struct _FTIME           /* ftime */
         {
            UINT   twosecs : 5;
            UINT   minutes : 6;
            UINT   hours   : 5;
         } FTIME;
         typedef FTIME *PFTIME;
      #else
         typedef struct _FTIME           /* ftime */
         {
            USHORT   twosecs : 5;
            USHORT   minutes : 6;
            USHORT   hours   : 5;
         } FTIME;
         typedef FTIME *PFTIME;
      #endif

#if __IBMC__ || __IBMCPP__
         typedef struct _FDATE           /* fdate */
         {
            UINT   day     : 5;
            UINT   month   : 4;
            UINT   year    : 7;
         } FDATE;
         typedef FDATE   *PFDATE;
      #else
         typedef struct _FDATE           /* fdate */
         {
            USHORT   day     : 5;
            USHORT   month   : 4;
            USHORT   year    : 7;
         } FDATE;
         typedef FDATE   *PFDATE;
      #endif
   #endif /* INCL_SAADEFS */


   typedef struct _VOLUMELABEL      /* vol */
   {
      BYTE cch;
      CHAR szVolLabel[12];
   } VOLUMELABEL;
   typedef VOLUMELABEL  *PVOLUMELABEL;

   typedef struct _FSINFO      /* fsinf */
   {
      FDATE fdateCreation;
      FTIME ftimeCreation;
      VOLUMELABEL vol;
   } FSINFO;
   typedef FSINFO *PFSINFO;

   /* HANDTYPE values */
   #define HANDTYPE_FILE         0x0000
   #define HANDTYPE_DEVICE       0x0001
   #define HANDTYPE_PIPE         0x0002
   #define HANDTYPE_PROTECTED    0x4000
   #define HANDTYPE_NETWORK      0x8000

   typedef struct _FILELOCK      /* flock */
   {
      LONG lOffset;
      LONG lRange;
   } FILELOCK;
   typedef FILELOCK  *PFILELOCK;

   typedef LHANDLE HFILE;     /* hf */
   typedef HFILE   *PHFILE;

   #ifndef __HEV__            /* INCL_SEMAPHORE may also define HEV */
      #define __HEV__
      typedef  ULONG    HEV;             /* hev */
      typedef  HEV      *PHEV;
   #endif

   typedef  ULONG  FHLOCK;
   typedef  PULONG  PFHLOCK;

   APIRET APIENTRY DosSetFileLocks(HFILE hFile,
                                   PFILELOCK pflUnlock,
                                   PFILELOCK pflLock,
                                   ULONG timeout,
                                   ULONG flags);
   APIRET APIENTRY DosProtectSetFileLocks(HFILE hFile,
                                          PFILELOCK pflUnlock,
                                          PFILELOCK pflLock,
                                          ULONG timeout, ULONG flags,
                                          FHLOCK fhFileHandleLockID);

   APIRET APIENTRY DosCancelLockRequest(HFILE hFile,
                                        PFILELOCK pflLock);


   #ifndef INCL_SAADEFS

      typedef struct _FILEFINDBUF     /* findbuf */
      {
         FDATE  fdateCreation;
         FTIME  ftimeCreation;
         FDATE  fdateLastAccess;
         FTIME  ftimeLastAccess;
         FDATE  fdateLastWrite;
         FTIME  ftimeLastWrite;
         ULONG  cbFile;
         ULONG  cbFileAlloc;
         USHORT attrFile;
         UCHAR  cchName;
         CHAR   achName[CCHMAXPATHCOMP];
      } FILEFINDBUF;

      typedef FILEFINDBUF *PFILEFINDBUF;

      /*NOINC */
      #pragma pack(2)
      /*INC  */
      typedef struct _FILEFINDBUF2    /* findbuf2 */
      {
         FDATE  fdateCreation;
         FTIME  ftimeCreation;
         FDATE  fdateLastAccess;
         FTIME  ftimeLastAccess;
         FDATE  fdateLastWrite;
         FTIME  ftimeLastWrite;
         ULONG  cbFile;
         ULONG  cbFileAlloc;
         USHORT attrFile;
         ULONG  cbList;
         UCHAR  cchName;
         CHAR   achName[CCHMAXPATHCOMP];
      } FILEFINDBUF2;
      typedef FILEFINDBUF2 *PFILEFINDBUF2;

      typedef struct _FILEFINDBUF3                 /* findbuf3 */
      {
         ULONG   oNextEntryOffset;            /* new field */
         FDATE   fdateCreation;
         FTIME   ftimeCreation;
         FDATE   fdateLastAccess;
         FTIME   ftimeLastAccess;
         FDATE   fdateLastWrite;
         FTIME   ftimeLastWrite;
         ULONG   cbFile;
         ULONG   cbFileAlloc;
         ULONG   attrFile;                    /* widened field */
         UCHAR   cchName;
         CHAR    achName[CCHMAXPATHCOMP];
      } FILEFINDBUF3;
      typedef FILEFINDBUF3 *PFILEFINDBUF3;

      typedef struct _FILEFINDBUF4                 /* findbuf4 */
      {
         ULONG   oNextEntryOffset;            /* new field */
         FDATE   fdateCreation;
         FTIME   ftimeCreation;
         FDATE   fdateLastAccess;
         FTIME   ftimeLastAccess;
         FDATE   fdateLastWrite;
         FTIME   ftimeLastWrite;
         ULONG   cbFile;
         ULONG   cbFileAlloc;
         ULONG   attrFile;                    /* widened field */
         ULONG   cbList;
         UCHAR   cchName;
         CHAR    achName[CCHMAXPATHCOMP];
      } FILEFINDBUF4;
      typedef FILEFINDBUF4  *PFILEFINDBUF4;

      /* extended attribute structures */

      typedef struct _GEA         /* gea */
      {
         BYTE cbName;        /* name length not including NULL */
         CHAR szName[1];     /* attribute name */
      } GEA;
      typedef GEA *PGEA;

      typedef struct _GEALIST     /* geal */
      {
         ULONG cbList;       /* total bytes of structure including full list */
         GEA list[1];        /* variable length GEA structures */
      } GEALIST;
      typedef GEALIST  *PGEALIST;

      typedef struct _FEA         /* fea */
      {
         BYTE fEA;           /* flags                              */
         BYTE cbName;        /* name length not including NULL */
         USHORT cbValue;     /* value length */
      } FEA;
      typedef FEA *PFEA;

      /* flags for _FEA.fEA */
      #define FEA_NEEDEA         0x80     /* need EA bit */

      typedef struct _FEALIST     /* feal */
      {
         ULONG cbList;       /* total bytes of structure including full list */
         FEA list[1];        /* variable length FEA structures */
      } FEALIST;
      typedef FEALIST *PFEALIST;

      typedef struct _EAOP        /* eaop */
      {
         PGEALIST fpGEAList; /* general EA list */
         PFEALIST fpFEAList; /* full EA list */
         ULONG oError;
      } EAOP;
      typedef EAOP *PEAOP;

      /*NOINC*/
      #pragma pack(1)
      /*INC*/

      typedef struct _FEA2         /* fea2 */
      {
         ULONG   oNextEntryOffset;    /* new field */
         BYTE    fEA;
         BYTE    cbName;
         USHORT  cbValue;
         CHAR    szName[1];           /* new field */
      } FEA2;
      typedef FEA2 *PFEA2;


      typedef struct _FEA2LIST     /* fea2l */
      {
         ULONG   cbList;
         FEA2    list[1];
      } FEA2LIST;
      typedef FEA2LIST *PFEA2LIST;

      typedef struct _GEA2          /* gea2 */
      {
         ULONG   oNextEntryOffset;     /* new field */
         BYTE    cbName;
         CHAR    szName[1];            /* new field */
      } GEA2;
      typedef GEA2 *PGEA2;

      typedef struct _GEA2LIST      /* gea2l */
      {
         ULONG   cbList;
         GEA2    list[1];
      } GEA2LIST;
      typedef GEA2LIST *PGEA2LIST;

      typedef struct _EAOP2         /* eaop2 */
      {
         PGEA2LIST   fpGEA2List;       /* GEA set */
         PFEA2LIST   fpFEA2List;       /* FEA set */
         ULONG       oError;           /* offset of FEA error */
      } EAOP2;
      typedef EAOP2 *PEAOP2;


      /*
      * Equates for the types of EAs that follow the convention that we have
      * established.
      *
      * Values 0xFFFE thru 0x8000 are reserved.
      * Values 0x0000 thru 0x7fff are user definable.
      * Value  0xFFFC is not used
      */

      #define EAT_BINARY      0xFFFE      /* length preceeded binary */
      #define EAT_ASCII       0xFFFD      /* length preceeded ASCII */
      #define EAT_BITMAP      0xFFFB      /* length preceeded bitmap */
      #define EAT_METAFILE    0xFFFA      /* length preceeded metafile */
      #define EAT_ICON        0xFFF9      /* length preceeded icon */
      #define EAT_EA          0xFFEE      /* length preceeded ASCII */
                                          /* name of associated data (#include) */
      #define EAT_MVMT        0xFFDF      /* multi-valued, multi-typed field */
      #define EAT_MVST        0xFFDE      /* multi-valued, single-typed field */
      #define EAT_ASN1        0xFFDD      /* ASN.1 field */

   #endif  /* !INCL_SAADEFS */
   /*NOINC*/
   #pragma pack()
   /*INC*/



#ifdef __cplusplus
   APIRET APIENTRY  DosOpen(PCSZ     pszFileName,
                            PHFILE pHf,
                            PULONG pulAction,
                            ULONG  cbFile,
                            ULONG  ulAttribute,
                            ULONG  fsOpenFlags,
                            ULONG  fsOpenMode,
                            PEAOP2 peaop2);
#else
   APIRET APIENTRY  DosOpen(PSZ    pszFileName,
                            PHFILE pHf,
                            PULONG pulAction,
                            ULONG  cbFile,
                            ULONG  ulAttribute,
                            ULONG  fsOpenFlags,
                            ULONG  fsOpenMode,
                            PEAOP2 peaop2);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosProtectOpen(PCSZ  pszFileName,
                                   PHFILE phf,
                                   PULONG pulAction,
                                   ULONG cbFile,
                                   ULONG ulAttribute,
                                   ULONG fsOpenFlags,
                                   ULONG fsOpenMode,
                                   PEAOP2 peaop2,
                                   PFHLOCK pfhFileHandleLockID);
#else
   APIRET APIENTRY  DosProtectOpen(PSZ pszFileName,
                                   PHFILE phf,
                                   PULONG pulAction,
                                   ULONG cbFile,
                                   ULONG ulAttribute,
                                   ULONG fsOpenFlags,
                                   ULONG fsOpenMode,
                                   PEAOP2 peaop2,
                                   PFHLOCK pfhFileHandleLockID);
#endif

   APIRET APIENTRY  DosClose(HFILE hFile);

   APIRET APIENTRY  DosProtectClose(HFILE hFile,
                                    FHLOCK fhFileHandleLockID);

   APIRET APIENTRY  DosRead(HFILE hFile,
                            PVOID pBuffer,
                            ULONG cbRead,
                            PULONG pcbActual);

   APIRET APIENTRY  DosProtectRead(HFILE hFile,
                                   PVOID pBuffer,
                                   ULONG cbRead,
                                   PULONG pcbActual,
                                   FHLOCK fhFileHandleLockID);

   APIRET APIENTRY  DosWrite(HFILE hFile,
                             PVOID pBuffer,
                             ULONG cbWrite,
                             PULONG pcbActual);

   APIRET APIENTRY  DosProtectWrite(HFILE hFile,
                                    PVOID pBuffer,
                                    ULONG cbWrite,
                                    PULONG pcbActual,
                                    FHLOCK fhFileHandleLockID);

   /* File time and date types */

   typedef struct _FILESTATUS      /* fsts */
   {
      FDATE  fdateCreation;
      FTIME  ftimeCreation;
      FDATE  fdateLastAccess;
      FTIME  ftimeLastAccess;
      FDATE  fdateLastWrite;
      FTIME  ftimeLastWrite;
      ULONG  cbFile;
      ULONG  cbFileAlloc;
      USHORT attrFile;
   } FILESTATUS;
   typedef FILESTATUS *PFILESTATUS;

   typedef struct _FILESTATUS2     /* fsts2 */
   {
      FDATE  fdateCreation;
      FTIME  ftimeCreation;
      FDATE  fdateLastAccess;
      FTIME  ftimeLastAccess;
      FDATE  fdateLastWrite;
      FTIME  ftimeLastWrite;
      ULONG  cbFile;
      ULONG  cbFileAlloc;
      USHORT attrFile;
      ULONG  cbList;
   } FILESTATUS2;
   typedef FILESTATUS2 *PFILESTATUS2;

   typedef struct _FILESTATUS3     /* fsts3 */
   {
      FDATE  fdateCreation;
      FTIME  ftimeCreation;
      FDATE  fdateLastAccess;
      FTIME  ftimeLastAccess;
      FDATE  fdateLastWrite;
      FTIME  ftimeLastWrite;
      ULONG  cbFile;
      ULONG  cbFileAlloc;
      ULONG  attrFile;
   } FILESTATUS3;
   typedef FILESTATUS3 *PFILESTATUS3;

   typedef struct _FILESTATUS4      /* fsts4 */
   {
      FDATE  fdateCreation;
      FTIME  ftimeCreation;
      FDATE  fdateLastAccess;
      FTIME  ftimeLastAccess;
      FDATE  fdateLastWrite;
      FTIME  ftimeLastWrite;
      ULONG  cbFile;
      ULONG  cbFileAlloc;
      ULONG  attrFile;
      ULONG  cbList;
   } FILESTATUS4;
   typedef FILESTATUS4  *PFILESTATUS4;


   typedef struct _FSALLOCATE      /* fsalloc */
   {
      ULONG  idFileSystem;
      ULONG  cSectorUnit;
      ULONG  cUnit;
      ULONG  cUnitAvail;
      USHORT cbSector;
   } FSALLOCATE;
   typedef FSALLOCATE *PFSALLOCATE;

   typedef LHANDLE HDIR;        /* hdir */
   typedef HDIR    *PHDIR;

   #define DosOpen2        DosOpen
   #define DosFindFirst2   DosFindFirst
   #define DosQFHandState  DosQueryFHState
   #define DosProtectQFHandState  DosProtectQueryFHState
   #define DosSetFHandState  DosSetFHState
   #define DosProtectSetFHandState  DosProtectSetFHState
   #define DosQHandType    DosQueryHType
   #define DosQFSAttach    DosQueryFSAttach
   #define DosNewSize      DosSetFileSize
   #define DosProtectNewSize  DosProtectSetFileSize
   #define DosBufReset     DosResetBuffer
   #define DosChgFilePtr   DosSetFilePtr
   #define DosProtectChgFilePtr   DosProtectSetFilePtr
   #define DosMkDir        DosCreateDir
   #define DosMkDir2       DosCreateDir
   #define DosRmDir        DosDeleteDir
   #define DosSelectDisk   DosSetDefaultDisk
   #define DosQCurDisk     DosQueryCurrentDisk
   #define DosChDir        DosSetCurrentDir
   #define DosQCurDir      DosQueryCurrentDir
   #define DosQFSInfo      DosQueryFSInfo
   #define DosQVerify      DosQueryVerify
   #define DosQFileInfo    DosQueryFileInfo
   #define DosProtectQFileInfo    DosProtectQueryFileInfo
   #define DosQPathInfo    DosQueryPathInfo

#ifdef __cplusplus
   APIRET APIENTRY  DosDelete(PCSZ  pszFile);
#else
   APIRET APIENTRY  DosDelete(PSZ pszFile);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosForceDelete(PCSZ  pszFile);
#else
   APIRET APIENTRY  DosForceDelete(PSZ pszFile);
#endif

   APIRET APIENTRY  DosDupHandle(HFILE hFile,
                                 PHFILE pHfile);

   APIRET APIENTRY  DosQueryFHState(HFILE hFile,
                                    PULONG pMode);
   APIRET APIENTRY  DosProtectQueryFHState(HFILE hFile,
                                           PULONG pMode,
                                           FHLOCK fhFileHandleLockID);

   APIRET APIENTRY  DosSetFHState(HFILE hFile,
                                  ULONG mode);

   APIRET APIENTRY  DosProtectSetFHState(HFILE hFile,
                                         ULONG mode,
                                         FHLOCK fhFileHandleLockID);

   APIRET APIENTRY  DosQueryHType(HFILE hFile,
                                  PULONG pType,
                                  PULONG pAttr);

#ifdef __cplusplus
   APIRET APIENTRY  DosFindFirst(PCSZ     pszFileSpec,
                                 PHDIR  phdir,
                                 ULONG  flAttribute,
                                 PVOID  pfindbuf,
                                 ULONG  cbBuf,
                                 PULONG pcFileNames,
                                 ULONG  ulInfoLevel);
#else
   APIRET APIENTRY  DosFindFirst(PSZ    pszFileSpec,
                                 PHDIR  phdir,
                                 ULONG  flAttribute,
                                 PVOID  pfindbuf,
                                 ULONG  cbBuf,
                                 PULONG pcFileNames,
                                 ULONG  ulInfoLevel);
#endif

   APIRET APIENTRY  DosFindNext(HDIR   hDir,
                                PVOID  pfindbuf,
                                ULONG  cbfindbuf,
                                PULONG pcFilenames);

   APIRET APIENTRY  DosFindClose(HDIR hDir);

#ifdef __cplusplus
   APIRET APIENTRY  DosFSAttach(PCSZ  pszDevice,
                                PCSZ  pszFilesystem,
                                PVOID pData,
                                ULONG cbData,
                                ULONG flag);
#else
   APIRET APIENTRY  DosFSAttach(PSZ pszDevice,
                                PSZ pszFilesystem,
                                PVOID pData,
                                ULONG cbData,
                                ULONG flag);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosQueryFSAttach(PCSZ     pszDeviceName,
                                     ULONG  ulOrdinal,
                                     ULONG  ulFSAInfoLevel,
                                     PFSQBUFFER2 pfsqb,
                                     PULONG pcbBuffLength);
#else
   APIRET APIENTRY  DosQueryFSAttach(PSZ    pszDeviceName,
                                     ULONG  ulOrdinal,
                                     ULONG  ulFSAInfoLevel,
                                     PFSQBUFFER2 pfsqb,
                                     PULONG pcbBuffLength);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosFSCtl(PVOID pData,
                             ULONG cbData,
                             PULONG pcbData,
                             PVOID pParms,
                             ULONG cbParms,
                             PULONG pcbParms,
                             ULONG function,
                             PCSZ  pszRoute,
                             HFILE hFile,
                             ULONG method);
#else
   APIRET APIENTRY  DosFSCtl(PVOID pData,
                             ULONG cbData,
                             PULONG pcbData,
                             PVOID pParms,
                             ULONG cbParms,
                             PULONG pcbParms,
                             ULONG function,
                             PSZ pszRoute,
                             HFILE hFile,
                             ULONG method);
#endif

   APIRET APIENTRY  DosSetFileSize(HFILE hFile,
                                   ULONG cbSize);

   APIRET APIENTRY  DosProtectSetFileSize(HFILE hFile,
                                          ULONG cbSize,
                                          FHLOCK fhFileHandleLockID);

   APIRET APIENTRY  DosResetBuffer(HFILE hFile);

   APIRET APIENTRY  DosSetFilePtr(HFILE hFile,
                                  LONG ib,
                                  ULONG method,
                                  PULONG ibActual);

   APIRET APIENTRY  DosProtectSetFilePtr(HFILE hFile,
                                         LONG ib,
                                         ULONG method,
                                         PULONG ibActual,
                                         FHLOCK fhFileHandleLockID);

#ifdef __cplusplus
   APIRET APIENTRY  DosMove(PCSZ  pszOld,
                            PCSZ  pszNew);
#else
   APIRET APIENTRY  DosMove(PSZ pszOld,
                            PSZ pszNew);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosCopy(PCSZ  pszOld,
                            PCSZ  pszNew,
                            ULONG option);
#else
   APIRET APIENTRY  DosCopy(PSZ pszOld,
                            PSZ pszNew,
                            ULONG option);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosEditName(ULONG metalevel,
                                PCSZ  pszSource,
                                PCSZ  pszEdit,
                                PBYTE pszTarget,
                                ULONG cbTarget);
#else
   APIRET APIENTRY  DosEditName(ULONG metalevel,
                                PSZ pszSource,
                                PSZ pszEdit,
                                PBYTE pszTarget,
                                ULONG cbTarget);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosCreateDir(PCSZ  pszDirName,
                                 PEAOP2 peaop2);
#else
   APIRET APIENTRY  DosCreateDir(PSZ pszDirName,
                                 PEAOP2 peaop2);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosDeleteDir(PCSZ  pszDir);
#else
   APIRET APIENTRY  DosDeleteDir(PSZ pszDir);
#endif

   APIRET APIENTRY  DosSetDefaultDisk(ULONG disknum);

   APIRET APIENTRY  DosQueryCurrentDisk(PULONG pdisknum,
                                        PULONG plogical);

#ifdef __cplusplus
   APIRET APIENTRY  DosSetCurrentDir(PCSZ  pszDir);
#else
   APIRET APIENTRY  DosSetCurrentDir(PSZ pszDir);
#endif

   APIRET APIENTRY  DosQueryCurrentDir(ULONG disknum,
                                       PBYTE pBuf,
                                       PULONG pcbBuf);

   APIRET APIENTRY  DosQueryFSInfo(ULONG disknum,
                                   ULONG infolevel,
                                   PVOID pBuf,
                                   ULONG cbBuf);

   APIRET APIENTRY  DosSetFSInfo(ULONG disknum,
                                 ULONG infolevel,
                                 PVOID pBuf,
                                 ULONG cbBuf);

   APIRET APIENTRY  DosQueryVerify(PBOOL32 pBool);

   APIRET APIENTRY  DosSetVerify(BOOL32);

   APIRET APIENTRY  DosSetMaxFH(ULONG cFH);

   APIRET APIENTRY  DosSetRelMaxFH(PLONG pcbReqCount,
                                   PULONG pcbCurMaxFH);

   APIRET APIENTRY  DosQueryFileInfo(HFILE hf,
                                     ULONG ulInfoLevel,
                                     PVOID pInfo,
                                     ULONG cbInfoBuf);

   APIRET APIENTRY  DosProtectQueryFileInfo(HFILE hf,
                                            ULONG ulInfoLevel,
                                            PVOID pInfo,
                                            ULONG cbInfoBuf,
                                            FHLOCK fhFileHandleLockID);

   APIRET APIENTRY  DosSetFileInfo(HFILE hf,
                                   ULONG ulInfoLevel,
                                   PVOID pInfoBuf,
                                   ULONG cbInfoBuf);

   APIRET APIENTRY  DosProtectSetFileInfo(HFILE hf,
                                          ULONG ulInfoLevel,
                                          PVOID pInfoBuf,
                                          ULONG cbInfoBuf,
                                          FHLOCK fhFileHandleLockID);

#ifdef __cplusplus
   APIRET APIENTRY  DosQueryPathInfo(PCSZ    pszPathName,
                                     ULONG ulInfoLevel,
                                     PVOID pInfoBuf,
                                     ULONG cbInfoBuf);
#else
   APIRET APIENTRY  DosQueryPathInfo(PSZ   pszPathName,
                                     ULONG ulInfoLevel,
                                     PVOID pInfoBuf,
                                     ULONG cbInfoBuf);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosSetPathInfo(PCSZ    pszPathName,
                                   ULONG ulInfoLevel,
                                   PVOID pInfoBuf,
                                   ULONG cbInfoBuf,
                                   ULONG flOptions);
#else
   APIRET APIENTRY  DosSetPathInfo(PSZ   pszPathName,
                                   ULONG ulInfoLevel,
                                   PVOID pInfoBuf,
                                   ULONG cbInfoBuf,
                                   ULONG flOptions);
#endif

   /* defines for DosSetPathInfo -pathinfo flag */
   #define DSPI_WRTTHRU    0x10    /* write through */

   APIRET APIENTRY  DosShutdown(ULONG ulReserved);

   APIRET APIENTRY  DosEnumAttribute(ULONG  ulRefType,
                                     PVOID  pvFile,
                                     ULONG  ulEntry,
                                     PVOID  pvBuf,
                                     ULONG  cbBuf,
                                     PULONG pulCount,
                                     ULONG  ulInfoLevel);

   APIRET APIENTRY  DosProtectEnumAttribute(ULONG ulRefType,
                                            PVOID pvFile,
                                            ULONG ulEntry,
                                            PVOID pvBuf,
                                            ULONG cbBuf,
                                            PULONG pulCount,
                                            ULONG ulInfoLevel,
                                            FHLOCK fhFileHandleLockID );

   /*NOINC*/
   #pragma pack(1)
   /*INC */

   typedef struct _DENA1 /* _dena1 level 1 info returned from DosEnumAttribute */
   {
      UCHAR       reserved;       /* 0 */
      UCHAR       cbName;         /* length of name exculding NULL */
      USHORT      cbValue;        /* length of value */
      UCHAR       szName[1];      /* variable length asciiz name */
   } DENA1;
   typedef DENA1 *PDENA1;

   typedef FEA2  DENA2;
   typedef PFEA2 PDENA2;

   /*NOINC*/
   #pragma pack()
   /*INC */
   /* Infolevels for DosEnumAttribute  */
   #define ENUMEA_LEVEL_NO_VALUE   1L      /* FEA without value */
   /* Reference types for DosEnumAttribute */
   #define ENUMEA_REFTYPE_FHANDLE  0       /* file handle */
   #define ENUMEA_REFTYPE_PATH     1       /* path name */
   #define ENUMEA_REFTYPE_MAX      ENUMEA_REFTYPE_PATH

#endif /* common INCL_DOSFILEMGR */

#if (defined(INCL_DOSMEMMGR) || !defined(INCL_NOCOMMON))
   /*** Memory management */

   APIRET APIENTRY  DosAllocMem(PPVOID ppb,
                                ULONG cb,
                                ULONG flag);

   APIRET APIENTRY  DosFreeMem(PVOID pb);

   APIRET APIENTRY  DosSetMem(PVOID pb,
                              ULONG cb,
                              ULONG flag);

   APIRET APIENTRY  DosGiveSharedMem(PVOID pb,
                                     PID pid,
                                     ULONG flag);

   APIRET APIENTRY  DosGetSharedMem(PVOID pb,
                                    ULONG flag);

#ifdef __cplusplus
   APIRET APIENTRY  DosGetNamedSharedMem(PPVOID ppb,
                                         PCSZ  pszName,
                                         ULONG flag);
#else
   APIRET APIENTRY  DosGetNamedSharedMem(PPVOID ppb,
                                         PSZ pszName,
                                         ULONG flag);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosAllocSharedMem(PPVOID ppb,
                                      PCSZ  pszName,
                                      ULONG cb,
                                      ULONG flag);
#else
   APIRET APIENTRY  DosAllocSharedMem(PPVOID ppb,
                                      PSZ pszName,
                                      ULONG cb,
                                      ULONG flag);
#endif

   APIRET APIENTRY  DosQueryMem(PVOID pb,
                                PULONG pcb,
                                PULONG pFlag);

   #define DosSubAlloc     DosSubAllocMem
   #define DOSSUBALLOC     DosSubAllocMem
   APIRET APIENTRY  DosSubAllocMem(PVOID pbBase,
                                   PPVOID ppb,
                                   ULONG cb);

   #define DosSubFree      DosSubFreeMem
   #define DOSSUBFREE      DosSubFreeMem
   APIRET APIENTRY  DosSubFreeMem(PVOID pbBase,
                                  PVOID pb,
                                  ULONG cb);

   #define DosSubSet       DosSubSetMem
   #define DOSSUBSET       DosSubSetMem
   APIRET APIENTRY  DosSubSetMem(PVOID pbBase,
                                 ULONG flag,
                                 ULONG cb);

   #define DosSubUnset     DosSubUnsetMem
   #define DOSSUBUNSET     DosSubUnsetMem
   APIRET APIENTRY  DosSubUnsetMem(PVOID pbBase);

   #include <bsememf.h>    /* get flags for API                            */

#endif /* INCL_DOSMEMMGR */



#if (defined(INCL_DOSSEMAPHORES) || !defined(INCL_NOCOMMON))

   /*
   *     32-bit Semaphore Support
   */

   /* Semaphore Attributes */

   #define DC_SEM_SHARED   0x01   /* DosCreateMutex, DosCreateEvent, and     */
                                  /*   DosCreateMuxWait use it to indicate   */
                                  /*   whether the semaphore is shared or    */
                                  /*   private when the PSZ is null          */
   #define DCMW_WAIT_ANY   0x02   /* DosCreateMuxWait option for wait on any */
                                  /*   event/mutex to occur                  */
   #define DCMW_WAIT_ALL   0x04   /* DosCreateMuxWait option for wait on all */
                                  /*   events/mutexs to occur                */

   #define SEM_INDEFINITE_WAIT     -1L
   #define SEM_IMMEDIATE_RETURN     0L

   #ifndef __HSEM__
/* NOINC */
      #define __HSEM__
      typedef VOID *HSEM;
      typedef HSEM *PHSEM;
/* INC */
   #endif

   typedef struct _PSEMRECORD      /* psr */
   {
      HSEM        hsemCur;
      ULONG       ulUser;
   } SEMRECORD;
   typedef SEMRECORD *PSEMRECORD;

#endif /* common INCL_DOSSEMAPHORES */



#ifdef INCL_DOSSEMAPHORES

   #ifndef __HEV__            /* INCL_SEMAPHORE may also define HEV */
      #define __HEV__
      typedef  ULONG    HEV;             /* hev */
      typedef  HEV      *PHEV;
   #endif

   typedef  ULONG    HMTX;            /* hmtx */
   typedef  HMTX     *PHMTX;
   typedef  ULONG    HMUX;            /* hmux */
   typedef  HMUX     *PHMUX;

#ifdef __cplusplus
   APIRET APIENTRY  DosCreateEventSem (PCSZ  pszName,
                                       PHEV phev,
                                       ULONG flAttr,
                                       BOOL32 fState);
#else
   APIRET APIENTRY  DosCreateEventSem (PSZ pszName,
                                       PHEV phev,
                                       ULONG flAttr,
                                       BOOL32 fState);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosOpenEventSem (PCSZ  pszName,
                                     PHEV phev);
#else
   APIRET APIENTRY  DosOpenEventSem (PSZ pszName,
                                     PHEV phev);
#endif

   APIRET APIENTRY  DosCloseEventSem (HEV hev);

   APIRET APIENTRY  DosResetEventSem (HEV hev,
                                      PULONG pulPostCt);

   APIRET APIENTRY  DosPostEventSem (HEV hev);

   APIRET APIENTRY  DosWaitEventSem (HEV hev,
                                     ULONG ulTimeout);

   APIRET APIENTRY  DosQueryEventSem (HEV hev,
                                      PULONG pulPostCt);

#ifdef __cplusplus
   APIRET APIENTRY  DosCreateMutexSem (PCSZ  pszName,
                                       PHMTX phmtx,
                                       ULONG flAttr,
                                       BOOL32 fState);
#else
   APIRET APIENTRY  DosCreateMutexSem (PSZ pszName,
                                       PHMTX phmtx,
                                       ULONG flAttr,
                                       BOOL32 fState);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosOpenMutexSem (PCSZ  pszName,
                                     PHMTX phmtx);
#else
   APIRET APIENTRY  DosOpenMutexSem (PSZ pszName,
                                     PHMTX phmtx);
#endif

   APIRET APIENTRY  DosCloseMutexSem (HMTX hmtx);

   APIRET APIENTRY  DosRequestMutexSem (HMTX hmtx,
                                        ULONG ulTimeout);

   APIRET APIENTRY  DosReleaseMutexSem (HMTX hmtx);

   APIRET APIENTRY  DosQueryMutexSem (HMTX hmtx,
                                      PID *ppid,
                                      TID *ptid,
                                      PULONG pulCount);

#ifdef __cplusplus
   APIRET APIENTRY  DosCreateMuxWaitSem (PCSZ  pszName,
                                         PHMUX phmux,
                                         ULONG cSemRec,
                                         PSEMRECORD pSemRec,
                                         ULONG flAttr);
#else
   APIRET APIENTRY  DosCreateMuxWaitSem (PSZ pszName,
                                         PHMUX phmux,
                                         ULONG cSemRec,
                                         PSEMRECORD pSemRec,
                                         ULONG flAttr);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosOpenMuxWaitSem (PCSZ  pszName,
                                       PHMUX phmux);
#else
   APIRET APIENTRY  DosOpenMuxWaitSem (PSZ pszName,
                                       PHMUX phmux);
#endif

   APIRET APIENTRY  DosCloseMuxWaitSem (HMUX hmux);

   APIRET APIENTRY  DosWaitMuxWaitSem (HMUX hmux,
                                       ULONG ulTimeout,
                                       PULONG pulUser);

   APIRET APIENTRY  DosAddMuxWaitSem (HMUX hmux,
                                      PSEMRECORD pSemRec);

   APIRET APIENTRY  DosDeleteMuxWaitSem (HMUX hmux,
                                         HSEM hSem);

   APIRET APIENTRY  DosQueryMuxWaitSem (HMUX hmux,
                                        PULONG pcSemRec,
                                        PSEMRECORD pSemRec,
                                        PULONG pflAttr);

#endif /* INCL_DOSSEMAPHORES */



#if (defined(INCL_DOSDATETIME) || !defined(INCL_NOCOMMON))

   /*** Time support */

   typedef struct _DATETIME      /* date */
   {
      UCHAR   hours;
      UCHAR   minutes;
      UCHAR   seconds;
      UCHAR   hundredths;
      UCHAR   day;
      UCHAR   month;
      USHORT  year;
      SHORT   timezone;
      UCHAR   weekday;
   } DATETIME;
   typedef DATETIME *PDATETIME;

   APIRET APIENTRY   DosGetDateTime(PDATETIME pdt);

   APIRET APIENTRY   DosSetDateTime(PDATETIME pdt);

#endif /* common INCL_DOSDATETIME */



#ifdef INCL_DOSDATETIME

   #define DosTimerAsync   DosAsyncTimer
   #define DosTimerStart   DosStartTimer
   #define DosTimerStop    DosStopTimer

   typedef LHANDLE HTIMER;
   typedef HTIMER  *PHTIMER;

   APIRET APIENTRY   DosAsyncTimer(ULONG msec,
                                   HSEM hsem,
                                   PHTIMER phtimer);

   APIRET APIENTRY   DosStartTimer(ULONG msec,
                                   HSEM hsem,
                                   PHTIMER phtimer);

   APIRET APIENTRY   DosStopTimer(HTIMER htimer);

#endif /* INCL_DOSDATETIME */




/*** Module manager */

#ifdef INCL_DOSMODULEMGR


#ifdef __cplusplus
   APIRET APIENTRY  DosLoadModule(PCSZ  pszName,
                                  ULONG cbName,
                                  PCSZ  pszModname,
                                  PHMODULE phmod);
#else
   APIRET APIENTRY  DosLoadModule(PSZ pszName,
                                  ULONG cbName,
                                  PSZ pszModname,
                                  PHMODULE phmod);
#endif

   APIRET APIENTRY  DosFreeModule(HMODULE hmod);

#ifdef __cplusplus
   APIRET APIENTRY  DosQueryProcAddr(HMODULE hmod,
                                     ULONG ordinal,
                                     PCSZ  pszName,
                                     PFN* ppfn);
#else
   APIRET APIENTRY  DosQueryProcAddr(HMODULE hmod,
                                     ULONG ordinal,
                                     PSZ pszName,
                                     PFN* ppfn);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosQueryModuleHandle(PCSZ  pszModname,
                                         PHMODULE phmod);
#else
   APIRET APIENTRY  DosQueryModuleHandle(PSZ pszModname,
                                         PHMODULE phmod);
#endif

   APIRET APIENTRY  DosQueryModuleName(HMODULE hmod,
                                       ULONG cbName,
                                       PCHAR pch);

   #define PT_16BIT        0
   #define PT_32BIT        1

#ifdef __cplusplus
   APIRET APIENTRY  DosQueryProcType(HMODULE hmod,
                                     ULONG ordinal,
                                     PCSZ  pszName,
                                     PULONG pulproctype);
#else
   APIRET APIENTRY  DosQueryProcType(HMODULE hmod,
                                     ULONG ordinal,
                                     PSZ pszName,
                                     PULONG pulproctype);
#endif

#endif /* INCL_DOSMODULEMGR */



#if (defined(INCL_DOSRESOURCES) || !defined(INCL_NOCOMMON))

   /*** Resource support */

   /* Predefined resource types */

   #define RT_POINTER      1   /* mouse pointer shape */
   #define RT_BITMAP       2   /* bitmap */
   #define RT_MENU         3   /* menu template */
   #define RT_DIALOG       4   /* dialog template */
   #define RT_STRING       5   /* string tables */
   #define RT_FONTDIR      6   /* font directory */
   #define RT_FONT         7   /* font */
   #define RT_ACCELTABLE   8   /* accelerator tables */
   #define RT_RCDATA       9   /* binary data */
   #define RT_MESSAGE      10  /* error msg     tables */
   #define RT_DLGINCLUDE   11  /* dialog include file name */
   #define RT_VKEYTBL      12  /* key to vkey tables */
   #define RT_KEYTBL       13  /* key to UGL tables */
   #define RT_CHARTBL      14  /* glyph to character tables */
   #define RT_DISPLAYINFO  15  /* screen display information */

   #define RT_FKASHORT     16  /* function key area short form */
   #define RT_FKALONG      17  /* function key area long form */

   #define RT_HELPTABLE    18  /* Help table for Cary Help manager */
   #define RT_HELPSUBTABLE 19  /* Help subtable for Cary Help manager */

   #define RT_FDDIR        20  /* DBCS uniq/font driver directory */
   #define RT_FD           21  /* DBCS uniq/font driver */

   #define RT_MAX          22  /* 1st unused Resource Type */


   #define RF_ORDINALID    0x80000000L     /* ordinal id flag in resource table */

#endif /* common INCL_DOSRESOURCES */




#ifdef INCL_DOSRESOURCES

   #define DosGetResource2 DosGetResource

   APIRET APIENTRY  DosGetResource(HMODULE hmod,
                                   ULONG idType,
                                   ULONG idName,
                                   PPVOID ppb);

   APIRET APIENTRY  DosFreeResource(PVOID pb);

   APIRET APIENTRY  DosQueryResourceSize(HMODULE hmod,
                                         ULONG idt,
                                         ULONG idn,
                                         PULONG pulsize);

#endif /* INCL_DOSRESOURCES */




/*** NLS Support */

#ifdef INCL_DOSNLS

   typedef struct _COUNTRYCODE   /* ctryc */
   {
      ULONG       country;
      ULONG       codepage;
   } COUNTRYCODE;
   typedef COUNTRYCODE *PCOUNTRYCODE;

   typedef struct _COUNTRYINFO   /* ctryi */
   {
      ULONG       country;
      ULONG       codepage;
      ULONG       fsDateFmt;
      CHAR        szCurrency[5];
      CHAR        szThousandsSeparator[2];
      CHAR        szDecimal[2];
      CHAR        szDateSeparator[2];
      CHAR        szTimeSeparator[2];
      UCHAR       fsCurrencyFmt;
      UCHAR       cDecimalPlace;
      UCHAR       fsTimeFmt;
      USHORT      abReserved1[2];
      CHAR        szDataSeparator[2];
      USHORT      abReserved2[5];
   } COUNTRYINFO, *PCOUNTRYINFO;

   #define DosGetCtryInfo  DosQueryCtryInfo
   #define DosGetDBCSEv    DosQueryDBCSEnv
   #define DosCaseMap      DosMapCase
   #define DosGetCollate   DosQueryCollate
   #define DosGetCp        DosQueryCp
   #define DosSetProcCp    DosSetProcessCp

   APIRET APIENTRY  DosQueryCtryInfo(ULONG cb,
                                     PCOUNTRYCODE pcc,
                                     PCOUNTRYINFO pci,
                                     PULONG pcbActual);

   APIRET APIENTRY  DosQueryDBCSEnv(ULONG cb,
                                    PCOUNTRYCODE pcc,
                                    PCHAR pBuf);

   APIRET APIENTRY  DosMapCase(ULONG cb,
                               PCOUNTRYCODE pcc,
                               PCHAR pch);

   APIRET APIENTRY  DosQueryCollate(ULONG cb,
                                    PCOUNTRYCODE pcc,
                                    PCHAR pch,
                                    PULONG pcch);

   APIRET APIENTRY  DosQueryCp(ULONG cb,
                               PULONG arCP,
                               PULONG pcCP);

   APIRET APIENTRY  DosSetProcessCp(ULONG cp);

#endif /* INCL_DOSNLS */




/*** Signal support */

#ifdef INCL_DOSEXCEPTIONS

   /* DosSetSigExceptionFocus codes */

   #define SIG_UNSETFOCUS 0
   #define SIG_SETFOCUS 1

   #include <bsexcpt.h>

   APIRET APIENTRY  DosSetExceptionHandler(PEXCEPTIONREGISTRATIONRECORD pERegRec);

   APIRET APIENTRY  DosUnsetExceptionHandler(PEXCEPTIONREGISTRATIONRECORD pERegRec);

   APIRET APIENTRY  DosRaiseException(PEXCEPTIONREPORTRECORD pexcept);

   APIRET APIENTRY  DosSendSignalException(PID pid,
                                           ULONG exception);

   APIRET APIENTRY  DosUnwindException(PEXCEPTIONREGISTRATIONRECORD phandler,
                                       PVOID pTargetIP,
                                       PEXCEPTIONREPORTRECORD pERepRec);

   APIRET APIENTRY  DosSetSignalExceptionFocus(BOOL32 flag,
                                               PULONG pulTimes);

   APIRET APIENTRY  DosEnterMustComplete(PULONG pulNesting);

   APIRET APIENTRY  DosExitMustComplete(PULONG pulNesting);

   APIRET APIENTRY  DosAcknowledgeSignalException(ULONG ulSignalNum);

#endif /* INCL_DOSEXCEPTIONS */


/*** Pipe and queue support */

#ifdef INCL_DOSQUEUES
   #if (defined(INCL_DOSFILEMGR) || !defined(INCL_NOCOMMON))

      typedef LHANDLE HQUEUE;  /* hq */
      typedef HQUEUE  *PHQUEUE;
      typedef struct _REQUESTDATA     /* reqqdata */
      {
         PID         pid;
         ULONG       ulData;
      } REQUESTDATA;
      typedef REQUESTDATA *PREQUESTDATA;

      #define QUE_FIFO              0L
      #define QUE_LIFO              1L
      #define QUE_PRIORITY          2L
      #define QUE_NOCONVERT_ADDRESS 0L
      #define QUE_CONVERT_ADDRESS   4L


      APIRET APIENTRY  DosCreatePipe(PHFILE phfRead,
                                     PHFILE phfWrite,
                                     ULONG cb);

      APIRET APIENTRY  DosCloseQueue(HQUEUE hq);

#ifdef __cplusplus
      APIRET APIENTRY  DosCreateQueue(PHQUEUE phq,
                                      ULONG priority,
                                      PCSZ  pszName);
#else
      APIRET APIENTRY  DosCreateQueue(PHQUEUE phq,
                                      ULONG priority,
                                      PSZ pszName);
#endif

#ifdef __cplusplus
      APIRET APIENTRY  DosOpenQueue(PPID ppid,
                                    PHQUEUE phq,
                                    PCSZ  pszName);
#else
      APIRET APIENTRY  DosOpenQueue(PPID ppid,
                                    PHQUEUE phq,
                                    PSZ pszName);
#endif

      APIRET APIENTRY  DosPeekQueue(HQUEUE hq,
                                    PREQUESTDATA pRequest,
                                    PULONG pcbData,
                                    PPVOID ppbuf,
                                    PULONG element,
                                    BOOL32 nowait,
                                    PBYTE ppriority,
                                    HEV hsem);

      APIRET APIENTRY  DosPurgeQueue(HQUEUE hq);

      APIRET APIENTRY  DosQueryQueue(HQUEUE hq,
                                     PULONG pcbEntries);

      APIRET APIENTRY  DosReadQueue(HQUEUE hq,
                                    PREQUESTDATA pRequest,
                                    PULONG pcbData,
                                    PPVOID ppbuf,
                                    ULONG element,
                                    BOOL32 wait,
                                    PBYTE ppriority,
                                    HEV hsem);

      APIRET APIENTRY  DosWriteQueue(HQUEUE hq,
                                     ULONG request,
                                     ULONG cbData,
                                     PVOID pbData,
                                     ULONG priority);

   #else /* INCL_DOSFILEMGR || !INCL_NOCOMMON */
      #error PHFILE not defined - define INCL_DOSFILEMGR or undefine INCL_NOCOMMON
   #endif /* INCL_DOSFILEMGR || !INCL_NOCOMMON */
#endif /* INCL_DOSQUEUES */



#ifdef INCL_DOSMISC

   /* definitions for DosSearchPath control word */
   #define DSP_IMPLIEDCUR          1 /* current dir will be searched first */
   #define DSP_PATHREF             2 /* from env.variable */
   #define DSP_IGNORENETERR        4 /* ignore net errs & continue search */

   /* indices for DosQuerySysInfo */
   #define QSV_MAX_PATH_LENGTH        1
   #define Q_MAX_PATH_LENGTH          QSV_MAX_PATH_LENGTH
   #define QSV_MAX_TEXT_SESSIONS      2
   #define QSV_MAX_PM_SESSIONS        3
   #define QSV_MAX_VDM_SESSIONS       4
   #define QSV_BOOT_DRIVE             5 /* 1=A, 2=B, etc.                     */
   #define QSV_DYN_PRI_VARIATION      6 /* 0=Absolute, 1=Dynamic              */
   #define QSV_MAX_WAIT               7 /* seconds                            */
   #define QSV_MIN_SLICE              8 /* milli seconds                      */
   #define QSV_MAX_SLICE              9 /* milli seconds                      */
   #define QSV_PAGE_SIZE             10
   #define QSV_VERSION_MAJOR         11
   #define QSV_VERSION_MINOR         12
   #define QSV_VERSION_REVISION      13 /* Revision letter                    */
   #define QSV_MS_COUNT              14 /* Free running millisecond counter   */
   #define QSV_TIME_LOW              15 /* Low dword of time in seconds       */
   #define QSV_TIME_HIGH             16 /* High dword of time in seconds      */
   #define QSV_TOTPHYSMEM            17 /* Physical memory on system          */
   #define QSV_TOTRESMEM             18 /* Resident memory on system          */
   #define QSV_TOTAVAILMEM           19 /* Available memory for all processes */
   #define QSV_MAXPRMEM              20 /* Avail private mem for calling proc */
   #define QSV_MAXSHMEM              21 /* Avail shared mem for calling proc  */
   #define QSV_TIMER_INTERVAL        22 /* Timer interval in tenths of ms     */
   #define QSV_MAX_COMP_LENGTH       23 /* max len of one component in a name */
   #define QSV_FOREGROUND_FS_SESSION 24 /* Session ID of current fgnd FS session*/
   #define QSV_FOREGROUND_PROCESS    25 /* Process ID of current fgnd process */
   #define QSV_MAX                   QSV_MAX_COMP_LENGTH

   /* definitions for DosError - combine with | */
   #define FERR_DISABLEHARDERR     0x00000000L     /* disable hard error popups */
   #define FERR_ENABLEHARDERR      0x00000001L     /* enable hard error popups */
   #define FERR_ENABLEEXCEPTION    0x00000000L     /* enable exception popups */
   #define FERR_DISABLEEXCEPTION   0x00000002L     /* disable exception popups */

   /* definitions for DosQueryRASInfo Index */
   #define SIS_MMIOADDR            0
   #define SIS_MEC_TABLE           1
   #define SIS_SYS_LOG             2

   /* definitions for DosQueryExtLIBPATH and DosSetExtLIBPATH flags parameter */
   #define BEGIN_LIBPATH   1
   #define END_LIBPATH     2

   #define DosInsMessage   DosInsertMessage
   #define DosQSysInfo     DosQuerySysInfo

   APIRET APIENTRY  DosError(ULONG error);

#ifdef __cplusplus
   APIRET APIENTRY  DosGetMessage(PCHAR* pTable,
                                  ULONG cTable,
                                  PCHAR pBuf,
                                  ULONG cbBuf,
                                  ULONG msgnumber,
                                  PCSZ  pszFile,
                                  PULONG pcbMsg);
#else
   APIRET APIENTRY  DosGetMessage(PCHAR* pTable,
                                  ULONG cTable,
                                  PCHAR pBuf,
                                  ULONG cbBuf,
                                  ULONG msgnumber,
                                  PSZ pszFile,
                                  PULONG pcbMsg);
#endif

   APIRET APIENTRY  DosErrClass(ULONG code,
                                PULONG pClass,
                                PULONG pAction,
                                PULONG pLocus);

#ifdef __cplusplus
   APIRET APIENTRY  DosInsertMessage(PCHAR* pTable,
                                     ULONG cTable,
                                     PCSZ  pszMsg,
                                     ULONG cbMsg,
                                     PCHAR pBuf,
                                     ULONG cbBuf,
                                     PULONG pcbMsg);
#else
   APIRET APIENTRY  DosInsertMessage(PCHAR* pTable,
                                     ULONG cTable,
                                     PSZ pszMsg,
                                     ULONG cbMsg,
                                     PCHAR pBuf,
                                     ULONG cbBuf,
                                     PULONG pcbMsg);
#endif

   APIRET APIENTRY  DosPutMessage(HFILE hfile,
                                  ULONG cbMsg,
                                  PCHAR pBuf);

   APIRET APIENTRY  DosQuerySysInfo(ULONG iStart,
                                    ULONG iLast,
                                    PVOID pBuf,
                                    ULONG cbBuf);

#ifdef __cplusplus
   APIRET APIENTRY  DosScanEnv(PCSZ  pszName,
                               PCSZ  *ppszValue);
#else
   APIRET APIENTRY  DosScanEnv(PSZ pszName,
                               PSZ *ppszValue);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosSearchPath(ULONG flag,
                                  PCSZ  pszPathOrName,
                                  PCSZ  pszFilename,
                                  PBYTE pBuf,
                                  ULONG cbBuf);
#else
   APIRET APIENTRY  DosSearchPath(ULONG flag,
                                  PSZ pszPathOrName,
                                  PSZ pszFilename,
                                  PBYTE pBuf,
                                  ULONG cbBuf);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosQueryMessageCP(PCHAR pb,
                                      ULONG cb,
                                      PCSZ  pszFilename,
                                      PULONG cbBuf);
#else
   APIRET APIENTRY  DosQueryMessageCP(PCHAR pb,
                                      ULONG cb,
                                      PSZ pszFilename,
                                      PULONG cbBuf);
#endif

   APIRET APIENTRY  DosQueryRASInfo(ULONG Index,
                                    PPVOID Addr);

#ifdef __cplusplus
   APIRET  APIENTRY DosSetExtLIBPATH( PCSZ  pszExtLIBPATH, ULONG flags);
#else
   APIRET  APIENTRY DosSetExtLIBPATH( PSZ pszExtLIBPATH, ULONG flags);
#endif

#ifdef __cplusplus
   APIRET  APIENTRY DosQueryExtLIBPATH( PCSZ  pszExtLIBPATH, ULONG flags);
#else
   APIRET  APIENTRY DosQueryExtLIBPATH( PSZ pszExtLIBPATH, ULONG flags);
#endif

#endif /* INCL_DOSMISC */


/*** Session manager support */

#ifdef INCL_DOSSESMGR

   typedef struct _STARTDATA     /* stdata */
   {
      USHORT  Length;
      USHORT  Related;
      USHORT  FgBg;
      USHORT  TraceOpt;
      PSZ     PgmTitle;
      PSZ     PgmName;
      PBYTE   PgmInputs;
      PBYTE   TermQ;
      PBYTE   Environment;
      USHORT  InheritOpt;
      USHORT  SessionType;
      PSZ     IconFile;
      ULONG   PgmHandle;
      USHORT  PgmControl;
      USHORT  InitXPos;
      USHORT  InitYPos;
      USHORT  InitXSize;
      USHORT  InitYSize;
      USHORT  Reserved;
      PSZ     ObjectBuffer;
      ULONG   ObjectBuffLen;
   } STARTDATA;
   typedef STARTDATA *PSTARTDATA;

   #define SSF_RELATED_INDEPENDENT 0
   #define SSF_RELATED_CHILD       1

   #define SSF_FGBG_FORE           0
   #define SSF_FGBG_BACK           1

   #define SSF_TRACEOPT_NONE       0
   #define SSF_TRACEOPT_TRACE      1
   #define SSF_TRACEOPT_TRACEALL   2

   #define SSF_INHERTOPT_SHELL     0
   #define SSF_INHERTOPT_PARENT    1

   /* note that these types are identical to those in pmshl.h for PROG_* */
   #define SSF_TYPE_DEFAULT        0
   #define SSF_TYPE_FULLSCREEN     1
   #define SSF_TYPE_WINDOWABLEVIO  2
   #define SSF_TYPE_PM             3
   #define SSF_TYPE_VDM            4
   #define SSF_TYPE_GROUP          5
   #define SSF_TYPE_DLL            6
   #define SSF_TYPE_WINDOWEDVDM    7
   #define SSF_TYPE_PDD            8
   #define SSF_TYPE_VDD            9

   /* note that these flags are identical to those in pmshl.h for SHE_* */
   #define SSF_CONTROL_VISIBLE     0x0000
   #define SSF_CONTROL_INVISIBLE   0x0001
   #define SSF_CONTROL_MAXIMIZE    0x0002
   #define SSF_CONTROL_MINIMIZE    0x0004
   #define SSF_CONTROL_NOAUTOCLOSE 0x0008
   #define SSF_CONTROL_SETPOS      0x8000

   typedef struct _STATUSDATA   /* stsdata */
   {
      USHORT Length;
      USHORT SelectInd;
      USHORT BondInd;
   } STATUSDATA;
   typedef STATUSDATA *PSTATUSDATA;

   /* SelectInd and BondInd paramater values for DosSetSession */
   #define SET_SESSION_UNCHANGED      0
   #define SET_SESSION_SELECTABLE     1
   #define SET_SESSION_NON_SELECTABLE 2
   #define SET_SESSION_BOND           1
   #define SET_SESSION_NO_BOND        2

   /* TargetOption (scope) parameter values for DosStopSession */
   #define STOP_SESSION_SPECIFIED     0
   #define STOP_SESSION_ALL           1


/* NOINC */
   #define DosQAppType     DosQueryAppType
/* INC */

   APIRET APIENTRY  DosStartSession(PSTARTDATA psd,
                                    PULONG pidSession,
                                    PPID ppid);

   APIRET APIENTRY  DosSetSession(ULONG idSession,
                                  PSTATUSDATA psd);

   APIRET APIENTRY  DosSelectSession(ULONG idSession);

   APIRET APIENTRY  DosStopSession(ULONG scope,
                                   ULONG idSession);

#ifdef __cplusplus
   APIRET APIENTRY DosQueryAppType(PCSZ  pszName,
                                   PULONG pFlags);
#else
   APIRET APIENTRY DosQueryAppType(PSZ pszName,
                                   PULONG pFlags);
#endif

#endif /* INCL_DOSSESMGR */



#if (defined(INCL_DOSSESMGR) || defined(INCL_DOSFILEMGR))

   /* AppType returned in by DosQueryAppType in pFlags as follows          */
   #define FAPPTYP_NOTSPEC         0x0000
   #define FAPPTYP_NOTWINDOWCOMPAT 0x0001
   #define FAPPTYP_WINDOWCOMPAT    0x0002
   #define FAPPTYP_WINDOWAPI       0x0003
   #define FAPPTYP_BOUND           0x0008
   #define FAPPTYP_DLL             0x0010
   #define FAPPTYP_DOS             0x0020
   #define FAPPTYP_PHYSDRV         0x0040  /* physical device driver       */
   #define FAPPTYP_VIRTDRV         0x0080  /* virtual device driver        */
   #define FAPPTYP_PROTDLL         0x0100  /* 'protected memory' dll       */
   #define FAPPTYP_WINDOWSREAL     0x0200  /* Windows real mode app        */
   #define FAPPTYP_WINDOWSPROT     0x0400  /* Windows protect mode app     */
   #define FAPPTYP_WINDOWSPROT31   0x1000  /* Windows 3.1 protect mode app */
   #define FAPPTYP_32BIT           0x4000
   #define FAPPTYP_EXETYPE         FAPPTYP_WINDOWAPI

   #define FAPPTYP_RESERVED        ~(FAPPTYP_WINDOWAPI | FAPPTYP_BOUND | FAPPTYP_DLL | FAPPTYP_DOS | FAPPTYP_PHYSDRV | FAPPTYP_VIRTDRV | FAPPTYP_PROTDLL | FAPPTYP_32BIT)

   #ifdef INCL_DOSFILEMGR

      #define EAT_APPTYP_PMAPI        0x00            /* Uses PM API */
      #define EAT_APPTYP_DOS          0x01            /* DOS APP */
      #define EAT_APPTYP_PMW          0x02            /* Window compatible */
      #define EAT_APPTYP_NOPMW        0x03            /* Not Window compatible */
      #define EAT_APPTYP_EXETYPE      0x03            /* EXE type mask */
      #define EAT_APPTYP_RESERVED     ~(EAT_APPTYP_EXETYPE)

   #endif /* INCL_DOSFILEMGR */

#endif /* INCL_DOSSESMGR || INCL_DOSFILEMGR */




/*** Device support */

#ifdef INCL_DOSDEVICES

   #define DosDevIOCtl2    DosDevIOCtl

   APIRET APIENTRY  DosDevConfig(PVOID pdevinfo,
                                 ULONG item);

   #define DEVINFO_PRINTER         0       /* Number of printers attached */
   #define DEVINFO_RS232           1       /* Number of RS232 ports */
   #define DEVINFO_FLOPPY          2       /* Number of diskette drives */
   #define DEVINFO_COPROCESSOR     3       /* Presence of math coprocessor */
   #define DEVINFO_SUBMODEL        4       /* PC Submodel Type */
   #define DEVINFO_MODEL           5       /* PC Model Type */
   #define DEVINFO_ADAPTER         6       /* Primary display adapter type */

   APIRET APIENTRY  DosDevIOCtl(HFILE hDevice,
                                ULONG category,
                                ULONG function,
                                PVOID pParams,
                                ULONG cbParmLenMax,
                                PULONG pcbParmLen,
                                PVOID pData,
                                ULONG cbDataLenMax,
                                PULONG pcbDataLen);


   APIRET APIENTRY  DosPhysicalDisk(ULONG function,
                                    PVOID pBuf,
                                    ULONG cbBuf,
                                    PVOID pParams,
                                    ULONG cbParams);

   #define INFO_COUNT_PARTITIONABLE_DISKS  1       /* # of partitionable disks */
   #define INFO_GETIOCTLHANDLE             2       /* Obtain handle            */
   #define INFO_FREEIOCTLHANDLE            3       /* Release handle           */

#endif /* INCL_DOSDEVICES */


/*** DosNamedPipes API Support */

#ifdef INCL_DOSNMPIPES

   /*** Data structures used with named pipes ***/

   typedef LHANDLE HPIPE;     /* hp */
   typedef HPIPE   *PHPIPE;

   typedef struct _AVAILDATA              /* AVAILDATA */
   {
      USHORT      cbpipe;            /* bytes left in the pipe */
      USHORT      cbmessage;         /* bytes left in the current message */
   } AVAILDATA;
   typedef AVAILDATA *PAVAILDATA;

   typedef struct _PIPEINFO               /* nmpinf */
   {
      USHORT cbOut;                  /* length of outgoing I/O buffer */
      USHORT cbIn;                   /* length of incoming I/O buffer */
      BYTE   cbMaxInst;              /* maximum number of instances   */
      BYTE   cbCurInst;              /* current number of instances   */
      BYTE   cbName;                 /* length of pipe name           */
      CHAR   szName[1];              /* start of name                 */
   } PIPEINFO;
   typedef PIPEINFO  *PPIPEINFO;

   typedef struct _PIPESEMSTATE    /* nmpsmst */
   {
      BYTE   fStatus;         /* type of record, 0 = EOI, 1 = read ok, */
                              /* 2 = write ok, 3 = pipe closed         */
      BYTE   fFlag;           /* additional info, 01 = waiting thread  */
      USHORT usKey;           /* user's key value                      */
      USHORT usAvail;         /* available data/space if status = 1/2  */
   } PIPESEMSTATE;
   typedef PIPESEMSTATE *PPIPESEMSTATE;

   #define NP_INDEFINITE_WAIT      -1
   #define NP_DEFAULT_WAIT         0L

   /* DosPeekNmPipe() pipe states */

   #define NP_STATE_DISCONNECTED   0x0001
   #define NP_STATE_LISTENING      0x0002
   #define NP_STATE_CONNECTED      0x0003
   #define NP_STATE_CLOSING        0x0004

   /* DosCreateNPipe open modes */

   #define NP_ACCESS_INBOUND       0x0000
   #define NP_ACCESS_OUTBOUND      0x0001
   #define NP_ACCESS_DUPLEX        0x0002
   #define NP_INHERIT              0x0000
   #define NP_NOINHERIT            0x0080
   #define NP_WRITEBEHIND          0x0000
   #define NP_NOWRITEBEHIND        0x4000

   /* DosCreateNPipe and DosQueryNPHState state */

   #define NP_READMODE_BYTE        0x0000
   #define NP_READMODE_MESSAGE     0x0100
   #define NP_TYPE_BYTE            0x0000
   #define NP_TYPE_MESSAGE         0x0400
   #define NP_END_CLIENT           0x0000
   #define NP_END_SERVER           0x4000
   #define NP_WAIT                 0x0000
   #define NP_NOWAIT               0x8000
   #define NP_UNLIMITED_INSTANCES  0x00FF


#ifdef __cplusplus
   APIRET APIENTRY  DosCallNPipe(PCSZ  pszName,
                                 PVOID pInbuf,
                                 ULONG cbIn,
                                 PVOID pOutbuf,
                                 ULONG cbOut,
                                 PULONG pcbActual,
                                 ULONG msec);
#else
   APIRET APIENTRY  DosCallNPipe(PSZ pszName,
                                 PVOID pInbuf,
                                 ULONG cbIn,
                                 PVOID pOutbuf,
                                 ULONG cbOut,
                                 PULONG pcbActual,
                                 ULONG msec);
#endif

   APIRET APIENTRY  DosConnectNPipe(HPIPE hpipe);

   APIRET APIENTRY  DosDisConnectNPipe(HPIPE hpipe);

#ifdef __cplusplus
   APIRET APIENTRY  DosCreateNPipe(PCSZ  pszName,
                                   PHPIPE pHpipe,
                                   ULONG openmode,
                                   ULONG pipemode,
                                   ULONG cbInbuf,
                                   ULONG cbOutbuf,
                                   ULONG msec);
#else
   APIRET APIENTRY  DosCreateNPipe(PSZ pszName,
                                   PHPIPE pHpipe,
                                   ULONG openmode,
                                   ULONG pipemode,
                                   ULONG cbInbuf,
                                   ULONG cbOutbuf,
                                   ULONG msec);
#endif

   APIRET APIENTRY  DosPeekNPipe(HPIPE hpipe,
                                 PVOID pBuf,
                                 ULONG cbBuf,
                                 PULONG pcbActual,
                                 PAVAILDATA pAvail,
                                 PULONG pState);

   APIRET APIENTRY  DosQueryNPHState(HPIPE hpipe,
                                     PULONG pState);

   APIRET APIENTRY  DosQueryNPipeInfo(HPIPE hpipe,
                                      ULONG infolevel,
                                      PVOID pBuf,
                                      ULONG cbBuf);

   APIRET APIENTRY  DosQueryNPipeSemState(HSEM hsem,
                                          PPIPESEMSTATE pnpss,
                                          ULONG cbBuf);

#ifdef __cplusplus
   APIRET APIENTRY  DosRawReadNPipe(PCSZ  pszName,
                                    ULONG cb,
                                    PULONG pLen,
                                    PVOID pBuf);
#else
   APIRET APIENTRY  DosRawReadNPipe(PSZ pszName,
                                    ULONG cb,
                                    PULONG pLen,
                                    PVOID pBuf);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosRawWriteNPipe(PCSZ  pszName,
                                     ULONG cb);
#else
   APIRET APIENTRY  DosRawWriteNPipe(PSZ pszName,
                                     ULONG cb);
#endif

   APIRET APIENTRY  DosSetNPHState(HPIPE hpipe,
                                   ULONG state);

   APIRET APIENTRY  DosSetNPipeSem(HPIPE hpipe,
                                   HSEM hsem,
                                   ULONG key);

   APIRET APIENTRY  DosTransactNPipe(HPIPE hpipe,
                                     PVOID pOutbuf,
                                     ULONG cbOut,
                                     PVOID pInbuf,
                                     ULONG cbIn,
                                     PULONG pcbRead);

#ifdef __cplusplus
   APIRET APIENTRY  DosWaitNPipe(PCSZ  pszName,
                                 ULONG msec);
#else
   APIRET APIENTRY  DosWaitNPipe(PSZ pszName,
                                 ULONG msec);
#endif

   /* values in fStatus */
   #define NPSS_EOI                   0     /* End Of Information    */
   #define NPSS_RDATA                 1     /* read data available   */
   #define NPSS_WSPACE                2     /* write space available */
   #define NPSS_CLOSE                 3     /* pipe in CLOSING state */

   /* values in npss_flag */
   #define NPSS_WAIT                  0x01  /* waiting thread on end of pipe */

   /* defined bits in pipe mode */
   #define NP_NBLK                    0x8000 /* non-blocking read/write */
   #define NP_SERVER                  0x4000 /* set if server end       */
   #define NP_WMESG                   0x0400 /* write messages          */
   #define NP_RMESG                   0x0100 /* read as messages        */
   #define NP_ICOUNT                  0x00FF /* instance count field    */


   /*Named pipes may be in one of several states depending on the actions
   * that have been taken on it by the server end and client end.  The
   * following state/action table summarizes the valid state transitions:
   *
   * Current state           Action                  Next state
   *
   *  <none>             server DosMakeNmPipe        DISCONNECTED
   *  DISCONNECTED       server connect              LISTENING
   *  LISTENING          client open                 CONNECTED
   *  CONNECTED          server disconn              DISCONNECTED
   *  CONNECTED          client close                CLOSING
   *  CLOSING            server disconn              DISCONNECTED
   *  CONNECTED          server close                CLOSING
   *  <any other>        server close                <pipe deallocated>
   *
   * If a server disconnects his end of the pipe, the client end will enter a
   * special state in which any future operations (except close) on the file
   * descriptor associated with the pipe will return an error.
   */

   /*
   *      Values for named pipe state
   */

   #define NP_DISCONNECTED       1         /* after pipe creation or Disconnect */
   #define NP_LISTENING          2         /* after DosNmPipeConnect            */
   #define NP_CONNECTED          3         /* after Client open                 */
   #define NP_CLOSING            4         /* after Client or Server close      */


#endif /* INCL_DOSNMPIPES */



/*** DosProfile API support */

#ifdef INCL_DOSPROFILE

   /*** Perfview API support */

   APIRET APIENTRY DosTmrQueryFreq(PULONG pulTmrFreq);

   APIRET APIENTRY DosTmrQueryTime(PQWORD pqwTmrTime);

   APIRET APIENTRY DosRegisterPerfCtrs(PBYTE pbDataBlk,
                                       PBYTE pbTextBlk,
                                       ULONG flFlags);

   /* DosProfile ordinal number */

   #define PROF_ORDINAL        133

   /* DosProfile usType */

   #define PROF_SYSTEM           0
   #define PROF_USER             1
   #define PROF_USEDD            2
   #define PROF_KERNEL           4
   #define PROF_VERBOSE          8
   #define PROF_ENABLE          16

   /* DosProfile usFunc */

   #define PROF_ALLOC            0
   #define PROF_CLEAR            1
   #define PROF_ON               2
   #define PROF_OFF              3
   #define PROF_DUMP             4
   #define PROF_FREE             5

   /* DosProfile tic count granularity (DWORD) */

   #define PROF_SHIFT            2

   /* DosProfile module name string length */

   #define PROF_MOD_NAME_SIZE   10

   /* DosProfile error code for end of data */

   #define PROF_END_OF_DATA     13

#endif /* INCL_DOSPROFILE */



/* Virtual DOS Machine API support */
#ifdef INCL_DOSMVDM

   typedef LHANDLE   HVDD;     /* hvdd */
   typedef HVDD     *PHVDD;    /* phvdd */

#ifdef __cplusplus
   APIRET APIENTRY  DosOpenVDD(PCSZ  pszVDD,
                               PHVDD phvdd);
#else
   APIRET APIENTRY  DosOpenVDD(PSZ pszVDD,
                               PHVDD phvdd);
#endif

   APIRET APIENTRY  DosRequestVDD(HVDD hvdd,
                                  SGID sgid,
                                  ULONG cmd,
                                  ULONG cbInput,
                                  PVOID pInput,
                                  ULONG cbOutput,
                                  PVOID pOutput);

   APIRET APIENTRY  DosCloseVDD(HVDD hvdd);

#ifdef __cplusplus
   APIRET APIENTRY  DosQueryDOSProperty(SGID sgid,
                                        PCSZ  pszName,
                                        ULONG cb,
                                        PCSZ  pch);
#else
   APIRET APIENTRY  DosQueryDOSProperty(SGID sgid,
                                        PSZ pszName,
                                        ULONG cb,
                                        PSZ pch);
#endif

#ifdef __cplusplus
   APIRET APIENTRY  DosSetDOSProperty(SGID sgid,
                                      PCSZ  pszName,
                                      ULONG cb,
                                      PCSZ  pch);
#else
   APIRET APIENTRY  DosSetDOSProperty(SGID sgid,
                                      PSZ pszName,
                                      ULONG cb,
                                      PSZ pch);
#endif
#endif /* INCL_DOSMVDM */


#ifdef INCL_BDCALLS

   #include "bdcalls.h"

#endif /*INCL_BDCALLS */

#endif /* __BSEDOS__ */

/* NOINC */
#ifdef __cplusplus
        }
#endif
#if __IBMC__ || __IBMCPP__
   #pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( restore )
   #endif
   #pragma info( restore )
#endif
/* INC */

/**************************** end of file **********************************/
