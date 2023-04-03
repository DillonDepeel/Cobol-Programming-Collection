/*static char *SCCSID = "@(#)vkbd.h     6.3 92/02/14";*/
/***    VKBD.H
 *
 *
 *      MVDM Virtual Keyboard Exported Definitions
 *      Copyright (c) 1992 IBM Corporation
 *
 *      For include file hierarchy, see MVDM.H
 *
 *
 */

#pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( none )
   #endif
#pragma info( restore )

#ifndef __VKBD__
#define __VKBD__


#ifndef INCL_NONE               /* include everything by default */
#define INCL_VKBD
#endif

#ifdef  INCL_VKBD
#define INCL_VKBDSYSREQ
#define INCL_VKBDINTERFACES
#endif



#ifdef  INCL_VKBDSYSREQ

/***    VKBD constants
 */

#define VKBD_NAME               "VKBD$"

/* VKBD functions for use by PMVDMP
 */
#define VKBDSYSREQ_SETACCESS    1   /* Obtain exclusive access for VDM    */
#define VKBDSYSREQ_SETFOCUS     2   /* Notify VDM is gaining/losing focus */
#define VKBDSYSREQ_POSTSCAN     3   /* Send scan code (typed)             */
#define VKBDSYSREQ_POSTCHAR     4   /* Send character code (pasted)       */
#define VKBDSYSREQ_MAX          4


/***    VKBD data types
 */

typedef struct keypkt_s {           /* kp (for VKBDSYSREQ_POSTSCAN/CHAR) */
    UCHAR       kp_Scan;            /* scan code                         */
    UCHAR       kp_Char;            /* translated char                   */
    USHORT      kp_fsState;         /* shift states                      */
    USHORT      kp_fsDDFlags;       /* translated flags                  */
    USHORT      kp_fsKey;           /* key flags                         */
} KEYPKT;
typedef KEYPKT *PKEYPKT;
typedef PKEYPKT *PPKEYPKT;

typedef struct shiftpkt_s {         /* sp (for VKBDSYSREQ_SETFOCUS)            */
    USHORT      fSetFocus;          /* TRUE to set focus, FALSE to clear focus */
    USHORT      fsShift;            /* PM shift states during set focus        */
                                    /*   undefined during clear focus          */
} SHIFTPKT;
typedef SHIFTPKT *PSHIFTPKT;
typedef PSHIFTPKT *PPSHIFTPKT;

#endif /* INCL_VKBDSYSREQ */



/***    VKBD function prototypes
 */

#ifdef  INCL_VKBDINTERFACES

typedef INT (* HOOKENTRY PFNSCRNP)(VOID);

VOID VDHENTRY VDHRegisterScreenProc(PFNSCRNP);
USHORT VDHENTRY VDHQueryKeyShift(HVDM);

/* VPL Idle detection services */
VOID VDHENTRY VDHWakeIdle(HVDM);
VOID VDHENTRY VDHReportPeek(ULONG);
VOID VDHENTRY VDHNotIdle(VOID);
BOOL * VDHENTRY VDHGetBusyFlagPtr(void);

/* requests for SVC access to VPL polling services */
#define VPL_NAME "VPL_IDLE"
#define VDHVPL_BUSY_ADDR 0         /* provide V86 busy byte address */
#define VDHVPL_WAIT      1         /* make VDM sleep                */
#define VDHVPL_NOIDLE    2         /* Turn idle detection off       */
#define VDHVPL_WAKE      3         /* Report busy activity          */

#endif

#endif /* __VKBD__ */

#pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( restore )
   #endif
#pragma info( restore )
