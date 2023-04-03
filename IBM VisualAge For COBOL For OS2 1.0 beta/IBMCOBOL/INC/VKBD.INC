;       SCCSID = @(#)vkbd.inc	6.1 92/03/25
;***    VKBD.INC
;
;
;       MVDM Virtual Keyboard Exported Definitions
;
;       For include file hierarchy, see MVDM.INC
;
;

ifndef INCL_NONE               ;include everything by default
INCL_VKBD       EQU 1
endif

ifdef  INCL_VKBD
INCL_VKBDSYSREQ         EQU 1
INCL_VKBDINTERFACES     EQU 1
endif



ifdef  INCL_VKBDSYSREQ

;***    VKBD constants
;

VKBD_NAME       EQU     "VKBD$"

; VKBD functions for use by PMVDMP
;
VKBDSYSREQ_SETACCESS    EQU     1   ;Obtain exclusive access for VDM
VKBDSYSREQ_SETFOCUS     EQU     2   ;Notify VDM is gaining/losing focus
VKBDSYSREQ_POSTSCAN     EQU     3   ;Send scan code (typed)
VKBDSYSREQ_POSTCHAR     EQU     4   ;Send character code (pasted)
VKBDSYSREQ_MAX  EQU     4


;***    VKBD data types
;

DefStruc ,keypkt_s                      ;kp (for VKBDSYSREQ_POSTSCAN/CHAR)
    UCHAR       kp_Scan            ;scan code
    UCHAR       kp_Char            ;translated char
    USHORT      kp_fsState         ;shift states
    USHORT      kp_fsDDFlags       ;translated flags
    USHORT      kp_fsKey           ;key flags
EndStruc KEYPKT
DefType PKEYPKT,dd
DefType PPKEYPKT,dd

DefStruc ,shiftpkt_s                    ;sp (for VKBDSYSREQ_SETFOCUS)
    USHORT      fSetFocus          ;TRUE to set focus, FALSE to clear focus
    USHORT      fsShift            ;PM shift states during set focus
                                    ;  undefined during clear focus
EndStruc SHIFTPKT
DefType PSHIFTPKT,dd
DefType PPSHIFTPKT,dd

endif ;INCL_VKBDSYSREQ



;***    VKBD function prototypes
;

ifdef INCL_VKBDINTERFACES

DefType PFNSCRNP,dd

VDHENTRY VDHRegisterScreenProc          <PFNSCRNP>
VDHENTRY VDHQueryKeyShift               <HVDM>

;VPL Idle detection services
VDHENTRY VDHWakeIdle            <HVDM>
VDHENTRY VDHReportPeek          <ULONG>
VDHENTRY VDHNotIdle             <VOID>
VDHENTRY VDHGetBusyFlagPtr              <void>

;requests for SVC access to VPL polling services
VPL_NAME        EQU     "VPL_IDLE"
VDHVPL_BUSY_ADDR        EQU     0         ;provide V86 busy byte address
VDHVPL_WAIT     EQU     1         ;make VDM sleep
VDHVPL_NOIDLE   EQU     2         ;Turn idle detection off
VDHVPL_WAKE     EQU     3         ;Report busy activity

endif
