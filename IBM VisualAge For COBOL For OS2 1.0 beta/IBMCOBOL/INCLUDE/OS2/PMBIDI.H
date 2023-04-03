/****************************** Module Header ******************************\
*
* Module Name: PMBIDI.H
*
* OS/2 Presentation Manager Window Manager - Bidirectional support include file.
*
* Copyright (c) International Business Machines Corporation 1993, 1994.
*
\***************************************************************************/

#ifndef PM_BIDI_INCLUDED    /* Eliminate multiple definitions... */
#define PM_BIDI_INCLUDED

/************************************************************************/
/* Bidirectional languages support window messages                      */
/************************************************************************/
#define WM_SETBIDIATTR       0x0BD0
#define WM_QUERYBIDIATTR     0x0BD1
#define WM_SETBIDISTAT       0x0BD2
#define WM_QUERYBIDISTAT     0x0BD3
#define WM_KBDLAYERCHANGED   0x0BD4


/************************************************************************/
/*  Language Viewer messages                                            */
/************************************************************************/
 #define WM_LANGVIEWINFOCHANGED   0x0BE0
 #define WM_LANG_OPTIONS_DIALOG   0x0BE2
 #define WM_LANGOPTIONSDIALOG     0x0BE2

 /* LVI values - indicating which Bidi information has changed */

 #define LVI_WND_BIDI_ATTR     1
 #define LVI_WND_BIDI_STAT     2
 #define LVI_FOCUS_CHANGE      3
 #define LVI_KBD_LAYER         4
 #define LVI_CSD               5
 #define LVI_ALL             (-1)



/* Possible return values from WM_SETBIDIATTR/WM_SETBIDISTAT */
#define SBI_MSG_NOT_PROCESSED   0L
#define SBI_MSG_PROCESSED       1L
#define SBI_MSG_PROCESSED_SELF  2L


/************************************************************************/
/* Bidirectional attributes masks                                       */
/************************************************************************/
#define BDAM_INIT               0x80000000
#define BDAM_LEVEL              0x70000000
#define BDAM_TEXTTYPE           0x01000000
#define BDAM_TEXT_ORIENTATION   0x00030000
#define BDAM_WND_ORIENTATION    0x00100000
#define BDAM_NUMERALS           0x00003000
#define BDAM_SYM_SWAP           0x00000100
#define BDAM_WORD_BREAK         0x00000200
#define BDAM_TEXT_SHAPE         0x000000FF

#define BDAM_ALL    (BDAM_INIT             | \
                     BDAM_LEVEL            | \
                     BDAM_TEXTTYPE         | \
                     BDAM_TEXT_ORIENTATION | \
                     BDAM_WND_ORIENTATION  | \
                     BDAM_NUMERALS         | \
                     BDAM_SYM_SWAP         | \
                     BDAM_WORD_BREAK       | \
                     BDAM_TEXT_SHAPE         )

/************************************************************************/
/* Bidirectional attributes values (in Bidi attributes word)            */
/*                                                                      */
/* Note: Value of the attributes must match the values in layout.h.     */
/************************************************************************/
#define BDA_INIT                0x80000000
#define BDA_LEVEL               0x30000000

#define BDA_TEXTTYPE_VISUAL     0x00000000
#define BDA_TEXTTYPE_IMPLICIT   0x01000000

#define BDA_WND_ORIENT_LTR      0x00000000
#define BDA_WND_ORIENT_RTL      0x00100000

#define BDA_TEXT_ORIENT_LTR     0x00000000
#define BDA_TEXT_ORIENT_RTL     0x00010000
#define BDA_TEXT_ORIENT_DYNAMIC 0x00020000
#define BDA_TEXT_ORIENT_CONTEXT 0x00020000

#define BDA_NUMERALS_NOMINAL    0x00000000
#define BDA_NUMERALS_PASSTHRU   0x00001000
#define BDA_NUMERALS_NATIONAL   0x00002000
#define BDA_NUMERALS_CONTEXTUAL 0x00003000

#define BDA_SYM_SWAP_OFF        0x00000000
#define BDA_SYM_SWAP_ON         0x00000100

#define BDA_WORD_BREAK_OFF      0x00000000
#define BDA_WORD_BREAK_ON       0x00000200

#define BDA_TEXT_DISPLAY_SHAPED 0x00000000
#define BDA_TEXT_SAVE_SHAPED    0x00000001
#define BDA_TEXT_NOMINAL        0x00000010
#define BDA_TEXT_INITIAL        0x00000011
#define BDA_TEXT_MIDDLE         0x00000012
#define BDA_TEXT_FINAL          0x00000013
#define BDA_TEXT_ISOLATED       0x00000014


/************************************************************************/
/* Bidirectional attribute (BIDIATTR) as specified in a window template */
/* or in the resource script.                                           */
/************************************************************************/
#define BIDIPARAM                  PRESPARAMS

#define PP_BDATTR_FIRST            0x100L  /* First BidiAttr PresParam  */
                                           /*                           */
#define PP_BDATTR_ALL              0x101L  /* Set ALL Bidi attrs        */
                                           /*                           */
#define PP_BDATTR_TEXTTYPE         0x102L  /* Text/Data type            */
#define PP_BDATTR_TEXT_ORIENTATION 0x103L  /* Text Orientation          */
#define PP_BDATTR_WND_ORIENTATION  0x104L  /* Window Orientation        */
#define PP_BDATTR_NUMERALS         0x105L  /* Arabic/Hindi Numerals     */
#define PP_BDATTR_SYM_SWAP         0x106L  /* Symetric Swapping         */
#define PP_BDATTR_WORD_BREAK       0x107L  /* Word break                */
#define PP_BDATTR_TEXT_SHAPE       0x108L  /* Char Shape Determination  */
                                           /*                           */
#define PP_BDATTR_LAST             0x108L  /* Last BidiAttr PresParam   */
                                           /*                           */
#define PP_BDSTATUS                0x110L  /* Bidirectional status flags*/


/************************************************************************/
/* Values used in Set/Query Bidirectional status word                   */
/************************************************************************/
#define BDS_HKFLAG_ENG_LAYER            0x00010000
#define BDS_HKFLAG_NAT_LAYER            0x00020000
#define BDS_HKFLAG_PUSH                 0x00040000
#define BDS_HKFLAG_END_PUSH             0x00080000
#define BDS_HKFLAG_AUTO_PUSH            0x00100000
#define BDS_HKFLAG_FIELD_REV            0x00200000
#define BDS_HKFLAG_SCREEN_REV           0x00400000
#define BDS_HKFLAG_STATUS_INDICATOR     0x02000000

#define BDS_HKFLAG_DISPLAY_SHAPED       0x04000000
#define BDS_HKFLAG_INITIAL              0x08000000
#define BDS_HKFLAG_MIDDLE               0x10000000
#define BDS_HKFLAG_FINAL                0x20000000
#define BDS_HKFLAG_ISOLATED             0x40000000
#define BDS_HKFLAG_SAVE_SHAPED          0x80000000
#define BDS_HKFLAG_ENTRY_SWITCH_UI      0x00800000

#define BDS_FAUTOPUSH_RTL_ON            0x00000001
#define BDS_FAUTOPUSH_LTR_ON            0x00000002
#define BDS_FPUSH_ON                    0x00000004

#define BDS_DISABLE_INPUT_PROCESSING    0x00000020


/************************************************************************/
/* Bidirectional status masks                                           */
/************************************************************************/
#define BDSM_HKFLAGS                    0xFE7F0000

#define BDSM_AUTOPUSH_RTL               0x00000001
#define BDSM_AUTOPUSH_LTR               0x00000002
#define BDSM_PUSH_ON                    0x00000004

#define BDSM_DISABLE_INPUT_PROCESSING   0x00000020

#define BDSM_ALL           ( BDSM_HKFLAGS      |                \
                             BDSM_AUTOPUSH_RTL |                \
                             BDSM_AUTOPUSH_LTR |                \
                             BDSM_PUSH_ON      |                \
                             BDSM_DISABLE_INPUT_PROCESSING )


/************************************************************************/
/* Functions to Set/Query Bidirectional Langugage Information           */
/************************************************************************/
#define  LI_BD_PROCESS_ATTR   0x00000010L /* Process Bidi attributes    */
#define  LI_BD_PROCESS_STAT   0x00000011L /* Process Bidi attributes    */
#define  LI_BD_WND_ATTR       0x00000020L /* Window Bidi Attributes     */
#define  LI_BD_WND_STAT       0x00000021L /* Window Bidi Status         */
#define  LI_BD_CLIP_ATTR      0x00000030L /* Clipboard Bidi Attributes  */
#define  LI_BD_CLIP_CONV_ATTR 0x00000031L /* Clipboard Conversion Bidi Attributes  */



/* Values used in WinSet/QueryLangInfo (in flFlags); */
#define LIF_NO_SENDMSG      0x00000001     /* No message is sent to the */
                                           /* window to inform it of the*/
                                           /* change                    */
#define LIF_CHILD_INHERIT   0x00000002     /* Send SetBidiAttr msg to   */
                                           /* all child windows.        */
#define LIF_WND_REFRESH     0x00000004     /* Refresh all windows whose */
                                           /* bidi attributes are set   */
                                           /* directly by the API (and  */
                                           /* not by a sent message.    */




/************************************************************************/
/* Flags for WinSetKbdLayer                                             */
/************************************************************************/
#define SKLF_SENDMSG 0x00000001

/************************************************************************/
/* Keyboard layers for WinSetKbdLayer                                   */
/************************************************************************/
#define KL_LATIN     0x00000000
#define KL_NATIONAL  0x00000001

/************************************************************************/
/* PM Bidirectional support - function prototypes.                      */
/************************************************************************/
 #define  WinSetLangInfo       Win32SetLangInfo
 #define  WinQueryLangInfo     Win32QueryLangInfo

 #define  WinSetKbdLayer       Win32SetKbdLayer
 #define  WinQueryKbdLayer     Win32QueryKbdLayer

 #define  WinSetLangViewer     Win32SetLangViewer
 #define  WinQueryLangViewer   Win32QueryLangViewer

 #define  GpiSetBidiAttr       Gpi32SetBidiAttr
 #define  GpiQueryBidiAttr     Gpi32QueryBidiAttr


ULONG   APIENTRY WinSetLangInfo     ( HWND    hwnd,
                                      ULONG   ulEffect,
                                      ULONG   ulData,
                                      ULONG   flMask,
                                      ULONG   flFlags,
                                      ULONG   ulReserved );

ULONG   APIENTRY WinQueryLangInfo   ( HWND    hwnd,
                                      ULONG   ulEffect,
                                      ULONG   flFlags,
                                      ULONG   ulReserved );

ULONG   APIENTRY WinSetKbdLayer     ( HWND hwnd, ULONG idKbdLayer, ULONG flFlags);
ULONG   APIENTRY WinQueryKbdLayer   ( HWND hwnd );

HWND    APIENTRY WinSetLangViewer   ( HAB hab, HWND hwndLangViewer, ULONG Codepage);
HWND    APIENTRY WinQueryLangViewer ( HAB hab, ULONG Codepage );

ULONG   APIENTRY GpiSetBidiAttr     ( HPS  hps, ULONG BidiAttr );
ULONG   APIENTRY GpiQueryBidiAttr   ( HPS  hps );

/*****************************************************************************/
/*  Macros to manipulate Bidi values                                         */
/*****************************************************************************/
/*
 * Macro to make a BidiAttribute/Status ULONG from several fields
 *
 * Example :      SET_BD_VALUE(BidiAtts,
 *                             BDA_TEXT_ORIENT_RTL   | BDA_TEXTTYPE_IMPLICIT,
 *                             BDAM_TEXT_ORIENTATION | BDAM_TEXTTYPE)
 *
 */
#define SET_BD_VALUE(BidiValue,NewBidiValue,Mask) \
        (BidiValue = ((BidiValue & (~(Mask))) | (NewBidiValue & (Mask))))

/*
 * Macro to extract one or more fields from a BidiAttribute/Status ULONG
 *
 * Example :      Orientation = QUERY_BD_VALUE(BidiAtts,BDAM_TEXT_ORIENTATION)
 *
 */
#define QUERY_BD_VALUE(BidiValue,Mask) \
       ( ((ULONG)BidiValue) & ((ULONG)Mask) )


/************************************************************************/
/* New CURSOR flags for Left-To-Right and Right-To-Left cursors         */
/* These are in addtion to those in the CURSOR section in PMWIN.H       */
/************************************************************************/
#define CURSOR_DIR       0x0100
#define CURSOR_DIR_LTR   0x0100
#define CURSOR_DIR_RTL   0x0300


/*****************************************************************************/
/*  PM Bidi Error codes.                                                     */
/*****************************************************************************/
#define PMERR_BIDI_FIRST               0x10F0
#define PMERR_BIDI_TEXT_CONV_FAILED    0x10F0
#define PMERR_BIDI_LAST                0x10FF

/*****************************************************************************/
/* Bidi virtual key definitions                                              */
/*****************************************************************************/

#define VK_BIDI_FIRST          0xE0

#define VK_START_PUSH          0xE0
#define VK_END_PUSH            0xE1
#define VK_REVERSE_FIELD       0xE2
#define VK_REVERSE_WINDOW      0xE3
#define VK_AUTOPUSH            0xE4
#define VK_STATUS_INDICATOR    0xE5

#define VK_TEXT_DISPLAY_SHAPED 0xE6
#define VK_TEXT_INITIAL        0xE7
#define VK_TEXT_MIDDLE         0xE8
#define VK_TEXT_FINAL          0xE9
#define VK_TEXT_ISOLATED       0xEA
#define VK_TEXT_SAVE_SHAPED    0xEB

#define VK_REQUIRED_SPACE      0xEC
#define VK_LTR_MARKER          0xED
#define VK_RTL_MARKER          0xEE

#define VK_ENTRY_SWITCH_UI     0xEF

#define VK_LAYER0              0xF0
#define VK_LAYER1              0xF1
#define VK_LAYER2              0xF2
#define VK_LAYER3              0xF3
#define VK_LATIN_LAYER         VK_LAYER0
#define VK_NATIONAL_LAYER      VK_LAYER1

#define VK_BIDI_LAST           0xFF


#ifdef INCL_WINSTDDLGS   /* StdDlgs related Bidi Definitions             */
 /************************************************************************/
 /*  Language-sensitive definition for standard File and Font dialogs.   */
 /************************************************************************/
 #define FNTS_NATIONAL_LANGUAGE   0x80000000L /* Use National Language */
 #define FDS_NATIONAL_LANGUAGE    0x80000000L /* Use National Language */


 /************************************************************************/
 /*  WM_ messages related to bidirectional language support for          */
 /*      CUATOOLS components.                                            */
 /************************************************************************/
 #define CM_SETITEMBIDIATTR             0x0390
 #define CM_SETFIELDBIDIATTR            0x0391
 #define CM_QUERYITEMBIDIATTR           0x0392
 #define CM_QUERYFIELDBIDIATTR          0x0393

 #define BKM_SETSTATUSLINEBIDIATTR      0x0394
 #define BKM_QUERYSTATUSLINEBIDIATTR    0x0395
 #define BKM_SETTABTEXTBIDIATTR         0x0396
 #define BKM_QUERYTABTEXTBIDIATTR       0x0397

 #define VM_SETITEMBIDIATTR             0x0398
 #define VM_QUERYITEMBIDIATTR           0x0399



 typedef struct _BD_ATTR_MASK      /* bam */
 {
      ULONG ulBdAttr;              /* Bidirectional Attributes word */
      ULONG ulBdMask;              /* Bitmask to define which bidi  */
 } BD_ATTR_MASK;                   /* attributes are to be used.    */
 typedef BD_ATTR_MASK FAR *PBD_ATTR_MASK;

 /**********************************************************************/
 /* BOOKPAGEBIDIINFO structure is pointed to from the BOOKPAGEINFO     */
 /* structure (BKM_SETPAGEINFO notebook message)                       */
 /**********************************************************************/
 typedef struct _BOOKPAGEBIDIINFO  /* bkpgbdi */
 {
      BD_ATTR_MASK bamMajorTab;    /* Major Tab Text BD_ATTR_MASK struct.   */
      BD_ATTR_MASK bamMinorTab;    /* Minor Tab Text BD_ATTR_MASK struct.   */
      BD_ATTR_MASK bamStatusLine;  /* Status Line Text BD_ATTR_MASK struct. */
 } BOOKPAGEBIDIINFO;
 typedef BOOKPAGEBIDIINFO FAR *PBOOKPAGEBIDIINFO;


#endif /*INCL_WINSTDDLGS */

#endif  /*PM_BIDI_INCLUDED*/    /* Eliminate multiple definitions... */

