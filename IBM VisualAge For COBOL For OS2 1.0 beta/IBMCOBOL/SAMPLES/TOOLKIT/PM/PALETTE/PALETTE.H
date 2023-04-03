/*��������������������������������������������������������������������������Ŀ*/
/*�                                                                          �*/
/*� PROGRAM NAME: PALETTE                                                    �*/
/*� -------------                                                            �*/
/*�  A PM program demonstrating vector fonts, palette manager APIs, and IPF  �*/
/*�  help.                                                                   �*/
/*�                                                                          �*/
/*� COPYRIGHT:                                                               �*/
/*� ----------                                                               �*/
/*�  Copyright (C) International Business Machines Corp., 1992               �*/
/*�                                                                          �*/
/*� DISCLAIMER OF WARRANTIES:                                                �*/
/*� -------------------------                                                �*/
/*�  The following [enclosed] code is sample code created by IBM Corporation.�*/
/*�  This sample code is not part of any standard IBM product and is provided�*/
/*�  to you solely for the purpose of assisting you in the development of    �*/
/*�  your applications.  The code is provided "AS IS", without warranty of   �*/
/*�  any kind.  IBM shall not be liable for any damages arising out of your  �*/
/*�  use of the sample code, even if they have been advised of the           �*/
/*�  possibility of such damages.                                            �*/
/*�                                                                          �*/
/*� For details on what this program does etc., please see the PALETTE.C     �*/
/*� file.                                                                    �*/
/*�                                                                          �*/
/*����������������������������������������������������������������������������*/

/*��������������������������������������������������������������������������Ŀ*/
/*� PALETTE.H                                                                �*/
/*�                                                                          �*/
/*� Palette Manager Header File                                              �*/
/*����������������������������������������������������������������������������*/

/*��������������������������������������������������������������������������Ŀ*/
/*� Constant Definitions                                                     �*/
/*����������������������������������������������������������������������������*/

#define ID_WINDOW              1
#define IDM_OPTIONS          100
#define IDM_STARTSTOP        101
#define IDM_SPEED            102
#define IDM_SPEEDSLOW        103
#define IDM_SPEEDMEDIUM      104
#define IDM_SPEEDFAST        105
#define IDM_LOGO             106
#define IDM_LOGOIBM          107
#define IDM_LOGOOS2          108
#define IDM_EXIT             109

#define IDM_HELP             200
#define IDM_HELPFORHELP      201
#define IDM_HELPABOUT        202
#define IDM_HELP_INDEX       203
#define IDM_KEYS_HELP        204
#define IDD_PRODUCTINFO      300
#define IDM_ABOUT_OK         301
#define IDR_BITMAP           400
#define IDC_BITMAP           401
#define IDC_OK               402
#define MAIN_HELPTABLE       501
#define SUBTABLE_OPTION      502
#define IDM_EXTENDED_HELP    503

#define ID_NULL              9999

/*��������������������������������������������������������������������������Ŀ*/
/*� Macro definitions                                                        �*/
/*����������������������������������������������������������������������������*/

#define LONGFromRGB(R,G,B) (LONG)(((LONG)R<<16)+((LONG)G<<8)+(LONG)B)

/*��������������������������������������������������������������������������Ŀ*/
/*� Function Prototypes                                                      �*/
/*����������������������������������������������������������������������������*/

void _System DrawingThread( ULONG );  /* _System linkage not _Optlink    */
                                      /* _Optlink uses Register passing  */

FNWP PaletteWndProc;  /* FNWP = Standard _System Callback function prototype */

FNWP AboutDlgProc;

int main(void);

LONG mprintf(PSZ pszFmt, ...);

void SetPulldownState( HWND   hwnd,
                       USHORT item,
                       BOOL   state);

LONG PaletteInit(VOID);

VOID DrawPaletteLines(RECTL rctl,
                      ULONG ulPalCount,
                      HPS hps);
/********************************  end of palette.h  **************************/

