/**************************************************************************
 *
 * File Name        : SHOW.H
 *
 * Description      : global data header for SHOW.EXE
 *
 * Copyright        : COPYRIGHT IBM CORPORATION, 1991, 1992
 *
 *         DISCLAIMER OF WARRANTIES.  The following [enclosed] code is
 *         sample code created by IBM Corporation. This sample code is not
 *         part of any standard or IBM product and is provided to you solely
 *         for  the purpose of assisting you in the development of your
 *         applications.  The code is provided "AS IS", without
 *         warranty of any kind.  IBM shall not be liable for any damages
 *         arising out of your use of the sample code, even if they have been
 *         advised of the possibility of such damages.
 *
 ********`*******************************************************************/

/*   Menu items definitions
*/
#define ID_MAINWND      256
#define ID_OPTIONS      257
#define ID_SNAP         258
#define ID_EXIT         259
#define ID_NEWTEXT      260
#define ID_QUERY        261

#define ID_DIALOG       262
#define ID_EF_11         11
#define ID_EF_12         12
#define ID_EF_13         13
#define ID_EF_14         14
#define ID_EF_15         15
#define ID_EF_16         16
#define ID_EF_17         17
#define ID_EF_18         18
#define ID_EF_19         19
#define ID_EF_20         20


/*   Maximum number of files to support
*/
#define MAX_FILE 16                    /* Maximum number of bitmap files    */



/*   Windoe data structure
*/
typedef struct _WINDATA
{
    BOOL   fVrnDisabled;             /* ????  Visual region enable/disable  */
    BOOL   fChgSrcPalette;           /* Flag for change of source palette   */
    HWND   hwndFrame;                /* Frame window handle                 */
    HWND   hwndClient;               /* Client window handle                */
    HDIVE  hDive;                    /* DIVE handle                         */
    ULONG  ulnumColor;               /* Number of colors in bitmaps         */
    ULONG  ulWidth;                  /* Bitmap width in pels                */
    ULONG  ulHeight;                 /* Bitmap Height in pels               */
    FOURCC fccColorFormat;           /* Bitmap color format                 */
    ULONG  ulMaxFiles;               /* Number of bimap files showing       */
    TID    tidBlitThread;            /* Thread ID for blitting routine      */
    ULONG  ulSrcLineSizeBytes;       /* source linesize                     */
    LONG   cxWidthWindow;            /* Size of width of frame window       */
    LONG   cyHeightWindow;           /* Size of height of frame window      */
} WINDATA, *PWINDATA;

