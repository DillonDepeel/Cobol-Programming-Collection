/***************************************************************************\
*
* Module Name: DIVE.H
*
* OS/2 2.1 Multimedia Extensions Display Engine API data structures
*
* Copyright (c) International Business Machines Corporation 1993, 1994
*                         All Rights Reserved
*
*
\****************************************************************************/
#ifdef __cplusplus
   extern "C" {
#endif

#ifndef _DIVE_H_
#define _DIVE_H_

#define MAX_DIVE_INSTANCES    64

#define FOURCC ULONG
#define HDIVE ULONG

#define DIVE_SUCCESS                                     0x00000000
#define DIVE_ERR_INVALID_INSTANCE                        0x00001000
#define DIVE_ERR_SOURCE_FORMAT                           0x00001001
#define DIVE_ERR_DESTINATION_FORMAT                      0x00001002
#define DIVE_ERR_BLITTER_NOT_SETUP                       0x00001003
#define DIVE_ERR_INSUFFICIENT_LENGTH                     0x00001004
#define DIVE_ERR_TOO_MANY_INSTANCES                      0x00001005
#define DIVE_ERR_NO_DIRECT_ACCESS                        0x00001006
#define DIVE_ERR_NOT_BANK_SWITCHED                       0x00001007
#define DIVE_ERR_INVALID_BANK_NUMBER                     0x00001008
#define DIVE_ERR_FB_NOT_ACQUIRED                         0x00001009
#define DIVE_ERR_FB_ALREADY_ACQUIRED                     0x0000100a
#define DIVE_ERR_ACQUIRE_FAILED                          0x0000100b
#define DIVE_ERR_BANK_SWITCH_FAILED                      0x0000100c
#define DIVE_ERR_DEACQUIRE_FAILED                        0x0000100d
#define DIVE_ERR_INVALID_PALETTE                         0x0000100e
#define DIVE_ERR_INVALID_DESTINATION_RECTL               0x0000100f
#define DIVE_ERR_INVALID_BUFFER_NUMBER                   0x00001010
#define DIVE_ERR_SSMDD_NOT_INSTALLED                     0x00001011
#define DIVE_ERR_BUFFER_ALREADY_ACCESSED                 0x00001012
#define DIVE_ERR_BUFFER_NOT_ACCESSED                     0x00001013
#define DIVE_ERR_TOO_MANY_BUFFERS                        0x00001014
#define DIVE_ERR_ALLOCATION_ERROR                        0x00001015
#define DIVE_ERR_INVALID_LINESIZE                        0x00001016
#define DIVE_ERR_FATAL_EXCEPTION                         0x00001017
#define DIVE_ERR_INVALID_CONVERSION                      0x00001018
#define DIVE_WARN_NO_SIZE                                0x00001100

#define DIVE_BUFFER_SCREEN                               0x00000000
#define DIVE_BUFFER_GRAPHICS_PLANE                       0x00000001
#define DIVE_BUFFER_ALTERNATE_PLANE                      0x00000002

/* Notes:
      Associated/Allocated memory buffers start at:      0x00000010

      Specifing DIVE_BUFFER_GRAPHICS_PLANE results in the image being
            transferred to the graphics plane.
      Specifing DIVE_BUFFER_ALTERNATE_PLANE results in the image being
            transferred to the alternate (e.g. overlay) plane.  If your
            hardware doesn't support such a plane, this is an error.
      Specifing DIVE_BUFFER_SCREEN will result in the image being
            transferred to either the graphics plane buffer or the alternate
            plane buffer based on if an alternate buffer exists and based on
            the suitability the overlay plane to accelerate the scaling of
            the image.  If DIVE chooses to use the alternate buffer, it
            will also paint the overlay "key" color on the graphics plane.
            This automatic painting does not occur if the alternate plane
            is explicitly specified.
*/



typedef struct _DIVE_CAPS {

   ULONG  ulStructLen;            /* Set equal to sizeof(DIVE_CAPS)          */
   ULONG  ulPlaneCount;           /* Number of defined planes.               */

   /* Info returned in the following fields pertains to ulPlaneID.           */
   BOOL   fScreenDirect;          /* TRUE if can get addressability to vram. */
   BOOL   fBankSwitched;          /* TRUE if vram is bank-switched.          */
   ULONG  ulDepth;                /* Number of bits per pixel.               */
   ULONG  ulHorizontalResolution; /* Screen width in pixels.                 */
   ULONG  ulVerticalResolution;   /* Screen height in pixels.                */
   ULONG  ulScanLineBytes;        /* Screen scan line size in bytes.         */
   FOURCC fccColorEncoding;       /* Colorspace encoding of the screen.      */
   ULONG  ulApertureSize;         /* Size of vram aperture in bytes.         */

   ULONG  ulInputFormats;         /* Number of input color formats.          */
   ULONG  ulOutputFormats;        /* Number of output color formats.         */
   ULONG  ulFormatLength;         /* Length of format buffer.                */
   PVOID  pFormatData;            /* Pointer to color format buffer FOURCC's.*/

   } DIVE_CAPS;
typedef DIVE_CAPS *PDIVE_CAPS;


/* Notes:
      DiveSetupBlitter may be called with a structure length at any of the
      breaks below (i.e. 8, 28, 32, 52, 60, or 68):
*/

typedef struct _SETUP_BLITTER {

   ULONG  ulStructLen;            /* Set equal to sizeof(SETUP_BLITTER)      */
   BOOL   fInvert;                /* TRUE if we are to invert image on blit. */

   FOURCC fccSrcColorFormat;      /* Color format of source data.            */
   ULONG  ulSrcWidth;             /* Source width in pixels.                 */
   ULONG  ulSrcHeight;            /* Source height in pixels.                */
   ULONG  ulSrcPosX;              /* Source start X position.                */
   ULONG  ulSrcPosY;              /* Source start Y position.                */

   ULONG  ulDitherType;           /* Where 0 is no dither, 1 is 2x2 dither.  */

   FOURCC fccDstColorFormat;      /* Color format of destination data.       */
   ULONG  ulDstWidth;             /* Destination width in pixels.            */
   ULONG  ulDstHeight;            /* Destination height in pixels.           */
   LONG   lDstPosX;               /* Destination start X position.           */
   LONG   lDstPosY;               /* Destination start Y position.           */

   LONG   lScreenPosX;            /* Destination start X position on screen. */
   LONG   lScreenPosY;            /* Destination start Y position on screen. */

   ULONG  ulNumDstRects;          /* Number of visible rectangles.           */
   PRECTL pVisDstRects;           /* Pointer to array of visible rectangles. */

   } SETUP_BLITTER;
typedef SETUP_BLITTER *PSETUP_BLITTER;



ULONG APIENTRY DiveQueryCaps ( PDIVE_CAPS pDiveCaps,
                               ULONG      ulPlaneBufNum );

ULONG APIENTRY DiveOpen ( HDIVE *phDiveInst,
                          BOOL   fNonScreenInstance,
                          PVOID  ppFrameBuffer );

ULONG APIENTRY DiveSetupBlitter ( HDIVE          hDiveInst,
                                  PSETUP_BLITTER pSetupBlitter );

ULONG APIENTRY DiveBlitImage ( HDIVE hDiveInst,
                               ULONG ulSrcBufNumber,
                               ULONG ulDstBufNumber );

ULONG APIENTRY DiveClose ( HDIVE hDiveInst );

ULONG APIENTRY DiveAcquireFrameBuffer ( HDIVE   hDiveInst,
                                        PRECTL  prectlDst );

ULONG APIENTRY DiveSwitchBank ( HDIVE hDiveInst,
                                ULONG ulBankNumber );

ULONG APIENTRY DiveDeacquireFrameBuffer ( HDIVE hDiveInst );

ULONG APIENTRY DiveCalcFrameBufferAddress ( HDIVE  hDiveInst,
                                            PRECTL prectlDest,
                                            PBYTE *ppDestinationAddress,
                                            PULONG pulBankNumber,
                                            PULONG pulRemLinesInBank );

/* Notes on DiveAllocImageBuffer:
      If pbImageBuffer is not NULL, the buffer is associated rather than
      allocated.  If pbImageBuffer is not NULL and the buffer number
      pointed to by pulBufferNumber is non-zero, a new buffer pointer is
      associated with the buffer number.  Even though no memory is
      allocated by DiveAllocImageBuffer when user-allocated buffers are
      associated, DiveFreeImageBuffer should be called to release the
      buffer association to avoid using up available buffer indexes.
      The specified line size will be used if a buffer is allocated in
      system memory, or if a user buffer is associated.  If the
      specified line size is zero, the allocated line size is rounded up
      to the nearest DWORD boundry.
*/

ULONG APIENTRY DiveAllocImageBuffer ( HDIVE  hDiveInst,
                                      PULONG pulBufferNumber,
                                      FOURCC fccColorSpace,
                                      ULONG  ulWidth,
                                      ULONG  ulHeight,
                                      ULONG  ulLineSizeBytes,
                                      PBYTE  pbImageBuffer );

ULONG APIENTRY DiveFreeImageBuffer ( HDIVE hDiveInst,
                                     ULONG ulBufferNumber );

ULONG APIENTRY DiveBeginImageBufferAccess ( HDIVE  hDiveInst,
                                            ULONG  ulBufferNumber,
                                            PBYTE *ppbImageBuffer,
                                            PULONG pulBufferScanLineBytes,
                                            PULONG pulBufferScanLines );

ULONG APIENTRY DiveEndImageBufferAccess ( HDIVE hDiveInst,
                                          ULONG ulBufferNumber );



/* Notes on palettes:
      Neither DiveSetSourcePalette nor DiveSetDestinationPalette API's will set
      the physical palette.  If your application MUST set the PHYSICAL palette,
      try using no more than 236 entries (the middle 236: 10-245, thus leaving
      the top and bottom 10 entries for the Workplace Shell).  If your
      application MUST use ALL 256 entries, it must do so as a full-screen
      (i.e. maximized) application.  Remember, No WM_REALIZEPALETTE message
      will be sent to other running applications, meaning they will not redraw
      and their colors will be all wrong.  It is not recommended that a
      developer use these commands:

   To set physical palette, do the following:
            hps = WinGetPS ( HWND_DESKTOP );
            hdc = GpiQueryDevice ( hps );
            GpiCreateLogColorTable ( hps, LCOL_PURECOLOR | LCOL_REALIZABLE,
                           LCOLF_CONSECRGB, 0, 256, (PLONG)plRGB2Entries );
            DiveSetPhysicalPalette ( hDiveInst, hdc );
            Gre32EntrY3 ( hdc, 0L, 0x000060C6L );
            WinInvalidateRect ( HWND_DESKTOP, (PRECTL)NULL, TRUE );
            WinReleasePS ( hps );

   To reset physical palette, do the following:
            hps = WinGetPS ( HWND_DESKTOP );
            hdc = GpiQueryDevice ( hps );
            Gre32EntrY3 ( hdc, 0L, 0x000060C7L );
            WinInvalidateRect ( HWND_DESKTOP, (PRECTL)NULL, TRUE );
            WinReleasePS ( hps );
*/


ULONG APIENTRY DiveSetDestinationPalette ( HDIVE hDiveInst,
                                           ULONG ulStartIndex,
                                           ULONG ulNumEntries,
                                           PBYTE pbRGB2Entries );

ULONG APIENTRY DiveSetSourcePalette ( HDIVE hDiveInst,
                                      ULONG ulStartIndex,
                                      ULONG ulNumEntries,
                                      PBYTE pbRGB2Entries );


#endif

#ifdef __cplusplus
}
#endif

