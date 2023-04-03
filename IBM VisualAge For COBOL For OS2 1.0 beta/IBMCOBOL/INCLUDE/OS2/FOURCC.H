/***************************************************************************\
*
* Module Name: FOURCC.H
*
* OS/2 2.1 Multimedia Extensions Color space definitions
*
* Copyright (c) International Business Machines Corporation 1993, 1994
*                         All Rights Reserved
*
*
\****************************************************************************/

#ifndef _FOURCC_H_
#define _FOURCC_H_

/* define the new color spaces - these actually map to FOURCC values */
#define FOURCC_SCRN  0                                /* Use screen format.                  */
#define FOURCC_GREY  mmioFOURCC( 'G', 'R', 'E', 'Y' ) /* Y8 (greyscale)                      */
#define FOURCC_LUT8  mmioFOURCC( 'L', 'U', 'T', '8' ) /* 8-bit palettized                    */
#define FOURCC_R565  mmioFOURCC( 'R', '5', '6', '5' ) /* RGB 565                             */
#define FOURCC_R555  mmioFOURCC( 'R', '5', '5', '5' ) /* RGB 555                             */
#define FOURCC_R664  mmioFOURCC( 'R', '6', '6', '4' ) /* RGB 664                             */
#define FOURCC_RGB3  mmioFOURCC( 'R', 'G', 'B', '3' ) /* RGB 24                              */
#define FOURCC_BGR3  mmioFOURCC( 'B', 'G', 'R', '3' ) /* BGR 24                              */
#define FOURCC_RGB4  mmioFOURCC( 'R', 'G', 'B', '4' ) /* RGB 32  Includes extra padding byte */
#define FOURCC_BGR4  mmioFOURCC( 'B', 'G', 'R', '4' ) /* BGR 32  Includes extra padding byte */
#define FOURCC_Y888  mmioFOURCC( 'Y', '8', '8', '8' ) /* Y888                                */
#define FOURCC_Y411  mmioFOURCC( 'Y', '4', '1', '1' ) /* YUV 411 interleaved 4x1 subsampled  */
#define FOURCC_Y422  mmioFOURCC( 'Y', '4', '2', '2' ) /* YUV 422 (CCIR601) interleaved       */
#define FOURCC_YUV9  mmioFOURCC( 'Y', 'U', 'V', '9' ) /* Indeo YUV multiplane                */
#define FOURCC_Y2X2  mmioFOURCC( 'Y', '2', 'X', '2' ) /* YUV 2x2 subsampled multiplane       */
#define FOURCC_Y4X4  mmioFOURCC( 'Y', '4', 'X', '4' ) /* YUV 4x4 subsampled multiplane       */
#define FOURCC_Y644  mmioFOURCC( 'Y', '6', '4', '4' ) /* YUV 4x4 subsampled 2-plane.         */
#define FOURCC_VGA   mmioFOURCC( 'V', 'G', 'A', ' ' ) /* VGA 16 Color mode multibit plane    */

#endif
