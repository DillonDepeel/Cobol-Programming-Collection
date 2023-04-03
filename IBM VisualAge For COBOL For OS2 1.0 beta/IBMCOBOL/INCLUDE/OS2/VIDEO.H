/*static char *SCCSID = "@(#)video.h	13.11 92/04/13";*/

/**************************START OF SPECIFICATIONS **************************/
/*                                                                          */
/* Module NAME:  VIDEO.H                                                    */
/*                                                                          */
/* OS/2 2.0 Multimedia Extensions Video structures and definitions          */
/*                                                                          */
/* Copyright (c) International Business Machines Corporation 1993           */
/*                         All Rights Reserved                              */
/************************** END OF SPECIFICATIONS ***************************/

/* XLATOFF */
#pragma pack(1)
/* XLATON */

typedef struct _VIDEO_FRAME_HDR {     /* vfh  */

   ULONG   FrameNumber;      /* relative frame number       */
   ULONG   FramesSkipped;    /* no. frames skipped between this and last frames */
   ULONG   StreamTime;       /* stream time in milliseconds */
   ULONG   FrameSize;        /* size in bytes               */
   ULONG   SHparm1;          /* used by stream handler      */
   ULONG   reserved3;        /* unused                      */
   ULONG   reserved2;        /* unused                      */
   ULONG   reserved1;        /* unused                      */

} VIDEO_FRAME_HDR;
typedef VIDEO_FRAME_HDR *PVIDEO_FRAME_HDR;

/* XLATOFF */
#pragma pack()
/* XLATON */
