/*static char *SCCSID = "@(#)vmdpmd.h   6.2 92/01/28";*/
/***    VMDPMD.H
 *
 *
 *      Mouse VDD/PDD shared structures/constants
 *      Copyright (c) 1993 IBM Corporation
 *
 *      For include file hierarchy, see VMD/PMD include file(s)
 *
 *
 */

#pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( none )
   #endif
#pragma info( restore )

#ifndef __VMDPMD__
#define __VMDPMD__


/***EK+ VMD/PMD constants
 */

#define PMD_NAME    "MOUSE$"            /* registered name of PMD */

#define PMDCMD_REGISTER         PDDCMD_REGISTER         /* register VDD    */
#define PMDCMD_DEREGISTER       (PMDCMD_REGISTER+1)     /* de-register VDD */
#define PMDCMD_QUERYTYPE        (PMDCMD_REGISTER+2)     /* query hwd info. */
#define PMDCMD_SETSCREENSIZE    (PMDCMD_REGISTER+3)     /* set screen size */
#define PMDCMD_GRABACCESS    (PMDCMD_REGISTER+4) /* Get Exclusive Access   */

#define VMDCMD_NOTIFYEVENT      0x01    /* notification of event availability */

/*end*/


/***EK+ Event packet flags (as defined by PMD)
 */

#define MEVSTAT_MOTION          0x0001  /* motion only                     */
#define MEVSTAT_MOTIONLEFT      0x0002  /* motion while LEFT button down   */
#define MEVSTAT_LEFTDOWN        0x0004  /* LEFT button down                */
#define MEVSTAT_MOTIONRIGHT     0x0008  /* motion while RIGHT button down  */
#define MEVSTAT_RIGHTDOWN       0x0010  /* RIGHT button down               */
#define MEVSTAT_MOTIONCENTER    0x0020  /* motion while CENTER button down */
#define MEVSTAT_CENTERDOWN      0x0040  /* CENTER button down              */
#define MEVSTAT_ALLMOTION       0x002B  /* mask for all motion events      */
#define MEVSTAT_ABSPIXEVENT     0x8000  /* absolute pixel coordinate event */
/*end*/


/***ET+ VMD-to-PMD communication structures
 *
 *  MHARDWARE is an output packet provided by VMD on the QUERYTYPE request,
 *  and is filled in by the PMD with appropriate information about the mouse
 *  hardware.  There is no input packet for this request.
 *
 *  MSSIZE is an input packet provided by VMD on the SETSCREENSIZE request
 *  and contains the screen size of a given video mode.  This may be used
 *  by the PMD to compute the coordinate range when reporting absolute pixel
 *  events.
 */

typedef struct mhw_s {                  /* mhw */
    USHORT  mhw_nb;                     /* size of structure, in bytes (6)   */
    UCHAR   mhw_nMickeys;               /* mickeys per centimeter            */
    UCHAR   mhw_nButtons;               /* # of buttons                      */
    UCHAR   mhw_irq;                    /* IRQ #                             */
    UCHAR   mhw_uType;                  /* 0=unk,1=bus,2=ser,3=inp,4=inboard */
} MHARDWARE;

typedef struct mss_s {                  /* mss */
    USHORT  mss_nb;                     /* size of structure, in bytes (6)   */
    USHORT  mss_uWidth;                 /* Column size                       */
    USHORT  mss_uHeight;                /* Row size                          */
} MSSIZE;

typedef struct mougrab{                 /* mougrab */
    USHORT  uscMouGrab;                 /* size of structure, in bytes (8)   */
    USHORT  usSG;                       /* Screen Group                      */
    BOOL    fMouGrab;                   /* Flag to grab/release mouse        */
} MOUGRAB;
/*end*/


/***ET+ PMD-to-VMD communication structures
 *
 *  MEVENT is an input packet provided by the PMD on the NOTIFYEVENT request,
 *  and indicates the VDM screen group to which the new event belongs.  There
 *  is no output packet for this request.
 */

/*Note that the mouse event is always in unscaled raw mickeys no matter
**     what the SETVDMPARMS is programmed.  SETVDMPARMS only serves
**     the purpose of telling PMSE the resolution of posting mouse events.
*/

typedef struct minfo_s {                /* minfo */
    USHORT  minfo_fs;                   /* button/motion states                    */
    SHORT   minfo_nx;                   /* x, in delta mickeys or abs pixels       */
    SHORT   minfo_ny;                   /* y, in delta mickeys or abs pixels       */
    SHORT   nxMickeys;                  /* x, in delta mickeys, when saving counts */
    SHORT   nyMickeys;                  /* y, in delta mickeys, when saving counts */

} MINFO;

typedef MINFO *PMINFO;                  /* pminfo */

typedef struct mev_s {                  /* mev */
    USHORT  mev_uSG;                    /* VDM screen group ID */
    MINFO   mev_mi;                     /* mouse event info.   */
} MEVENT;

typedef MEVENT *PMEV;                   /* pmev */

/*end*/

#endif /* __VMDPMD__ */

#pragma info( none )
   #ifndef __CHKHDR__
      #pragma info( restore )
   #endif
#pragma info( restore )
