/*
 *   COMPONENT_NAME: somu
 *
 *   ORIGINS: 27
 *
 *
 *   10H9767, 10H9769  (C) COPYRIGHT International Business Machines Corp. 1992,1994
 *   All Rights Reserved
 *   Licensed Materials - Property of IBM
 *   US Government Users Restricted Rights - Use, duplication or
 *   disclosure restricted by GSA ADP Schedule Contract with IBM Corp.
 */
/* @(#) 2.5 src/somem/eventmsk.h, somu, som2.1 9/1/94 17:37:09 [10/20/94 10:50:24] */

/*
 */

#ifndef H_EVENTMASKDEF
#define H_EVENTMASKDEF
 
/* Event Types */
#define EMTimerEvent                    54 
#define EMSignalEvent                   55 
#define EMSinkEvent                     56 

#define EMWorkProcEvent                 57 

#define EMClientEvent                   58

#define EMMsgQEvent			59

/* Sink input/output condition mask */

#define EMInputReadMask			(1L<<0)
#define EMInputWriteMask		(1L<<1)
#define EMInputExceptMask		(1L<<2)

/* Process Event mask */

#define EMProcessTimerEvent             (1L<<0)
#define EMProcessSinkEvent              (1L<<1)
#define EMProcessWorkProcEvent          (1L<<2)
#define EMProcessClientEvent            (1L<<3)
#define EMProcessAllEvents              (1L<<6)

#endif   /* H_EVENTMASKDEF */
