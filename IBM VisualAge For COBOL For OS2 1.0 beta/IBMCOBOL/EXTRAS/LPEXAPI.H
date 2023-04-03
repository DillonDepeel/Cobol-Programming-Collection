/*************************************************************
*                                                            *
*  LPEXAPI.H: Include file for external C/C++ LPEX commands. *
*                                                            *
**************************************************************
*                                                            *
*          (C) Copyright IBM Corporation 1993-1995           *
*                                                            *
*************************************************************/

#ifdef __cplusplus
   extern "C" {
#endif

#ifndef LPEXAPI_H
#define LPEXAPI_H
 
/*-----------------------------------------------------------------*/
#include "lpexcpat.h"
 
#define _cinit
#define _cterm
/*-----------------------------------------------------------------*/
 
#define MAXLEN   2500                            /* Maximum element text size */
#define MAXQUERY 2800                      /* Maximum length for query result */

#ifndef uchr
   #ifdef __cplusplus
   #define uchr char
   #else
   #define uchr unsigned char           /* LPEX uses unsigned char throughout */
   #endif
#endif

/* LP_COMMAND - execute an LPEX command.  mp2 is MPFROM2SHORT(a, b) */
/* where "a" is either LP_COMMAND_INTERNAL or LP_COMMAND_EXTERNAL,  */
/*   and "b" is either LP_COMMAND_WINDOWED or LP_COMMAND_CURRENT.   */
#ifndef LP_COMMAND
#if defined( WM_USER )
#define LP_COMMAND        (WM_USER + 4)   
#endif
#define LP_COMMAND_EXTERNAL ((short)0) 
#define LP_COMMAND_INTERNAL ((short)1) 
#define LP_COMMAND_WINDOWED ((short)0) 
#define LP_COMMAND_CURRENT  ((short)1)
#endif
 
int     LpexEntry(uchr *);                     /* Command entry point */
int     LpexExit(uchr *);                      /* Command unlink exit point */

int     LpexCommand(const uchr *);             /* run an LPEX command */
int     LpexCall(const uchr *, const uchr *);  /* call an LPEX function */
uchr *  LpexQuery(const uchr *, uchr *);       /* query an LPEX item */

/* all storage allocation/freeing to be done by LPEX */
uchr *  LpexAlloc(int);                        /* allocate LPEX storage */
int     LpexFree(uchr *);                      /* free LPEX allocated storage */
 
#define LpexMsg(a)          LpexCall("msg",a)
 
/* the following functions support the user word */
int     LpexQueryUserLink(long int scope,void **pObjectPtr);
int     LpexSetUserLink(long int scope, void *object);
int     LpexFindUserLink(long int scope,void *object);
 
/* valid values of scope: */
#define LPEX_DOCUMENT 1
#define LPEX_VIEW     2
#define LPEX_ELEMENT  3
 
/* the following extend the fonts recognised by GML */
int     LpexQueryGMLFonts(uchr *);             /* query special GML fonts */
int     LpexAddGMLFonts(uchr *);               /* add to special GML fonts */
 
/* the following are fast path entry to some common functions */
int     LpexNext(const uchr *);                /* NEXT          */
int     LpexPrev(const uchr *);                /* PREV          */
int     LpexInsert(const uchr *);              /* INSERT text   */
int     LpexQueryRawText(uchr *);              /* query RAWTEXT */
int     LpexQueryText(uchr *);                 /* query CONTENT */
int     LpexSetText(const uchr *);             /* set   CONTENT */
int     LpexQueryFonts(uchr *);                /* query FONTS   */
int     LpexSetFonts(const uchr *);            /* set   FONTS   */
int     LpexQueryCharModes(uchr *);            /* query MODES   */
int     LpexQueryClass(unsigned long *);       /* query CLASS   */
int     LpexSetClass(unsigned long);           /* set   CLASS   */

int     LpexQueryElementState(unsigned long *);/* query ELESTATE:     */
#define LPEX_ELE_VISIBLE   (0x80000000)        /*   visible element   */
#define LPEX_ELE_SHOW      (0x40000000)        /*   show element      */
#define LPEX_ELE_PROTECTED (0x20000000)        /*   protected element */

/* Note:  The following is provided *for compatibility */
/* only*.  It may not be available in future releases! */
HAB     LpexHab(void);                         /* obtain the hab */
void*   LpexRock(void);                        /* obtain the rock pointer */

#endif /* LPEXAPI_H */ 

#ifdef __cplusplus
   }
#endif
