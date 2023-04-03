/*************************************************************************
*                                                                        *
*  LPEXCPAT.H: Header file for upgrading LPEX commands to current API.   *
*              This file is included in LPEXAPI.H.                       *
*                                                                        *
**************************************************************************
*                                                                        *
*             (C) Copyright IBM Corporation 1993-1995                    *
*                                                                        *
*************************************************************************/

#ifndef H_LPEXCPAT
#define H_LPEXCPAT

#include <os2def.h>

#define lxmain(a)                   LpexEntry(a)
#define lxexit(a)                   LpexExit(a)

#define lxcmd(a)                    LpexCommand(a)
#define lxcall(a,b)                 LpexCall(a,b)
#define lxmsg(a)                    LpexCall("msg",a)
#define lxalloc(a)                  LpexAlloc(a)
#define lxfree(a)                   LpexFree(a)
#define lxquery(a,b)                LpexQuery(a,b)
#define lxquery_user(a,b)           LpexQueryUserLink(a,b)
#define lxlink_user(a,b)            LpexLinkUser(a,b)
#define lxfind_user(a,b)            LpexFindUserLink(a,b)
#define lxqgmlfonts(a)              LpexQueryGMLFonts(a)
#define lxgmlfontadd(a)             LpexAddGMLFonts(a)
#define lxnext()                    LpexNext("")
#define lxprev()                    LpexPrev("")
#define lxqrawtx(a)                 LpexQueryRawText(a)
#define lxqtext(a)                  LpexQueryText(a)
#define lxstext(a)                  LpexSetText(a)
#define lxqfont(a)                  LpexQueryFonts(a)
#define lxsfont(a)                  LpexSetFonts(a)
#define lxqmode(a)                  LpexQueryCharModes(a)
#define lxqclass(a)                 LpexQueryClass(a)
#define lxsclass(a)                 LpexSetClass(a)
#define lxqelestate(a)              LpexQueryElementState(a)
#define lxinsert(a)                 LpexInsert(a)

/* Note:  The following is provided *for compatibility */
/* only*.  It may not be available in future releases! */
#define lxhab(a)                    LpexHab(a)

#endif /* H_LPEXCPAT */
