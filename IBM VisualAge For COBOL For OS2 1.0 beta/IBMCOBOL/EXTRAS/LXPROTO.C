/********************************************************************
*                                                                   *
*  LXPROTO - A simple prototype statement inserter for LPEX.        *
*                                                                   *
*            (C) Copyright IBM Corporation 1989-1995                *
*                                                                   *
*********************************************************************
*                                                                   *
*  The PROTO command (which invokes LXPROTO.DLL) is best tied to    *
*  a key (such as Ctrl-X). When invoked, it checks the content of   *
*  the current line.  If there is a word under the cursor, or       *
*  immediately before it, PROTO checks whether this word, prefixed  *
*  with 'xxx' (where 'xxx' is the parameter supplied to PROTO) is a *
*  current global variable.  If so, the value of the variable is    *
*  treated as a set of elements to be inserted at the current       *
*  position.  If no parameter is supplied, '#' is used.             *
*                                                                   *
*  Thus, given the global variable definition:                      *
*                                                                   *
*     GLOBAL.#if /if (?) {//} else {//}                             *
*                                                                   *
*  and the actionkey definition:                                    *
*                                                                   *
*     ACTION.c-x proto                                              *
*                                                                   *
*  pressing Ctrl-x when the cursor is on a line under the word "if" *
*  will cause LPEX to replace the text with:                        *
*                                                                   *
*      if (?) {                                                     *
*                                                                   *
*      } else {                                                     *
*                                                                   *
*      }                                                            *
*                                                                   *
*  Note that:                                                       *
*                                                                   *
*  1. BLOCK SET WORD is used to locate word to use as a base for    *
*     the global variable.  This allows the use of accented chars   *
*     and numerics, but not of delimiters and specials.             *
*                                                                   *
*  2. The new text is aligned with the word on the original line.   *
*                                                                   *
*  3. The cursor is positioned at the first ? in the replacement    *
*     text (or at the start of the first line if none).             *
*                                                                   *
*  4. The line separator ("/" in the example above) is taken as     *
*     the first non-blank character in the variable definition.     *
*                                                                   *
*  5. Any text after the original word will be placed at the end    *
*     of the last line of the replacement text.                     *
*                                                                   *
*********************************************************************
*                                                                   *
* Operation:                                                        *
*  1. obtain content of current line                                *
*  2. identify the word under the cursor (if none, return)          *
*  3. prefix word with 'xxx' and check if this is a global variable *
*  4. if no, return                                                 *
*  5. delete current line                                           *
*  6. scan variable definition, inserting lines                     *
*  7. append remainder of original line to last line                *
*  8. set position at first "?" (or first line).                    *
*                                                                   *
********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "lpexapi.h"


#define  BLANK  ' '


/* A large work area for LPEX calls */
char result[MAXQUERY];                             /* result used for queries */
char quer[100] = "GLOBAL.";                  /* used to test if global exists */
char posi[100];                                       /* used to set position */
char newl[300];                                /* used to hold inserted lines */


char *lxxquer(char *item, char *buff)        /* return pointer to query value */
{
   char *p;

   lxquery(item, buff);                        /* returns "ITEM value" string */
   p = buff;
   while (*p != BLANK && *p != '\0') ++p;              /* step over item name */
   if (*p == BLANK) ++p;                               /* and following blank */
   return p;
}

int lxxqnum(char *item)            /* returns as an int the result of a query */
{
   char work[100];
   return atoi(lxxquer(item, work));
}


lxmain(uchr *arg)
{
   char *p, *q, *r, *s, buff[300];
   int  i;
   int  k;                                /* position of first "?" in pattern */
   char delim;                                   /* line delimiter in pattern */
   int  join1;                                    /* true if text before word */
   int  join2;                                     /* true if text after word */
   int  offset;                    /* start of word offset from start of line */
   int  cnt;                                         /* counts inserted lines */
   char autoparse[20];                    /* used to remember autoparse state */
   int  noword = 0;


   while (arg[0] == BLANK)                             /* skip leading blanks */
      arg++;
   p = quer + 7;
   if (*arg == '\0')                                    /* set default prefix */
      strcpy(p, "#");
   else {
      while (*arg != '\0' && *arg != BLANK)
         *p++ = *arg++;                            /* set prefix as requested */
      *p = '\0';
      }

   lxquery("AUTOPARSE", buff);
   strcpy(autoparse, buff);

   /* Get hold of the current element */
   p = lxxquer("CONTENT", result);
   i = lxxqnum("POSITION");
   if (i > strlen(p) + 1)
      noword = 1;
   else if (lxcmd("BLOCK SET WORD") != 0)            /* mark the current word */
      noword = 1;                                        /* can't find a word */
   else {
      lxcmd("BLOCK FIND");
      offset = lxxqnum("POSITION") - 1;
      q = p + offset;                            /* q points to start of word */
      lxcmd("BLOCK FIND END");
      i = lxxqnum("POSITION");
      r = p + i;                        /* r points to char after end of word */
      lxcmd("BLOCK CLEAR");

      if ((i = r - q) > 30)                            /* variable max length */
         return 0;
      s = quer + strlen(quer);

      memcpy(s, q, i);                               /* set up variable query */
      memset(s+i, '\0', 1);
      s = lxxquer(quer, buff);                               /* and get value */
      noword = (*s == '\0');                      /* there isn't one, so exit */
      }

   if (noword) {
      /* lxcmd("MSG PROTO - cursor is not positioned on a prototype word."); */
      lxcmd("ALARM");
      return 0;
      }

   lxcmd("SET AUTOPARSE OFF");
   lxcmd("MARK SET PROTO.001");                  /* remember start of changes */
   join1 = join2 = 0;
   if (q != p) {                                        /* if preceding text: */
      *q = '\0';
      lxcall("INSERT", p);                              /*    insert before   */
      join1 = 1;
      }
   if (*r != '\0') {                                    /* if following text: */
      lxcall("INSERT", r);                              /*    insert after    */
      join2 = 1;
      lxcmd("PREV");
      }

   cnt = 0;

   q = s; while (*q == BLANK) ++q;                      /* skip over blank(s) */
   delim = *q++;                               /* get and skip over delimiter */

   k = 0;
   p = newl;                               /* first line doesn't have padding */
   while (*q != '\0') {                            /* while pattern not ended */
      if (cnt == 1) {                            /* later lines do get padded */
         memset(newl, BLANK, offset);                    /* pad start of line */
         p = newl + offset;
         }
      r = p;
      while (*q != delim && *q != '\0') {         /* for each line in pattern */
         if (k == 0 && *q == '?') {                     /* if it is first "?" */
            k = r - p + 1;                              /*   remember it      */
            }
         *r++ = *q++;
         }
      *r = '\0';
      lxcall("INSERT", newl);                                  /* insert line */
      if (cnt == 0 && join1 == 1) {                 /* join to preceding text */
         lxcmd("PREV");
         lxcmd("SPLITJOIN JOIN");
         }
      if (k > 0) {                                      /* if a "?" was found */
         sprintf(posi, "SET POSITION %u", offset + k);
         lxcmd(posi);                                     /* put cursor there */
         lxcmd("MARK SET PROTO.002");                        /* note position */
         k = -1;                              /* indicate mark 2 has been set */
         }
      if (*q == delim) ++q;                                 /* skip delimiter */
      ++cnt;
      }
   if (join2 == 1) {                     /* join last line to succeeding text */
      lxcmd("SPLITJOIN JOIN");
      }

   /* go back to original line and delete it */
   lxcmd("MARK FIND PROTO.001");
   lxcmd("MARK CLEAR PROTO.001");
   lxcmd("DELETE");
   if (k < 0) {                                              /* "?" was found */
      lxcmd("MARK FIND PROTO.002");                    /* so put cursor there */
      lxcmd("MARK CLEAR PROTO.002");
      }
   lxcall("SET", autoparse);                         /* reset autoparse state */

   return 0;
}
