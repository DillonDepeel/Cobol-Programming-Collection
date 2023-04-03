/*****************************************************************************/
/*                                                                           */
/* LXPRPAS.C  -  A simple LPEX Parser for Pascal files.                      */
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/* This is an example of a simple programming language parser for the LPEX   */
/* editor.  It will colour the various tokens of a Pascal program in diffe-  */
/* rent colours, as an aid to the programmer.  This code is provided as an   */
/* example of how a parser is written - it should be easy to adapt it to     */
/* handle any of the common block structured programming languages.          */
/*                                                                           */
/* This parser is designed to handle standard Pascal.  Most real Pascal      */
/* implementations have many changes and extensions.                         */
/*                                                                           */
/* To LPEX a parser appears as an editor command.  Indeed, it can be invoked */
/* like any other command by typing its name on the command line.  However,  */
/* it would normally be invoked automatically by the live parsing mechanism. */
/* By convention, commands that are parsers begin with the two letters "PR", */
/* so this Pascal parser is the command "PRPAS".                             */
/*                                                                           */
/* Command Syntax:                                                           */
/*                                                                           */
/*     PRPAS ALL  -  this will cause the parser to process the whole docu-   */
/*                   ment.  It would normally only be used once, when the    */
/*                   document is first loaded, and would normally be invoked */
/*                   from the .LXL load macro.                               */
/*                                                                           */
/*     PRPAS      -  this will cause the parser to process just the current  */
/*                   element.  The live parsing mechanism works by calling   */
/*                   the parser command for each element that is changed.    */
/*                   It ensures that when the parser is called the changed   */
/*                   element is the current element.                         */
/*                                                                           */
/* Return codes:                                                             */
/*                                                                           */
/*    -2  -  Parser table not found                                          */
/*    -3  -  Error reading parser table                                      */
/*    -8  -  Unable to allocate memory                                       */
/*   -11  -  Internal scanning error                                         */
/*   -12  -  Invalid parameters                                              */
/*   The parser may also return any code that the LPEX API calls return.     */
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/* Operation:                                                                */
/*                                                                           */
/* The parser scans the program lines and recognises the basic tokens of the */
/* language.  It sets different symbolic fonts for the various types of      */
/* token.  The user may then assign these symbolic fonts to colours, in      */
/* order to highlight different items in the program.                        */
/*                                                                           */
/* The following symbolic fonts are used:                                    */
/*                                                                           */
/*     C  -  Comments                                                        */
/*     K  -  Language keywords                                               */
/*     B  -  Built-in functions and procedures                               */
/*     S  -  Language symbols                                                */
/*     A  -  Names (of variables and functions)                              */
/*     N  -  Numbers                                                         */
/*     L  -  Literals                                                        */
/*     E  -  Errors and unexpected characters                                */
/*     _  -  (underscore) Layout space.                                      */
/*                                                                           */
/* In addition, each element has one or more classes associated with it.     */
/* The following classes are used:                                           */
/*                                                                           */
/*     CODE        - Element contains program code                           */
/*     SPACE       - Element is just layout space                            */
/*     FUNCTION    - Element contains a procedure or function declaration    */
/*     COMMENT     - Element contains a comment                              */
/*     OPENCOMMENT - Element contains an unterminated comment                */
/*     ERROR       - Element contains an error.                              */
/*                                                                           */
/* The class OPENCOMMENT is mainly for use by the parser itself.             */
/* The live parsing mechanism presents the parser with single elements to    */
/* examine.  For a simple parser such as this, it is often enough just to    */
/* process that one element in isolation, but there is an exception.  As     */
/* comments can extend across several lines, a change to one line (say, the  */
/* removal of a close comment symbol) might require several lines before and */
/* after the current line to be re-examined as well.  The OPENCOMMENT class  */
/* is used by the parser to determine the parsing limits in such cases.      */
/*                                                                           */
/* To make the Parser adaptable to the various different flavours of Pascal, */
/* its list of keywords and language symbols is held in an external data     */
/* file called LPEXPAS.DAT.  This table is read into storage the first time  */
/* the parser is invoked.  It is only necessary to read it once per LPEX     */
/* session.                                                                  */
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/*               (C) Copyright IBM Corporation 1989-1995                     */
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/* Possible Improvements:                                                    */
/*                                                                           */
/* There are many ways this simple example parser could be improved.  Here   */
/* are a few.                                                                */
/*                                                                           */
/* 1. The parser tables are held in a file called 'LPEXPAS.DAT'.  This name  */
/*    is hard-wired in the code, but should really be made a command param-  */
/*    eter called, say, 'TABLE'.  This could even be extended to allow dif-  */
/*    ferent table names for different invocations of the parser.            */
/*                                                                           */
/*    A 'REFRESH' option would be useful too, so that if changes are made to */
/*    the tables the in-store copies could be updated without having to      */
/*    restart LPEX.                                                          */
/*                                                                           */
/* 2. Some of the 'readXxxxx()' functions are rather too simple and should   */
/*    be improved.  readNumber(), for example, only accepts a string of      */
/*    decimal digits, and really ought to accept real numbers (with decimal  */
/*    points and/or exponents) too.                                          */
/*                                                                           */
/* 3. No distinction is made between the two types of comment markers ('{ }' */
/*    and '(* *)', so an opening '{' can be matched to a closing '*)'.  This */
/*    could be improved.                                                     */
/*                                                                           */
/* Note, however, that an 'emphasis parser' of this sort is only meant as an */
/* aid to the programmer.  It may not be worthwhile trying to make it 'too'  */
/* perfect if, as a result, its performance starts to suffer.                */
/*                                                                           */
/*****************************************************************************/
/*                                                                           */
/* The parser is now DBCS (double byte character sets) enabled.  That is, it */
/* will scan the elements correctly, never confusing the second byte of a    */
/* DBCS character with an SBCS character.  DBCS characters (byte pairs) are  */
/* permitted in quoted strings and within comments, but not within function  */
/* or variable names.  In those cases, they are higlighted in FONT_ERROR to  */
/* indicate that the compiler will also have problems.  For further details  */
/* on the implications of DBCS, see the OS/2 Development Toolkit documenta-  */
/* tion and related literature.                                              */
/*                                                                           */
/*****************************************************************************/


/* include standard C functions */

#include <stdio.h>
#include <string.h>
#include <memory.h>
#include <ctype.h>
#include <stdlib.h>

/* the base OS/2 calls */

#define  INCL_BASE
#include <os2.h>

/* LPEX API access functions */

#include "lpexapi.h"


/* some #defines make the code easier to read */

#define FALSE 0

#define MAXTOKENLENGTH  32
#define MAXTABLESIZE   100

#define FONT_NAME     'A'
#define FONT_BUILTIN  'B'
#define FONT_KEYWORD  'K'
#define FONT_COMMENT  'C'
#define FONT_ERROR    'E'
#define FONT_LITERAL  'L'
#define FONT_SYMBOL   'S'
#define FONT_NUMBER   'N'
#define FONT_LAYOUT   '_'

#define CLASSES "CLASSES space opencomment comment code function error"

#define CLASS_SPACE        0x80000000       /* the order of these bits must   */
#define CLASS_OPENCOMMENT  0x40000000       /*  match that of the string      */
#define CLASS_COMMENT      0x20000000       /*  above, with the MSB the first */
#define CLASS_CODE         0x10000000       /*  class in the string           */
#define CLASS_FUNCTION     0x08000000
#define CLASS_ERROR        0x04000000
#define CLASSMASK          0x03FFFFFF       /* only reset first 6 classes     */


/* declare all our functions */

int loadAllTables(char *);
int loadTable(char **, FILE *);
char *getLine(FILE *);

int parseCurrent(void);
int parseElement(void);
int readComment(int);
int readLayout(int);
int readLiteral(int);
int readNumber(int);
int readWord(int);

int testSymbol(int);

int isKeyWord(char *);
int isBuiltIn(char *);
int isOpenComment(void);


/*---------------------------------------------------------------------------*/
/* Global data.                                                              */
/*---------------------------------------------------------------------------*/

/* we define three large buffers, suitable for passing data to & from LPEX */

uchr *textbuf;
uchr *fontbuf;
uchr *modebuf;

/* a flag to indicate we have bought memory for the three buffers above */

int buffers = FALSE;

/* a flag to indicate if the keyword and symbol tables have been loaded yet */

int tablesLoaded = FALSE;

/* pointers to the data tables, and their associated sizes */

char *keywords[MAXTABLESIZE];
char *builtin[MAXTABLESIZE];
char *symbols[MAXTABLESIZE];

int keysize;
int builtsize;
int symsize;

/* there is some shared data among the parsing routines */

int InsideComment = FALSE;            /* flag to indicate we are in a comment */
int Error         = FALSE;            /* flag to indicata an error detected   */
int fPendOff      = FALSE;            /* TRUE if ALL and PARSER set           */

char LastWord[MAXTOKENLENGTH+1];      /* last word processed                  */
                                      /*  N.B. We assume that the data tables */
                                      /*  will only contain SBCS.             */


/***************************************************************************/
/* lxxquer() return pointer to query value lxquery() returns a string      */
/* like "ITEM setting" so we need to point past the "ITEM " and at the     */
/* setting itself.                                                         */
/***************************************************************************/

char *lxxquer(char *item, char *buff)
{
   uchr *p;

   lxquery(item, buff);                        /* returns "ITEM value" string */
   if (*(p = buff + strlen(item)) == '\0')
      return p;                    /* if not set, LPEX just returns "ITEM'\0' */
   else
      return (p + 1);                          /* step over item name & blank */
}


/***************************************************************************/
/* lxxqnum() returns as an int the result of a query                       */
/***************************************************************************/

int lxxqnum(char *item)
{
   char work[100];
   return (atoi(lxxquer(item, work)));
}


/***************************************************************************/
/* setup()  allocate space for the arrays.                                 */
/***************************************************************************/

int setup(void)
{
   if (!buffers) {
      /* now buy some buffers... */
      if ((textbuf = lxalloc(MAXLEN +1)) == NULL)
         return -8;
      if ((fontbuf = lxalloc(MAXLEN +1)) == NULL) {
         lxfree(textbuf);
         return -8;
         }
      if ((modebuf = lxalloc(MAXLEN +1)) == NULL) {
         lxfree(textbuf);
         lxfree(fontbuf);
         return -8;
         }
      buffers = TRUE;
      }
   return 0;
}


/***************************************************************************/
/* lxexit() This is called when the user asks for an UNLINK, or when LPEX  */
/* is about to come down.                                                  */
/***************************************************************************/

int lxexit(uchr *parm)
{
   if (modebuf != NULL)
      lxfree(modebuf);
   if (textbuf != NULL)
      lxfree(textbuf);
   if (fontbuf != NULL)
      lxfree(fontbuf);
   return 0;
}


/*---------------------------------------------------------------------------*/
/* This is the entry point to the program.  We examine the parameters        */
/* passed to us from LPEX and act accordingly.                               */
/*---------------------------------------------------------------------------*/

int lxmain(uchr *parameters)
{
   char *p;
   int rc;

   if ((rc = setup()) != 0)                  /* allocate space for the arrays */
      return rc;
   strcpy(textbuf, parameters);                          /* make a local copy */
   strupr(textbuf);                    /* force parameters to upper case, and */
   for (p = textbuf; *p == ' '; p++);  /*  skip over any leading blanks       */

   if (*p != '\0' &&                 /* only valid parameter, if any, is ALL: */
       strncmp(p, "ALL", 3) != 0) {
      sprintf(fontbuf, "PRPAS - unexpected argument %.40s", p);
      lxcall("MSG", fontbuf);        /*  issue error message otherwise...     */
      return -12;
      }
   else {
      uchr buff[255];
      lxquery("PARSER", buff);
      fPendOff = (strlen(buff) > 7);
      }

   /* first we check if the tables of key words and language symbols have    */
   /* been loaded from disk yet.  If not, we try to load them.               */

   if (!tablesLoaded) {
      if ((rc = loadAllTables("LPEXPAS.DAT")) != 0)
         return rc;
      tablesLoaded = TRUE;
      }

   /* parsing may well alter the current position, so we set a mark here to  */
   /* preserve it, then restore it again when we've finished.  By convention */
   /* mark names containing periods are used only inside commands and macros */
   /* so we can be sure that no mark called "PARSER.SAVE" already exists.    */

   lxcmd("MARK SET PARSER.SAVE");

   /* now we call the parsing routine itself.  Either for the current ele-   */
   /* ment or, if the "ALL" parameter is specified, for each element in the  */
   /* document.  The "ALL" option is designed to be called once from a .LXL  */
   /* load macro, so also sets the classes used by the parser.               */

   if (*p == '\0')                         /* just parse the current element */
      rc = parseCurrent();
   else {                                        /* or do the whole document: */
      lxcall("SET", CLASSES);
      lxcmd("TOP");
      InsideComment = FALSE;
      do {
         rc = parseElement();                 /* 0 or positive = all was well */
         if (fPendOff)
            lxcmd("SET PENDING OFF");
         } while ((rc >= 0) && lxnext() == 0);
      }

   /* restore the original position and return to LPEX */

   lxcmd("MARK FIND PARSER.SAVE");
   lxcmd("MARK CLEAR PARSER.SAVE");

   return rc;
}


/*---------------------------------------------------------------------------*/
/* These are the routines to load the data tables from disk.                 */
/*---------------------------------------------------------------------------*/

/* loadAllTables() - Load all three data tables. */

int loadAllTables(char *filename)
{
   char *p;
   int rc;
   FILE *datafile;

   /* we should look for the file containing the tables either in the  */
   /* current directory or in the path held in LPEX's "LPATH" setting. */
   /* OS/2 provides a function that makes this easy.                   */

   p = lxxquer("LPATH", textbuf);                            /* get the LPATH */

   if (DosSearchPath(1, p, filename, fontbuf, MAXLEN) != 0) {
      sprintf(textbuf, "PRPAS - parser tables \"%.40s\" not found", filename);
      lxcall("MSG", textbuf);
      return -2;
      }

   /* open the parser tables as a simple text file */

   if ((datafile = fopen(fontbuf, "r")) == NULL) {
      sprintf(textbuf, "PRPAS - cannot open parser tables \"%.40s\"", fontbuf);
      lxcall("MSG", textbuf);
      return -2;
      }

   /* there are three tables to be loaded, first the keywords, then */
   /* the built-in functions, and lastly the language symbols.      */

   rc = 0;

   if ((keysize   = loadTable(keywords, datafile)) < 0 ||
       (builtsize = loadTable(builtin,  datafile)) < 0 ||
       (symsize   = loadTable(symbols,  datafile)) < 0)
      rc = -3;

   /* close the parser tables */

   fclose(datafile);

   return rc;
}


/*****************************************************************************/
/* loadTable() - Load the next parser table from disk.                       */
/*               Returns the number of items in the table, or -1 if there is */
/*               an error.                                                   */
/*****************************************************************************/

int loadTable(char **table, FILE *file)
{
   char *p, *q;
   int len, size;

   size = 0;                                  /* no keywords in the table yet */

   do {                                       /* get the first non-comment    */
      p = getLine(file);                      /*  line in the parser tables   */
      } while (p != NULL && *p == '\n');

   if (feof(file) || ferror(file)) {               /* was there a read error? */
      sprintf(textbuf, "PRPAS - error reading parser tables");
      lxcall("MSG", textbuf);
      return -1;
      }

   while (p != NULL && *p != '\n') {
      if (size >= MAXTABLESIZE) {
         sprintf(textbuf, "PRPAS - parser table too large");
         lxcall("MSG", textbuf);
         return -1;
         }

      q = p;                                      /* find the end of the item */
      while (*q != ' ' && *q != '\n' && *q != '\0') ++q;

      len = q - p;
      table[size] = lxalloc(len+1);                 /* get space for the word */
      strncpy(table[size], p, len);                       /* copy the word in */
      *(table[size]+len) = '\0';
      strupr(table[size]);                    /* store all words in uppercase */
      ++size;

      p = getLine(file);
      }

   if (ferror(file)) {                             /* was there a read error? */
      sprintf(textbuf, "PRPAS - error reading parser tables");
      lxcall("MSG", textbuf);
      return -1;
      }

   return size;
}


/*****************************************************************************/
/* getLine() - read a line from the parser tables and determine if it was a  */
/*             comment.                                                      */
/*             Returns pointer to the first non-space and non-comment        */
/*             character on the line.                                        */
/*****************************************************************************/

char *getLine(FILE *file)
{
   char *p;

   if ((p = fgets(textbuf, MAXLEN, file)) != NULL) {
      if (*p == '*')                                       /* ignore comments */
         *p = '\n';
      while (*p == ' ') ++p;                           /* skip leading spaces */
      }

   return p;
}


/*---------------------------------------------------------------------------*/
/* These are the Parser Routines                                             */
/*---------------------------------------------------------------------------*/

/*****************************************************************************/
/* parseCurrent() - this is called to parse the current element.  It also    */
/*                  handles the case of a comment continuing over several    */
/*                  lines, by parsing elements before and after the current  */
/*                  element if need be.                                      */
/*****************************************************************************/

int parseCurrent(void)
{
   int wascomment;    /* BOOLEAN */
   int rc = 0;

   /* if the previous element contained an open comment, we must set the */
   /* flag to indicate we are in a comment.                              */

   if (lxprev() == 0) {                      /* is there a previous element?  */
      InsideComment = isOpenComment();
      lxnext();                              /*  move back to the start point */
      }

   /* parse the starting element, noting whether it is an */
   /* open comment before we parse it.                    */

   wascomment = isOpenComment();
   if ((rc =parseElement()) < 0 )
      return rc;

   /* now we carry on parsing if either the current element used to be an */
   /* open comment, or has now become one.  Any extra elements parsed in  */
   /* this way are dropped from the trigger list to ensure that (if they  */
   /* were on the list in the first place) they will not be parsed again  */
   /* unnecessarily.                                                      */

   while ((wascomment || InsideComment) && lxnext() == 0) {
      wascomment = isOpenComment();
      if ((rc = parseElement()) < 0)
         break;
      if (fPendOff)
         lxcmd("SET PENDING OFF");
      }

   return rc;
}


/*****************************************************************************/
/* parseElement() - this routine parses a single element.  It builds up a    */
/*                  font string for the element and a list of possible       */
/*                  classes then sets these items.                           */
/*****************************************************************************/

int parseElement(void)
{
   int position, length,maxpos,rc;
   uchr ch, font;
   unsigned long pasclass;

   /* get hold of the text for the current element, and position ourselves */
   /* at the start of the element's text.                                  */

   if ((rc = lxqtext(textbuf)) < 0)      /* the result in 'textbuf' will be:  */
       return rc;
   position = 0;                         /* "xxxxxxxxxxxxxxxxxx...."          */
   maxpos  = strlen(textbuf) - 1;        /*  ^ = index position 0             */

   /* get its class, and clear our own bits */

   lxqclass(&pasclass);
   pasclass &= CLASSMASK;                    /* reset all except user classes */
   pasclass |= CLASS_SPACE;                  /* no PASCAL classes defined yet */

   /* step though all the characters in the element, identifying */
   /* the tokens and building a font string.                     */

   while ((ch = textbuf[position]) != '\0' ) {
      if (InsideComment) {         /* (a) first check for a continued comment */
         length = readComment(position);
         font   = FONT_COMMENT;
         pasclass |= CLASS_COMMENT;
         }

      else if (isspace(ch)) {                             /* (b) layout space */
         length = readLayout(position);
         font   = FONT_LAYOUT;
         pasclass |= CLASS_SPACE;
         }

      else if (isdigit(ch)) {                                   /* (c) number */
         length = readNumber(position);
         font   = FONT_NUMBER;
         pasclass |= CLASS_CODE;
         }

      else if (ch == '\'') {           /* (d) literal string in single quotes */
         length = readLiteral(position);
         font   = FONT_LITERAL;
         pasclass |= CLASS_CODE;
         }

      else if (ch == '\"') {           /* (e) literal string in double quotes */
         length = readLiteral(position);        /* (strictly, this will only  */
         font   = FONT_ERROR;                   /*  cause a compiler warning, */
         pasclass |= CLASS_ERROR;               /*  but it's naughty!)        */
         }

      else if (ch == '{' || (ch == '(' && textbuf[position+1] == '*')) {
         length = readComment(position);                      /* (f) comments */
         font   = FONT_COMMENT;
         pasclass |= CLASS_COMMENT;
         }

      else if (ch == '!') {               /* (g) special case: comment to EOL */
         length = maxpos - position + 1;
         font   = FONT_COMMENT;
         pasclass |= CLASS_COMMENT;
         }

      else if (isalpha(ch)) {                                   /* (h) words: */
         length = readWord(position);
         pasclass |= CLASS_CODE;

         if (isKeyWord(LastWord)) {                              /* - keyword */
            font = FONT_KEYWORD;
            if (strcmp(LastWord, "FUNCTION") == 0       /* check if we have a */
                || strcmp(LastWord, "PROCEDURE") == 0   /*  function or proc- */
                || strcmp(LastWord, "PROGRAM") == 0)    /*  edure declaration */
               pasclass |= CLASS_FUNCTION;
            }
         else if (isBuiltIn(LastWord))                 /* - built-in function */
            font = FONT_BUILTIN;
         else                                                       /* - name */
            font = FONT_NAME;
         }

      else if ((length = testSymbol(position)) > 0)     /* (i) special symbol */
         font = FONT_SYMBOL;

      else {                                            /* (j) what else?!... */
         length = 1;                        /* the length is always 1 in SBCS */
         Error = TRUE;
         }

      /* was there an error of any kind? */

      if (Error) {
         font = FONT_ERROR;
         pasclass |= CLASS_ERROR;
         Error = FALSE;
         }

      /* ensure that our internal scans worked correctly */
      if (position + length > maxpos + 1) {
         lxcmd("MSG PASCAL Parser error: scan past end of string");
         lxcall("QUERY", "CONTENT");
         sprintf(modebuf, "MSG length = %d, position = %d, maxpos = %d",
                          length, position, maxpos);
         lxcall("MSG", modebuf);
         lxcall("QUERY", "ELEMENT");
         return -11;
         }

      /* now build up the font for the last thing found */
      memset(fontbuf+position, font, length);
      position += length;
      }

   /* when we reach the end of the element, we add a terminator to the font */
   /* string, and set the fonts                                             */

   fontbuf[position] = '\0';
   if ((rc = lxsfont(fontbuf)) < 0)
       return rc;                                 /* return on LPEX errors... */

   /* decide what classes to give the element */

   if (InsideComment)
      pasclass |= CLASS_OPENCOMMENT;

   if ((pasclass & CLASS_SPACE) && (pasclass != CLASS_SPACE))
      pasclass &= ~CLASS_SPACE;         /* remove the SPACE class unless it's */
                                        /*  the only class for the element    */
   return lxsclass(pasclass);
}


/*****************************************************************************/
/* readComment() - This routine will read characters up to an end comment    */
/*                 marker or the end of element.                             */
/*                 It returns the length of the comment.                     */
/*                                                                           */
/*                 It also sets the 'InsideComment' flag if the end of the   */
/*                 element is encountered before the comment ends.           */
/*                                                                           */
/* Pascal: Comments are ended by '}' or '*)'.  This routine accepts either.  */
/* Assumes that the current position cannot be DBCS2.                        */
/*****************************************************************************/

int readComment(int pos)
{
   int start;
   char ch;

   start = pos;

   if (!InsideComment)                       /* if this is not a continuation */
      if (textbuf[pos] == '{')               /*  comment, skip over the start */
         ++pos;                              /*  comment symbol               */

   for (;;) {
      if ((ch = textbuf[pos]) == '\0') {
         InsideComment = TRUE;
         return (pos - start);
         }

      if (ch == '}')                          /* if we reached a matching '}' */
         break;                               /*  that's the end of comment   */

      ++pos;
      if (ch == '*' && textbuf[pos] == ')')
         break;
      }

   InsideComment = FALSE;
   return (pos + 1 - start);
}


/*****************************************************************************/
/* readLayout() - This routine will read characters comprising layout space  */
/*                It returns the length of the layout characters             */
/*****************************************************************************/

int readLayout(int pos)
{
   int start;
   char ch;

   start = pos;

   while ((ch = textbuf[pos]) != '\0') {
       if (isspace(ch))
          ++pos;
       else
          break;
   }
   return (pos - start);
}


/*****************************************************************************/
/* readLiteral() - This routine read a literal (any characters enclosed in   */
/*                 single quotes).                                           */
/*                 It returns the length of the literal.                     */
/*                                                                           */
/*                 We assume that literals must all be contained on one line */
/*                 if no closing quote is found it's treated as an error.    */
/*****************************************************************************/

int readLiteral(int pos)
{
   int start;
   char ch, quote;

   start = pos;
   quote = textbuf[pos++];                        /* note the quote character */
                                                  /*  and step over it        */

   while ((ch = textbuf[pos]) != '\0') {
      if (ch != quote)
         ++pos;                                   /* find the closing quote   */
      else
         return (pos +1 - start);                 /* we matched the quote     */
      }
                                                  /* we have reached the end  */
   Error = TRUE;                                  /*  of the element before   */
   return (pos - start);                          /*  the close quote         */
}


/*****************************************************************************/
/* readNumber() - This reads the next number from an element.                */
/*                It returns the length of the number.                       */
/*                                                                           */
/* Pascal: Numbers consist of Decimal digits.                                */
/* Note: stops at the first byte of DBCS character including ROMA-JI numbers */
/*****************************************************************************/

int readNumber(int pos)
{
   int start;
   char ch;

   start = pos;

   while ((ch = textbuf[pos]) != '\0') {
      if (isdigit(ch))  {
         ++pos;
         }
      else {
         break;
         }
      }
   return (pos - start);
}


/*****************************************************************************/
/* readWord() - This reads the next word from an element.                    */
/*              It returns the length of the word and places a copy of the   */
/*              word in 'LastWord'.                                          */
/*                                                                           */
/* Pascal:  Words consist of letters, digits, underscores, starting with a   */
/*          letter.  Therefore, stop the scan on space / punctuation / DBCS. */
/*****************************************************************************/

int readWord(int pos)
{
   int start, i;
   char ch;

   start = pos;
   i = 0;

   while ((ch = textbuf[pos]) != '\0')  {
      if (!(isalnum(ch) || ch == '_') || i >= MAXTOKENLENGTH) {
         break;                            /* not alphanumeric / underscore / */
         }                                 /*  maximum word length reached... */
      else {
         LastWord[i++] = ch;                   /* valid character, so copy it */
         }
      ++pos;
   }

   LastWord[i] = '\0';
   strupr(LastWord);                         /* convert the word to uppercase */

   return (pos - start);
}


/*****************************************************************************/
/* testSymbol() - See if the next token in the element is one of the special */
/*                symbols listed in the table.  Choose the longest possible  */
/*                special symbol.                                            */
/*                It returns the length of the symbol, or zero if not a      */
/*                symbol.                                                    */
/*                                                                           */
/* The table of language symbols is pointed to by the 'symbols' global       */
/* and is set up from the parser tables the first time the parser is used.   */
/* NOTE: must think more on the DBCS implications of this...                 */
/*****************************************************************************/

int testSymbol(int pos)
{
   char ch;
   int upper, lower, ttry, c;
   int length;

   /* first do a binary look-up of the first character                   */
   /* Note: this doesn't necessarily put us on the first symbol to start */
   /*       with this character.                                         */

   ch = textbuf[pos];
   lower = 0;
   upper = ttry = symsize;
   c = -1;

   do {
      if (c < 0)
         upper = ttry-1;
      else
         lower = ttry+1;

      ttry = (upper+lower)/2;

      if ((c = ch - *symbols[ttry]) == 0)
         break;
      } while (lower < upper);

   /* if there was no match we have no symbol, otherwise look for the */
   /* longest possible symbol at this point                           */

   if (c != 0)
      return 0;

   /* move backwards to find the first entry in the table starting with */
   /* the required character                                            */

   while (ttry > 0 && *symbols[ttry-1] == ch)
      --ttry;

   /* now find the longest symbol which start with the character */

   for (;;) {
      length = strlen(symbols[ttry]);
      if (strncmp(textbuf+pos, symbols[ttry], length) == 0)
         return length;
      ++ttry;
      }
   return 0;       /* no match */
}


/*****************************************************************************/
/* isKeyWord() - Test a word to see if it is a keyword.                      */
/*               Return TRUE or FALSE as appropriate.                        */
/*                                                                           */
/* The table of keywords is pointed to by the global 'keywords', which is    */
/* set up from the parser tables when the parser is first used.              */
/* Note this is passed the string 'LastWord' which will NOT contain DBCS.    */
/*****************************************************************************/

int isKeyWord(char *word)
{
   int upper, lower, ttry, c;

   /* do a binary look-up in the keyword table */

   lower = 0;  upper = ttry = keysize;  c = -1;

   do {
      if (c < 0)
         upper = ttry - 1;
      else
         lower = ttry + 1;

      ttry = (upper + lower) / 2;

      if ((c = strcmp(word, keywords[ttry])) == 0)
         return TRUE;

      } while (lower < upper);

   return FALSE;
}


/*****************************************************************************/
/* isBuiltIn() - Test a word for a built-in function or procedure.           */
/*               Return TRUE or FALSE as appropriate.                        */
/*                                                                           */
/* The table of built-in functions is pointed to by the global 'builtin',    */
/* which is set up from the parser tables when the parser is first used.     */
/*****************************************************************************/

int isBuiltIn(char *word)
{
   int upper, lower, ttry, c;

   /* do a binary look-up in the builtin table */

   lower = 0;
   upper = ttry = builtsize;
   c = -1;

   do {
      if (c < 0)
         upper = ttry - 1;
      else
         lower = ttry + 1;

      ttry = (upper + lower) / 2;

      if ((c = strcmp(word, builtin[ttry])) == 0)
         return TRUE;

      } while (lower < upper);

   return FALSE;
}


/*****************************************************************************/
/* isOpenComment() - Determine whether the current element has the class     */
/*                   OPENCOMMENT set.                                        */
/*****************************************************************************/

int isOpenComment(void)
{
   unsigned long c;

   lxqclass(&c);
   return ((c & CLASS_OPENCOMMENT) != 0L);
}

