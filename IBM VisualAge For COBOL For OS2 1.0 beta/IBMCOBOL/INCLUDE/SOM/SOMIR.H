/*
 *   COMPONENT_NAME: somi
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
/* @(#) 2.6 src/somi/ir/somir/somir.h, somi, som2.1 9/1/94 18:30:44 [10/20/94 10:51:15] */

/*
 */

/*
 *    SOMIR.H
 *    Typedefs, structs, & enums in support of the CORBA Interface Repository
 *    for use with SOM
 */

#ifndef SOMIR_h
#define SOMIR_h

#ifdef SOMIR_xh
  #include <somtc.xh>
#else
  #include <somtc.h>
#endif /* SOMIR_xh */

/*
 * The following structure is for a SOM-unique extension
 * to the CORBA Interface Repository
 */

typedef struct somModifier {
    string name;
    string value;
} somModifier;

/*
 * Miscellaneous "ir" helper functions
 */

#ifdef __IBMC__
  #pragma linkage(irGetFileNames, system)
  #pragma linkage(irGetLastFileName, system)
#endif /* __IBMC__ */

SOMEXTERN char * SOMLINK irGetFileNames (void);
SOMEXTERN char * SOMLINK irGetLastFileName (void);

#endif /* SOMIR_h */
