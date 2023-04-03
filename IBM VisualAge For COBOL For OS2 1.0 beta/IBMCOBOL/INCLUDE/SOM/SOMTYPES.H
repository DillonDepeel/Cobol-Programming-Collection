/*
 *   COMPONENT_NAME: somk
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
/* @(#) somk/somtypes.h 2.6 9/1/94 18:11:27 [10/20/94 10:52:03] */

/*
 */

/*
 *    SOMTYPES.H
 *    SOM types for C
 *    Multiple Inheritance Version
 */
#ifndef somtypes_h
#define somtypes_h

#include <stdarg.h>

/* SOM Base Types */
#include <sombtype.h>

/*  -- Object Instance Structure */
struct somMethodTabStruct;
typedef struct SOMAny_struct {
    struct somMethodTabStruct  *mtab;
    integer4 body[1];
} SOMAny;


/* SOM Primitive Classes */
/* primitive classes */
#define SOMObject SOMAny
#define SOMClass SOMAny
#define SOMClassMgr SOMAny


/* SOM Implementation Types */
#include <somitype.h>


#endif  /* somtypes_h */
