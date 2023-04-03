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
/* @(#) 2.7 src/somk/som.h, somk, som2.1 9/1/94 18:11:45 [10/20/94 10:51:24] */

/*
 */

/*
 *    SOM.H
 *    First level include file for System Object Model
 *    Multiple Inheritance Version
 */

#ifndef som_h
#define som_h

#include <somnames.h>
#include <somltype.h>
#include <somtypes.h>
#include <somcdev.h>
#include <somcorba.h>
#include <somapi.h>

/*
 * C usage bindings for the primitive classes are responsible
 * for indicating the coding style they support (i.e., original vs.
 * CORBA style) by #defining SOM_STRICT_IDL if the CORBA style of
 * usage bindings is desired. Thus, when this file (som.h) is 
 * included by C usage bindings, the following #ifdef provides
 * definitions for the primitive classes appropriate to the
 * usage bindings.
 */

#ifdef SOM_STRICT_IDL

   #undef SOMObject
   #undef SOMClass
   #undef SOMClassMgr

   typedef SOMAny *SOMAnyStrict;

   #define SOMObject SOMAnyStrict
   #define SOMClass SOMAnyStrict
   #define SOMClassMgr SOMAnyStrict

#endif

#ifndef SOM_NO_OBJECTS
   #define SOMObject_VA_EXTERN
   #include <somobj.h>
   #include <somcls.h>
   #include <somcm.h>
#endif /* !SOM_NO_OBJECTS */

#endif /* som_h */
