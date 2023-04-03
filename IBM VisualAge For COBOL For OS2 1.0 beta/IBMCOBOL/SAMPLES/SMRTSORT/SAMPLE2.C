/*********************************************************/
/*                                                        */ 
/*  Licensed Material -- Property of IBM                  */    
/*  5765-349                                              */
/*  (c) Copyright IBM Corporation 1995                    */
/*  All rights reserved                                   */
/*                                                        */
/**********************************************************/

/**********************************************************/
/*                                                        */
/* SMARTsort -- sample2.c                                 */
/*                                                        */
/* This program sorts a single file of department         */
/* members where all of the input data to sort is         */
/* passed from an input exit.                             */
/*                                                        */
/**********************************************************/

#pragma linkage (SMARTsort,system)

#define INCL_DOSFILEMGR
#include <os2.h>
#include "smrtsort.h"

#define READ_LENGTH     10

int getdata(SMARTSORT_BUFFER *to_SMARTsort, SMARTSORT_BUFFER *from_SMARTsort);
int (*inputexit)(SMARTSORT_BUFFER *to_SMARTsort, SMARTSORT_BUFFER *from_SMARTsort) = getdata;

HFILE  hf;
ULONG  ulAction;
APIRET rc;
ULONG bytesread = READ_LENGTH;


void main(void)
{
 char *filename=".\\data\\dept.L92";
 char *sortcommand="-s sort -o .\\output\\dept.L92 -k 2b,2";
 int rc;
 
 /* Open file from where data will be read */
 rc=DosOpen(filename,&hf,&ulAction,0,FILE_NORMAL,FILE_OPEN,
            OPEN_ACCESS_READONLY | OPEN_SHARE_DENYNONE,(PEAOP2) NULL);
  
 rc=SMARTsort(sortcommand,NULL,NULL,inputexit,NULL,NULL,NULL);
 
 /* Close file used by input exit */
 DosClose(hf);

}

int getdata(SMARTSORT_BUFFER *to_SMARTsort, SMARTSORT_BUFFER *from_SMARTsort) 
{
 /* End input exit processing if previous call to this routine encountered EOF */
 if ( bytesread<READ_LENGTH ) return(UE_END);

 /* Read bytes from a file into the buffer supplied by SMARTsort */
 DosRead(hf,(BYTE *) to_SMARTsort->buffer, READ_LENGTH, &bytesread);

 /* Set to_SMARTsort structure according to results of read */
 if ( bytesread<1 ) {
   to_SMARTsort->buffer_size=to_SMARTsort->nbytes_used=0;
 } else {
   to_SMARTsort->buffer_size=to_SMARTsort->nbytes_used=bytesread;
 }
   
 return(UE_OK);  
}
