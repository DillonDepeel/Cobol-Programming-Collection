/**********************************************************/
/*                                                        */ 
/*  Licensed Material -- Property of IBM                  */    
/*  5765-349                                              */
/*  (c) Copyright IBM Corporation 1995                    */
/*  All rights reserved                                   */
/*                                                        */
/**********************************************************/

/**********************************************************/
/*                                                        */
/* SMARTsort -- sample6.c                                 */
/*                                                        */
/* This program demonstrates how SMARTsort can be used    */
/* to create a sorted file consisting of records that     */
/* have been restructured from the original input file.   */
/*                                                        */
/**********************************************************/

#pragma linkage (SMARTsort,system)
#include <string.h>
                                          
void main(void)
{
 
 char *sort_command="-s sort -x 'reformat 3b,3 \" - \" 2b,2 \"\\r\" \"\\n\"' -k 2b,2";
 char SMARTcommand[100];

 /* Define file names that will be used */
 char *deptall=".\\data\\dept.all"; 
 char *reformat=".\\output\\reformat.out "; 
 int rc;

 /*********************************************************/
 /* Build command string and call SMARTsort to create a   */
 /* sorted file consisting of a rearranged subset of the  */
 /* original data                                         */
 /*********************************************************/
 strcpy(SMARTcommand,sort_command);         
 strcat(SMARTcommand," -o ");
 strcat(SMARTcommand,reformat);
 strcat(SMARTcommand,deptall);
 rc=SMARTsort(SMARTcommand,NULL,NULL,NULL,NULL,NULL,NULL);

}
