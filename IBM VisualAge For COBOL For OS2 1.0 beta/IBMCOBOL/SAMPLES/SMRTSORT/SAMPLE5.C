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
/* SMARTsort -- sample5.c                                 */
/*                                                        */
/* This program demonstrates how SMARTsort can be used    */
/* to filter data from a file.                            */
/*                                                        */
/**********************************************************/

#pragma linkage (SMARTsort,system)
#include <string.h>
                                          
void main(void)
{
 
 char *copy_command="-s copy -x 'filter 3b,3 eq \"L92\"'";
 char copycommand[100];

 /* Define file names that will be used */
 char *deptall=".\\data\\dept.all"; 
 char *filtered=".\\output\\filter.out "; 
 int rc;

 /**********************************************/
 /* Build command string and call SMARTsort to */
 /* filter data from original file             */       
 /**********************************************/
 strcpy(copycommand,copy_command);         
 strcat(copycommand," -o ");
 strcat(copycommand,filtered);
 strcat(copycommand,deptall);
 rc=SMARTsort(copycommand,NULL,NULL,NULL,NULL,NULL,NULL);

}
