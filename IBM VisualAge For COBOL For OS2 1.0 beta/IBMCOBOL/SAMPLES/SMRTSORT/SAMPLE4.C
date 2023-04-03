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
/* SMARTsort -- sample4.c                                 */
/*                                                        */
/* This program copies three files into one file.         */
/*                                                        */
/**********************************************************/

#pragma linkage (SMARTsort,system)
#include <string.h>
                                          
void main(void)
{
 
 char *copy_command="-s copy";
 char copycommand[100];

 /* Define file names that will be used */
 char *deptL92=" .\\data\\dept.L92"; 
 char *deptJ69=" .\\data\\dept.J69"; 
 char *dept403=" .\\data\\dept.403"; 
 char *combined=".\\output\\dept.all "; 
 int rc;

 /*********************************************************/
 /* Build command string and call SMARTsort to copy files */
 /*********************************************************/
 strcpy(copycommand,copy_command);         
 strcat(copycommand," -o ");
 strcat(copycommand,combined);
 strcat(copycommand,deptL92);
 strcat(copycommand,deptJ69);
 strcat(copycommand,dept403);
 rc=SMARTsort(copycommand,NULL,NULL,NULL,NULL,NULL,NULL);

}
