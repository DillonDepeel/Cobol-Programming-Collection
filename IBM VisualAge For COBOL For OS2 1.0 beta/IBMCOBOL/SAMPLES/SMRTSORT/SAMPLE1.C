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
/* SMARTsort -- sample1.c                                 */
/*                                                        */
/* This program sorts three files of department members   */
/* then merges the output of each sort to create a single */
/* file of all departments and members.                   */
/*                                                        */
/**********************************************************/

#pragma linkage (SMARTsort,system)
#include <string.h>
                                          
void main(void)
{
 
 static char *sort_command="-s sort -k 2b,2";
 static char *merge_command="-s merge -k 3b,3";
 char SMARTcommand[100];

 /* Define file names that will be used */
 static char *deptL92=".\\data\\dept.L92";
 static char *deptJ69=".\\data\\dept.J69";
 static char *dept403=".\\data\\dept.403";
 static char *deptL92_sorted=".\\output\\dept.L92 ";
 static char *deptJ69_sorted=".\\output\\dept.J69 ";
 static char *dept403_sorted=".\\output\\dept.403 ";
 static char *depts_merged=" -o .\\output\\dept.mrg ";
 int rc;

 /*****************************************************************/
 /* Build command string and call SMARTsort to sort file dept.L92 */
 /*****************************************************************/
 strcpy(SMARTcommand,sort_command);         
 strcat(SMARTcommand," -o ");
 strcat(SMARTcommand,deptL92_sorted);
 strcat(SMARTcommand,deptL92);
 rc=SMARTsort(SMARTcommand,NULL,NULL,NULL,NULL,NULL,NULL);

 /*****************************************************************/
 /* Build command string and call SMARTsort to sort file dept.J69 */
 /*****************************************************************/
 strcpy(SMARTcommand,sort_command);
 strcat(SMARTcommand," -o ");
 strcat(SMARTcommand,deptJ69_sorted);
 strcat(SMARTcommand,deptJ69);
 rc=SMARTsort(SMARTcommand,NULL,NULL,NULL,NULL,NULL,NULL);

 /*****************************************************************/
 /* Build command string and call SMARTsort to sort file dept.403 */
 /*****************************************************************/
 strcpy(SMARTcommand,sort_command);
 strcat(SMARTcommand," -o ");
 strcat(SMARTcommand,dept403_sorted);
 strcat(SMARTcommand,dept403);
 rc=SMARTsort(SMARTcommand,NULL,NULL,NULL,NULL,NULL,NULL);

 /****************************************************/
 /* Build command string and call SMARTsort to merge */
 /* all files sorted above.                          */
 /****************************************************/
 strcpy(SMARTcommand,merge_command);
 strcat(SMARTcommand,depts_merged);
 strcat(SMARTcommand,deptL92_sorted);
 strcat(SMARTcommand,deptJ69_sorted);
 strcat(SMARTcommand,dept403_sorted);
 rc=SMARTsort(SMARTcommand,NULL,NULL,NULL,NULL,NULL,NULL);

}
