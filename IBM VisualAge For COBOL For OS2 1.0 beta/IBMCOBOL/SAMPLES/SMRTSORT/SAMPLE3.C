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
/* SMARTsort -- sample3.c                                 */
/*                                                        */
/* This program shows how to use SMARTsort to sort an     */
/* array of structures defined in memory.                 */
/*                                                        */
/**********************************************************/

#pragma linkage (SMARTsort,system)
#include <string.h>
#include "smrtsort.h"

typedef struct INVENTORY {
                          int id;
                          int stockonhand;
                          float price;
                          char itemname[12];
                         } INVENTORY;

void main(void)
{
 INVENTORY *health_foods;
 INVENTORY *by_quantity;
 SMARTSORT_BUFFER source[2], target[2];
 char sortcommand[100] = "-s sort ";
 int rc;
 int numberofitems;
  
 /* Create inventory list */
 health_foods=(INVENTORY *) malloc( sizeof(INVENTORY)*10 );
 health_foods[0].id=1;
 health_foods[0].stockonhand=12;
 health_foods[0].price=8.75;
 strcpy(health_foods[0].itemname,"Oranges");
 health_foods[1].id=2;
 health_foods[1].stockonhand=45;
 health_foods[1].price=15.00;
 strcpy(health_foods[1].itemname,"Raspberries");
 health_foods[2].id=3;
 health_foods[2].stockonhand=10;
 health_foods[2].price=0.99;
 strcpy(health_foods[2].itemname,"Pears");
 health_foods[3].id=4;
 health_foods[3].stockonhand=16;
 health_foods[3].price=4.50;
 strcpy(health_foods[3].itemname,"Tangerines");
 health_foods[4].id=5;
 health_foods[4].stockonhand=36;
 health_foods[4].price=2.99;
 strcpy(health_foods[4].itemname,"Grapes");
 health_foods[5].id=6;
 health_foods[5].stockonhand=12;
 health_foods[5].price=1.00;
 strcpy(health_foods[5].itemname,"Apples");
 numberofitems=6;
 
 /* Define structure of data for SMARTsort */
 strcat(sortcommand,"-x 'format int int float char 12'");
 
 /* Indicate to sort by stock on hand */
 strcat(sortcommand," -k 2,2");

 /* Allocate space for results of sort */
 by_quantity=(INVENTORY *) malloc( sizeof(INVENTORY)*10 );

 /* Initialize SMARTsort source and target structures */
 source[0].buffer=(char *) health_foods;
 source[0].buffer_size=sizeof(INVENTORY)*numberofitems;
 source[0].nbytes_used=NULL;
 source[1].buffer=NULL;
 source[1].buffer_size=0;
 source[1].nbytes_used=NULL;
 target[0].buffer=(char *) by_quantity;
 target[0].buffer_size=sizeof(INVENTORY)*10;
 target[0].nbytes_used=NULL;
 target[1].buffer=NULL;
 target[1].buffer_size=0;
 target[1].nbytes_used=NULL;

 /* Call SMARTsort to sort the structures */
 rc=SMARTsort(sortcommand,source,target,NULL,NULL,NULL,NULL);

 /* Clean up and get out */
 free(health_foods);
 free(by_quantity);
 
}
