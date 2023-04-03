      *
      * Licensed Material -- Property of IBM 
      * 5765-349
      * (c) Copyright IBM Corporation 1995 
      * All rights reserved
      *
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. sample8.                                                      
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
       01  parmlist.
           05  command        pic X(100) value Z"-s sort -x 'reformat 
      -        "3b,3 "" - "" 2b,2 ""\n""' -o.\output\reformat.out
      -        ".\data\dept.all ".
           05  inbuffer       usage pointer.
           05  outbuffer      usage pointer.
           05  inexit         usage procedure-pointer.
           05  outexit        usage procedure-pointer. 
           05  altseq         usage pointer.
           05  ioerror        usage procedure-pointer.
       PROCEDURE DIVISION.                                                      
           SET INBUFFER TO NULL.
           SET OUTBUFFER TO NULL.
           SET INEXIT TO NULL.
           SET OUTEXIT TO NULL.
           SET ALTSEQ TO NULL.
           SET IOERROR TO NULL.
           CALL "SMARTsort" using by reference command
                            by value inbuffer 
                                     outbuffer
                                     inexit
                                     outexit
                                     altseq
                                     ioerror.
           STOP RUN.                                                            
