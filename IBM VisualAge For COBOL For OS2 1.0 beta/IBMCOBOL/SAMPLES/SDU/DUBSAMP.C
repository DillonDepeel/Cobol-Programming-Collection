/**************************************************************************
****************************  DUBSAMP.C  **********************************
***************************************************************************
*
*                              Record Level I/O
*
* Module Name: DUBSAMP.C
*   This module demonstrates some basic record level I/O functions.
*
* RLIO (program no. 5648-02011)
*
* Version: 1.0
* Release: 1.0
*
* Copyright (C)
* International Business Machines Corporation 1992
*
* DISCLAIMER OF WARRANTIES: The following (enclosed) code is sample code
* created by the IBM Corporation.  This sample code is not a part of any
* IBM product and is provided to you solely for the purpose of assisting
* you in the development of your applications.  The code is provided
* "AS IS", without warranty of any kind.  IBM shall not be liable for any
* damages arising out of your use of the sample code, even if they have
* been advised of the possibility of such damages.
*
* The sample program does the following:
*
* 1) Create a sequential file
* 2) Try to create the same sequential file again in order to get a reply
*    message.
* 3) Set path information for sequential file
*    (i.e. DDM_TITLE attribute is set to "USER_NAME")
* 4) Query the sequential file to see if it is a record file
*    (i.e. DDM_FILCLS attribute = SEQFIL, DIRFIL, or KEYFIL)
* 5) Open the sequential file
* 6) Insert a record into the sequential file
* 7) Get a record from the sequential file
* 8) Close the sequential file
* 9) Delete the sequential file
*
* The following operations are done on both direct and keyed files:
*
* 10) Create the file
* 11) Open the file
* 12) Insert a record into file
* 13) Get a record from the file
* 14) Close the file
* 15) Delete the file
*
* The following operations are done on an alternate index file and its
* base file:
*
* 16) Create a base keyed file
* 17) Create an alternate index file
* 18) Open the alternate index file
* 19) Close the alternate index file
* 20) Delete the alternate index file
* 21) Delete the base keyed file
*
* COMMAND LINE INVOCATION:
*
*   DUBSAMP [pathname] [filenamesuffix]
*
*   Examples:
*
*     ENTERED                   RESULT
*
*     dubsamp                   uses predefined filenames with the current path
*
*     dubsamp d:\rlio\          prefixes predefined filenames with the
*                               specified path:
*                                    d:\rlio\dubsamp.seq
*
*     dubsamp d:\rlio\ test1    prefixes predefined file class names with
*                               the first parameter and suffixes the result
*                               with the second parameter:
*                                    d:\rlio\SEQtest1
*
***************************************************************************/

#include  <os2.h>                       /* required for RLIO applications */
#include  <stdio.h>
#include  <string.h>
#include  <memory.h>
#include  <malloc.h>
#include  "dub.h"        /* required master include for RLIO applications */

/*-------------------------------------------------------------------------
--                       SYMBOLIC CONSTANTS
--------------------------------------------------------------------------*/
#define FILCLS_SIZE sizeof(OBJLENGTH) + (2 * sizeof(CODEPOINT))
#define FILCLS_NAME ".DDM_FILCLS"
#define RECDATALEN  100
#define RPYMSBFLN   546                    /* reply message buffer length */
#define PATHLEN     100

/*-------------------------------------------------------------------------
--                     LOCAL FUNCTION DECLARATIONS
--------------------------------------------------------------------------*/
VOID DumpBuffer(PDDMOBJECT pAttribute, USHORT Count);

VOID ReplyMsg(VOID);

VOID CleanUp(CHAR *File1, CHAR *File2, CHAR *File3, CHAR *File4, CHAR *File5);

/*-------------------------------------------------------------------------
--                             DUBSAMP
--------------------------------------------------------------------------*/
main(int argc, char* argv[])
{
    APIRET SevCode;              /* RLIO severity code */

    PDDMRECORD pRecord;
    RECLENGTH  RecordSize;
    PDDMRECAL  pRecAL;
    PDDMRECALK  pRecALK;
    RECLENGTH  RecALSize;
    PBYTE      pData;

    HDDMFILE FileHandle;

    /* OS/2 extended attribute structures */
    EAOP2 Eaop;
    EAOP2 Eaop2;
    PFEA2 pFEA;
    INT   FEASize;
    ULONG FEA2Size;
    INT   GEASize;

    PSZ   TitleName = ".DDM_TITLE\0";      /* extended attribute name for title */
    PSZ   TitleString  = "Title String\0";
    ULONG TitleObjectSize = sizeof(OBJLENGTH) + sizeof(CODEPOINT) + strlen(TitleString);

    /* Filenames of files to be operated on */
    CHAR SeqFN[PATHLEN];
    CHAR DirFN[PATHLEN];
    CHAR KeyFN[PATHLEN];
    CHAR AltFN[PATHLEN];
    CHAR KeyFN2[PATHLEN];

    #pragma pack(2)

    typedef struct _MYKEYDEFBUF     /* key definition buffer */
    {  ULONG     cbKeyDefBuf;
       CODEPOINT cpKeyDefBuf;
       KEYFLDDEF KeyFldDef[1];
    } MYKEYDEFBUF;

    MYKEYDEFBUF KeyDefBuf;

    /*---------------------------------------------------------------------
    -- Construct file specifications from input parameters
    ----------------------------------------------------------------------*/
    switch (argc)
    {  case 1: /* no user arguments */
          strcpy(SeqFN,"dubsamp.seq");
          strcpy(DirFN,"dubsamp.dir");
          strcpy(KeyFN,"dubsamp.key");
          strcpy(AltFN,"dubsamp.alt");
          strcpy(KeyFN2,"dubsamp.ky2");
          break;
       case 2: /* 1 user argument: pathname */
          strcpy(SeqFN,argv[1]);
          strcpy(DirFN,argv[1]);
          strcpy(KeyFN,argv[1]);
          strcpy(AltFN,argv[1]);
          strcpy(KeyFN2,argv[1]);
          strcat(SeqFN,"dubsamp.seq");
          strcat(DirFN,"dubsamp.dir");
          strcat(KeyFN,"dubsamp.key");
          strcat(AltFN,"dubsamp.alt");
          strcat(KeyFN2,"dubsamp.ky2");
          break;
       case 3: /* 2 user arguments: pathname filenamesuffix */
          strcpy(SeqFN,argv[1]);
          strcpy(DirFN,argv[1]);
          strcpy(KeyFN,argv[1]);
          strcpy(AltFN,argv[1]);
          strcpy(KeyFN2,argv[1]);
          strcat(SeqFN,"SEQ");
          strcat(DirFN,"DIR");
          strcat(KeyFN,"KEY");
          strcat(AltFN,"ALT");
          strcat(KeyFN2,"KY2");
          strcat(SeqFN,argv[2]);
          strcat(DirFN,argv[2]);
          strcat(KeyFN,argv[2]);
          strcat(AltFN,argv[2]);
          strcat(KeyFN2,argv[2]);
          break;
       default: /* > 2 user arguments */
          printf("Incorrect command line syntax.\n\n");
          printf("Correct syntax: dubsamp [pathname] [filenamesuffix]\n\n");
          printf("     where pathname and filenamesuffix are optional\n");
          printf("           When filenamesuffix is specified, the prefixes are:\n");
          printf("             SEQ, DIR, KEY, ALT, and KY2\n\n");
          printf("Example: dubsamp g:\\dir1\\dir2\\ test1 produces:\n\n");
          printf("                 g:\\dir1\\dir2\\SEQtest1\n");
          printf("                 g:\\dir1\\dir2\\DIRtest1\n");
          printf("                    and so on ... \n");
          return;
    }

    /*---------------------------------------------------------------------
    -- Create a sequential file with get, insert, modify, and delete capability
    ----------------------------------------------------------------------*/
    SevCode = DDMCreateRecFile
              (SeqFN,                      /* FileName        */
               DDM_GETCP | DDM_INSCP |     /* CreateFlags     */
               DDM_MODCP | DDM_DELCP,
               (ULONG)RECDATALEN,          /* RecLen          */
               RECFIX,                     /* RecLenCls       */
               NULL,                       /* KeyDefBuf       */
               0UL,                        /* InitFilSiz      */
               -1L,                        /* MaxFileSiz      */
               DUPFILDO,                   /* DupFilOpt       */
               NIL,                        /* DftRecOp        */
               0UL,                        /* RecCnt          */
               NULL,                       /* EABuf           */
               SEQFIL,                     /* FileClass       */
               NULL                        /* DftRecBuf       */
              );
    if (SevCode == SC_NO_ERROR)
       printf("\nSuccessfully created sequential file %s\n",SeqFN);
    else
    {  printf("Error creating sequential file %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Try to create the same sequential file as before (this should be rejected)
    ----------------------------------------------------------------------*/
    SevCode = DDMCreateRecFile
              (SeqFN,                      /* FileName        */
               DDM_GETCP | DDM_INSCP |     /* CreateFlags     */
               DDM_MODCP | DDM_DELCP,
               (ULONG)RECDATALEN,          /* RecLen          */
               RECFIX,                     /* RecLenCls       */
               NULL,                       /* KeyDefBuf       */
               0UL,                        /* InitFilSiz      */
               -1L,                        /* MaxFileSiz      */
               DUPFILDO,                   /* DupFilOpt       */
               NIL,                        /* DftRecOp        */
               0UL,                        /* RecCnt          */
               NULL,                       /* EABuf           */
               SEQFIL,                     /* FileClass       */
               NULL                        /* DftRecBuf       */
              );
    if (SevCode == SC_NO_ERROR)
    {  printf("\nUnsuccessful in rejecting creation of sequential file %s\n",SeqFN);
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }
    else
    {  printf("\nSuccessful in rejecting the creation of sequential file %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
    }

    /*---------------------------------------------------------------------
    -- Set up for DDMSetPathInfo:
    --
    -- Build an extended attribute structure with a FEA list containing
    -- the TITLE extended attribute.
    ----------------------------------------------------------------------*/
    FEA2Size = sizeof(Eaop2.fpFEA2List->list[0].oNextEntryOffset)
             + sizeof(Eaop2.fpFEA2List->list[0].fEA)
             + sizeof(Eaop2.fpFEA2List->list[0].cbName)
             + sizeof(Eaop2.fpFEA2List->list[0].cbValue)
             + sizeof(Eaop2.fpFEA2List->cbList)
             + strlen(TitleName) + 1
             + TitleObjectSize;
    if ((Eaop2.fpFEA2List = (PFEA2LIST)malloc(FEA2Size)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    Eaop2.fpFEA2List->cbList = FEA2Size;
    pFEA = Eaop2.fpFEA2List->list;
    pFEA->oNextEntryOffset = 0l;
    pFEA->fEA = 0;
    pFEA->cbName = LOBYTE(strlen(TitleName));
    pFEA->cbValue = TitleObjectSize;
    pData = (PBYTE)&pFEA->cbValue + sizeof(pFEA->cbValue);
    strcpy(pData,TitleName);
    pData+= pFEA->cbName + 1;
    *(PULONG)pData = TitleObjectSize;
    pData+= sizeof(ULONG);
    *(PUSHORT)pData = TITLE;
    pData+= sizeof(USHORT);
    strcpy((PBYTE)pData,TitleString);

    Eaop2.fpGEA2List = NULL;
    Eaop2.oError = 0l;

    SevCode = DDMSetPathInfo
              (SeqFN,                      /* PathName        */
               1UL,                        /* PathInfoLevel   */
               (PBYTE)&Eaop2,              /* PathInfoBuf     */
               (ULONG)sizeof(EAOP2)        /* PathInfoBufSize */
              );
    if (SevCode == SC_NO_ERROR)
       printf("\nSuccessful DDMSetPathInfo call to file %s\n",SeqFN);
    else
    {  printf("Error in DDMSetPathInfo call to file %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Set up for DDMQueryPathInfo:
    --
    -- Build an extended attribute structure with a GEA list defining
    -- the attribute name for FILCLS.
    --
    ----------------------------------------------------------------------*/
    FEASize = sizeof(Eaop.fpFEA2List->list[0].oNextEntryOffset)
            + sizeof(Eaop.fpFEA2List->list[0].fEA)
            + sizeof(Eaop.fpFEA2List->list[0].cbName)
            + sizeof(Eaop.fpFEA2List->list[0].cbValue)
            + sizeof(Eaop.fpFEA2List->cbList)
            + FILCLS_SIZE
            + strlen(FILCLS_NAME)
            + 1;
    GEASize = sizeof(Eaop.fpGEA2List->list[0].oNextEntryOffset)
            + sizeof(Eaop.fpGEA2List->list[0].cbName)
            + sizeof(Eaop.fpGEA2List->cbList)
            + strlen(FILCLS_NAME)
            + 1;
    if ((Eaop.fpFEA2List = (PFEA2LIST)malloc(FEASize)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    if ((Eaop.fpGEA2List = (PGEA2LIST)malloc(GEASize)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    pFEA = Eaop.fpFEA2List->list;
    Eaop.fpFEA2List->cbList = FEASize;
    Eaop.fpGEA2List->cbList = GEASize;
    Eaop.fpGEA2List->list->cbName = LOBYTE(strlen(FILCLS_NAME));
    Eaop.fpGEA2List->list->oNextEntryOffset = 0;
    Eaop.fpGEA2List->list->oNextEntryOffset =
                      sizeof(Eaop.fpGEA2List->list->oNextEntryOffset)
                    + sizeof(Eaop.fpGEA2List->list->cbName)
                    + Eaop.fpGEA2List->list->cbName +1;
    strcpy(Eaop.fpGEA2List->list->szName, FILCLS_NAME);
    Eaop.oError = 0L;

    /*---------------------------------------------------------------------
    -- Query a file to get DDM_FILCLS EA.
    -- Display its file class.
    ----------------------------------------------------------------------*/
    SevCode = DDMQueryPathInfo
              (SeqFN,                           /* PathName        */
               1UL,                             /* PathInfoLevel   */
               (PBYTE)&Eaop,                    /* PathInfoBuf     */
               (ULONG)sizeof(EAOP2)             /* PathInfoBufSize */
              );
    if (SevCode == SC_NO_ERROR)
    {  printf("\n\nSuccessful DDMQueryPathInfo call to file %s\n",SeqFN);
       pData = (PBYTE)&pFEA->szName + pFEA->cbName + 1;
       switch (*(PCODEPOINT)((PDDMOBJECT)pData)->pData)
       {  case SEQFIL:  printf("%s is a sequential file\n",SeqFN);
                        break;
          case DIRFIL:  printf("%s is a direct file\n",SeqFN);
                        break;
          case KEYFIL:  printf("%s is a keyed file\n",SeqFN);
                        break;
          case ALTINDF: printf("%s is a alternate index file\n",SeqFN);
                        break;
          default:      printf("%s is an invalid record file\n",SeqFN);
                        break;
       }
    }
    else
    {  printf("Error in DDMQueryPathInfo call to file %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }
    free(Eaop.fpFEA2List);
    free(Eaop.fpGEA2List);

    /*---------------------------------------------------------------------
    -- Open the sequential file
    ----------------------------------------------------------------------*/
    SevCode = DDMOpen
              (SeqFN,                           /* FileName        */
               &FileHandle,                     /* FileHandle      */
               CMBRNBAM,                        /* AccessMethod    */
               DDM_GETAI | DDM_INSAI,           /* AccIntList      */
               DDM_NOSHARE,                     /* FileShare       */
               NULL,                            /* EABuf           */
               NULL                             /* reserved        */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully opened file %s\n",SeqFN);
    else
    {  printf("Error opening file %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Build a record
    ----------------------------------------------------------------------*/
    RecordSize = sizeof(RECLENGTH) + sizeof(CODEPOINT) + RECDATALEN;
    if ((pRecord = (PDDMRECORD)malloc(RecordSize)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    pRecord->cbRecord = RecordSize;
    pRecord->cpRecord = RECORD;
    memset(pRecord->pRecord, 'N', RECDATALEN);

    /*---------------------------------------------------------------------
    -- Insert a record into the sequential file
    ----------------------------------------------------------------------*/
    SevCode = DDMInsertRecEOF
              (FileHandle,                      /* FileHandle      */
               DDM_UPDCSR,                      /* AccessFlags     */
               pRecord,                         /* RecordBuf       */
               1UL,                             /* RecCount        */
               NULL,                            /* FdbkBuf         */
               0UL                              /* FdbkBufLen      */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully inserted record in %s\n",SeqFN);
    else
    {  printf("Error inserting record in %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }
    free(pRecord);

    /*---------------------------------------------------------------------
    -- Get a record from the sequential file
    ----------------------------------------------------------------------*/
    RecALSize = RecordSize + sizeof(DDMRECAL);
    if ((pRecAL = (PDDMRECAL)malloc(RecALSize)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    SevCode = DDMGetRec
              (FileHandle,                      /* FileHandle      */
               DDM_RECNBRFB,                    /* AccessFlags     */
               (PDDMRECORD)pRecAL,              /* RecordBuf       */
               (ULONG)RecALSize                 /* RecordBufLen    */
              );
    if (SevCode == SC_NO_ERROR)
    {  printf("Successfully retrieved record from %s\n",SeqFN);
       printf("LL = %ld\n",pRecAL->cbRecAL);
       printf("CP = 0x%X\n",pRecAL->cpRecAL);
       printf("L1 = %ld\n",pRecAL->cbRecNum);
       printf("CP = 0x%X\n",pRecAL->cpRecNum);
       printf("Record Number = %lu\n",pRecAL->RecNum);
    }
    else
    {  printf("Error getting record in %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }
    free(pRecAL);

    /*---------------------------------------------------------------------
    -- Close the sequential file
    ----------------------------------------------------------------------*/
    SevCode = DDMClose
              (FileHandle             /* FileHandle      */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully closed file %s\n",SeqFN);
    else
    {  printf("Error closing file %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Delete the sequential file
    ----------------------------------------------------------------------*/
    SevCode = DDMDelete
              (SeqFN,                  /* File Name       */
               0UL                     /* Flags           */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully deleted file %s\n",SeqFN);
    else
    {  printf("Error deleting file %s\n",SeqFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Create a direct file with get, insert, modify, and delete capability
    ----------------------------------------------------------------------*/
    SevCode = DDMCreateRecFile
              (DirFN,                           /* FileName        */
               DDM_GETCP | DDM_INSCP |          /* CreateFlags     */
               DDM_MODCP | DDM_DELCP,
               (ULONG)RECDATALEN,               /* RecLen          */
               RECFIX,                          /* RecLenCls       */
               NULL,                            /* KeyDefBuf       */
               0UL,                             /* InitFilSiz      */
               -1L,                             /* MaxFileSiz      */
               DUPFILDO,                        /* DupFilOpt       */
               NIL,                             /* DftRecOp        */
               0UL,                             /* RecCnt          */
               NULL,                            /* EABuf           */
               DIRFIL,                          /* FileClass       */
               NULL                             /* DftRecBuf       */
              );
    if (SevCode == SC_NO_ERROR)
       printf("\n\nSuccessfully created direct file %s\n",DirFN);
    else
    {  printf("Error creating direct file %s\n",DirFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Open the direct file
    ----------------------------------------------------------------------*/
    SevCode = DDMOpen
              (DirFN,                           /* FileName        */
               &FileHandle,                     /* FileHandle      */
               CMBRNBAM,                        /* AccessMethod    */
               DDM_GETAI | DDM_INSAI,           /* AccIntList      */
               DDM_NOSHARE,                     /* FileShare       */
               NULL,                            /* EABuf           */
               NULL                             /* reserved        */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully opened file %s\n",DirFN);
    else
    {  printf("Error opening file %s\n",DirFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Build a record
    ----------------------------------------------------------------------*/
    RecordSize = sizeof(RECLENGTH) + sizeof(CODEPOINT) + RECDATALEN;
    if ((pRecord = (PDDMRECORD)malloc(RecordSize)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    pRecord->cbRecord = RecordSize;
    pRecord->cpRecord = RECORD;
    memset(pRecord->pRecord, 'N', RECDATALEN);

    /*---------------------------------------------------------------------
    -- Insert a record into the direct file
    ----------------------------------------------------------------------*/
    SevCode = DDMInsertRecEOF
              (FileHandle,                      /* FileHandle      */
               DDM_UPDCSR,                      /* AccessFlags     */
               pRecord,                         /* RecordBuf       */
               1UL,                             /* RecCount        */
               NULL,                            /* FdbkBuf         */
               0UL                              /* FdbkBufLen      */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully inserted record in %s\n",DirFN);
    else
    {  printf("Error inserting record in %s\n",DirFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }
    free(pRecord);

    /*---------------------------------------------------------------------
    -- Get a record from the direct file
    ----------------------------------------------------------------------*/
    RecALSize = RecordSize + sizeof(DDMRECAL);
    if ((pRecAL = (PDDMRECAL)malloc(RecALSize)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    SevCode = DDMGetRec
              (FileHandle,                      /* FileHandle      */
               DDM_RECNBRFB,                    /* AccessFlags     */
               (PDDMRECORD)pRecAL,              /* RecordBuf       */
               (ULONG)RecALSize                 /* RecordBufLen    */
              );
    if (SevCode == SC_NO_ERROR)
    {  printf("Successfully retrieved record from %s\n",DirFN);
       printf("LL = %ld\n",pRecAL->cbRecAL);
       printf("CP = 0x%X\n",pRecAL->cpRecAL);
       printf("L1 = %ld\n",pRecAL->cbRecNum);
       printf("CP = 0x%X\n",pRecAL->cpRecNum);
       printf("Record Number = %lu\n",pRecAL->RecNum);
    }
    else
    {  printf("Error getting record in %s\n",DirFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }
    free(pRecAL);

    /*---------------------------------------------------------------------
    -- Close the direct file
    ----------------------------------------------------------------------*/
    SevCode = DDMClose
              (FileHandle              /* FileHandle      */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully closed file %s\n",DirFN);
    else
    {  printf("Error closing file %s\n",DirFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Delete the direct file
    ----------------------------------------------------------------------*/
    SevCode = DDMDelete
              (DirFN,                /* File Name       */
               0UL                   /* Flags           */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully deleted file %s\n",DirFN);
    else
    {  printf("Error deleting file %s\n",DirFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Create a keyed file with get, insert, and modify capability
    ----------------------------------------------------------------------*/
    KeyDefBuf.cbKeyDefBuf = (ULONG)sizeof(MYKEYDEFBUF);
    KeyDefBuf.cpKeyDefBuf = KEYDEF;
    KeyDefBuf.KeyFldDef[0].cbKeyFldDef = (ULONG)sizeof(KEYFLDDEF);
    KeyDefBuf.KeyFldDef[0].cpKeyFldDef = KEYFLDDF;
    KeyDefBuf.KeyFldDef[0].cpSequence  = SEQASC;
    KeyDefBuf.KeyFldDef[0].cpKeyClass  = BYTSTRDR;
    KeyDefBuf.KeyFldDef[0].cbKeyField  = (USHORT)19;
    KeyDefBuf.KeyFldDef[0].oKeyField   = (ULONG)4;

    SevCode = DDMCreateRecFile
              (KeyFN,                           /* FileName        */
               DDM_GETCP | DDM_INSCP |          /* CreateFlags     */
               DDM_MODCP,
               (ULONG)RECDATALEN,               /* RecLen          */
               RECFIX,                          /* RecLenCls       */
               (PKEYDEFBUF)&KeyDefBuf,          /* KeyDefBuf       */
               0UL,                             /* InitFilSiz      */
               -1L,                             /* MaxFileSiz      */
               DUPFILDO,                        /* DupFilOpt       */
               NIL,                             /* DftRecOp        */
               0UL,                             /* RecCnt          */
               NULL,                            /* EABuf           */
               KEYFIL,                          /* FileClass       */
               NULL                             /* DftRecBuf       */
              );
    if (SevCode == SC_NO_ERROR)
       printf("\n\nSuccessfully created keyed file %s\n",KeyFN);
    else
    {  printf("Error creating keyed file %s\n",KeyFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Open the keyed file
    ----------------------------------------------------------------------*/
    SevCode = DDMOpen
              (KeyFN,                           /* FileName        */
               &FileHandle,                     /* FileHandle      */
               CMBKEYAM,                        /* AccessMethod    */
               DDM_GETAI | DDM_INSAI,           /* AccIntList      */
               DDM_NOSHARE,                     /* FileShare       */
               NULL,                            /* EABuf           */
               NULL                             /* reserved        */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully opened file %s\n",KeyFN);
    else
    {  printf("Error opening file %s\n",KeyFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Build a record
    ----------------------------------------------------------------------*/
    RecordSize = sizeof(RECLENGTH) + sizeof(CODEPOINT) + RECDATALEN;
    if ((pRecord = (PDDMRECORD)malloc(RecordSize)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    pRecord->cbRecord = RecordSize;
    pRecord->cpRecord = RECORD;
    memset(pRecord->pRecord, 'N', RECDATALEN);

    /*---------------------------------------------------------------------
    -- Insert a record into the keyed file
    ----------------------------------------------------------------------*/
    SevCode = DDMInsertRecKey
              (FileHandle,                      /* FileHandle      */
               DDM_UPDCSR,                      /* AccessFlags     */
               pRecord,                         /* RecordBuf       */
               NULL,                             /* RecordNumber    */
               1UL                            /* RecCount        */
               
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully inserted record in %s\n",KeyFN);
    else
    {  printf("Error inserting record in %s\n",KeyFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }
    free(pRecord);

    /*---------------------------------------------------------------------
    -- Get a record from the keyed file
    ----------------------------------------------------------------------*/
    RecALSize = RecordSize + sizeof(DDMRECALK) + 18; /* 18 is len. of key -1 */
    if ((pRecALK = (PDDMRECALK)malloc(RecALSize)) == NULL)
    {  printf("Out of memory\n");
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(1);
    }
    SevCode = DDMGetRec
              (FileHandle,                      /* FileHandle      */
               DDM_KEYVALFB,                    /* AccessFlags     */
               (PDDMRECORD)pRecALK,              /* RecordBuf       */
               (ULONG)RecALSize                 /* RecordBufLen */
              );
    if (SevCode == SC_NO_ERROR)
    {  printf("Successfully retrieved record from %s\n",KeyFN);
       printf("LL = %ld\n",pRecALK->cbRecAL);
       printf("CP = 0x%X\n",pRecALK->cpRecAL);
       printf("L1 = %ld\n",pRecALK->cbKeyVal);
       printf("CP = 0x%X\n",pRecALK->cpKeyVal);
       printf("Record Key Value Feed Back = %.19s\n",pRecALK->pKeyVal);
    }
    else
    {  printf("Error getting record in KeyFN\n");
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }
    free(pRecALK);

    /*---------------------------------------------------------------------
    -- Close the keyed file
    ----------------------------------------------------------------------*/
    SevCode = DDMClose
              (FileHandle               /* FileHandle      */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully closed file %s\n",KeyFN);
    else
    {  printf("Error closing file %s\n",KeyFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Delete the keyed file
    ----------------------------------------------------------------------*/
    SevCode = DDMDelete
              (KeyFN,                 /* File Name       */
               0UL                    /* Flags           */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully deleted file %s\n",KeyFN);
    else
    {  printf("Error deleting file %s\n",KeyFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Create a base file (keyed) with get, insert, modify, and delete
    -- capability
    ----------------------------------------------------------------------*/
    SevCode = DDMCreateRecFile
              (KeyFN2,                          /* FileName        */
               DDM_GETCP | DDM_INSCP |          /* CreateFlags     */
               DDM_MODCP | DDM_DELCP,
               (ULONG)RECDATALEN,               /* RecLen          */
               RECFIX,                          /* RecLenCls       */
               (PKEYDEFBUF)&KeyDefBuf,          /* KeyDefBuf       */
               0UL,                             /* InitFilSiz      */
               -1L,                             /* MaxFileSiz      */
               DUPFILDO,                        /* DupFilOpt       */
               NIL,                             /* DftRecOp        */
               0UL,                             /* RecCnt          */
               NULL,                            /* EABuf           */
               KEYFIL,                          /* FileClass       */
               NULL                             /* DftRecBuf       */
              );
    if (SevCode == SC_NO_ERROR)
       printf("\n\nSuccessfully created keyed file %s\n",KeyFN2);
    else
    {  printf("Error creating keyed file %s\n",KeyFN2);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

 /*---------------------------------------------------------------------                
 -- Open the keyed file                                                                 
 ----------------------------------------------------------------------*/               
 SevCode = DDMOpen                                                                      
           (KeyFN2,                           /* FileName        */
            &FileHandle,                     /* FileHandle      */                      
            CMBKEYAM,                        /* AccessMethod    */                      
            DDM_GETAI | DDM_INSAI,           /* AccIntList      */                      
            DDM_NOSHARE,                     /* FileShare       */                      
            NULL,                            /* EABuf           */                      
            NULL                             /* reserved        */                      
           );                                                                           
 if (SevCode == SC_NO_ERROR)                                                            
    printf("Successfully opened file %s\n",KeyFN2);
 else                                                                                   
 {  printf("Error opening file %s\n",KeyFN2);
    printf("Severity code = %u\n",SevCode);                                             
    ReplyMsg();                                                                         
    CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);                                            
    return(SevCode);                                                                    
 }                                                                                      
                                                                                        
 /*---------------------------------------------------------------------                
 -- Build a record                                                                      
 ----------------------------------------------------------------------*/               
 RecordSize = sizeof(RECLENGTH) + sizeof(CODEPOINT) + RECDATALEN;                       
 if ((pRecord = (PDDMRECORD)malloc(RecordSize)) == NULL)                                
 {  printf("Out of memory\n");                                                          
    CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);                                            
    return(1);                                                                          
 }                                                                                      
 pRecord->cbRecord = RecordSize;                                                        
 pRecord->cpRecord = RECORD;                                                            
 memset(pRecord->pRecord, 'N', RECDATALEN);                                             
                                                                                        
 /*---------------------------------------------------------------------                
 -- Insert a record into the keyed file                                                 
 ----------------------------------------------------------------------*/               
 SevCode = DDMInsertRecKey                                                              
           (FileHandle,                      /* FileHandle      */                      
            DDM_UPDCSR,                      /* AccessFlags     */                      
            pRecord,                         /* RecordBuf       */                      
            NULL,                            /* ReccordNumbr   */
            1UL                            /* RecCount        */
                             
           );                                                                           
 if (SevCode == SC_NO_ERROR)                                                            
    printf("Successfully inserted record in %s\n",KeyFN2);
 else                                                                                   
 {  printf("Error inserting record in %s\n",KeyFN2);
    printf("Severity code = %u\n",SevCode);                                             
    ReplyMsg();                                                                         
    CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);                                            
    return(SevCode);                                                                    
 }                                                                                      
 free(pRecord);                                                                         
                                                                                        
 /*---------------------------------------------------------------------                                                  
 -- Close the keyed file                                                                                                  
 ----------------------------------------------------------------------*/                                                 
 SevCode = DDMClose                                                                                                       
           (FileHandle               /* FileHandle      */                                                                
           );                                                                                                             
 if (SevCode == SC_NO_ERROR)                                                                                              
    printf("Successfully closed file %s\n",KeyFN2);
 else                                                                                                                     
 {  printf("Error closing file %s\n",KeyFN);                                                                              
    printf("Severity code = %u\n",SevCode);                                                                               
    ReplyMsg();                                                                                                           
    CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);                                                                              
    return(SevCode);                                                                                                      
 }                                                                                                                        
                                                                                                                          

        


    /*---------------------------------------------------------------------
    -- Create an alternate index file
    ----------------------------------------------------------------------*/
    SevCode = DDMCreateAltIndex
              (AltFN,                           /* FileName        */
               KeyFN2,                          /* BaseName    */
               0UL,                             /* CreateFlags     */
               (PKEYDEFBUF)&KeyDefBuf,          /* KeyDefBuf       */
               DUPFILDO,                        /* DupFilOpt       */
               NULL                             /* DftRecBuf       */
              );
    if (SevCode == SC_NO_ERROR)
       printf("\n\nSuccessfully created alternate index file %s\n",AltFN);
    else
    {  printf("Error creating alternate index file %s\n",AltFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Open the alternate index file
    ----------------------------------------------------------------------*/
    SevCode = DDMOpen
              (AltFN,                           /* FileName        */
               &FileHandle,                     /* FileHandle      */
               CMBKEYAM,                        /* AccessMethod    */
               DDM_GETAI | DDM_INSAI,           /* AccIntList      */
               DDM_NOSHARE,                     /* FileShare       */
               NULL,                            /* EABuf           */
               NULL                             /* reserved        */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully opened file %s\n",AltFN);
    else
    {  printf("Error opening file %s\n",AltFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Close the alternate index file
    ----------------------------------------------------------------------*/
    SevCode = DDMClose
              (FileHandle              /* FileHandle      */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully closed file %s\n",AltFN);
    else
    {  printf("Error closing file %s\n",AltFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Delete the alternate index file
    ----------------------------------------------------------------------*/
    SevCode = DDMDelete
              (AltFN,                   /* File Name   */
               0UL                      /* Flags       */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully deleted file %s\n",AltFN);
    else
    {  printf("Error deleting file %s\n",AltFN);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    /*---------------------------------------------------------------------
    -- Delete the base file
    ----------------------------------------------------------------------*/
    SevCode = DDMDelete
              (KeyFN2,               /* File Name */
               0UL                   /* Flags     */
              );
    if (SevCode == SC_NO_ERROR)
       printf("Successfully deleted file %s\n",KeyFN2);
    else
    {  printf("Error deleting file %s\n",KeyFN2);
       printf("Severity code = %u\n",SevCode);
       ReplyMsg();
       CleanUp(SeqFN,DirFN,KeyFN,AltFN,KeyFN2);
       return(SevCode);
    }

    printf("*****************TEST CASE DUBSAMP WAS SUCCESSFUL**************\n");
    return(SC_NO_ERROR);

} /* sample main */


/**************************************************************************
****************************  ReplyMsg  ***********************************
***************************************************************************
*   Process the reply message if there is a Severity Code other than
*   SC_NO_ERROR;
*
***************************************************************************/
VOID ReplyMsg(VOID)
{
    static BYTE pRpyMsgBuf[RPYMSBFLN];

    APIRET     rc;
    CODEPOINT  CodePoint;
    PDDMOBJECT pReplyObject;
    USHORT     index;

    /*---------------------------------------------------------------------
    --  The following table contains the count for the number of parameters
    --  expected for each reply message (1st column), and it also contains
    --  the expanded error messages
    --
    -- The first message in the table, KEYUDIRM, has the lowest
    -- code point value.  It is also the first message in a block of
    -- message code points that ends with RECNAVRM.
    --
    -- The next block of message code points (in ascending code point order)
    -- begins with OS2ERRRM and ends with FILERRRM.
    -- The low-order byte is used as the index into this block.
    ----------------------------------------------------------------------*/
    static struct
    {  USHORT   Count;
       BYTE     msg[52];
    } ErrorMsgBuffer[] =
      { 6, "Key Update Not Allowed by Different Index         \0",   /* KEYUDIRM */
        0, "                                                  \0",
        0, "                                                  \0",
        0, "Default Record Error                              \0",   /* DFTRECRM */
        5, "Cursor Not Selecting a Record Position            \0",   /* CSRNSARM */
        7, "Invalid Data Record                               \0",   /* DTARECRM */
        3, "Duplicate File Name                               \0",   /* DUPFILRM */
        8, "Duplicate Key Different Index                     \0",   /* DUPKDIRM */
        7, "Duplicate Key Same Index                          \0",   /* DUPKSIRM */
        7, "Duplicate Record Number                           \0",   /* DUPRNBRM */
        3, "End of File                                       \0",   /* ENDFILRM */
        7, "File is Full                                      \0",   /* FILFULRM */
        4, "File in Use                                       \0",   /* FILIUSRM */
        3, "File Not Found                                    \0",   /* FILNFNRM */
        6, "File Space Not Available                          \0",   /* FILSNARM */
        0, "                                                  \0",
        0, "                                                  \0",
        3, "Invalid File Name                                 \0",   /* FILNAMRM */
        0, "                                                  \0",
        0, "                                                  \0",
        7, "Record Length Mismatch                            \0",   /* RECLENRM */
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        2, "Not Authorized to Function                        \0",   /* FUNATHRM */
        0, "                                                  \0",
        4, "File Temporarily Not Available                    \0",   /* FILTNARM */
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        7, "Record Number Out of Bounds                       \0",   /* RECNBRRM */
        5, "Record Not Found                                  \0",   /* RECNFNRM */
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        3, "Invalid Key Length                                \0",   /* KEYLENRM */
        0, "                                                  \0",
        0, "                                                  \0",
        3, "Not Authorized to Access Method                   \0",   /* ACCATHRM */
        0, "Invalid Access Method                             \0",   /* ACCMTHRM */
        3, "Permanent Agent Error                             \0",   /* AGNPRMRM */
        6, "Resource Limits Reached on Target System          \0",   /* RSCLMTRM */
        3, "Invalid Base File Name                            \0",   /* BASNAMRM */
        0, "                                                  \0",
        0, "                                                  \0",
        2, "Not Authorized to Directory                       \0",   /* DRCATHRM */
        0, "Management Class Conflict                         \0",   /* MGMCNFRM */
        0, "Storage Class Conflict                            \0",   /* STGCNFRM */
        3, "Existing Condition                                \0",   /* EXSCNDRM */
        4, "Not Authorized to File                            \0",   /* FILATHRM */
        6, "Invalid Request                                   \0",   /* INVRQSRM */
        4, "Invalid Key Definition                            \0",   /* KEYDEFRM */
        0, "                                                  \0",
        5, "Key Update Not Allowed by Same Index              \0",   /* KEYUSIRM */
        8, "Invalid Key Value                                 \0",   /* KEYVALRM */
        0, "                                                  \0",
        0, "                                                  \0",
        3, "Open Exclusive by Same User                       \0",   /* OPNEXCRM */
        4, "Concurrent Open Exceeds Maximum                   \0",   /* OPNMAXRM */
        4, "Conversational Protocol Error                     \0",   /* PRCCNVRM */
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        7, "Record Damaged                                    \0",   /* RECDMGRM */
        7, "Record in Use                                     \0",   /* RECIUSRM */
        0, "                                                  \0",
        5, "Data Stream Syntax Error                          \0",   /* SYNTAXRM */
        7, "Update Cursor Error                               \0",   /* UPDCSRRM */
        5, "No Update Intent on Record                        \0",   /* UPDINTRM */
        3, "Invalid New File Name                             \0",   /* NEWNAMRM */
        3, "Function Not Supported                            \0",   /* FUNNSPRM */
        3, "Parameter Not Supported                           \0",   /* PRMNSPRM */
        4, "Parameter Value Not Supported                     \0",   /* VALNSPRM */
        4, "Object Not Supported                              \0",   /* OBJNSPRM */
        5, "Command Check                                     \0",   /* CMDCHKRM */
        0, "                                                  \0",
        0, "                                                  \0",
        2, "File Handle Not Found                             \0",   /* HDLNFNRM */
        3, "Directory Full                                    \0",   /* DRCFULRM */
        3, "Record Inactive                                   \0",   /* RECINARM */
        7, "File Damaged                                      \0",   /* FILDMGRM */
        4, "Load Records Count Mismatch                       \0",   /* LODRECRM */
        3, "Not Authorized to Open Intent for Named File      \0",   /* INTATHRM */
        0, "                                                  \0",
        3, "File Closed with Damage                           \0",   /* CLSDMGRM */
        2, "Target Not Supported                              \0",   /* TRGNSPRM */
        5, "Key Value Modified after Cursor was Last Set      \0",   /* KEYMODRM */
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "Access Intent List Error                          \0",   /* ACCINTRM */
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        5, "Record Not Available                              \0",   /* RECNAVRM */

     /************ START OF SECOND CODE POINT RANGE *************/
        0, "OS/2 Error                                        \0",   /* OS2ERRRM */
        0, "Data Description File Not Found                   \0",   /* DDFNFNRM */
        0, "Conversion Table Not Found                        \0",   /* CVTNFNRM */
        2, "Translation Error                                 \0",   /* XLATERM  */
        0, "                                                  \0",
        2, "Invalid Flag                                      \0",   /* INVFLGRM */
        0, "                                                  \0",
        2, "Communications Error                              \0",   /* COMMRM   */
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        0, "                                                  \0",
        2, "Resource Limit Reached in OS/2 V2.0 Source Syste  \0",   /* SRCLMTRM */
        2, "Field Length Error                                \0",   /* LENGTHRM */
        2, "Address Error                                     \0",   /* ADDRRM   */
        0, "                                                  \0",
        2, "Function Continuation Error                       \0",   /* CONTRM   */
        0, "                                                  \0",
        2, "File Error                                        \0"    /* FILERRRM */
      };

    /*---------------------------------------------------------------------
    -- For each reply message available, retrieve and display it.
    ----------------------------------------------------------------------*/
    do
    {  /*------------------------------------------------------------------
       -- Get the reply message
       -------------------------------------------------------------------*/
       rc = DDMGetReplyMessage(pRpyMsgBuf, (ULONG)RPYMSBFLN, (ULONG)1);

       switch (rc)
       {  case SC_NO_ERROR:      /* All reply messages have been received */
          case SC_WARNING: /* There are more reply messages to be received */
             break;
          case SC_ERROR:
             printf("   ReplyMsg: reply message buffer is too small -\n");
             printf("              enlarge and recompile ...\n");
             return;
             break;
          case SC_SEVERE:
             printf("   ReplyMsg: Warning: A reply message was requested,\n");
             printf("              but there are none available ...\n");
             return;
             break;
          case SC_ACCESSDAMAGE:
             printf("   ReplyMsg: Error: An invalid reply message buffer\n");
             printf("              address was specified ...\n");
             return;
             break;
          case SC_PERMDAMAGE:
             printf("   ReplyMsg: Severe Error: An unarchitected reply message\n");
             printf("              object was encountered ...\n");
             return;
             break;
          default:
             printf("   ReplyMsg: Unknown return code from DDMGetReplyMessage\n");
             return;
             break;
       } /* endswitch */

       /*------------------------------------------------------------------
       -- Get the reply message
       -------------------------------------------------------------------*/
       pReplyObject = (PDDMOBJECT)pRpyMsgBuf;

       CodePoint = pReplyObject->cpObject;              /* get code point */

       /* reset pointer to first parm base */
       pReplyObject = (PDDMOBJECT)((PBYTE)pReplyObject
                                   + (sizeof(CODEPOINT)
                                      + sizeof(OBJLENGTH))
                                  );

       /*------------------------------------------------------------------
       -- Calculate the index into the parameter/msg table based on
       -- the codepoint.
       -------------------------------------------------------------------*/
       if (CodePoint <= RECNAVRM)         /* if code point in first block */
          index = (USHORT)(CodePoint - KEYUDIRM);
       else                               /* code point in second block */
          index = (USHORT)
                  ((RECNAVRM - KEYUDIRM + 1) /* number of entries in
                                                first block */
                   + (CodePoint % 0x0100UL) /* index into second block */
                  );

       /*------------------------------------------------------------------
       -- Begin dissecting the reply message buffer
       -------------------------------------------------------------------*/
       if (ErrorMsgBuffer[index].Count > 0)
       {  printf("RPYMSG: %s\n",ErrorMsgBuffer[index].msg);
          DumpBuffer(pReplyObject, ErrorMsgBuffer[index].Count);
          printf("\n");
       }

    } while (rc == SC_WARNING); /* enddo */

} /* ReplyMsg */

/**************************************************************************
**************************  DumpBuffer  ***********************************
***************************************************************************
*
*   For each object in the reply message buffer, print out its contents.
*
***************************************************************************/
VOID DumpBuffer(PDDMOBJECT  pAttribute,
                USHORT      Count)
{
    do
    {  if (pAttribute->cbObject == (sizeof(CODEPOINT) + sizeof(OBJLENGTH)))
       {  printf("Null object returned = %x\n",pAttribute->cbObject);
          pAttribute->cpObject = 0;
       }
       else
       {  switch(pAttribute->cpObject)
          {  case ACCMTHCL:                 /* Access Method Class */
                printf("ACCMTHCL = 0x%X\n", *(PCODEPOINT)(pAttribute->pData));
                break;
             case BASFILNM:                 /* Base File Name */
               printf("BASFILNM = %s\n", pAttribute->pData);
                break;
             case CODPNT:                   /* Code Point */
                printf("CODPNT = 0x%X\n", *(PCODEPOINT)(pAttribute->pData));
                break;
             case CSRPOSST:                 /* Cursor Position Status */
                printf("CSRPOSST = 0x%hX\n", *(PBYTE)(pAttribute->pData));
                break;
             case DTALCKST:                 /* Data Lock Status */
                printf("DTALCKST = 0x%hX\n", *(PBYTE)(pAttribute->pData));
                break;
             case ERRFILNM:                 /* Error File Name */
                printf("ERRFILNM = %s\n", pAttribute->pData);
                break;
             case FILNAM:                   /* File Name */
                printf("FILNAM = %s\n", pAttribute->pData);
                break;
             case KEYDEFCD:                 /* Key Definition Error Code */
                printf("KEYDEFCD = 0x%hX\n", *(PBYTE)(pAttribute->pData));
                break;
             case MAXOPN:                   /* Maximum Number of File Extents
                                                Concurrent Opens Allowed */
                printf("MAXOPN = %d\n", *(PUSHORT)(pAttribute->pData));
                break;
             case NEWFILNM:                 /* New File Name */
                printf("NEWFILNM = %s\n", pAttribute->pData);
                break;
             case PRCCNVCD:         /* Conversational Protocol Error Code */
                printf("PRCCNVCD = 0x%hX\n", *(PBYTE)(pAttribute->pData));
                break;
             case RECCNT:                   /* Record Count */
                printf("RECCNT = %ld\n", *(PULONG)(pAttribute->pData));
                break;
             case RECNBR:                   /* Record Number */
                printf("RECNBR = %ld\n", *(PRECNUM)(pAttribute->pData));
                break;
             case SRVDGN:      {            /* Server Diagnostic Information */
                int i;
                printf("SRVDGN = 0x\n");
                for (i=1; i < (pAttribute->cbObject-5); i++) /* 2 byte len, 2 byte codept*/
                    { if (i % 16 ==0)
                                  printf("%02X\n", *(PBYTE)(pAttribute->pData+i-1));
                       else
                       if (i % 4 ==0) 
                                  printf("%02X ", *(PBYTE)(pAttribute->pData+i-1));
                       else
                           printf("%02X", *(PBYTE)(pAttribute->pData+i-1));
                     }
               }

    
    
                
                break;
             case SVRCOD:                   /* Severity Code */
                printf("SVRCOD = 0x%X\n", *(PCODEPOINT)(pAttribute->pData));
                break;
             case SYNERRCD:                 /* Syntax Error Code */
                printf("SYNERRCD = 0x%hX\n", *(PBYTE)(pAttribute->pData));
                break;
             default:
                printf("Unknown code point - 0x%X\n",
                       *(PCODEPOINT)(pAttribute->pData));
                break;
          } /* endswitch */
       } /* endif */

       /* go to next object */
       pAttribute = (PDDMOBJECT)((PBYTE)pAttribute + pAttribute->cbObject);

    } while(--Count > 0);

} /* DumpBuffer */

/**************************************************************************
****************************  CleanUp *************************************
***************************************************************************
*   Delete all files if any error occurs during processing
*
***************************************************************************/
VOID CleanUp(CHAR *File1,
             CHAR *File2,
             CHAR *File3,
             CHAR *File4,
             CHAR *File5)
{
   DDMDelete(File1, 0);
   DDMDelete(File2, 0);
   DDMDelete(File3, 0);
   DDMDelete(File4, 0);
   DDMDelete(File5, 0);
} /* CleanUp */

