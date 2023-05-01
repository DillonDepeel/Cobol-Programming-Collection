
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMAIL.
       INSTALLATION.
       AUTHOR. SUPERK.
       DATE-WRITTEN. 4/8/2006.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

         FILE-CONTROL.
           SELECT REXX-FILE ASSIGN TO UT-S-SYSPROC
             ORGANIZATION IS SEQUENTIAL
             ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  REXX-FILE
           LABEL RECORD STANDARD
           BLOCK 0 RECORDS
           RECORDING MODE F
           RECORD CONTAINS 255 CHARACTERS.
       01 REXX-RECORD     PIC X(255).

       WORKING-STORAGE SECTION.
       01 MAILBOX-ADDRESS              PIC X(64).
       01 REPLYTO-ADDRESS              PIC X(64).
       01 DEBUG-ACTION                 PIC X(5).
       01 FILLER.
          05  WS-DUMMY                 PIC S9(8) COMP.
          05  WS-RETURN-CODE           PIC S9(8) COMP.
          05  WS-REASON-CODE           PIC S9(8) COMP.
          05  WS-INFO-CODE             PIC S9(8) COMP.
          05  WS-CPPL-ADDRESS          PIC S9(8) COMP.
          05  WS-FLAGS                 PIC X(4) VALUE X'00010001'.
          05  WS-BUFFER                PIC X(256).
          05  WS-LENGTH                PIC S9(8) COMP VALUE 256.
       01 JOB-NAME                     PIC X(8).
       01 PROC-STEP                    PIC X(8).
       01 STEP-NAME                    PIC X(8).
       01 JOB-NUMBER                   PIC X(8).
       01 PROGRAM-NAME                 PIC X(8).
       01 USER-ID                      PIC X(8).
       01 GROUP-NAME                   PIC X(8).
       01 USER-NAME                    PIC X(20).
       01 BATCH-OR-CICS                PIC X(5).
       01 FOUR-BYTES.
          05  FULL-WORD                PIC S9(8) COMP.
       01 JOB-NAMEX                    PIC X(8).

       01 SMTP-SERVER-1                PIC X(64) VALUE
           'SMTP.SERVER.1'.
       01 SMTP-SERVER-2                PIC X(64) VALUE
           'SMTP.SERVER.2'.
       01 SMTP-SERVER-3                PIC X(64) VALUE
           'SMTP.SERVER.3'.

       01 MESSAGE-TABLE.
          05 MESSAGE-TEXT OCCURS 30 TIMES.
              10 FILLER                PIC X(80).
       01 IDX                          PIC 9(2) COMP.

       LINKAGE SECTION.
       01 CB1.
          05 PTR1 POINTER OCCURS 256.
       01 CB2.
          05 PTR2 POINTER OCCURS 256.
       01  PARM.
           05 PARM-LENGTH        PIC S9(04) COMP SYNC.
           05 THE-PARM           PIC X(100).

       PROCEDURE DIVISION USING PARM.
           UNSTRING THE-PARM
           DELIMITED BY ',' INTO MAILBOX-ADDRESS
                            REPLYTO-ADDRESS
                            DEBUG-ACTION.
           INSPECT MAILBOX-ADDRESS REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT REPLYTO-ADDRESS REPLACING ALL LOW-VALUES BY SPACES.
           INSPECT DEBUG-ACTION REPLACING ALL LOW-VALUES BY SPACES.

           MOVE SPACES TO MESSAGE-TABLE.
           PERFORM GET-MESSAGE-TEXT
             VARYING IDX FROM 1 BY 1
               UNTIL IDX = 30.

           CALL 'IKJTSOEV' USING WS-DUMMY WS-RETURN-CODE WS-REASON-CODE
             WS-INFO-CODE WS-CPPL-ADDRESS.
           IF WS-RETURN-CODE > 0
             DISPLAY 'IKJTSOEV FAILED, RETURN-CODE=' WS-RETURN-CODE
               ' REASON-CODE=' WS-REASON-CODE 'INFO-CODE=' WS-INFO-CODE
             UPON CONSOLE
             MOVE WS-RETURN-CODE TO RETURN-CODE
             STOP RUN.

           MOVE SPACES TO WS-BUFFER.
           MOVE 'ALLOC DD(SYSOUT) SYSOUT(*) REUSE'
             TO WS-BUFFER.
           CALL 'IKJEFTSR' USING WS-FLAGS WS-BUFFER WS-LENGTH
             WS-RETURN-CODE WS-REASON-CODE WS-DUMMY.

           DISPLAY WS-BUFFER UPON CONSOLE.
           IF WS-RETURN-CODE > 0
             DISPLAY 'IKJEFTSR FAILED, RETURN-CODE=' WS-RETURN-CODE
               ' REASON-CODE=' WS-REASON-CODE
             UPON CONSOLE
             MOVE WS-RETURN-CODE TO RETURN-CODE
             STOP RUN.

           SET ADDRESS OF CB1 TO NULL.
           SET ADDRESS OF CB1 TO PTR1(136).
           SET ADDRESS OF CB2 TO PTR1(4).
           MOVE CB2(1:8) TO JOB-NAME.
           MOVE CB2(9:8) TO PROC-STEP.
           MOVE CB2(17:8) TO STEP-NAME.
           SET ADDRESS OF CB2 TO PTR1(46).
           MOVE CB2(361:8) TO PROGRAM-NAME.
           SET ADDRESS OF CB2 TO PTR2(80).
           MOVE CB2(13:8) TO JOB-NUMBER.
           SET ADDRESS OF CB2 TO PTR1(53).
           IF CB2(21:4) = LOW-VALUES THEN
             MOVE 'BATCH' TO BATCH-OR-CICS
           ELSE
             MOVE 'CICS ' TO BATCH-OR-CICS
           END-IF.
           SET ADDRESS OF CB1 TO NULL.
           SET ADDRESS OF CB1 TO PTR1(138).
           SET ADDRESS OF CB2 TO PTR1(28).
           MOVE CB2(193:8) TO USER-ID.
           SET ADDRESS OF CB2 TO PTR2(51).
           MOVE CB2(31:8) TO GROUP-NAME.
           SET ADDRESS OF CB1 TO PTR2(26).
           MOVE ZERO TO FULL-WORD.
           MOVE CB1(1:1) TO FOUR-BYTES(4:1).
           MOVE CB1(2:FULL-WORD) TO USER-NAME.
           MOVE JOB-NAME TO JOB-NAMEX.
           INSPECT JOB-NAMEX REPLACING ALL ' ' BY '@'.

           MOVE SPACES TO WS-BUFFER.
           IF DEBUG-ACTION IS NOT EQUAL TO 'DEBUG'
             STRING 'ALLOC DD(SYSPROC)'
                    ' DA('
                    QUOTE
                    'PPDD.'
                    JOB-NAMEX
                    '.REXX'
                    QUOTE
                    ') NEW REUSE DELETE RECFM(F B) LRECL(255)'
                    ' BLKSIZE(0) SPACE(1,1) CYLINDERS'
                    ' UNIT(SYSDA)'
             DELIMITED BY SIZE INTO WS-BUFFER
           ELSE
             STRING 'ALLOC DD(SYSPROC)'
                    ' DA('
                    QUOTE
                    'HLQ.'
                    JOB-NAMEX
                    '.REXX'
                    QUOTE
                    ') NEW REUSE CATALOG RECFM(F B) LRECL(255)'
                    ' BLKSIZE(0) SPACE(1,1) CYLINDERS'
                    ' UNIT(SYSDA)'
             DELIMITED BY SIZE INTO WS-BUFFER.
           CALL 'IKJEFTSR' USING WS-FLAGS WS-BUFFER WS-LENGTH
             WS-RETURN-CODE WS-REASON-CODE WS-DUMMY.

           IF DEBUG-ACTION IS EQUAL TO 'DEBUG'
             DISPLAY WS-BUFFER.
           IF WS-RETURN-CODE > 0
             DISPLAY 'IKJEFTSR FAILED, RETURN-CODE=' WS-RETURN-CODE
               ' REASON-CODE=' WS-REASON-CODE
             MOVE WS-RETURN-CODE TO RETURN-CODE
             STOP RUN.

           OPEN OUTPUT REXX-FILE.

           MOVE SPACES TO REXX-RECORD.
           MOVE '/* REXX */' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'TRACE O' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'CRLF = X2C("0D25")' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'JOBNAME = STRIP("'
                  JOB-NAME
                  '")'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'JOBID = STRIP("'
                  JOB-NUMBER
                  '")'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'SMTP_ADDRESS1 = STRIP("'
                  SMTP-SERVER-1
                  '")'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'SMTP_ADDRESS2 = STRIP("'
                  SMTP-SERVER-2
                  '")'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'SMTP_ADDRESS3 = STRIP("'
                  SMTP-SERVER-3
                  '")'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'SMTP_MAILBOX = STRIP("'
                  MAILBOX-ADDRESS
                  '")'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'SMTP_REPLYTO = STRIP("'
                  REPLYTO-ADDRESS
                  '")'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'SMTP_SUBJECT = "MESSAGE FROM "'
                  '||JOBNAME||"."||JOBID||"@TIAA-CREF.ORG"'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.

           MOVE 'STR = SOCKET("INITIALIZE",DATE(B))' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SOCKET","AF_INET","STREAM","TCP")'
             TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'PARSE VAR STR SOCKRC SOCKID' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'STR = SOCKET("SETSOCKOPT",SOCKID,"SOL_SOCKET",'
                  '"SO_ASCII","ON")'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'SERVER_INFO="AF_INET 25 "SMTP_ADDRESS1' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("CONNECT",SOCKID,SERVER_INFO)'
             TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("RECV",SOCKID,10000)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'MSG = "HELO "||SMTP_ADDRESS1||CRLF' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'SAY MSG' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,MSG)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("RECV",SOCKID,10000)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'PARSE VAR STR SOCKRC DATA_LENGTH SMTP_RESPONSE'
             TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'SAY "RECV:"STR' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE SPACES TO REXX-RECORD.
           STRING 'MSG = "MAIL FROM:<AUTOOPERATOR@TIAA-CREF.ORG>"'
                  '||CRLF'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,MSG)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("RECV",SOCKID,10000)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'PARSE VAR STR SOCKRC DATA_LENGTH SMTP_RESPONSE'
             TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'MSG = "RCPT TO:<"||SMTP_MAILBOX||">"||CRLF'
             TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,MSG)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("RECV",SOCKID,10000)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'PARSE VAR STR SOCKRC DATA_LENGTH SMTP_RESPONSE'
             TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'MSG = "DATA"||CRLF' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,MSG)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("RECV",SOCKID,10000)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'PARSE VAR STR SOCKRC DATA_LENGTH SMTP_RESPONSE'
             TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'MSG = "TO:"||SMTP_MAILBOX||CRLF,' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE '||"REPLY-TO:"||SMTP_REPLYTO||CRLF,' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE '||"SUBJECT:"SMTP_SUBJECT||CRLF,' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE '||"X-MAILER: REXX EXEC ON MVS"||CRLF' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,MSG)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,CRLF)' TO REXX-RECORD.
           WRITE REXX-RECORD.

           PERFORM WRITE-MESSAGE-TEXT
             VARYING IDX FROM 1 BY 1
               UNTIL IDX = 30.

           MOVE 'MSG = CRLF||"."||CRLF' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,MSG)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("RECV",SOCKID,10000)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'PARSE VAR STR SOCKRC DATA_LENGTH SMTP_RESPONSE'
             TO REXX-RECORD.
           WRITE REXX-RECORD.

           MOVE 'MSG = "QUIT"||CRLF' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,MSG)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("CLOSE",SOCKID)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("TERMINATE",SUBTASKID)' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'SAY "EMAIL SENT TO "||SMTP_MAILBOX' TO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'EXIT 0' TO REXX-RECORD.
           WRITE REXX-RECORD.
           CLOSE REXX-FILE.

           MOVE SPACES TO WS-BUFFER.
           STRING 'EXEC '
                  QUOTE
                  'PPDD.'
                  JOB-NAMEX
                  '.REXX'
                  QUOTE
           DELIMITED BY SIZE INTO WS-BUFFER.
           CALL 'IKJEFTSR' USING WS-FLAGS WS-BUFFER WS-LENGTH
             WS-RETURN-CODE WS-REASON-CODE WS-DUMMY.

           IF DEBUG-ACTION IS EQUAL TO 'DEBUG'
             DISPLAY WS-BUFFER.
           IF WS-RETURN-CODE > 0
             DISPLAY 'IKJEFTSR FAILED, RETURN-CODE=' WS-RETURN-CODE
             ' REASON-CODE=' WS-REASON-CODE
             MOVE WS-RETURN-CODE TO RETURN-CODE
             STOP RUN.

           MOVE SPACES TO WS-BUFFER.
           MOVE 'FREE DD(SYSPROC)' TO WS-BUFFER.
           CALL 'IKJEFTSR' USING WS-FLAGS WS-BUFFER WS-LENGTH
             WS-RETURN-CODE WS-REASON-CODE WS-DUMMY.

           IF DEBUG-ACTION IS EQUAL TO 'DEBUG'
             DISPLAY WS-BUFFER.
           IF WS-RETURN-CODE > 0
             DISPLAY 'IKJEFTSR FAILED, RETURN-CODE=' WS-RETURN-CODE
             ' REASON-CODE=' WS-REASON-CODE
             MOVE WS-RETURN-CODE TO RETURN-CODE
             STOP RUN.

           MOVE 0 TO RETURN-CODE.
           STOP RUN.

       GET-MESSAGE-TEXT.
           ACCEPT MESSAGE-TEXT(IDX) FROM SYSIN.

       WRITE-MESSAGE-TEXT.
           MOVE SPACES TO REXX-RECORD.
           STRING 'MSG = STRIP("'
                  MESSAGE-TEXT(IDX)
                  '")||CRLF'
           DELIMITED BY SIZE INTO REXX-RECORD.
           WRITE REXX-RECORD.
           MOVE 'STR = SOCKET("SEND",SOCKID,MSG)' TO REXX-RECORD.
           WRITE REXX-RECORD.
