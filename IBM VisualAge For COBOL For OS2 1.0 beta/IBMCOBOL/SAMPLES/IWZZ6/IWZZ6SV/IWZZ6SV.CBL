
     *************************************************************
      *                                                           *
      *    Licensed Materials - Property of IBM                   *
      *                                                           *
      *    28H2177  (C) Copyright IBM Corp. 1994, 1995.           *
      *    All rights reserved                                    *
      *                                                           *
      *    US Government Users Restricted Rights - Use,           *
      *    duplication, or disclosure restricted by GSA ADP       *
      *    Schedule Contract with IBM Corp.                       *
      *                                                           *
      *************************************************************

      *************************************************************
       IDENTIFICATION DIVISION.
      *************************************************************
       PROGRAM-ID.    IWZZ6SV.
       AUTHOR.        Programmer.

      *************************************************************
      *NAME......IWZZ6SV                                        ***
      *                                                         ***
      *LANGUAGE.... COBOL                                       ***
      *                                                         ***
      *SETUP: This server program accesses a VSAM file for      ***
      *       the Employee lookup data.  This VSAM file can     ***
      *       reside on your local machine or on the MVS Host.  ***
      *       In order to successfully run this sample program, ***
      *       you will need to create the VSAM file outside this***
      *       program using file layout found in IWZZ6SV-EMP-DATA**
      *                                                         ***
      *       In addition, you will need to set the environment ***
      *       variabel DDDIRECT to your drive\directory\file name**
      *       The DDDIRECT variable should be added in the      ***
      *       Tools Settings notebook of the GUI project.       ***
      *       This Project has been defined with a "dummy"      ***
      *       DDDIRECT variable set to C:\SAMPLE\IWZZ6\MYVSAM.FIL *
      *                                                         ***
      *Function: IWZZ6SV performs the processing for            ***
      *          the Employee Lookup client/server application. ***
      *          IWZZ6SV is called by the GUI program           ***
      *          IWZZ6GU.                                       ***
      *                                                         ***
      *                                                         ***
      *          This program returns a list of employees       ***
      *          with associated data based on the GUI's        ***
      *          request.  The GUI's requests include:          ***
      *            o return all employees                       ***
      *            o return all employees that match the        ***
      *              search criteria exactly                    ***
      *            o return all employees that match the        ***
      *              search criteria partially, that is, if the ***
      *              client specifies ABC for last name, return ***
      *              all employees with last names beginning    ***
      *              with the letters ABC                       ***
      *          The search criteria which the client can       ***
      *          specify includes the employee's last name,     ***
      *          department or a combination of both.           ***
      *                                                         ***
      *External Subroutines: None                               ***
      *                                                         ***
      *COPY Members:                                            ***
      *          IWZZ6SI.CPY Interface mapping                  ***
      *                                                         ***
      *Last update: 09/20/95                                    ***
      *                                                         ***
      *************************************************************

      *************************************************************
       ENVIRONMENT DIVISION.
      *************************************************************
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IWZZ6SV-EMP-DATA ASSIGN to DDDIRECT
                  ORGANIZATION is INDEXED
                  ACCESS MODE IS DYNAMIC
                  RECORD KEY  is EMP-NAME
                  FILE STATUS is EMP-FILE-STAT.

      *************************************************************
       DATA DIVISION.
      *************************************************************
       FILE SECTION.
       FD  IWZZ6SV-EMP-DATA
           RECORD CONTAINS 47 CHARACTERS.
       01  EMP-DATA-REC.
           05 EMP-NAME.
              10 EMP-LAST-NAME          PIC X(15).
              10 EMP-FIRST-NAME         PIC X(10).
              10 EMP-MIDDLE-INITIAL     PIC X(1).
           05 EMP-DEPT                  PIC X(3).
           05 EMP-PHONE                 PIC X(12).
           05 EMP-HIRE-DATE             PIC 9(6).

       WORKING-STORAGE SECTION.

      *************************************************************
      *  Internal variables                                       *
      *************************************************************
       01  EMP-FILE-STAT          PIC x(02) value SPACES.
              88  STATUS-OK        VALUE "00".
              88  END-OF-FILE      VALUE "10".
       01  ARRAY-MAX-ENTRIES.
           05  EMP-ARRAY-MAX          PIC 9(2) VALUE 12.
           05  RESULT-DATA-MAX        PIC 9(2) VALUE 0.

       01  PROGRAM-WORK-FIELDS.
           05  RESULTS-PTR            PIC 9(2).
           05  EMP-PTR                PIC 9(2).
           05  BLANK-COUNT            PIC 9(2).
           05  LASTNAME-LENGTH        PIC 9(2).
           05  DEPT-LENGTH            PIC 9(2).

       LINKAGE SECTION.
      ******************************************************
      *     Subroutine pass area from the GUI VDE program  *
      *     Dynamic call from GUI-SAMP.                    *
      ******************************************************

       01 CS-LINKAGE.
           COPY IWZZ6SI.

      *************************************************************
       PROCEDURE DIVISION USING CS-LINKAGE.
      *************************************************************

      *************************************************************
      * 1000-MAIN: Main Processing
      *
      *
      *   Process the appropriate request type:
      *     D: Return all entries (1100-DISPLAY-ALL)
      *     P: Return entries that match search data that is fully
      *        qualified or consists of leading characters.
      *        (1200-PARTIAL-MATCH)
      *     E: Return entries that match fully qualified search data.
      *        (1300-EXACT-MATCH)
      *     If matching entries are found, the data will be moved
      *     from the employee database (EMP-ARRAY) to the interface
      *     area (CS-RESULT-DATA).
      *
      *   Return code is set if matches were found or not found.
      *************************************************************

       1000-MAIN.

           OPEN INPUT IWZZ6SV-EMP-DATA.
           IF STATUS-OK
              CONTINUE
           ELSE
              PERFORM 1500-VSAM-FILE-ERROR thru
                      1500-VSAM-FILE-ERROR-EXIT.

      * Initialize CS-RESULT-DATA
            INITIALIZE CS-RESULT-DATA
            MOVE 0 TO RESULT-DATA-MAX

      * Initialize return code
            MOVE 0 TO CS-RETURN-CODE

      * Process the appropriate request and
      * load matched entries from EMP-ARRAY to CS-RESULT-DATA.
      * Set return code if match is found or not found.

            EVALUATE TRUE
              WHEN CS-DISPLAY-ALL
                PERFORM 1100-DISPLAY-ALL THRU
                                  1100-DISPLAY-ALL-EXIT
              WHEN CS-PARTIAL-MATCH
                PERFORM 1200-PARTIAL-MATCH THRU
                                  1200-PARTIAL-MATCH-EXIT
              WHEN CS-EXACT-MATCH
                PERFORM 1300-EXACT-MATCH THRU
                                  1300-EXACT-MATCH-EXIT
            END-EVALUATE.

      ** Return to (the calling gui program)

           CLOSE IWZZ6SV-EMP-DATA.

           IF STATUS-OK
              CONTINUE
           ELSE
              PERFORM 1500-VSAM-FILE-ERROR thru
                      1500-VSAM-FILE-ERROR-EXIT.
      ****************************************************************
      ** Return to the Calling GUI program                          **
      ****************************************************************

           GOBACK.

       1000-MAIN-EXIT. EXIT.

      *************************************************************
      * 1100-DISPLAY-ALL:
      *   Return all of the entries in EMP-ARRAY.
      *   Move all of the entries from EMP-DATA to CS-RESULT-DATA.
      *************************************************************

       1100-DISPLAY-ALL.

      * Initialize subscripts
           MOVE 1 TO RESULTS-PTR.

           PERFORM UNTIL END-OF-FILE
            PERFORM 1400-READ-VSAM-FILE THRU 1400-READ-VSAM-FILE-EXIT

            IF NOT END-OF-FILE
               MOVE EMP-DATA-REC TO CS-RESULT-DETAILS(RESULTS-PTR)
               ADD 1 TO RESULTS-PTR
            END-IF
           END-PERFORM.
      * Indicate number of entries processed
            MOVE EMP-ARRAY-MAX TO RESULT-DATA-MAX.

       1100-DISPLAY-ALL-EXIT. EXIT.


      *************************************************************
      * 1200-PARTIAL-MATCH:
      *   Process search fields that are fully qualified of
      *   consist of leading characters.
      *   Determine the lengths of the Lastname and Department
      *   fields on the screen.                 (1210-FIND-LENGTHS)
      *   Move matching entries from EMP-DATA to CS-RESULT-DATA.
      *************************************************************

       1200-PARTIAL-MATCH.

      *   Determine the lengths of the client's inputs: Lastname, Dept
           PERFORM 1210-FIND-LENGTHS THRU 1210-FIND-LENGTHS-EXIT.

      *   Initialize subscripts
           MOVE 1 TO RESULTS-PTR.

      *   Search through Employee data
           PERFORM UNTIL END-OF-FILE
            PERFORM 1400-READ-VSAM-FILE THRU 1400-READ-VSAM-FILE-EXIT

            IF END-OF-FILE
               MOVE LOW-VALUES TO EMP-LAST-NAME EMP-DEPT
            END-IF

      *   Lastname not specified
              IF CS-LASTNAME = SPACES
      *   Dept only is specified
                IF CS-DEPT (1:DEPT-LENGTH) =
                     EMP-DEPT(1:DEPT-LENGTH)
                  MOVE EMP-DATA-REC   TO
                       CS-RESULT-DETAILS(RESULTS-PTR)
                  ADD 1 TO RESULTS-PTR
                ELSE
                  CONTINUE
                END-IF

      *   Lastname specified
              ELSE
              IF CS-LASTNAME  (1:LASTNAME-LENGTH) =
                 EMP-LAST-NAME(1:LASTNAME-LENGTH)

      *   Lastname only was specified
                  IF CS-DEPT = SPACES
                    MOVE EMP-DATA-REC  TO
                       CS-RESULT-DETAILS(RESULTS-PTR)
                    ADD 1 TO RESULTS-PTR
                  ELSE

      *   Lastname and Dept both specified
                  IF CS-DEPT (1:DEPT-LENGTH) =
                      EMP-DEPT(1:DEPT-LENGTH)
                      MOVE EMP-DATA-REC TO
                         CS-RESULT-DETAILS(RESULTS-PTR)
                      ADD 1 TO RESULTS-PTR
                  END-IF
              END-IF
             END-IF
            END-IF
           END-PERFORM.

      ***************************************************
      * Check for any matches, if not set Return code  **
      ***************************************************

            IF RESULTS-PTR  = 1
              MOVE 1 TO CS-RETURN-CODE
            END-IF.

       1200-PARTIAL-MATCH-EXIT. EXIT.

      *************************************************************
      * 1210-FIND-LENGTHS: Determine length of Last Name and Dept
      *   Determine length of Last Name and Dept fields entered
      *   by the client.
      *   The backward search for the first non-blank character
      *   is done to allow imbedded blanks.
      *   Currently, Lastname is up to 15 characters and Dept is
      *   up to 3 characters.
      *************************************************************

       1210-FIND-LENGTHS.

      * Determine length of Last Name field
            IF CS-LASTNAME = SPACES
      * Lastname not specified.
              MOVE 0 TO LASTNAME-LENGTH
            ELSE
              INITIALIZE BLANK-COUNT
      * Search for trailing blanks by reversing the
      * input characters using COBOL/370 intrinsic function REVERSE
              INSPECT FUNCTION REVERSE(CS-LASTNAME)
              TALLYING BLANK-COUNT FOR LEADING SPACES
              COMPUTE LASTNAME-LENGTH = 15 - BLANK-COUNT
            END-IF

      * Determine length of Department field
            IF CS-DEPT = SPACES
      * Dept not specified.
              MOVE 0 TO DEPT-LENGTH
            ELSE
              INITIALIZE BLANK-COUNT
      * Facilitate search for trailing blanks by reversing the
      * input characters using COBOL/370 intrinsic function REVERSE
              INSPECT FUNCTION REVERSE(CS-DEPT)
              TALLYING BLANK-COUNT FOR LEADING SPACES
              COMPUTE DEPT-LENGTH = 3 - BLANK-COUNT
            END-IF.

       1210-FIND-LENGTHS-EXIT. EXIT.

      *************************************************************
      * 1300-EXACT-MATCH:                                         *
      *   Process search fields that are fully qualified.         *
      *   Move matching entries from EMP-DATA to CS-RESULT-DATA.  *
      *   This procedure uses a simple sequential search          *
      *   because there are currently only twelve records.        *
      *************************************************************

       1300-EXACT-MATCH.

      *  Initialize subscripts.
           MOVE 1 TO RESULTS-PTR.

           PERFORM UNTIL END-OF-FILE
            PERFORM 1400-READ-VSAM-FILE THRU 1400-READ-VSAM-FILE-EXIT

            IF END-OF-FILE
               MOVE LOW-VALUES TO EMP-LAST-NAME EMP-DEPT
            END-IF

      *  Lastname not specified
            IF CS-LASTNAME = SPACES
      *  Only Dept specified
                IF CS-DEPT = EMP-DEPT
                  MOVE EMP-DATA-REC     TO
                       CS-RESULT-DETAILS(RESULTS-PTR)
                  ADD 1 TO RESULTS-PTR
      *  Dept did not match. Continue search.
                ELSE
                  CONTINUE
                END-IF
      *  Lastname specified
              ELSE
                IF CS-LASTNAME = EMP-LAST-NAME
      *  Only Lastname specified
                  IF CS-DEPT = SPACES
                    MOVE EMP-DATA-REC   TO
                         CS-RESULT-DETAILS(RESULTS-PTR)
                    ADD 1 TO RESULTS-PTR
                  ELSE
      *  Lastname and Dept specified
                  IF CS-DEPT = EMP-DEPT
                    MOVE EMP-DATA-REC   TO
                           CS-RESULT-DETAILS(RESULTS-PTR)
                    ADD 1 TO RESULTS-PTR
                  END-IF
                END-IF
             END-IF
            END-IF
           END-PERFORM.
      ***********************************************************
      * Check for any matches, if not set Return code          **
      ***********************************************************

            IF RESULTS-PTR  = 1
              MOVE 1 TO CS-RETURN-CODE
            END-IF.

       1300-EXACT-MATCH-EXIT. EXIT.

      ***********************************************************
      * Common VSAM Read of the Employee file Status 00 and 10 OK
      ***********************************************************

       1400-READ-VSAM-FILE.

           READ IWZZ6SV-EMP-DATA NEXT RECORD.
           IF STATUS-OK
              CONTINUE
           ELSE
            IF END-OF-FILE
              CONTINUE
            ELSE
              PERFORM 1500-VSAM-FILE-ERROR thru
                      1500-VSAM-FILE-ERROR-EXIT
             END-IF.

       1400-READ-VSAM-FILE-EXIT.  EXIT.

      ************************************************************
      *Setting CS-RETURN-CODE to 2 causes DATABASE error         *
      * to display in a separate GUI window                      *
      ************************************************************
       1500-VSAM-FILE-ERROR.


           MOVE 2 TO CS-RETURN-CODE.
           CLOSE IWZZ6SV-EMP-DATA.

           GOBACK.

       1500-VSAM-FILE-ERROR-EXIT.   EXIT.
