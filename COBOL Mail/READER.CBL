001000 @OPTIONS NOALPHA
001010 @OPTIONS MAIN
001020
001030 *>
001040 *>
001050 *>  Source Module: READER.CBL
001060 *>
001070 *>  Last Modified: September 1, 1999
001080 *>
001090 *>  Author:  MDM
001100 *>
001110 *>  Must be linked with SEE32.LIB
001120 *>
001130 *> IMPORTANT:  Edit POP3_SERVER, POP3_USER, and POP3_PASS
001140 *>    with appropriate strings before compiling.
001150 *>    See SEE4CB_U.TXT and SEE4CB_R.TXT manuals.
001160 *>
001170
001180 IDENTIFICATION DIVISION.
001190 PROGRAM-ID.    READER.
001200 AUTHOR.        Mike Marshall.
001210 INSTALLATION.  Fujitsu COBOL.
001220
001230 ENVIRONMENT DIVISION.
001240 CONFIGURATION SECTION.
001250 SOURCE-COMPUTER. Fujitsu.
001260 OBJECT-COMPUTER. Fujitsu.
001270 SPECIAL-NAMES.
001280
001290 COPY "SEE32.CBI".
001300
001310 DATA DIVISION.
001320
001330 WORKING-STORAGE SECTION.
001340
001350 COPY "KEYCODE.CBI".
001360  01  SEE_CODE     PIC S9(9) COMP-5 VALUE 0.
001370  01  IS_ZERO      PIC 9(9)  COMP-5 VALUE 0.
001380  01  BUFFER       PIC X(80).
001390  01  BUF_LEN      PIC 9(9)  COMP-5 VALUE 80.
001400  01  NBR_CHANS    PIC S9(9) COMP-5 VALUE 1.
001410  01  CHANNEL      PIC S9(9) COMP-5 VALUE 0.
001420  01  POP3_SERVER.
001430      05  FILLER PIC X(8) VALUE "10.0.0.1".
001440      05  FILLER PIC X VALUE X'00'.
001450  01  POP3_USER.
001460      05  FILLER PIC X(4) VALUE "mike".
001470      05  FILLER PIC X VALUE X'00'.
001480  01  POP3_PASS.
001490      05  FILLER PIC X(4) VALUE "mike".
001500      05  FILLER PIC X VALUE X'00'.
001510  01  CHAR_SET.
001520      02  CHARS PIC X(35) VALUE "123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
001530      02  FILLER REDEFINES CHARS.
001540          03 WORK PIC X OCCURS 35 TIMES.
001550  01  EMAIL_NAME.
001560      02  FILLER PIC X(6) VALUE "EMAIL_".
001570      02  SECOND PIC X(1) VALUE "A".
001580      02  FILLER PIC X(4) VALUE ".MAI".
001590      02  FILLER PIC X VALUE X'00'.
001600  01  EMAIL_DIR.
001610      05  FILLER PIC X(6) VALUE ".\".
001620      05  FILLER PIC X    VALUE X'00'.
001630  01  ATTACH_DIR.
001640      05  FILLER PIC X(6) VALUE ".\".
001650      05  FILLER PIC X    VALUE X'00'.
001660  01  NBR_MESSAGES PIC S9(9) COMP-5 VALUE 0.
001670  01  COUNTER PIC S9(9) COMP-5 VALUE 1.
001680  01  RESULT.
001690      05  FILLER PIC X(55).
001700      05  FILLER PIC X VALUE X'00'.
001710  01  RESULT-LEN PIC S9(9) COMP-5 VALUE 55.
001720
001730 PROCEDURE DIVISION.
001740
001750      DISPLAY "READER.CBL Program"
001760      DISPLAY " "
001770
001780      DISPLAY "Server is " POP3_SERVER
001790      DISPLAY "User is " POP3_USER
001800      DISPLAY " "
001810
001820      *> attach SEE
001830      CALL "seeAttach" WITH STDCALL USING
001840           BY VALUE     NBR_CHANS     *> number of channels
001850           BY VALUE     SEE_KEY_CODE  *> See KEYCODE.CBI
001860      END-CALL.
001870
001880      *> connect to POP3 server
001890      DISPLAY "Calling seePop3Connect()..."
001900      CALL "seePop3Connect" WITH STDCALL USING
001910           BY VALUE     CHANNEL       *> channel
001920           BY REFERENCE POP3_SERVER   *> POP3 server name
001930           BY REFERENCE POP3_USER     *> user name
001940           BY REFERENCE POP3_PASS     *> user password
001950      END-CALL.
001960
001970      *> check result
001980      IF PROGRAM-STATUS < 0 THEN
001990        DISPLAY "Cannot connect to POP3 server"
002000        MOVE PROGRAM-STATUS TO SEE_CODE
002010        GO TO ERROR-EXIT
002020      END-IF.
002030
002040      *> get # messages waiting
002050      DISPLAY "Calling seeGetEmailCount()..."
002060      CALL "seeGetEmailCount" WITH STDCALL USING
002070           BY VALUE     CHANNEL       *> channel
002080      END-CALL.
002090
002100      *> check result
002110      IF PROGRAM-STATUS < 0 THEN
002120        DISPLAY "Error getting email count"
002130        MOVE PROGRAM-STATUS TO SEE_CODE
002140        GO TO ERROR-EXIT
002150      END-IF.
002160
002170      MOVE PROGRAM-STATUS TO NBR_MESSAGES
002180      DISPLAY NBR_MESSAGES " messages waiting on server."
002190
002200      MOVE 1 TO COUNTER.
002210
002220  PROCESS-LOOP.
002230
002240      *> any more email messages ?
002250      IF COUNTER > NBR_MESSAGES THEN
002260        GO TO PROCESS-DONE
002270      END-IF.
002280
002290      *> construct email message name
002300      MOVE WORK(COUNTER) TO SECOND
002310      *> read email message
002320      DISPLAY "Calling seeGetEmailFile()..."
002330      CALL "seeGetEmailFile" WITH STDCALL USING
002340         BY VALUE     CHANNEL       *> channel
002350         BY VALUE     COUNTER       *> message number (1,2,...)
002360         BY REFERENCE EMAIL_NAME    *> email filename
002370         BY REFERENCE EMAIL_DIR     *> email directory
002380         BY REFERENCE ATTACH_DIR    *> attachment directory
002390      END-CALL
002400      DISPLAY "Email saved as " EMAIL_NAME
002410
002420      ADD 1 TO COUNTER
002430      GO TO PROCESS-LOOP.
002440
002450  PROCESS-DONE.
002460
002470
002480      *> close SEE
002490      DISPLAY "Calling seeClose()..."
002500      CALL "seeClose" WITH STDCALL USING
002510         BY VALUE     CHANNEL       *> channel
002520      END-CALL
002530
002540      *> release SEE
002550      DISPLAY "Calling seeRelease()..."
002560      CALL "seeRelease" WITH STDCALL
002570      END-CALL
002580
002590      DISPLAY "All done."
002600      STOP RUN.
002610
002620  ERROR-EXIT.
002630
002640      DISPLAY "SEE4CB Error " SEE_CODE
002650      CALL "seeErrorText" WITH STDCALL USING
002660         BY VALUE     CHANNEL       *> channel
002670         BY VALUE     SEE_CODE
002680         BY REFERENCE BUFFER
002690         BY VALUE     BUF_LEN
002700      END-CALL
002710      DISPLAY BUFFER
002720      STOP RUN.
002730
002740 END PROGRAM READER.
002750
002760
