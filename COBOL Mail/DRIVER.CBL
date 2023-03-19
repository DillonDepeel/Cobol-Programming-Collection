001000 @OPTIONS NOALPHA
001010 @OPTIONS MAIN
001020
001030 *>
001040 *>
001050 *>  Source Module: DRIVER.CBL
001060 *>
001070 *>  Last Modified: September 1, 1999.
001080 *>
001090 *>  Author:  MDM
001100 *>
001110 *>  Must be linked with SEE32.LIB
001120 *>
001130 *> IMPORTANT:  Edit SMTP_SERVER, EMAIL_TO, EMAIL_FROM, EMAIL_SUBJ,
001140 *>    EMAIL_MSG, and EMAIL_ATTACH with appropriate strings before.
001150 *>    compiling. See SEE4CB_U.TXT and SEE4CB_R.TXT manuals.
001160 *>
001170
001180 IDENTIFICATION DIVISION.
001190 PROGRAM-ID.    DRIVER.
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
001320 WORKING-STORAGE SECTION.
001330 COPY "KEYCODE.CBI".
001340  01  NBR_CHANS    PIC S9(9) COMP-5 VALUE 1.
001350  01  AUTO_CALL    PIC 9(9) COMP-5 VALUE 9.
001360  01  IS_ZERO      PIC 9(9) COMP-5 VALUE 0.
001370  01  IS_ONE       PIC 9(9) COMP-5 VALUE 1.
001380  01  BUFFER       PIC X(80).
001390  01  BUF_LEN      PIC 9(9) COMP-5 VALUE 80.
001400  01  SEE_CODE     PIC S9(9) COMP-5 VALUE 0.
001410  01  CHANNEL      PIC S9(9) COMP-5 VALUE 0.
001420  01  BYTES_SENT   PIC S9(9) COMP-5 VALUE 0.
001430  01  SMTP_SERVER.
001440      05  FILLER PIC X(8) VALUE "10.0.0.1".
001450      05  FILLER PIC X VALUE X'00'.
001460  01  EMAIL_TO.
001470      05  FILLER PIC X(15) VALUE "<mike@10.0.0.1>".
001480      05  FILLER PIC X VALUE X'00'.
001490  01  EMAIL_FROM.
001500      05  FILLER PIC X(15) VALUE "<mike@10.0.0.1>".
001510      05  FILLER PIC X VALUE X'00'.
001520  01  EMAIL_SUBJ.
001530      05  FILLER PIC X(18) VALUE "Attaching TEST.ZIP".
001540      05  FILLER PIC X VALUE X'00'.    
001550  01  EMAIL_MSG.
001560      05  FILLER PIC X(9) VALUE "@test.mai".
001570      05  FILLER PIC X VALUE X'00'. 
001580  01  EMAIL_ATTACH.
001590      05  FILLER PIC X(8) VALUE "test.zip".
001600      05  FILLER PIC X VALUE X'00'.
001610  01  RESULT.
001620      05  FILLER PIC X(40).
001630      05  FILLER PIC X VALUE X'00'.
001640  01  COUNTER    PIC 9(9) COMP-5 VALUE 0.
001650
001660 PROCEDURE DIVISION.
001670 
001680      DISPLAY "DRIVER.CBL Program"
001690      DISPLAY " " 
001700
001710      DISPLAY "Server is " SMTP_SERVER
001720      DISPLAY "Email To "  EMAIL_TO
001730      DISPLAY "Email From "  EMAIL_FROM
001740      DISPLAY "Email Subject "  EMAIL_SUBJ
001750      DISPLAY " "
001760
001770      *> attach SEE
001780      CALL "seeAttach" WITH STDCALL USING
001790           BY VALUE     NBR_CHANS     *> number of channels
001800           BY VALUE     SEE_KEY_CODE  *> See KEYCODE.CBI
001810      END-CALL.
001820
001830      *> connect to SMTP server
001840      DISPLAY "Calling seeSmptConnect()..."
001850      CALL "seeSmtpConnect" WITH STDCALL USING
001860           BY VALUE     CHANNEL       *> channel
001870           BY REFERENCE SMTP_SERVER   *> SMTP server name
001880           BY REFERENCE EMAIL_FROM    *> sender's email address
001890           BY REFERENCE EMAIL_ATTACH  *> no Reply-To header
001900      END-CALL.
001910
001920      *> check result
001930      IF PROGRAM-STATUS < 0 THEN
001940        DISPLAY "Cannot connect to SMTP server"
001950        MOVE PROGRAM-STATUS TO SEE_CODE
001960        GO TO ERROR-EXIT
001970      END-IF.
001980 
001990      *> turn OFF automatic calls to driver for seeSendEmail
002000      CALL "seeIntegerParam" WITH STDCALL USING
002010           BY VALUE     CHANNEL       *> channel
002020           BY VALUE     AUTO_CALL     *> Command   
002030           BY VALUE     IS_ZERO       *> off
002040      END-CALL.
002050
002060      *> set up to send email
002070      DISPLAY "Calling seeSendEmail()..."
002080      CALL "seeSendEmail" WITH STDCALL USING
002090           BY VALUE     CHANNEL       *> channel
002100           BY REFERENCE EMAIL_TO      *> receipient
002110           BY REFERENCE IS_ZERO       *> no CC list
002120           BY REFERENCE IS_ZERO       *> no BCC list
002130           BY REFERENCE EMAIL_SUBJ    *> email subject
002140           BY REFERENCE EMAIL_MSG     *> text of message
002150           BY REFERENCE EMAIL_ATTACH  *> attachment
002160      END-CALL.
002170
002180      *> check result of call to seeSendEmail
002190      IF PROGRAM-STATUS < 0 THEN
002200        DISPLAY "Cannot send email"
002210        MOVE PROGRAM-STATUS TO SEE_CODE
002220        GO TO ERROR-EXIT
002230      END-IF.   
002240        
002250      *> call driver
002260
002270  DRIVER-LOOP.      
002280 
002290      CALL "seeDriver" WITH STDCALL USING
002300        BY VALUE     CHANNEL       *> channel
002310      END-CALL.
002320
002330      *> check results
002340      MOVE PROGRAM-STATUS TO SEE_CODE
002350      IF SEE_CODE < 0 THEN
002360        DISPLAY "Error returned from seeDriver"       
002370        GO TO ERROR-EXIT
002380      END-IF.   
002390
002400      IF SEE_CODE >= 0 THEN
002410        *> don't count wait states (9999)
002420        IF SEE_CODE NOT EQUAL 9999 THEN
002430          *> check every 10th call
002440          ADD 1 TO COUNTER
002450          IF (COUNTER = 10) OR (SEE_CODE = 0) THEN
002460            *> ask for bytes sent
002470            CALL "seeStatistics" WITH STDCALL USING
002480              BY VALUE     CHANNEL
002490              BY VALUE     SEE_GET_TOTAL_BYTES_SENT 
002500            END-CALL
002510            *> display bytes sent if changed
002520            IF PROGRAM-STATUS NOT = BYTES_SENT THEN
002530              MOVE PROGRAM-STATUS TO BYTES_SENT
002540              DISPLAY BYTES_SENT " bytes sent."
002550            END-IF
002560            MOVE IS_ZERO TO COUNTER
002570          END-IF
002580        END-IF
002581        IF SEE_CODE > 0 THEN
002590          GO TO DRIVER-LOOP
002591        END-IF
002600      END-IF. 
002610
002620      *> SEE_CODE was 0
002630      DISPLAY "Email sent." 
002640
002650      *> turn ON automatic calls to driver
002660      CALL "seeIntegerParam" WITH STDCALL USING
002670           BY VALUE     CHANNEL       *> channel
002680           BY VALUE     AUTO_CALL     *> Command   
002690           BY VALUE     IS_ONE        *> off
002700      END-CALL.
002710
002720      *> close SEE
002730      DISPLAY "Calling seeClose()..."
002740      CALL "seeClose" WITH STDCALL USING
002750         BY VALUE     CHANNEL       *> channel
002760      END-CALL
002770
002780      *> release SEE
002790      CALL "seeRelease" WITH STDCALL 
002800      END-CALL
002810      
002820      DISPLAY "All done."
002830      STOP RUN.
002840        
002850  ERROR-EXIT.
002860
002870      DISPLAY "SEE4CB Error " SEE_CODE
002880      CALL "seeErrorText" WITH STDCALL USING 
002890         BY VALUE     CHANNEL       *> channel
002900         BY VALUE     SEE_CODE
002910         BY REFERENCE BUFFER
002920         BY VALUE     BUF_LEN
002930      END-CALL
002940      DISPLAY BUFFER
002950      STOP RUN.
002960
002970 END PROGRAM DRIVER.
002980 
002990
