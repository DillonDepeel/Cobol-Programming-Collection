      **********************************************************
      *
      *  PRODUCT = Data Description and Conversion for OS/2
      *
      *  SOURCE FILE NAME = IWZZSS2.CBL
      *
      *  DESCRIPTIVE NAME = Conversion Plan Executor Example
      *
      *  FUNCTION = This sample program calls the function of the
      *              conversion plan executor to convert data based on
      *              the conversion plans created by the conversion
      *              plan builder in program IWZZSS1.
      *              In this sample the hex string C1C2C301234C will
      *              be converted with specified plan COBOL_TO_C into
      *              the hex string 41424300D204. Then the hex string
      *              41424300d204 will be converted with the specified  
      *              plan C_TO_COBOL into the hex string C1C2C301234C.
      *              The result will be printed on screen.
      *
      **********************************************************
        IDENTIFICATION DIVISION.
        PROGRAM-ID. IWZZSS2.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. IBM-PS2.
        OBJECT-COMPUTER. IBM-PS2.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT Sample-SPC ASSIGN TO IWZZSS1F
                              ORGANIZATION IS SEQUENTIAL.
        DATA DIVISION.
        FILE SECTION.
        FD  Sample-SPC
                 RECORD CONTAINS 5000 CHARACTERS
                 RECORDING MODE IS F.
        01  CnvPlnSpc  PICTURE X(5000).
        WORKING-STORAGE SECTION.
      **********************************************************
      *  Parameters for FMTCPXI, FMTCPXC, and FMTCPXT
      **********************************************************
        77  CnvPlnSpcHdl PICTURE 9(6) USAGE IS BINARY.
        77  PlnNamLength PICTURE 9(6) USAGE IS BINARY.
        77  PlnNam PICTURE X(255).
        77  InputParmNum PICTURE 9(6) USAGE IS BINARY.
        77  OutputParmNum PICTURE 9(6) USAGE IS BINARY.
        01  InputData.
             05  InputDataPtr USAGE IS POINTER OCCURS 32 TIMES.
        01  OutputData.
             05  OutputDataPtr USAGE IS POINTER OCCURS 32 TIMES.
        01     FeedBack.
             05 Condition-ID.
                10 MsgSev  PICTURE 9(2) USAGE IS BINARY.
                10 MsgNo    PICTURE 9(2) USAGE IS BINARY.
             05 Case-Severity-Control  PICTURE X.
             05 Facility-ID  PICTURE X(3).
             05 I-S-Info.
                10 AdlExId  PICTURE 9(6) USAGE IS BINARY.
                10 AdlCommAreaPtr REDEFINES AdlExId USAGE IS POINTER.
                10 User-ExitCtokPtr  REDEFINES AdlExId USAGE IS POINTER.
      **********************************************************
      *  ADL Communication Area
      **********************************************************
        01     AdlCommArea.
            05 AdlCALength  PICTURE 9(6) USAGE IS BINARY.
            05 ExId  PICTURE 9(6) USAGE IS BINARY.
            05 SevCod  PICTURE 9(2) USAGE IS BINARY.
            05 PlanId.
                10  PreLength  PICTURE 9(2) USAGE IS BINARY.
                10  CharData  PICTURE X(255).
            05 PlanStmt  PICTURE 9(6) USAGE IS BINARY.
            05 InpErrDta.
                10  PreLength  PICTURE 9(2) USAGE IS BINARY.
                10  CharData  PICTURE X(255).
            05 SrcFldID.
                10  PreLength  PICTURE 9(2) USAGE IS BINARY.
                10  CharData  PICTURE X(255).
            05 TrgFldId.
                10  PreLength  PICTURE 9(2) USAGE IS BINARY.
                10  CharData  PICTURE X(255).
      **********************************************************
      *  Return Codes and ADL Exception Codes
      **********************************************************
        77  CPX-NO-ERROR  PICTURE 9(6) USAGE IS BINARY
                          VALUE IS 0.
        77  CPX-ADL-EXCEPTION-SEV2  PICTURE 9(6) USAGE IS BINARY
                          VALUE IS 102.
        77  CPX-ADL-EXCEPTION-SEV3  PICTURE 9(6) USAGE IS BINARY
                          VALUE IS 103.
        77  CpxType  PICTURE X(7).
      **********************************************************
      *  Input and Output buffers and test data for SAMPLE2
      **********************************************************
        77  SizeOfInput  PICTURE 9(6) USAGE IS BINARY VALUE IS 6.
        77  InValue  PICTURE X(6).
        01  OutBuffer.
             05 OutValue  PICTURE X(6).
             05 OutValueTbl REDEFINES OutValue
                            PICTURE X(1) OCCURS 6 TIMES
                            INDEXED BY M.
        01  OutHexBuffer VALUE IS "X'".
             05 OutHexValue PICTURE X(1) OCCURS 15 TIMES
                            INDEXED BY K.
        77  EBCD  PICTURE X(6)  VALUE IS X"C1C2C301234C".
        77  ASCII  PICTURE X(6)  VALUE IS X"41424300D204".
      **********************************************************
      *  Tables and Work Areas for displaying the value in the output
      *  buffer in hex
      **********************************************************
        77  First-Digit  PICTURE 9(6) USAGE IS BINARY.
        77  Second-Digit  PICTURE 9(6) USAGE IS BINARY.
        77  I  PICTURE 9(6) USAGE IS BINARY.
        77  Switch  PICTURE 9(6) USAGE IS BINARY.
        01  HexIndex.
             05 HexValue-1  PICTURE X(128)
                             VALUE IS X"000102030405060708090A0B0C0D0E0F
      -                                "101112131415161718191A1B1C1D1E1F
      -                                "202122232425262728292A2B2C2D2E2F
      -                                "303132333435363738393A3B3C3D3E3F
      -                                "404142434445464748494A4B4C4D4E4F
      -                                "505152535455565758595A5B5C5D5E5F
      -                                "606162636465666768696A6B6C6D6E6F
      -                              "707172737475767778797A7B7C7D7E7F".
            05 HexTable-1  REDEFINES HexValue-1
                               PICTURE X(1) OCCURS 128 TIMES.
             05 HexValue-2  PICTURE X(128)
                             VALUE IS X"808182838485868788898A8B8C8D8E8F
      -                                "909192939495969798999A9B9C9D9E9F
      -                                "A0A1A2A3A4A5A6A7A8A9AAABACADAEAF
      -                                "B0B1B2B3B4B5B6B7B8B9BABBBCBDBEBF
      -                                "C0C1C2C3C4C5C6C7C8C9CACBCCCDCECF
      -                                "D0D1D2D3D4D5D6D7D8D9DADBDCDDDEDF
      -                                "E0E1E2E3E4E5E6E7E8E9EAEBECEDEEEF
      -                            "F0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF".
            05 HexTable-2  REDEFINES HexValue-2
                               PICTURE X(1) OCCURS 128 TIMES.
        01  Hex-To-Char.
             05 CharValue  PICTURE X(16)
                           VALUE IS "0123456789ABCDEF".
             05 Hex-To-Char-Tbl  REDEFINES CharValue PICTURE X(1)
                                 OCCURS 16 TIMES.
        PROCEDURE DIVISION.
      **********************************************************
      *  Read the conversion plan space created in SAMPLE1
      **********************************************************
            OPEN INPUT Sample-SPC.
            READ Sample-SPC.
            CLOSE Sample-SPC.
      **********************************************************
      *  Call Conversion Plan Executor Initialization
      **********************************************************
            CALL "FMTCPXI" USING
                        BY REFERENCE CnvPlnSpc
                        BY REFERENCE CnvPlnSpcHdl
                        BY REFERENCE FeedBack.
      **********************************************************
      *  Check the Condition Token
      **********************************************************
            IF MsgNo OF Condition-ID IN FeedBack NOT = CPX-NO-ERROR
            THEN
               MOVE "Init" TO CpxType
               PERFORM PrintCtok
            ELSE
               SET AdlCommAreaPtr TO ADDRESS OF AdlCommArea
      **********************************************************
      *  Call Conversion Plan Executor Convert
      *  In this conversion the plan COBOL_TO_C is executed. The input
      *  data are:
      *     "ABC"  in international EBCDIC format -> X'C1C2C3'
      *     1234   in PACKED PRECISION(5) format  -> X'01234C'
      *
      *  The output data after conversion should be:
      *     "ABC"  in Latin PC Data format + suffix -> X'41424300'
      *     1234   in BINARY byte reversed format   -> X'D204'
      **********************************************************
               MOVE 10 TO PlnNamLength
               MOVE "COBOL_TO_C" TO PlnNam
               MOVE 1 TO InputParmNum
               MOVE 1 TO OutputParmNum
               MOVE EBCD TO InValue
               MOVE SPACES TO OutValue
               SET InputDataPtr(1) TO ADDRESS OF InValue
               SET OutputDataPtr(1) TO ADDRESS OF OutValue
               CALL "FMTCPXC" USING
                           BY VALUE     CnvPlnSpcHdl
                           BY VALUE     PlnNamLength
                           BY REFERENCE PlnNam
                           BY VALUE     InputParmNum
                           BY REFERENCE InputData
                           BY VALUE     OutputParmNum
                           BY REFERENCE OutputData
                           BY REFERENCE FeedBack
               IF MsgNo OF Condition-ID IN FeedBack NOT = CPX-NO-ERROR
               THEN
      **********************************************************
      *  An error occurred. Print the condition token
      **********************************************************
                  MOVE "Convert" TO CpxType
                  PERFORM PrintCtok
                  STOP RUN
               ELSE
      **********************************************************
      *  Print the converted value
      **********************************************************
                  SET K TO 3
                  PERFORM Hex-Convert VARYING M FROM 1 BY 1
                                      UNTIL M > SizeOfInput
                  MOVE "'" TO OutHexValue(K)
                  DISPLAY " Converted value for plan COBOL_TO_C: "
                                                OutHexBuffer
               END-IF
      **********************************************************
      *  Call Conversion Plan Executor Convert
      *  In this conversion the plan C_TO_COBOL is executed. The input
      *  data are:
      *     "ABC"  in Latin PC Data format + suffix -> X'41424300'
      *     1234   in BINARY byte reversed format   -> X'D204'
      *
      *  The output data after conversion should be:
      *     "ABC"  in international EBCDIC format -> X'C1C2C3'
      *     1234   in PACKED PRECISION(5) format  -> X'01234C'
      **********************************************************
               MOVE 10 TO PlnNamLength
               MOVE "C_TO_COBOL" TO PlnNam
               MOVE 1 TO InputParmNum
               MOVE 1 TO OutputParmNum
               MOVE ASCII TO InValue
               MOVE SPACES TO OutValue
               SET InputDataPtr(1) TO ADDRESS OF InValue
               SET OutputDataPtr(1) TO ADDRESS OF OutValue
               CALL "FMTCPXC" USING
                           BY VALUE     CnvPlnSpcHdl
                           BY VALUE     PlnNamLength
                           BY REFERENCE PlnNam
                           BY VALUE     InputParmNum
                           BY REFERENCE InputData
                           BY VALUE     OutputParmNum
                           BY REFERENCE OutputData
                           BY REFERENCE FeedBack
               IF MsgNo OF Condition-ID IN FeedBack NOT = CPX-NO-ERROR
               THEN
      **********************************************************
      *  An error occurred. Print the condition token
      **********************************************************
                  MOVE "Convert" TO CpxType
                  PERFORM PrintCtok
               ELSE
      **********************************************************
      *  Print the converted value
      **********************************************************
                  SET K TO 3
                  PERFORM Hex-Convert VARYING M FROM 1 BY 1 
                                      UNTIL M > SizeOfInput
                  MOVE "'" TO OutHexValue(K)
                  DISPLAY " Converted value for plan C_TO_COBOL: "
                                                       OutHexBuffer
               END-IF
      **********************************************************
      *  Call Conversion Plan Executor Termination
      **********************************************************
               CALL "FMTCPXT" USING
                           BY VALUE     CnvPlnSpcHdl
                           BY REFERENCE FeedBack
      **********************************************************
      *  Check the Condition Token
      **********************************************************
               IF MsgNo OF Condition-ID IN FeedBack NOT = CPX-NO-ERROR
               THEN
                  MOVE "Term" TO CpxType
                  PERFORM PrintCtok
               END-IF
            END-IF.
            STOP RUN.
      **********************************************************
      *  The procedure PrintCtok prints the condition token and the ADL
      *  communication area after an error occurred in a conversion 
      *  plan executor function.
      *  Note: The Case-Severity-Control field is further divided into
      *        three sub-fields. You may want to display it in hex.
      **********************************************************
         PrintCtok.
            DISPLAY "Error in Conversion Plan Executor " CpxType.
            DISPLAY "The Condition Token has the following contents:".
            DISPLAY "Message Severity "
                    MsgSev OF Condition-ID IN FeedBack
                    " Number "
                    MsgNo OF Condition-ID IN FeedBack.
            DISPLAY "Case+Severity+Control "
                    Case-Severity-Control IN FeedBack.
            DISPLAY "Facility ID "
                          Facility-ID OF FeedBack.
      **********************************************************
      *  Check whether an ADL exception occurred.
      **********************************************************
            IF MsgNo OF Condition-ID = CPX-ADL-EXCEPTION-SEV2  OR
                MsgNo OF Condition-ID = CPX-ADL-EXCEPTION-SEV3
            THEN
               IF CpxType = "INIT"
               THEN
                  DISPLAY "ADL exception " AdlExId IN FeedBack
               ELSE
                  DISPLAY
                   "The ADL communication area has the following
      -                         " contents:"
                  DISPLAY "ADL exception: " ExId OF AdlCommArea
                  DISPLAY "Severity of ADL exception: "
                          SevCod OF AdlCommArea
                  DISPLAY "Name of processed plan: "
                          CharData OF PlanId OF AdlCommArea
                  DISPLAY "Number of processed PLAN statement: "
                          PlanStmt OF AdlCommArea
                  DISPLAY "Input data portion that caused the error: "
                          CharData OF InpErrDta OF AdlCommArea
                  DISPLAY "Source identifier of processed assignment
      -                          " statement: "
                          CharData OF SrcFldId OF AdlCommArea
                  DISPLAY "Target identifier of processed assignment
      -                          " statement: "
                          CharData OF TrgFldId OF AdlCommArea
               END-IF
            END-IF.
      **********************************************************
      *  The procedure Hex-Convert converts a byte string to a hex
      *  string which can be DISPLAYed.
      **********************************************************
         Hex-Convert.
            MOVE 0 TO Switch.
            PERFORM Hex-Convert-1 VARYING I FROM 1 BY 1
                                        UNTIL Switch = 1 OR I > 128.
            IF Switch = 0
            THEN
               PERFORM Hex-Convert-2 VARYING I FROM 1 BY 1
                                        UNTIL Switch = 1 
            END-IF.
         Hex-Convert-1.
            IF OutValueTbl(M) = HexTable-1(I)
            THEN
               DIVIDE 16 INTO I GIVING First-Digit
                                REMAINDER Second-Digit
               MOVE Hex-To-Char-Tbl(First-Digit + 1) 
                                    TO OutHexValue(K)
               SET K UP BY 1
               MOVE Hex-To-Char-Tbl(Second-Digit) TO OutHexValue(K)
               SET K UP BY 1
               MOVE 1 TO Switch
            END-IF.
         Hex-Convert-2.
            IF OutValueTbl(M) = HexTable-2(I)
            THEN
               DIVIDE 16 INTO I GIVING First-Digit
                                REMAINDER Second-Digit
               MOVE Hex-To-Char-Tbl(First-Digit + 9)
                           TO OutHexValue(K)
               SET K UP BY 1
               MOVE Hex-To-Char-Tbl(Second-Digit) TO OutHexValue(K)
               SET K UP BY 1
               MOVE 1 TO Switch
            END-IF.
