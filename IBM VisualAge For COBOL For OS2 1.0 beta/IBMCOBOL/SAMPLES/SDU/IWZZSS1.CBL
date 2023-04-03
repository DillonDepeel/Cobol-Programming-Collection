      **********************************************************
      *  PRODUCT   = Data Description and Conversion for OS/2
      *
      *  SOURCE FILE NAME = IWZZSS1.CBL
      *
      *  DESCRIPTIVE NAME = ADL Declaration Translator and CPB sample
      *
      *  FUNCTION = This sample program calls the parse function of the
      *              ADL declaration translator to compile ADL source
      *              text IWZZSS1.ADL into the appropriate ADL declare
      *              and plan spaces, calls the generate function of
      *              the ADL declaration translator to reproduce the
      *              ADL source file IWZZSS1.GEN. The parse function's
      *              output is also used to call the conversion plan
      *              builder to create conversion plans from the encoded
      *              descriptions.
      *              The conversion plan space generated as the output
      *              of the conversion plan builder is stored in the  
      *              file IWZZSS1F.
      *
      **********************************************************
        IDENTIFICATION DIVISION.
        PROGRAM-ID. IWZZSS1.
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
        FD   Sample-SPC
              RECORDING MODE IS F
              RECORD CONTAINS 5000 CHARACTERS.
        01    CnvPlnSpc  PICTURE X(5000).
        WORKING-STORAGE SECTION.
      **********************************************************
      *  Work areas and values specific to this sample program
      **********************************************************
        77     BUFLEN-ADLDCLSPC  PICTURE 9(6) USAGE IS BINARY
                                 VALUE IS 64000.
        77     BUFLEN-ADLPLNSPC  PICTURE 9(6) USAGE IS BINARY 
                                 VALUE IS 64000.
        77     BUFLEN-CNVPLNSPC  PICTURE 9(6) USAGE IS BINARY
                                 VALUE IS 5000.
        77     UserAdlPlnSpcNull  PICTURE 9(6) USAGE IS BINARY 
                                  VALUE IS 0.
        77     AdlDclSpc  PICTURE X(64000).
        77     AdlPlnSpc  PICTURE X(64000).
      **********************************************************
      *  Return codes and ADL exception codes
      **********************************************************
        77     PRS-NO-ERROR  PICTURE 9(2) USAGE BINARY VALUE IS 0.
        77     GEN-NO-ERROR  PICTURE 9(2) USAGE BINARY VALUE IS 0.
        77     CPB-NO-ERROR  PICTURE 9(2) USAGE BINARY VALUE IS 0.
        77     CPB-ADL-EXCEPTION-SEV2  PICTURE 9(6)
                                          USAGE IS BINARY VALUE is 2.
        77     CPB-ADL-EXCEPTION-SEV3  PICTURE 9(6)
                                          USAGE IS BINARY VALUE IS 18.
      **********************************************************
      *  Parameters for FMTPRS, FMTGEN and FMTCRCP
      **********************************************************
        77     DclXlrId        PICTURE X(8).
        77     ADLDECLTRANSLATOR  PICTURE X(8)
                                    VALUE IS X"2B12000301886D01".
        77     ParameterCCSID      PICTURE 9(6) USAGE IS BINARY
                                   VALUE IS 0.
        77     SrcFilNamLength     PICTURE  9(6) USAGE IS BINARY.
        77     SrcFilNam           PICTURE  X(255).
        77     SrcFilCCSID         PICTURE  9(6) USAGE IS BINARY
                                   VALUE IS 0.
        77     DclXlrOptLength     PICTURE  9(6) USAGE IS BINARY.
        77     DclXlrOpt           PICTURE  X(255).
        77     LstOptLength       PICTURE  9(6) USAGE IS BINARY.
        77     LstOpt              PICTURE  X(255).
        77     LstFilNamLength    PICTURE  9(6) USAGE IS BINARY.
        77     LstFilNam           PICTURE  X(255).
        77     ADLDclSpcCCSID      PICTURE  9(6)  USAGE IS BINARY
                                   VALUE IS 0.
        77     ADLDclSpcLength   PICTURE  9(6) USAGE IS BINARY.
        77     ADLPlnSpcLength   PICTURE  9(6) USAGE IS BINARY.
        77     ADLSpcCCSID      PICTURE 9(6) USAGE IS BINARY
                                VALUE IS 0.
        77     LstFilCCSID         PICTURE 9(6) USAGE IS BINARY
                                   VALUE IS 0.
        77     AdlDclSpcCount   PICTURE 9(6) USAGE IS BINARY.
        77     UserAdlPlnSpcCount   PICTURE  9(6) USAGE IS BINARY.
        77     DefaultAdlPlnSpcCount  PICTURE  9(6) USAGE IS BINARY.
        77     CnvPlnSpcLength   PICTURE 9(6) USAGE IS BINARY.
        77     FlagList      PICTURE 9(6) USAGE IS BINARY.
        01     AdlDclSpcList.
            05 AdlDclSpcptr   USAGE IS POINTER OCCURS 32 TIMES.
        01     UserAdlPlnSpcList.
            05 UserAdlPlnSpcPtr   USAGE IS POINTER OCCURS 32 TIMES.
        01     DefaultAdlPlnSpcList.
            05 DefaultAdlPlnSpcPtr  USAGE IS POINTER OCCURS 32 TIMES.
        01     Cnstkn.  
            05 CnstknLength  PICTURE 9(6) USAGE IS BINARY.
            05 CnstknClass   PICTURE 9(2) USAGE IS BINARY.
            05 CnstknValue   PICTURE X(16).
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
        PROCEDURE DIVISION.
      **********************************************************
      *  Call PARSE function of ADL Declaration Translator for
      *  ADL source text to get ADLDCLSPC and ADLPLNSPC.
      *  Translator id is set to ADL.
      *  Note: Currently all CCSIDs should be zero
      **********************************************************
             MOVE ADLDECLTRANSLATOR TO DclXlrId.
             MOVE 11 TO SrcFilNamLength.
             MOVE "IWZZSS1.ADL" TO SrcFilNam.
             MOVE 8 TO DclXlrOptLength.
             MOVE "AUTOSKIP" TO DclXlrOpt.
             MOVE 4 TO LstOptLength.
             MOVE "LIST" TO LstOpt.
             MOVE 12 TO LstFilNamLength.
             MOVE "IWZZSS1P.LST" TO LstFilNam.
             MOVE BUFLEN-ADLDCLSPC TO ADLDclSpcLength.
             MOVE BUFLEN-ADLPLNSPC TO ADLPlnSpcLength.
             CALL "FMTPRS" USING
                                  BY REFERENCE DclXlrId
                                  BY VALUE     ParameterCCSID
                                  BY VALUE     SrcFilNamLength
                                  BY REFERENCE SrcFilNam
                                  BY VALUE     SrcFilCCSID
                                  BY VALUE     DclXlrOptLength
                                  BY REFERENCE DclXlrOpt
                                  BY VALUE     LstOptLength
                                  BY REFERENCE LstOpt
                                  BY VALUE     LstFilNamLength
                                  BY REFERENCE LstFilNam
                                  BY VALUE     ADLDclSpcLength
                                  BY REFERENCE AdlDclSpc
                                  BY VALUE     ADLDclSpcCCSID
                                  BY REFERENCE Cnstkn
                                  BY VALUE     ADLPlnSpcLength
                                  BY REFERENCE AdlPlnSpc
                                  BY REFERENCE FeedBack.
      **********************************************************
      *  Check the Condition Token
      **********************************************************
             IF MsgNo OF Condition-ID IN FeedBack NOT = PRS-NO-ERROR
             THEN
                  DISPLAY "Error in PARSE function"
                  DISPLAY
                   "The Condition Token has the following contents:"
                  DISPLAY "Message Severity "
                          MsgSev OF Condition-ID IN FeedBack
                          " Number "
                          MsgNo OF Condition-ID IN FeedBack
                  DISPLAY "Case+Severity+Control "
                          Case-Severity-Control IN FeedBack
                  DISPLAY "Facility ID "
                          Facility-ID OF FeedBack
                  DISPLAY "Instance Specific "
                          AdlExId OF FeedBack 
                  STOP RUN
             END-IF.
      **********************************************************
      *  Call GENERATE function of ADL Declaration Translator for
      *  ADLDCLSPC to get ADL Source test.
      *  This call is not necessary to create a conversion plan. 
      *  It is mainly done for debugging of the PARSE function.
      *  Translator id is set to ADL.
      *  Note: Currently all CCSIDs should be zero.
      **********************************************************
             MOVE 11 to SrcFilNamLength.
             MOVE "IWZZSS1.GEN" TO SrcFilNam.
             MOVE 0 to DclXlrOptLength.
             MOVE 12 TO LstOptLength.
             MOVE "LIST FLAG(I)" to LstOpt.
             MOVE 12 to LstFilNamLength.
             MOVE "IWZZSS1G.LST" TO LstFilNam.
             CALL "FMTGEN" USING
                                  BY REFERENCE DclXlrId
                                  BY VALUE     ParameterCCSID
                                  BY VALUE     DclXlrOptLength
                                  BY REFERENCE DclXlrOpt
                                  BY REFERENCE AdlDclSpc
                                  BY VALUE AdlSpcCCSID
                                  BY VALUE SrcFilNamLength
                                  BY REFERENCE SrcFilNam
                                  BY VALUE     SrcFilCCSID
                                  BY VALUE     LstOptLength
                                  BY REFERENCE LstOpt
                                  BY VALUE     LstFilNamLength
                                  BY REFERENCE LstFilNam
                                  BY VALUE     LstFilCCSID
                                  BY REFERENCE FeedBack.
      **********************************************************
      *  Check the Condition Token
      **********************************************************
             IF MsgNo OF Condition-ID IN FeedBack NOT = GEN-NO-ERROR
             THEN
                  DISPLAY "Error in GENERATE function"
                  DISPLAY
                   "The Condition Token has the following contents:"
                  DISPLAY "Message Severity "
                          MsgSev OF Condition-ID IN FeedBack
                          " Number "
                          MsgNo OF Condition-ID IN FeedBack
                  DISPLAY "Case+Severity+Control "
                          Case-Severity-Control IN FeedBack
                  DISPLAY "Facility ID      "
                          Facility-ID OF FeedBack
                  DISPLAY "Instance Specific "
                          AdlExId OF FeedBack 
                  STOP RUN
             END-IF.
      **********************************************************
      *  Call Conversion Plan Builder
      *  Note: If UserAdlPlnSpcCount (third parameter) is not zero,
      *        then the fourth parameter must be replaced with
      *             BY REFERENCE UserAdlPlnSpcList
      *        and addresses of user defined plan spaces must be
      *        entered into the list.
      **********************************************************
             MOVE 1 TO AdlDclSpcCount.
             SET AdlDclSpcPtr(1) TO ADDRESS OF AdlDclSpc.
             MOVE 0 TO UserAdlPlnSpcCount.
             MOVE 1 TO DefaultAdlPlnSpcCount.
             SET DefaultAdlPlnSpcPtr(1) TO ADDRESS OF AdlPlnSpc.
             MOVE BUFLEN-CNVPLNSPC TO CnvPlnSpcLength.
             MOVE 0 TO FlagList.
             SET AdlCommAreaPtr TO ADDRESS OF AdlCommArea.
             CALL "FMTCRCP" USING
                                  BY VALUE     AdlDclSpcCount
                                  BY REFERENCE AdlDclSpcList
                                  BY VALUE     UserAdlPlnSpcCount
                                  BY VALUE     UserAdlPlnSpcNull
                                  BY VALUE     DefaultAdlPlnSpcCount
                                  BY REFERENCE DefaultAdlPlnSpcList
                                  BY VALUE     CnvPlnSpcLength
                                  BY REFERENCE CnvPlnSpc
                                  BY VALUE     FlagList
                                  BY REFERENCE FeedBack.
      **********************************************************
      *  Check the Condition Token
      *  Note: The Case-Severity-Control field is further divided into
      *        three sub-fields. You may want to display it in hex.
      **********************************************************
             IF MsgNo OF Condition-ID IN FeedBack NOT = CPB-NO-ERROR
             THEN
                  DISPLAY "Error in Conversion Plan Builder"
                  DISPLAY
                   "The Condition Token has the following contents:"
                  DISPLAY "Message Severity "
                          MsgSev OF Condition-ID IN FeedBack
                          " Number "
                          MsgNo OF Condition-ID IN FeedBack
                  DISPLAY "Case+Severity+Control "
                          Case-Severity-Control IN FeedBack
                  DISPLAY "Facility ID "
                          Facility-ID OF FeedBack
      **********************************************************
      *  Check whether an ADL exception occurred. If so, the ADL
      *  communication area is displayed.
      **********************************************************
                  IF MsgNo OF Condition-ID = CPB-ADL-EXCEPTION-SEV2  OR
                     MsgNo OF Condition-ID = CPB-ADL-EXCEPTION-SEV3
                 THEN
                     DISPLAY
                      "The ADL communication area has the following
      -                            " contents:"
                     DISPLAY "ADL exception: " ExId OF AdlCommArea
                     DISPLAY "Severity of ADL exception: " 
                             SevCod OF AdlCommArea
                     DISPLAY "Name of processed plan: " 
                             CharData OF PlanId OF AdlCommArea
                     DISPLAY "Number of processed PLAN statement: "
                             PlanStmt OF AdlCommArea
                     DISPLAY "Source identifier of processed assignment
      -                             " statement: "
                             CharData OF SrcFldId OF AdlCommArea
                     DISPLAY "Target identifier of processed assignment
      -                             " statement: "
                             CharData OF TrgFldId OF AdlCommArea
                     STOP RUN
                  END-IF 
             ELSE
      ********************************************************** 
      *  Write conversion plan space into file.
      **********************************************************
                OPEN OUTPUT Sample-SPC
                WRITE  CnvPlnSpc
                CLOSE SAMPLE-SPC.
             STOP RUN.
