      **********************************************************
      *  PRODUCT   = Data Description and Conversion for OS/2
      *
      *  SOURCE FILE NAME = IWZZSUX.CBL
      *
      *  DESCRIPTIVE NAME = User Exit Sample
      *
      *  FUNCTION = This user exit sample program can be called via the
      *              DDC/2 user exit facility.
      *              It converts any character string from upper case   
      *              to lower case characters or from lower case to
      *              upper case characters, depending on a boolean
      *              parameter.
      *
      *  ENTRY POINT = ConvertToUpperLowerCaseChar
      *
      **********************************************************
        IDENTIFICATION DIVISION.
        PROGRAM-ID. ConvertToUpperLowerCaseChar.
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER. IBM-PS2.
        OBJECT-COMPUTER. IBM-PS2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
      **********************************************************
      *  Conversion tables and local variables
      **********************************************************
        01  UpperCase VALUE IS "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
            05 UpperChar PICTURE X(1) OCCURS 26 TIMES
                         INDEXED BY J. 
        01  LowerCase  VALUE IS "abcdefghijklmnopqrstuvwxyz".
            05 LowerChar PICTURE X(1) OCCURS 26 TIMES
                         INDEXED BY L.
        77  Switch  PICTURE 9(6) USAGE IS BINARY.
        77  ADLTrue  PICTURE X(1) VALUE IS X"01".
        LINKAGE SECTION.
      **********************************************************
      *  Expected parameters:
      *     ParamCount:      5
      *     ParameterPtr(1)  Reference to the input character field.
      *     ParameterPtr(2)  Reference to the length of the input
      *                      character field.
      *     ParameterPtr(3)  Reference to a boolean condition.
      *     ParameterPtr(4)  Reference to output character field.
      *     ParameterPtr(5)  Reference to the length of output
      *                      character field.
      *     ParameterPtr(6)  Reference to feedback area
      *                      (NO ERROR: MsgSev = 0).
      **********************************************************
        77  ParamCount  PICTURE 9(6) USAGE IS BINARY.
        01  ParameterList.
            05  ParameterPtr  USAGE IS POINTER OCCURS 5 TIMES.
        01  FeedBack.
            05 Condition-ID.
                10 MsgSev  PICTURE 9(2) USAGE IS BINARY.
                10 MsgNo    PICTURE 9(2) USAGE IS BINARY.
            05 Case-Severity-Control  PICTURE X.
            05 Facility-ID  PICTURE X(3).
            05 I-S-Info.
                10 AdlExId  PICTURE 9(6) USAGE IS BINARY.
                10 AdlCommAreaPtr REDEFINES AdlExId USAGE IS POINTER.
                10 User-ExitCtokPtr  REDEFINES AdlExId USAGE IS POINTER.
        01  InputCharField.
            05 InputChar PICTURE X(1) OCCURS 1 TO 256 TIMES
                         DEPENDING ON ByteSizeOfInputChar
                         INDEXED BY I.
        77  ByteSizeOfInputChar  PICTURE 9(6) USAGE IS BINARY.
        77  ToUpperCaseLetter PICTURE X(1).
        01  OutputCharField.
            05 OutputChar PICTURE X(1) OCCURS 1 TO 256 TIMES
                         DEPENDING ON ByteSizeOfOutputChar
                         INDEXED BY K.
        77  ByteSizeOfOutputChar  PICTURE 9(6) USAGE IS BINARY.
        PROCEDURE DIVISION USING BY VALUE ParamCount
                                 BY REFERENCE ParameterList
                                 BY REFERENCE FeedBack.
      **********************************************************
      *  Initialization of variables
      **********************************************************
            SET ADDRESS OF InputCharField TO ParameterPtr(1).
            SET ADDRESS OF ByteSizeOfInputChar TO ParameterPtr(2).
            SET ADDRESS OF ToUpperCaseLetter TO ParameterPtr(3).
            SET ADDRESS OF OutputCharField TO ParameterPtr(4).
            SET ADDRESS OF ByteSizeOfOutputChar TO ParameterPtr(5).
            MOVE 0 TO MsgSev OF Condition-ID IN FeedBack.
            MOVE 0 TO MsgNo OF Condition-ID IN FeedBack.
      **********************************************************
      *  Check buffer sizes and parameter count
      **********************************************************
            IF ByteSizeOfInputChar <= ByteSizeOfOutputChar AND
               ParamCount = 5
            THEN
               IF ToUpperCaseLetter = ADLTrue
               THEN
      **********************************************************
      *  Convert all lower case characters to upper case characters
      **********************************************************
                  PERFORM ToUpper VARYING I FROM 1 BY 1
                                  UNTIL I > ByteSizeOfInputChar
               ELSE
      **********************************************************
      *  Convert all upper case characters to lower case characters
      **********************************************************
                  PERFORM ToLower VARYING I FROM 1 BY 1
                                  UNTIL I > ByteSizeOfInputChar
            ELSE
      **********************************************************
      *  Set error conditions
      **********************************************************
               MOVE 3 TO MsgSev OF Condition-ID IN FeedBack
               MOVE 2647 TO MsgNo OF Condition-ID IN FeedBack
            END-IF.
            EXIT PROGRAM.
        ToUpper.
            MOVE 0 TO Switch.
            SET K TO I.
            MOVE Inputchar(I) TO OutputChar(K).
            PERFORM VARYING J FROM 1 BY 1
                    UNTIL J > 26 OR Switch = 1
               IF InputChar(I) = LowerChar(J)
               THEN
                  MOVE UpperChar(J) TO OutputChar(K)
                  MOVE 1 TO Switch
               END-IF                                                                                                           
            END-PERFORM.
        ToLower.
            MOVE 0 TO Switch.
            SET K TO I.
            MOVE Inputchar(I) TO OutputChar(K).
            PERFORM VARYING L FROM 1 BY 1
                    UNTIL L > 26 OR Switch = 1
               IF InputChar(I) = UpperChar(L)
               THEN
                  MOVE LowerChar(L) TO OutputChar(I)
                  MOVE 1 TO Switch
               END-IF
            END-PERFORM.
        END PROGRAM  ConvertToUpperLowerCaseChar.
