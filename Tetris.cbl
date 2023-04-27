IDENTIFICATION DIVISION.
PROGRAM-ID. Tts.
ENVIRONMENT    DIVISION.

INPUT-OUTPUT SECTION.
FILE-CONTROL.

	SELECT  FHIGH ASSIGN TO "TTS.SCORE"
		STATUS IS HIGH-STAT.

DATA DIVISION.
FILE SECTION.

FD FHIGH.
01 SHIGH.
	03 HIGH-NAME	PIC X(20).
	03 HIGH-POINTS	PIC 9(18) COMP.
	03 HIGH-LINES	PIC 9(09) COMP.
	03 HIGH-ENTRY	PIC 9(04) COMP.
	03 HIGH-LEVEL	PIC 9(04) COMP.
	03 HIGH-BONUS	PIC 9(04) COMP.
	03 HIGH-CPU	PIC 9(09) COMP.

WORKING-STORAGE SECTION.

01 C-Dim-X	Pic  9(4) Comp Value 17.
01 C-Dim-Y	Pic  9(4) Comp Value 19.
01 C-Max-Obj	Pic  9(4) Comp Value 12.
01 C-Max-Dim-O	Pic  9(4) Comp Value  3.
01 C-Max-Rot	Pic  9(4) Comp Value  4.
01 C-Middle-X	Pic  9(4) Comp Value  7.
01 C-Delim-C1	Pic  X(1) Value ".".
01 C-Delim-C2	Pic  X(1) Value ":".

01 Rows         Pic S9(9) Comp Value 22.
01 Columns      Pic S9(9) Comp Value 78.
01 Rows-O	Pic S9(9) Comp Value 24.
01 Columns-O	Pic S9(9) Comp Value 78.
01 Rows-ED	Pic S9(9) Comp Value  5.
01 Columns-ED	Pic S9(9) Comp Value  5.
01 Pos-x        Pic S9(9) Comp Value  2.
01 Pos-y        Pic S9(9) Comp Value  2.
01 Pos-xO       Pic S9(9) Comp Value  1.
01 Pos-yO       Pic S9(9) Comp Value  2.
01 Pos-xED      Pic S9(9) Comp Value 10.
01 Pos-yED      Pic S9(9) Comp Value 37.
01 Rows-E       Pic S9(9) Comp Value  3.
01 Columns-E    Pic S9(9) Comp Value 12.
01 Rows-L       Pic S9(9) Comp Value  1.
01 Columns-L    Pic S9(9) Comp Value 32.
01 Pos-xE       Pic S9(9) Comp Value 10.
01 Pos-yE       Pic S9(9) Comp Value 34.
01 Pos-xL       Pic S9(9) Comp Value 11.
01 Pos-yL       Pic S9(9) Comp Value 23.
01 RCX1		Pic S9(9) Comp Value  1.
01 RCX2		Pic S9(9) Comp Value  3.
01 RCY1		Pic S9(9) Comp Value  1.
01 RCY2		Pic S9(9) Comp Value 12.

01 C-Origin-X	Pic  9(4) Comp Value  1.
01 C-Origin-Y	Pic  9(4) Comp Value 30.

01 Label-O.
	03 Pic x(28) Value Space.
	03 Pic x(23) Value "Create your own objects".
	03 Pic x(29) Value Space.

01 C-Line-Points	Pic  9(4) Comp Value 1000.
01 C-First-Points	Pic S9(4) Comp Value -10.
01 C-Object-Points	Pic  9(4) Comp Value  10.

01 Point-X		Pic S9(9) Comp Value   1.
01 Point-Y		Pic S9(9) Comp Value  69.
01 Bonus-X		Pic S9(9) Comp Value   5.
01 Bonus-Y		Pic S9(9) Comp Value  63.

*Position for drawing next object.
01 PNO-X		Pic S9(9) Comp Value  70.
01 PNO-Y		Pic S9(9) Comp Value  18.

01 M_BOLD          PIC S9(9) COMP VALUE IS EXTERNAL SMG$M_BOLD.
01 M_REVERSED      PIC S9(9) COMP VALUE IS EXTERNAL SMG$M_REVERSE.
01 M_BLINKING	   PIC S9(9) COMP VALUE IS EXTERNAL SMG$M_BLINK.
01 M_UNDERLINED	   PIC S9(9) COMP VALUE IS EXTERNAL SMG$M_UNDERLINE.
01 M_CUR_ON        PIC S9(9) COMP VALUE IS EXTERNAL SMG$M_CURSOR_ON.
01 M_CUR_OFF       PIC S9(9) COMP VALUE IS EXTERNAL SMG$M_CURSOR_OFF.
01 M_BLOCK         PIC S9(9) COMP VALUE IS EXTERNAL SMG$M_BLOCK_BORDER.
01 M-BORDER	   PIC S9(9) COMP VALUE IS EXTERNAL SMG$M_BORDER.

01 Type-OUT	Pic S9(9) Comp.
01 ReversedOUT	Pic S9(9) Comp.

*Timing.
01 T-Code	Pic S9(9) Comp Value 2.
01 T-Cont       Pic  9(9) Comp Value 0.

01 Time-Zero    Pic S9(9) Comp   Value  0.
01 Wait-Unit    	  Comp-2 Value  0.008.
01 Max-Wait     Pic  9(4) Comp   Value  100.
01 Wait-Count	Pic  9(4) Comp.

01 Level	Pic  9(4) Comp Value  1.
01 Max-Level	Pic  9(4) Comp Value  9.
01 C-New-Level	Pic  9(4) Comp Value 10.

01 Level-X		Pic S9(9) Comp Value   2.
01 Level-Y		Pic S9(9) Comp Value  69.
01 Lines-X		Pic S9(9) Comp Value   3.
01 Lines-Y		Pic S9(9) Comp Value  69.

01 Cmp-Lines	Pic  9(9) Comp Value  0.
01 Lines-Factor	Pic  9(4) Comp.

01 LevelsTiming.
	03 Level-Table Occurs 10 Times.
		05 LEV-Count	Pic 9(4) Comp.
		05 LEV-Factor	Pic 9(9) Comp.

*For editing objects.

01 DrawObjectsPosition.
	03 Draw-Table Occurs 12 Times.
		04 Draw-Rotation Occurs 4 Times.
			05 DT-X	Pic S9(9) Comp.
			05 DT-Y Pic S9(9) Comp.

*Objects.

01 Seed		Pic  9(9) Comp.

01 Obj1		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"33111111111 33111111111 33111111111 33111111111".
01 Obj1-No-Rot	Pic 9(4) Comp Value 1.

01 Obj2		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"32111001000 23010010110 32100111000 23110100100".
01 Obj2-No-Rot	Pic 9(4) Comp Value 0.

01 Obj3		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"32111100000 23110010010 32001111000 23100100110".
01 Obj3-No-Rot	Pic 9(4) Comp Value 0.

01 Obj4		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"13100100100 31111000000 13100100100 31111000000".
01 Obj4-No-Rot	Pic 9(4) Comp Value 0.

01 Obj5		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"11100000000 11100000000 11100000000 11100000000".
01 Obj5-No-Rot	Pic 9(4) Comp Value 1.

01 Obj6		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"32110011000 23010110100 32110011000 23010110100".
01 Obj6-No-Rot	Pic 9(4) Comp Value 0.

01 Obj7		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"32011110000 23100110010 32011110000 23100110010".
01 Obj7-No-Rot	Pic 9(4) Comp Value 0.

01 Obj8		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"22110110000 22110110000 22110110000 22110110000".
01 Obj8-No-Rot	Pic 9(4) Comp Value 1.

01 Obj9		Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"32111010000 23010110010 32010111000 23100110100".
01 Obj9-No-Rot	Pic 9(4) Comp Value 0.

01 Obj10	Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"12100100000 21110000000 12100100000 21110000000".
01 Obj10-No-Rot	Pic 9(4) Comp Value 0.

01 Obj11	Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"22110010000 22010110000 22100110000 22110100000".
01 Obj11-No-Rot	Pic 9(4) Comp Value 0.

01 Obj12	Pic x(47) Value 
*                  123456789   123456789   123456789   123456789
*               012345678901234567890123456789012345678901234567
		"22110100000 22110010000 22010110000 22100110000".
01 Obj12-No-Rot	Pic 9(4) Comp Value 0.

01 Obj-X	Pic  9(4) Comp.
01 Obj-Y	Pic  9(4) Comp.
01 R-Obj-X	Pic  9(4) Comp.
01 R-Obj-Y	Pic  9(4) Comp.
01 Rot		Pic  9(4) Comp.

01 Objects-Indeks.
	02 Table-Object-Indeks Occurs 4 Times.
		03 OIdx-X	Pic 9(4) Comp.
		03 OIdx-Y	Pic 9(4) Comp.

01 Objects.
	02 Table-Objects Occurs 12 Times.
		03 Table-Objects-Rotation Occurs 4 Times.
			05 C-OBJ 	Pic x(9)      Value Space.

01 DimObjects.
	03 Table-Dim-Objects Occurs 12 Times.
		04 Table-Dim-Objects-Rotation Occurs 4 Times.
			05 C-DIM-OBJ-X Pic 9(4) Comp Value 0.
			05 C-DIM-OBJ-Y Pic 9(4) Comp Value 0.

01 NoRotationForObject.
	03 Table-No-Rotation Occurs 12 Times.
			05 C-NO-ROT	Pic 9(4) Comp Value 0.

*Upper left corner of the object.
01 Px		Pic 9(4) Comp.
01 Py		Pic 9(4) Comp.

*Variables defining window.
01 Disp         Pic 9(9) Comp.
01 Disp-E       Pic 9(9) Comp.
01 Disp-L	Pic 9(9) Comp.
01 Disp-O	Pic 9(9) Comp.
01 Disp-ED	Pic 9(9) Comp.
01 Paste        Pic 9(9) Comp.
01 Keyb         Pic 9(9) Comp.

*Displaying.
01 From-X	Pic 9(4) Comp Value  0.
01 From-Y	Pic 9(4) Comp Value  0.
01 To-X		Pic 9(4) Comp Value 17.
01 To-Y		Pic 9(4) Comp Value 19.

*GameBoard

01 Game-Table.
	02 Game-Table-X Occurs 17 Times.
		03 Game-Table-Y Occurs 19 Times.
			05 T-EL Pic 9(4) Comp Value 0.

*Selected object.

01 Game-Object.
	02 Game-Object-X Occurs 3 Times.
		03 Game-Object-Y Occurs 3 Times.
			05 O-EL Pic 9(4) Comp Value 0.

01 O-Idx	Pic 9(4) Comp Value 1.
01 O-Idx-Next	Pic 9(4) Comp Value 1.
01 O-Idx-Next-C	Pic 9(4) Comp Value 1.

*Structure for moving tests.

01 Move-Object-Test.
	02 Move-Object-X Occurs 3 Times.
		03 Move-Object-Y Occurs 3 Times.
			05 TMP-EL Pic 9(4) Comp Value 0.

*Structure for rotating tests.

01 Rotate-Object-Test.
	02 Rotate-Object-X Occurs 3 Times.
		03 Rotate-Object-Y Occurs 3 Times.
			05 ROT-EL Pic 9(4) Comp Value 0.

*
01 Key-Code	Pic  9(4) Comp.

01 K-N0		PIC 9(4) COMP VALUE  48.
01 K-N1		PIC 9(4) COMP VALUE  49.
01 K-N2		PIC 9(4) COMP VALUE  50.
01 K-N3		PIC 9(4) COMP VALUE  51.
01 K-N4		PIC 9(4) COMP VALUE  52.
01 K-N5		PIC 9(4) COMP VALUE  53.
01 K-N6		PIC 9(4) COMP VALUE  54.
01 K-N7		PIC 9(4) COMP VALUE  55.
01 K-N8		PIC 9(4) COMP VALUE  56.
01 K-N9		PIC 9(4) COMP VALUE  57.
01 K-UP         PIC 9(4) COMP VALUE 274.
01 K-DOWN       PIC 9(4) COMP VALUE 275.
01 K-LEFT       PIC 9(4) COMP VALUE 276.
01 K-RIGHT      PIC 9(4) COMP VALUE 277.
01 K-SPACE      PIC 9(4) COMP VALUE  32.
01 K-PF1        PIC 9(4) COMP VALUE 256.
01 K-PF2        PIC 9(4) COMP VALUE 257.
01 K-PF3        PIC 9(4) COMP VALUE 258.
01 K-PF4        PIC 9(4) COMP VALUE 259.
01 K-F10	PIC 9(4) COMP VALUE 290.
01 K-TIMEOUT	PIC 9(4) COMP VALUE 509.
01 K-CTRL-R	PIC 9(4) COMP VALUE  18.
01 K-CTRL-W	PIC 9(4) COMP VALUE  23.
01 K-UPPER-P	PIC 9(4) COMP VALUE  80.
01 K-LOWER-P	PIC 9(4) COMP VALUE 112.
01 KEYB-FLAG	PIC 9(9) COMP VALUE   0.

*
01 Points	Pic  S9(18) Comp.
01 Bonus	Pic   9(04) Comp.
01 Bonus-All	Pic   9(04) Comp Value 0.
01 Bonus-OUT	Pic   9(04) Comp Value 0.

*For HIGH SCORES.

01 HIGH-Stat	Pic x(2).

01 First-Level	Pic 9(4) Comp Value 0.
01 Player-Name	Pic x(20).

01 C-Max-HS	Pic 9(4) Comp Value 20.

01 HIGHSCORES.
	02 Scores-Table Occurs 20 Times.
		03 HS-NAME	PIC X(20).
		03 HS-POINTS	PIC 9(18) COMP.
		03 HS-LINES	PIC 9(09) COMP.
		03 HS-ENTRY	PIC 9(04) COMP.
		03 HS-LEVEL	PIC 9(04) COMP.
		03 HS-BONUS	PIC 9(04) COMP.
		03 HS-CPU	PIC 9(09) COMP.

*

01 To-Out	Pic  9(4) Comp.
01 To-Play	Pic  9(4) Comp.
01 To-Stop	Pic  9(4) Comp.
01 Try-OK	Pic  9(4) Comp.
01 Down-OK	Pic  9(4) Comp.
01 CMP-OK	Pic  9(4) Comp.
01 O-K		Pic  9(4) Comp.

01 TIndX	Pic  9(4) Comp.
01 TIndY	Pic  9(4) Comp.
01 Idx		Pic  9(4) Comp.
01 Idx1		Pic  9(4) Comp.
01 Idx2		Pic  9(4) Comp.
01 Idx3		Pic  9(4) Comp.
01 Idx4		Pic  9(4) Comp.
01 Idx-X	Pic  9(4) Comp.
01 Idx-Y	Pic  9(4) Comp.
01 Idx-MX	Pic  9(4) Comp.
01 Idx-MY	Pic  9(4) Comp.
01 Idx-Px	Pic  9(4) Comp.
01 Idx-Py	Pic  9(4) Comp.
01 Idx-R	Pic  9(4) Comp.
01 Idx-T	Pic  9(4) Comp.
01 Idx-HS	Pic  9(4) Comp.
01 NPx		Pic  9(4) Comp.
01 NPy		Pic  9(4) Comp.
01 NRot		Pic  9(4) Comp.
01 Min-X	Pic  9(4) Comp.
01 Max-X	Pic  9(4) Comp.
01 Min-Y	Pic  9(4) Comp.
01 Max-Y	Pic  9(4) Comp.
01 FrX		Pic  9(4) Comp.
01 FrY		Pic  9(4) Comp.
01 OldX		Pic  9(4) Comp.
01 OldY		Pic  9(4) Comp.
01 CtX		Pic  9(4) Comp.
01 CtY		Pic  9(4) Comp.
01 Crazy-Colors Pic  9(4) Comp.

01 LinX		Pic S9(9) Comp.
01 ColY		Pic S9(9) Comp.
01 X		Pic S9(9) Comp.
01 Y		Pic S9(9) Comp.

01 O-Points	Pic 9(08).
01 O-Level	Pic 9(01).
01 O-Lines	Pic 9(04).
01 O-CPU	Pic 9(09).
01 O-Dim	Pic 9(01).
01 Z-Points	Pic Z(08).
01 Z-Level	Pic Z(01).
01 Z-Entry	Pic Z(01).
01 Z-Lines	Pic Z(04).
01 Z-Bonus	Pic Z(04).
01 Z-CPU	Pic Z(09).
01 Z-Num	Pic Z9.

01 Odg		Pic x.
01 Txt80	Pic x(80).
01 Delim-Char	Pic X(01).


PROCEDURE DIVISION.

DECLARATIVES.
*******************
FILE-ERROR SECTION.
*******************
        USE AFTER STANDARD ERROR PROCEDURE ON FHIGH.
BEGIN.
	Continue.

END DECLARATIVES.

**************
MAIN SECTION.
**************
BEGIN.
	Perform INIT.

	Perform MAKE-ENVIRONMENT.

	Perform OWN-OBJECTS.

	Perform DRAW.

	Perform ACCEPT-LEVEL.

	Call "LIB$INIT_TIMER".

	Perform DRAW-FIELD-ALL.

	Perform UNTIL TO-PLAY = 0

		Perform MOVES

	End-Perform.

        Call "LIB$STAT_TIMER" Using T-Code T-Cont.

	Perform CLOS-ALL.

	If To-Stop = 0
		Perform INSERT-INTO-HIGH-SCORES
	End-If.

	Display "" Line 1 Column 1 Erase Screen.

	Stop RUN.



***************
MOVES SECTION.
***************
BEGIN.

	Move 0 to TO-OUT.

	Perform SELECT-OBJECT.

	Perform START-POSITION.
	If To-Play = 0
		Perform DRAW-FIELD-ALL
		Move 1 to To-OUT
		Perform INFORM-ABOUT-END
	End-If.

	Move 0 to Wait-Count.

	Perform UNTIL TO-OUT = 1

		If Wait-Count Not = 0 And To-OUT Not = 1
			Call "LIB$WAIT" Using WAIT-UNIT End-Call
		End-If
		Call "SMG$READ_KEYSTROKE" Using KEYB, KEY-CODE, Omitted,
						TIME-ZERO
		End-Call

		Add 1 to Wait-Count
		If Wait-Count >= Max-Wait
			Move         0 to Wait-Count
			Move K-TIMEOUT to KEY-CODE
		End-If

		Evaluate KEY-CODE

			When K-TIMEOUT  
				     If Wait-Count = 0
					     Perform MOVE-DOWN
					     If Down-OK = 0
						Move 1 to To-Out
					     End-If
				     End-If

			When K-N4
			When K-LEFT  Perform MOVE-LEFT
		
			When K-N6
			When K-RIGHT Perform MOVE-RIGHT

			When K-N8
			When K-UP    If C-NO-ROT(O-Idx) = 0
					Perform ROTATE
				     End-If

			When K-N2
			When K-DOWN
			When K-SPACE Perform MOVE-TO-BOTTOM

			When K-PF1 
			 	Move  8 to LinX
				Move  2 to ColY
				If Crazy-Colors = 1 Then
				 Move 0 to Crazy-Colors
				 Compute ReversedOUT = M_Reversed
				 Call "SMG$PUT_CHARS" Using DISP By Descriptor "PF1   : Crazy colors"
				        By Reference  LINX COLY
				 End-Call
				Else
				 Move 1 to Crazy-Colors
				 Compute ReversedOUT = M_Reversed + M_Bold + M_Underlined
				 Call "SMG$PUT_CHARS" Using DISP By Descriptor "PF1   : Crazy colors"
				        By Reference  LINX COLY
					Omitted ReversedOUT
				 End-Call
				End-If

			When K-F10
			When K-PF2
			When K-PF3
			When K-PF4   Move 1 to To-Out
				     Move 0 to To-Play
				     Move 1 to To-Stop

			When K-CTRL-R
			When K-CTRL-W Call "SMG$INVALIDATE_DISPLAY" Using DISP
				      End-Call
				      Call "SMG$REPAINT_SCREEN"     Using PASTE
				      End-Call

			When Other   Perform PAUSE-GAME

		End-Evaluate

	End-Perform.

	If To-Play = 1
		Perform CHK-COMPLETED-LINES
	End-If.


********************
PAUSE-GAME SECTION.
********************
BEGIN.

	Call "SMG$UNPASTE_VIRTUAL_DISPLAY" Using DISP PASTE.
	Display "" Line 1 Column 1 Erase Screen.

	Call "SMG$READ_KEYSTROKE" Using KEYB, KEY-CODE.

	Call "SMG$PASTE_VIRTUAL_DISPLAY" Using DISP PASTE POS-X POS-Y.

************************
MOVE-TO-BOTTOM SECTION.
************************
BEGIN.

	Compute OldX = Px.
	Compute OldY = Py.
	Compute CtX  = C-MAX-DIM-O.
	Compute CtY  = C-MAX-DIM-O.

	Perform With Test After UNTIL DOWN-OK = 0

		Move 0 to Down-OK

		If Py < (C-Dim-Y - OBJ-Y + 1)
			Perform TRY-MOVE-DOWN
		End-If
	
	End-Perform.

	Compute FrX = Px.
	Compute FrY = Py.
	Perform ERASE-DRAW-FIELD.

	Move 1 to To-OUT.


********************
MOVE-LEFT SECTION.
********************
BEGIN.

	If Px > 1
		Perform TRY-MOVE-LEFT
	End-If.


***********************
TRY-MOVE-LEFT SECTION.
***********************
BEGIN.

*Delete object O-EL from T-EL on position NPx, NPy.

	Compute NPx = Px.
	Compute NPy = Py.
	Perform DELETE-OBJECT.

*Get the existing field (dimesions OBJ-X,OBJ-Y) from T-EL on position NPx,NPy.

	Compute NPx = NPx - 1.
	Perform COPY-INTO-TMP.

*Put object O-EL to the copied field.

	Perform PUT-ONTO-TMP.
	If Try-OK = 0
		Compute NPx = Px
		Perform INSERT-OBJECT
	Else
		Compute Px = Px - 1
		Perform INSERT-TMP

		Compute FrX = Px
		Compute FrY = Py
		Compute CtX = C-MAX-DIM-O + 1
		Compute CtY = C-MAX-DIM-O
		Perform DRAW-FIELD
	End-If.


 
********************
MOVE-RIGHT SECTION.
********************
BEGIN.

	If Px < (C-Dim-X - OBJ-X + 1)
		Perform TRY-MOVE-RIGHT
	End-If.


***********************
TRY-MOVE-RIGHT SECTION.
***********************
BEGIN.

	Compute NPx = Px.
	Compute NPy = Py.
	Perform DELETE-OBJECT.

	Compute NPx = NPx + 1.
	Perform COPY-INTO-TMP.

	Perform PUT-ONTO-TMP.
	If Try-OK = 0
		Compute NPx = Px
		Perform INSERT-OBJECT
	Else
		Compute Px = Px + 1
		Perform INSERT-TMP

		Compute FrX = Px - 1
		Compute FrY = Py
		Compute CtX = C-MAX-DIM-O + 1
		Compute CtY = C-MAX-DIM-O
		Perform DRAW-FIELD
	End-If.


*******************
MOVE-DOWN SECTION.
*******************
BEGIN.

	Move 0 to Down-OK.

	If Py < (C-Dim-Y - OBJ-Y + 1)
		Perform TRY-MOVE-DOWN

		If Down-OK = 1
			Compute FrX = Px
			Compute FrY = Py - 1
			Compute CtX = C-MAX-DIM-O
			Compute CtY = C-MAX-DIM-O + 1
			Perform DRAW-FIELD
		End-If
	End-If.


***********************
TRY-MOVE-DOWN SECTION.
***********************
BEGIN.

	Compute NPx = Px.
	Compute NPy = Py.
	Perform DELETE-OBJECT.

	Compute NPy = NPy + 1.
	Perform COPY-INTO-TMP.

	Perform PUT-ONTO-TMP.
	If Try-OK = 0
		Compute NPy = Py
		Perform INSERT-OBJECT
	Else
		Compute Py = Py + 1
		Perform INSERT-TMP
		Move 1 to Down-OK
	End-If.



****************
ROTATE SECTION.
****************
BEGIN.

	Compute NRot = Rot + 1.
	If NRot > C-Max-Rot
		Move 1 to NRot
	End-If.

	Compute R-Obj-X = C-DIM-OBJ-X(O-Idx, NRot).
	Compute R-Obj-Y = C-DIM-OBJ-Y(O-Idx, NRot).

	If (R-Obj-X + Px - 1) > C-DIM-X Or
	   (R-Obj-Y + Py - 1) > C-DIM-Y
		Continue
	Else
		Perform ROTATE-RIGHT
	End-If.


***********************
ROTATE-RIGHT SECTION.
***********************
BEGIN.

	Compute NPx = Px.
	Compute NPy = Py.
	Perform DELETE-OBJECT.

	Perform COPY-INTO-TMP-ROT.

	Perform ROTATE-OBJECT.

	Perform PUT-ONTO-TMP-ROT.
	If Try-OK = 0
		Perform INSERT-OBJECT
	Else
		Compute ROT   = NROT
		Compute OBJ-X = R-OBJ-X
		Compute OBJ-Y = R-OBJ-Y
		Perform INSERT-TMP-ROT

		Compute FrX = Px
		Compute FrY = Py
		Compute CtX = C-MAX-DIM-O
		Compute CtY = C-MAX-DIM-O
		Perform DRAW-FIELD
	End-If.

***********************
ROTATE-OBJECT SECTION.
***********************
*(input C-OBJ, O-Idx, NRot; output ROT-EL)
BEGIN.

	Perform Varying TIndY From 1 By 1 Until TIndY > C-Max-Dim-O
	 Perform Varying TIndX From 1 By 1 Until TIndX > C-Max-Dim-O

		Compute Idx = (TIndY - 1) * C-Max-Dim-O + TIndX
		If C-OBJ(O-Idx, NRot)(Idx:1) = "1"
			Move 1 to ROT-EL(TIndX, TIndY)
		Else
			Move 0 to ROT-EL(TIndX, TIndY)
		End-If

	 End-Perform
	End-Perform.



************************
DELETE-OBJECT SECTION.
************************
*(input NPx, NPy, OBJ-X, OBJ-Y, O-EL, T-EL; output T-EL)
BEGIN.

	Compute Idx-MX = NPx + OBJ-X - 1.
	Compute Idx-MY = NPy + OBJ-Y - 1.

	Perform Varying Idx-Px From NPx By 1 Until Idx-Px > Idx-MX
	 Perform Varying Idx-Py From NPy By 1 Until Idx-Py > Idx-MY

		Compute Idx-X = Idx-Px - NPx + 1
		Compute Idx-Y = Idx-Py - NPy + 1

		If O-EL(Idx-X, Idx-Y) = 1
			Move 0 to T-EL(Idx-Px, Idx-Py)
		End-If

	 End-Perform
	End-Perform.


************************
INSERT-OBJECT SECTION.
************************
*(input NPx, NPy, OBJ-X, OBJ-Y, O-EL, T-EL; output T-EL)
BEGIN.

	Compute Idx-MX = NPx + OBJ-X - 1.
	Compute Idx-MY = NPy + OBJ-Y - 1.

	Perform Varying Idx-Px From NPx By 1 Until Idx-Px > Idx-MX
	 Perform Varying Idx-Py From NPy By 1 Until Idx-Py > Idx-MY

		Compute Idx-X = Idx-Px - NPx + 1
		Compute Idx-Y = Idx-Py - NPy + 1

		If O-EL(Idx-X, Idx-Y) = 1
			Move 1 to T-EL(Idx-Px, Idx-Py)
		End-If

	 End-Perform
	End-Perform.


************************
COPY-INTO-TMP SECTION.
************************
*(input NPx, NPy, OBJ-X, OBJ-Y, T-EL; output TMP-EL)
BEGIN.

	Compute Idx-MX = NPx + OBJ-X - 1.
	Compute Idx-MY = NPy + OBJ-Y - 1.

	Perform Varying Idx-Px From NPx By 1 Until Idx-Px > Idx-MX
	 Perform Varying Idx-Py From NPy By 1 Until Idx-Py > Idx-MY

		Compute Idx-X = Idx-Px - NPx + 1
		Compute Idx-Y = Idx-Py - NPy + 1

		Move T-EL(Idx-Px, Idx-Py) to TMP-EL(Idx-X, Idx-Y)

	 End-Perform
	End-Perform.


***************************
COPY-INTO-TMP-ROT SECTION.
***************************
*(input NPx, NPy, R-OBJ-X, R-OBJ-Y, T-EL; output TMP-EL)
BEGIN.

	Compute Idx-MX = NPx + R-OBJ-X - 1.
	Compute Idx-MY = NPy + R-OBJ-Y - 1.

	Perform Varying Idx-Px From NPx By 1 Until Idx-Px > Idx-MX
	 Perform Varying Idx-Py From NPy By 1 Until Idx-Py > Idx-MY

		Compute Idx-X = Idx-Px - NPx + 1
		Compute Idx-Y = Idx-Py - NPy + 1

		Move T-EL(Idx-Px, Idx-Py) to TMP-EL(Idx-X, Idx-Y)

	 End-Perform
	End-Perform.


**********************
PUT-ONTO-TMP SECTION.
**********************
*(input OBJ-X, OBJ-Y, O-EL, TMP-EL; output Try-OK)
BEGIN.

	Move 1 to Try-OK.

	Perform Varying Idx-X From 1 By 1 Until Idx-X > OBJ-X
	 Perform Varying Idx-Y From 1 By 1 Until Idx-Y > OBJ-Y

		Add O-EL(Idx-X, Idx-Y) to TMP-EL(Idx-X, Idx-Y)
		If TMP-EL(Idx-X, Idx-Y) > 1
			Move 0 to Try-OK
			Move OBJ-Y to Idx-Y
			Move OBJ-X to Idx-X
		End-If

	 End-Perform
	End-Perform.

**************************
PUT-ONTO-TMP-ROT SECTION.
**************************
*(input R-OBJ-X, R-OBJ-Y, ROT-EL, TMP-EL; output Try-OK)
BEGIN.

	Move 1 to Try-OK.

	Perform Varying Idx-X From 1 By 1 Until Idx-X > R-OBJ-X
	 Perform Varying Idx-Y From 1 By 1 Until Idx-Y > R-OBJ-Y

		Add ROT-EL(Idx-X, Idx-Y) to TMP-EL(Idx-X, Idx-Y)
		If TMP-EL(Idx-X, Idx-Y) > 1
			Move 0 to Try-OK
			Move R-OBJ-Y to Idx-Y
			Move R-OBJ-X to Idx-X
		End-If

	 End-Perform
	End-Perform.

*********************
INSERT-TMP SECTION.
*********************
*(input Px, Py, OBJ-X, OBJ-Y, TMP-EL, T-EL; output T-EL)
BEGIN.

	Compute Idx-MX = Px + OBJ-X - 1.
	Compute Idx-MY = Py + OBJ-Y - 1.

	Perform Varying Idx-Px From Px By 1 Until Idx-Px > Idx-MX
	 Perform Varying Idx-Py From Py By 1 Until Idx-Py > Idx-MY

		Compute Idx-X = Idx-Px - Px + 1
		Compute Idx-Y = Idx-Py - Py + 1

		Move TMP-EL(Idx-X, Idx-Y) to T-EL(Idx-Px, Idx-Py)

	 End-Perform
	End-Perform.

************************
INSERT-TMP-ROT SECTION.
************************
*(input Px, Py, OBJ-X, OBJ-Y, ROT-EL, TMP-EL, T-EL; output T-EL, O-EL)
BEGIN.

	Compute Idx-MX = Px + OBJ-X - 1.
	Compute Idx-MY = Py + OBJ-Y - 1.

	Perform Varying Idx-Px From Px By 1 Until Idx-Px > Idx-MX
	 Perform Varying Idx-Py From Py By 1 Until Idx-Py > Idx-MY

		Compute Idx-X = Idx-Px - Px + 1
		Compute Idx-Y = Idx-Py - Py + 1

		Move TMP-EL(Idx-X, Idx-Y) to T-EL(Idx-Px, Idx-Py)

	 End-Perform
	End-Perform.

	Perform Varying Idx-X From 1 By 1 Until Idx-X > C-MAX-DIM-O
	 Perform Varying Idx-Y From 1 By 1 Until Idx-Y > C-MAX-DIM-O

		Move ROT-EL(Idx-X, Idx-Y) to O-EL(Idx-X, Idx-Y)

	 End-Perform
	End-Perform.


*****************************
CHK-COMPLETED-LINES SECTION.
*****************************
BEGIN.
	Move 0 to Bonus.
	Move 0 to CMP-OK.

	Move C-DIM-Y to Idx-Y.
	
	Perform UNTIL Idx-Y < 1

		Perform CHK-FOR-ENTIRE-LINE
		If O-K = 0
			Compute Idx-Y = Idx-Y - 1
		Else
			Move 1 to CMP-OK
		End-If

	End-Perform.

	If CMP-OK = 1
		Perform DRAW-FIELD-ALL
	End-If.

	If Bonus-OUT = 1
		Call "SMG$PUT_CHARS" Using DISP 
				By Descriptor "             "
				By Reference  BONUS-X BONUS-Y
		End-Call
		Move 0 to Bonus-OUT
	End-If.

*Bonus only if more than 1 line completed!
	If Bonus > 1
		Compute Bonus = Bonus - 1
		Compute Points    = Points + 
				    (C-Line-Points * LEV-Factor(LEVEL) * Bonus)
		Move 1 to Bonus-OUT
		Compute Type-OUT = M_BOLD + M_BLINKING
		Call "SMG$PUT_CHARS" Using DISP 
				By Descriptor "*** BONUS ***"
				By Reference  BONUS-X BONUS-Y
				Omitted TYPE-OUT
		End-Call

		Compute Bonus-All = Bonus-All + Bonus
	End-If.


******************************
CHK-FOR-ENTIRE-LINE SECTION.
******************************
BEGIN.

	Move 1 to O-K.

	Perform Varying Idx-X From 1 By 1 Until Idx-X > C-DIM-X

		If T-EL(Idx-X, Idx-Y) = 0
			Move       0 to O-K
			Move C-DIM-X to Idx-X
		End-If

	End-Perform.

	If O-K = 1
		Perform DELETE-ENTIRE-LINE
		Add 1 to Bonus
	End-If.

****************************
DELETE-ENTIRE-LINE SECTION.
****************************
BEGIN.

	Perform Varying Idx-Py From Idx-Y By -1 Until Idx-Py <= 1
	 Perform Varying Idx-Px From 1 By 1 Until Idx-Px > C-DIM-X

		Move T-EL(Idx-Px, Idx-Py - 1) to T-EL(Idx-Px, Idx-Py)

	 End-Perform
	End-Perform.

	Perform Varying Idx-Px From 1 By 1 Until Idx-Px > C-DIM-X

		Move 0 to T-EL(Idx-Px, 1)

	End-Perform.

	Perform CHECK-LEVEL.

*********************
CHECK-LEVEL SECTION.
*********************
BEGIN.

	Compute CMP-Lines = CMP-Lines + 1.
	Compute Points    = Points + (C-Line-Points * LEV-Factor(LEVEL)).

	If LEVEL < Max-LEVEL
		If Function MOD(CMP-Lines, C-New-Level) = 0
			Divide CMP-Lines By C-New-Level Giving Lines-Factor
			End-Divide
			If Lines-Factor >= LEVEL
				Compute LEVEL = LEVEL + 1
				Move LEV-Count(LEVEL) to Max-Wait
			End-If
		End-If
	End-If.

********************************************************************************

**************************
DRAW-FIELD-ALL SECTION.
**************************
BEGIN.

	Call "SMG$BEGIN_DISPLAY_UPDATE" Using DISP.

	Perform Varying TIndY From 1 By 1 Until TIndY > C-Dim-Y
	 Perform Varying TIndX From 1 By 1 Until TIndX > C-Dim-X

		Compute ColY = TIndX + C-Origin-Y
		Compute LinX = TIndY + C-Origin-X

		If T-EL(TIndX, TIndY) = 0
			Perform DRAW-DELIMITER-CHAR
		Else
			Call "SMG$PUT_CHARS" Using DISP 
					     By Descriptor " "
					     By Reference LinX ColY
					     Omitted M_Reversed
			End-Call
		End-If

	 End-Perform
	End-Perform.

	Perform INFORM.

	Call "SMG$END_DISPLAY_UPDATE" Using DISP.

********************
DRAW-FIELD SECTION.
********************
BEGIN.

	Compute Max-Y = FrY + CtY - 1.
	Compute Max-X = FrX + CtX - 1.

	If Max-X > C-Dim-X Move C-Dim-X to Max-X End-If.
	If Max-Y > C-Dim-Y Move C-Dim-Y to Max-Y End-If.
	If FrX   < 1       Move       1 to FrX   End-If.
	If FrY   < 1       Move       1 to FrY   End-If.

	Call "SMG$BEGIN_DISPLAY_UPDATE" Using DISP.

	Perform Varying TIndY From FrY By 1 Until TIndY > Max-Y
	 Perform Varying TIndX From FrX By 1 Until TIndX > Max-X

		Compute ColY = TIndX + C-Origin-Y
		Compute LinX = TIndY + C-Origin-X

		If T-EL(TIndX, TIndY) = 0
			Perform DRAW-DELIMITER-CHAR
		Else
			Call "SMG$PUT_CHARS" Using DISP 
					     By Descriptor " "
					     By Reference LinX ColY
					     Omitted ReversedOUT
			End-Call
		End-If

	 End-Perform
	End-Perform.

	Call "SMG$END_DISPLAY_UPDATE" Using DISP.

**************************
ERASE-DRAW-FIELD SECTION.
**************************
BEGIN.
	If OldX Not = Px Or
	   OldY Not = Py
		Perform ERASE-FIRST-DRAW-NEXT
	End-If.


*******************************
ERASE-FIRST-DRAW-NEXT SECTION.
*******************************
BEGIN.

*Now erase object.
	Compute Max-Y = OldY + CtY - 1.
	Compute Max-X = OldX + CtX - 1.

	If Max-X > C-Dim-X Move C-Dim-X to Max-X End-If.
	If Max-Y > C-Dim-Y Move C-Dim-Y to Max-Y End-If.
	If OldX  < 1       Move       1 to OldX  End-If.
	If OldY  < 1       Move       1 to OldY  End-If.

	Call "SMG$BEGIN_DISPLAY_UPDATE" Using DISP.

	Perform Varying TIndY From OldY By 1 Until TIndY > Max-Y
	 Perform Varying TIndX From OldX By 1 Until TIndX > Max-X

		Compute ColY = TIndX + C-Origin-Y
		Compute LinX = TIndY + C-Origin-X

		Compute Idx-X = TIndX - OldX + 1
		Compute Idx-Y = TIndY - OldY + 1

		If O-EL(Idx-X, Idx-Y) = 1
			Perform DRAW-DELIMITER-CHAR
		End-If

	 End-Perform
	End-Perform.

*Then draw at the new position.
	Compute Max-Y = FrY + CtY - 1.
	Compute Max-X = FrX + CtX - 1.

	If Max-X > C-Dim-X Move C-Dim-X to Max-X End-If.
	If Max-Y > C-Dim-Y Move C-Dim-Y to Max-Y End-If.
	If FrX   < 1       Move       1 to FrX   End-If.
	If FrY   < 1       Move       1 to FrY   End-If.

	Perform Varying TIndY From FrY By 1 Until TIndY > Max-Y
	 Perform Varying TIndX From FrX By 1 Until TIndX > Max-X

		Compute ColY = TIndX + C-Origin-Y
		Compute LinX = TIndY + C-Origin-X

		If T-EL(TIndX, TIndY) = 0
			Perform DRAW-DELIMITER-CHAR
		Else
			Call "SMG$PUT_CHARS" Using DISP 
					     By Descriptor " "
					     By Reference LinX ColY
					     Omitted ReversedOUT
			End-Call
		End-If

	 End-Perform
	End-Perform.

	Call "SMG$END_DISPLAY_UPDATE" Using DISP.


****************
INFORM SECTION.
****************
BEGIN.

	Move Points to O-Points.
	Call "SMG$PUT_CHARS" Using DISP 
				By Descriptor O-POINTS
				By Reference  POINT-X POINT-Y
				Omitted M_BOLD.

	Move Level to O-Level.
	Call "SMG$PUT_CHARS" Using DISP 
				By Descriptor O-LEVEL
				By Reference  LEVEL-X LEVEL-Y
				Omitted M_BOLD.

	Move CMP-Lines to O-Lines.
	Call "SMG$PUT_CHARS" Using DISP 
				By Descriptor O-LINES
				By Reference  LINES-X LINES-Y
				Omitted M_BOLD.


************************
SELECT-OBJECT SECTION.
************************
BEGIN.

	Move O-Idx-Next to O-Idx.

	Move Function Current-Date(11:6) to SEED.

	Compute O-Idx-Next = Function RANDOM(SEED).
	Compute O-Idx-Next Rounded = (Function RANDOM * (C-Max-Obj)) + 1.

*	Coloring.
	Move M_Reversed to ReversedOUT.
	If Crazy-Colors = 1 Then
	 Compute O-Idx-Next-C Rounded = (Function RANDOM * 8) + 1
	 Evaluate O-Idx-Next-C
		When 2 	Compute ReversedOUT = M_Reversed + M_Bold
		When 3 	Compute ReversedOUT = M_Reversed + M_Bold + M_Underlined
		When 4 	Compute ReversedOUT = M_Reversed + M_Bold + M_Blinking
		When 5 	Compute ReversedOUT = M_Reversed + M_Bold + M_Underlined + M_Blinking
		When 6 	Compute ReversedOUT = M_Reversed + M_Underlined
		When 7 	Compute ReversedOUT = M_Reversed + M_Underlined + M_Blinking
		When 8 	Compute ReversedOUT = M_Reversed + M_Blinking
	 End-Evaluate
	End-If.


	If O-Idx-Next > C-Max-Obj
		Move C-Max-Obj to O-Idx-Next
	End-If.
	If O-Idx-Next = 0
		Move         1 to O-Idx-Next
	End-If.

	If O-Idx > C-Max-Obj
		Move C-Max-Obj to O-Idx
	End-If.
	If O-Idx = 0
		Move         1 to O-Idx
	End-If.

	Initialize GAME-OBJECT.

*Rotation state: basic state.
	Move                     1 to Rot.

	Move C-DIM-OBJ-X(O-Idx, Rot) to OBJ-X.
	Move C-DIM-OBJ-Y(O-Idx, Rot) to OBJ-Y.

	Perform Varying TIndY From 1 By 1 Until TIndY > C-Max-Dim-O
	 Perform Varying TIndX From 1 By 1 Until TIndX > C-Max-Dim-O

		Compute Idx = (TIndY - 1) * C-Max-Dim-O + TIndX
		If C-OBJ(O-Idx, Rot)(Idx:1) = "1"
			Move 1 to O-EL(TIndX, TIndY)
		Else
			Move 0 to O-EL(TIndX, TIndY)
		End-If

	 End-Perform
	End-Perform.

	Move           0 to From-X, From-Y.
	Move C-Max-Dim-O to To-X,   To-Y.

	Add C-Object-Points to Points.


************************
START-POSITION SECTION.
************************
BEGIN.

	Perform Varying TIndY From 1 By 1 Until TIndY > OBJ-Y
	 Perform Varying TIndX From 1 By 1 Until TIndX > OBJ-X

		Compute Idx = TIndX + C-Middle-X

		Add O-EL(TIndX, TIndY) to T-EL(Idx, TIndY)
		If T-EL(Idx, TIndY) > 1
			Move 0 to To-Play
		End-If

	 End-Perform
	End-Perform.

	Compute Px = C-Middle-X + 1.
	Compute Py = 1.

	Compute FrX = Px.
	Compute FrY = Py.
	Compute CtX = C-MAX-DIM-O.
	Compute CtY = C-MAX-DIM-O.

	Perform DRAW-FIELD.
	Perform INFORM.
	Perform DRAW-NEXT-OBJECT.


**************************
MAKE-ENVIRONMENT SECTION.
**************************
BEGIN.
	Display "" line 1 column 1 erase screen.

	Call "SMG$CREATE_PASTEBOARD"       Using PASTE.
	Call "SMG$CREATE_VIRTUAL_KEYBOARD" Using KEYB.
	Call "SMG$CREATE_VIRTUAL_DISPLAY"  Using ROWS COLUMNS DISP.

        Call "SMG$SET_KEYPAD_MODE"         Using KEYB, KEYB-Flag.

******************
CLOS-ALL SECTION.
******************
BEGIN.
	Call "SMG$SET_CURSOR_MODE" Using PASTE M_CUR_ON.

	Call "SMG$DELETE_VIRTUAL_KEYBOARD" Using KEYB.
	Call "SMG$DELETE_VIRTUAL_DISPLAY"  Using DISP.
	Call "SMG$DELETE_PASTEBOARD"       Using PASTE.

	Display "" line 1 column 1 erase screen.


***************************
DRAW-NEXT-OBJECT SECTION.
***************************
BEGIN.

	Perform Varying TIndY From 1 By 1 Until TIndY > C-Max-Dim-O
	 Perform Varying TIndX From 1 By 1 Until TIndX > C-Max-Dim-O

		Compute Idx = (TIndY - 1) * C-Max-Dim-O + TIndX

		Compute ColY = TIndX + PNO-X - 1
		Compute LinX = TIndY + PNO-Y - 1

		If C-OBJ(O-Idx-Next, Rot)(Idx:1) = "1"
			Call "SMG$PUT_CHARS" Using DISP 
					     By Descriptor " "
					     By Reference LinX ColY
					     Omitted ReversedOUT
			End-Call
		Else
			Call "SMG$PUT_CHARS" Using DISP 
					     By Descriptor " "
					     By Reference LinX ColY
			End-Call
		End-If

	 End-Perform
	End-Perform.


**************
DRAW SECTION.
**************
BEGIN.

	Move  1 to LinX.
	Move  1 to ColY.
	Move 14 to X.
	Move 22 to Y.
	Call "SMG$DRAW_RECTANGLE" Using DISP LinX ColY X Y.

	Move 16 to LinX.
	Move 68 to ColY.
	Move 22 to X.
	Move 74 to Y.
	Call "SMG$DRAW_RECTANGLE" Using DISP LinX ColY X Y.
	Move 69 to ColY.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "NEXT"
				        By Reference  LINX COLY.

	Move  2 to LinX.
	Move  2 to ColY.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "LEFT  : Move left"
				        By Reference  LINX COLY.
	Move  3 to LinX.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "RIGHT : Move right"
				        By Reference  LINX COLY.
	Move  4 to LinX.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "UP    : Rotate"
				        By Reference  LINX COLY.

	Move  6 to LinX.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "SPACE : Drop"
				        By Reference  LINX COLY.

	Move  8 to LinX.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "PF1   : Crazy colors"
				        By Reference  LINX COLY.
	Move  9 to LinX.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "F10   : Exit"
				        By Reference  LINX COLY.

	Move 11 to LinX.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "P     : Pause"
				        By Reference  LINX COLY.

	Move 13 to LinX.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "Domen Suligoj"
				        By Reference  LINX COLY.

	Move  1 to LinX.
	Move 62 to ColY.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "Points:"
				        By Reference  LINX COLY.
	Move  2 to LinX.
	Move 63 to ColY.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "Level:"
				        By Reference  LINX COLY.

	Move  3 to LinX.
	Move 63 to ColY.
	Call "SMG$PUT_CHARS" Using DISP By Descriptor "Lines:"
				        By Reference  LINX COLY.

	Compute LinX = C-Origin-X + 1.
	Compute ColY = C-Origin-Y.
	Compute X = C-Origin-X + C-Dim-X + 3.
	Call "SMG$DRAW_LINE" Using DISP LinX ColY X ColY M_Bold.

	Compute LinX = C-Origin-X + 1.
	Compute ColY = C-Origin-Y + C-Dim-Y - 1.
	Compute    X = C-Origin-X + C-Dim-X + 3.
	Call "SMG$DRAW_LINE" Using DISP LinX ColY X ColY M_Bold.

	Compute LinX = C-Origin-X + C-Dim-X + 3.
	Compute ColY = C-Origin-Y.
	Compute    Y = C-Origin-Y + C-Dim-Y - 1.
	Call "SMG$DRAW_LINE" Using DISP LinX ColY LinX Y M_Bold.


	Call "SMG$SET_CURSOR_MODE" Using PASTE M_CUR_OFF.

	Call "SMG$PASTE_VIRTUAL_DISPLAY" Using DISP PASTE POS-X POS-Y.

**********************
ACCEPT-LEVEL SECTION.
**********************
BEGIN.

	Move 1 to To-Play.
	Move 0 to To-Stop.

        Call "SMG$CREATE_VIRTUAL_DISPLAY"  Using ROWS-L COLUMNS-L DISP-L
						 M_BORDER.

	Move  1 to LinX.
	Move  2 to ColY.
	Call "SMG$PUT_CHARS" Using DISP-L By Descriptor 
					"Enter starting level (1-9): 1"
				        By Reference  LINX COLY.

	Call "SMG$PASTE_VIRTUAL_DISPLAY" Using DISP-L PASTE POS-XL POS-YL.

	Call "SMG$READ_KEYSTROKE" Using KEYB, KEY-CODE.

	Evaluate KEY-CODE

		When K-N2 Move 2 to Level
		When K-N3 Move 3 to Level
		When K-N4 Move 4 to Level
		When K-N5 Move 5 to Level
		When K-N6 Move 6 to Level
		When K-N7 Move 7 to Level
		When K-N8 Move 8 to Level
		When K-N9 Move 9 to Level

		When K-F10
		When K-PF1
		When K-PF2
		When K-PF3
		When K-PF4 Move 1 to To-Stop
			   Move 0 to To-Play

	End-Evaluate.

        Call "SMG$DELETE_VIRTUAL_DISPLAY"  Using DISP-L.

	Move LEV-Count(LEVEL) to Max-Wait.

	Move Level to First-Level.

**************************
INFORM-ABOUT-END SECTION.
**************************
BEGIN.

        Call "SMG$CREATE_VIRTUAL_DISPLAY"  Using ROWS-E  COLUMNS-E DISP-E
                                                 Omitted M_BLINKING.

	Call "SMG$DRAW_RECTANGLE" Using DISP-E RCX1 RCY1 RCX2 RCY2 M_REVERSED.

        Move 2 to LinX.
        Move 2 to ColY.

        Call "SMG$PUT_CHARS" Using DISP-E
                             By Descriptor " THE  END "
                             By Reference LinX ColY.

        Call "SMG$PASTE_VIRTUAL_DISPLAY" Using DISP-E PASTE POS-XE POS-YE.

        Call "SMG$READ_KEYSTROKE" Using KEYB, KEY-CODE.

        Call "SMG$DELETE_VIRTUAL_DISPLAY"  Using DISP-E.


**************
INIT SECTION.
**************
BEGIN.

	Move M_Reversed to ReversedOUT.
	Move 0 to Crazy-Colors.

*Predefined falling objects.
	Move  3 to OIdx-X(1).
	Move 15 to OIdx-X(2).
	Move 27 to OIdx-X(3).
	Move 39 to OIdx-X(4).

	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ1(OIdx-X(Idx):9) to C-OBJ(1,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ2(OIdx-X(Idx):9) to C-OBJ(2,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ3(OIdx-X(Idx):9) to C-OBJ(3,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ4(OIdx-X(Idx):9) to C-OBJ(4,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ5(OIdx-X(Idx):9) to C-OBJ(5,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ6(OIdx-X(Idx):9) to C-OBJ(6,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ7(OIdx-X(Idx):9) to C-OBJ(7,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ8(OIdx-X(Idx):9) to C-OBJ(8,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ9(OIdx-X(Idx):9) to C-OBJ(9,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ10(OIdx-X(Idx):9) to C-OBJ(10,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ11(OIdx-X(Idx):9) to C-OBJ(11,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move OBJ12(OIdx-X(Idx):9) to C-OBJ(12,Idx)
	End-Perform.

	Move  1 to OIdx-X(1).
	Move  2 to OIdx-Y(1).
	Move 13 to OIdx-X(2).
	Move 14 to OIdx-Y(2).
	Move 25 to OIdx-X(3).
	Move 26 to OIdx-Y(3).
	Move 37 to OIdx-X(4).
	Move 38 to OIdx-Y(4).

	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ1(OIDX-X(Idx):1)) to C-DIM-OBJ-X(1,Idx)
		Move Function NUMVAL(OBJ1(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(1,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ2(OIDX-X(Idx):1)) to C-DIM-OBJ-X(2,Idx)
		Move Function NUMVAL(OBJ2(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(2,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ3(OIDX-X(Idx):1)) to C-DIM-OBJ-X(3,Idx)
		Move Function NUMVAL(OBJ3(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(3,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ4(OIDX-X(Idx):1)) to C-DIM-OBJ-X(4,Idx)
		Move Function NUMVAL(OBJ4(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(4,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ5(OIDX-X(Idx):1)) to C-DIM-OBJ-X(5,Idx)
		Move Function NUMVAL(OBJ5(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(5,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ6(OIDX-X(Idx):1)) to C-DIM-OBJ-X(6,Idx)
		Move Function NUMVAL(OBJ6(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(6,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ7(OIDX-X(Idx):1)) to C-DIM-OBJ-X(7,Idx)
		Move Function NUMVAL(OBJ7(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(7,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ8(OIDX-X(Idx):1)) to C-DIM-OBJ-X(8,Idx)
		Move Function NUMVAL(OBJ8(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(8,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ9(OIDX-X(Idx):1)) to C-DIM-OBJ-X(9,Idx)
		Move Function NUMVAL(OBJ9(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(9,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ10(OIDX-X(Idx):1)) to C-DIM-OBJ-X(10,Idx)
		Move Function NUMVAL(OBJ10(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(10,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ11(OIDX-X(Idx):1)) to C-DIM-OBJ-X(11,Idx)
		Move Function NUMVAL(OBJ11(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(11,Idx)
	End-Perform.
	Perform Varying Idx From 1 By 1 Until Idx > 4
		Move Function NUMVAL(OBJ12(OIDX-X(Idx):1)) to C-DIM-OBJ-X(12,Idx)
		Move Function NUMVAL(OBJ12(OIDX-Y(Idx):1)) to C-DIM-OBJ-Y(12,Idx)
	End-Perform.

	Move OBJ1-No-Rot	to C-NO-ROT(1).
	Move OBJ2-No-Rot	to C-NO-ROT(2).
	Move OBJ3-No-Rot	to C-NO-ROT(3).
	Move OBJ4-No-Rot	to C-NO-ROT(4).
	Move OBJ5-No-Rot	to C-NO-ROT(5).
	Move OBJ6-No-Rot	to C-NO-ROT(6).
	Move OBJ7-No-Rot	to C-NO-ROT(7).
	Move OBJ8-No-Rot	to C-NO-ROT(8).
	Move OBJ9-No-Rot	to C-NO-ROT(9).
	Move OBJ10-No-Rot	to C-NO-ROT(10).
	Move OBJ11-No-Rot	to C-NO-ROT(11).
	Move OBJ12-No-Rot	to C-NO-ROT(12).

*Set levels.
	Move 100		to LEV-Count(1).
	Move  90		to LEV-Count(2).
	Move  80		to LEV-Count(3).
	Move  60		to LEV-Count(4).
	Move  50		to LEV-Count(5).
	Move  40		to LEV-Count(6).
	Move  30		to LEV-Count(7).
	Move  25		to LEV-Count(8).
	Move  15		to LEV-Count(9).

	Move     1 	to LEV-Factor(1).
	Move     2 	to LEV-Factor(2).
	Move     5 	to LEV-Factor(3).
	Move    10 	to LEV-Factor(4).
	Move    20 	to LEV-Factor(5).
	Move    50	to LEV-Factor(6).
	Move   100 	to LEV-Factor(7).
	Move   200 	to LEV-Factor(8).
	Move   500 	to LEV-Factor(9).

	Move  1		      to Level.
	Move  0		      to Cmp-Lines.
	Move  0		      to Bonus-All.
	Move LEV-Count(LEVEL) to Max-Wait

*Initialize RANDOM.
	Move Function Current-Date(9:6) to SEED.
	Move                          1 to O-Idx.
	Move                          1 to O-Idx-Next.

*Set points.
	Move C-First-Points		to Points.

	Move 1		to FrX.
	Move 1		to FrY.
	Move C-DIM-X	to CtX.
	Move C-DIM-Y	to CtY.

***********************
INIT-DRAW-POS SECTION.
***********************
BEGIN.

	Perform Varying Idx-R From 1 By 1 Until Idx-R > C-MAX-ROT
		Move  1 to DT-X(1, Idx-R)
		Move  1 to DT-X(7, Idx-R)
	End-Perform.
	Perform Varying Idx-R From 1 By 1 Until Idx-R > C-MAX-ROT
		Move  5 to DT-X(2, Idx-R)
		Move  5 to DT-X(8, Idx-R)
	End-Perform.
	Perform Varying Idx-R From 1 By 1 Until Idx-R > C-MAX-ROT
		Move  9 to DT-X(3, Idx-R)
		Move  9 to DT-X(9, Idx-R)
	End-Perform.
	Perform Varying Idx-R From 1 By 1 Until Idx-R > C-MAX-ROT
		Move 13 to DT-X(4, Idx-R)
		Move 13 to DT-X(10, Idx-R)
	End-Perform.
	Perform Varying Idx-R From 1 By 1 Until Idx-R > C-MAX-ROT
		Move 17 to DT-X(5, Idx-R)
		Move 17 to DT-X(11, Idx-R)
	End-Perform.
	Perform Varying Idx-R From 1 By 1 Until Idx-R > C-MAX-ROT
		Move 21 to DT-X(6, Idx-R)
		Move 21 to DT-X(12, Idx-R)
	End-Perform.

	Perform Varying Idx From 1 By 1 Until Idx > 6
		Move  2 to DT-Y(Idx, 1)
		Move 11 to DT-Y(Idx, 2)
		Move 20 to DT-Y(Idx, 3)
		Move 29 to DT-Y(Idx, 4)
	End-Perform.
	Perform Varying Idx From 7 By 1 Until Idx > C-MAX-OBJ
		Move 45 to DT-Y(Idx, 1)
		Move 54 to DT-Y(Idx, 2)
		Move 63 to DT-Y(Idx, 3)
		Move 72 to DT-Y(Idx, 4)
	End-Perform.


*********************
OWN-OBJECTS SECTION.
*********************
BEGIN.

	Perform INIT-DRAW-POS.

	Call "SMG$CREATE_VIRTUAL_DISPLAY"  Using ROWS-O COLUMNS-O DISP-O.
	Call "SMG$PUT_STATUS_LINE" Using PASTE
			           By Descriptor LABEL-O.

	Call "SMG$CREATE_VIRTUAL_DISPLAY"  Using ROWS-ED COLUMNS-ED DISP-ED.
	Call "SMG$LABEL_BORDER" Using DISP-ED
				By Descriptor "Edit object".


	Perform Varying Idx From 1 By 1 Until Idx > C-MAX-OBJ
	 Perform Varying Idx-R From 1 By 1 Until Idx-R > C-MAX-ROT

		Perform DRAW-OBJECT

	 End-Perform
	End-Perform.


	Call "SMG$PASTE_VIRTUAL_DISPLAY" Using DISP-O PASTE POS-XO POS-YO.

	Call "SMG$READ_KEYSTROKE" Using KEYB, KEY-CODE.

	Call "SMG$DELETE_VIRTUAL_DISPLAY"  Using DISP-O.
	Call "SMG$DELETE_VIRTUAL_DISPLAY"  Using DISP-ED.

	Call "SMG$PUT_STATUS_LINE" Using PASTE
			           By Descriptor "".

*********************
DRAW-OBJECT SECTION.
*********************
*(input Idx, Idx-R, C-OBJ(Idx,Idx-R), C-DIM-OBJ-XY(Idx,Idx-R), DT-X,DT-Y)
BEGIN.

	Compute X = DT-X(Idx, Idx-R).
	Compute Y = DT-Y(Idx, Idx-R).
	Move C-DIM-OBJ-X(Idx, Idx-R) to O-DIM.
	Call "SMG$PUT_CHARS" Using DISP-O By Descriptor O-DIM
					  By Reference X Y.

	Add 1 to Y.
	Call "SMG$PUT_CHARS" Using DISP-O By Descriptor "x"
					  By Reference X Y.

	Add 1 to Y
	Move C-DIM-OBJ-Y(Idx, Idx-R) to O-DIM.
	Call "SMG$PUT_CHARS" Using DISP-O By Descriptor O-DIM
					  By Reference X Y.


	Perform Varying TIndY From 1 By 1 Until TIndY > C-Max-Dim-O
	 Perform Varying TIndX From 1 By 1 Until TIndX > C-Max-Dim-O

		Compute Idx-T = (TIndY - 1) * C-Max-Dim-O + TIndX

		Compute ColY = TIndX + DT-Y(Idx, Idx-R) + 2
		Compute LinX = TIndY + DT-X(Idx, Idx-R) - 1

		If C-OBJ(Idx, Idx-R)(Idx-T:1) = "1"
			Call "SMG$PUT_CHARS" Using DISP-O
					     By Descriptor " "
					     By Reference LinX ColY
					     Omitted ReversedOUT
			End-Call
		Else
			Call "SMG$PUT_CHARS" Using DISP-O
					     By Descriptor " "
					     By Reference LinX ColY
			End-Call
		End-If

	 End-Perform
	End-Perform.




*********************************
INSERT-INTO-HIGH-SCORES SECTION.
*********************************
BEGIN.

	Open Input FHIGH.
	If HIGH-Stat Not = "00"
		Open OUTPUT FHIGH

		Initialize HIGHSCORES
		Initialize SHIGH

		Perform Varying Idx-HS From 1 By 1 Until Idx-HS > C-Max-HS
			Write SHIGH End-Write
		End-Perform

		Close FHIGH
		Open INPUT FHIGH
	End-If.

	If HIGH-Stat = "00"
		Perform  READ-HIGH
		Perform  CHK-FOR-INSERT-HIGH
		If Idx > 0
			Perform  INSERT-HIGH
			Close    FHIGH
			Open I-O FHIGH
			If HIGH-Stat = "00"
				Perform REWRITE-HIGH
			Else
			 Display " "
			 Display "Sorry, someone is locking the HIGH SCORES. Status " No
			 Display HIGH-Stat Bold
			End-If
		Else
			Perform SHOW-HIGH-SCORES
			Display "SEE HIGH SCORES" Reversed Bold 
						  Line 23 Column 33 No
			Accept ODG Protected With No Echo Line 24 Column 80
		End-If
	Else
		Display " "
		Display "Sorry, someone is locking the HIGH SCORES. Status " No
		Display HIGH-Stat Bold
	End-If.

	Close FHIGH.

*******************
READ-HIGH SECTION.
*******************
BEGIN.

	Initialize HIGHSCORES.

	Perform Varying Idx-HS From 1 By 1 Until Idx-HS > C-Max-HS

		Read FHIGH  At End Move C-Max-HS to Idx-HS
			Not At End
				Move HIGH-Name		to HS-NAME(Idx-HS)
				Move HIGH-Points	to HS-POINTS(Idx-HS)
				Move HIGH-Lines		to HS-LINES(Idx-HS) 
				Move HIGH-Entry		to HS-ENTRY(Idx-HS) 
				Move HIGH-Level		to HS-LEVEL(Idx-HS) 
				Move HIGH-Bonus		to HS-BONUS(Idx-HS)
				Move HIGH-CPU		to HS-CPU(Idx-HS) 
		End-Read

	End-Perform.

*********************
REWRITE-HIGH SECTION.
*********************
BEGIN.

	Perform Varying Idx-HS From 1 By 1 Until Idx-HS > C-Max-HS

		Read FHIGH  At End Move C-Max-HS to Idx-HS
			Not At End
				Move HS-NAME(Idx-HS)	to HIGH-Name
				Move HS-POINTS(Idx-HS)	to HIGH-Points
				Move HS-LINES(Idx-HS) 	to HIGH-Lines
				Move HS-ENTRY(Idx-HS) 	to HIGH-Entry
				Move HS-LEVEL(Idx-HS) 	to HIGH-Level
				Move HS-BONUS(Idx-HS)	to HIGH-Bonus
				Move HS-CPU(Idx-HS) 	to HIGH-CPU
				ReWrite SHIGH End-ReWrite
		End-Read

	End-Perform.


*****************************
CHK-FOR-INSERT-HIGH SECTION.
*****************************
BEGIN.
	Move 0 to Idx.

	Perform Varying Idx-HS From 1 By 1 Until Idx-HS > C-Max-HS

		If HS-Points(Idx-HS) <= Points
			Move Idx-HS   to Idx
			Move C-Max-HS to Idx-HS
		End-If

	End-Perform.


*********************
INSERT-HIGH SECTION.
*********************
BEGIN.

	Perform Varying Idx-HS From C-Max-HS By -1 Until Idx-HS <= Idx

		Move SCORES-TABLE(Idx-HS - 1) to SCORES-TABLE(Idx-HS)

	End-Perform.

	Move Space		to HS-NAME(Idx)
	Move Points		to HS-POINTS(Idx)
	Move CMP-Lines		to HS-LINES(Idx) 
	Move First-Level	to HS-ENTRY(Idx) 
	Move Level		to HS-LEVEL(Idx) 
	Move Bonus-All		to HS-BONUS(Idx)
	Move T-Cont		to HS-CPU(Idx).

	Perform SHOW-HIGH-SCORES.

	Perform EDIT-PLAYER-NAME.

	Move Player-Name	to HS-NAME(Idx).

**************************
SHOW-HIGH-SCORES SECTION.
**************************
BEGIN.

	Display "" Line 1 Column 1 Erase Screen.

	Move All "-" to Txt80.
	Display Txt80 Line 1 Column 1.

	Display "Name" 		Bold Line 1 Column  5 No.
	Display "Points"	Bold Line 1 Column 31 No.
	Display "Lines"		Bold Line 1 Column 40 No.
	Display "Level"		Bold Line 1 Column 49 No.
	Display "Bonus"		Bold Line 1 Column 57 No.
	Display "CPU"		Bold Line 1 Column 73 No.

	Perform Varying Idx-HS From 1 By 1 Until Idx-HS > C-Max-HS

		Compute LinX = Idx-HS + 1

		Move HS-Points(Idx-HS)		to Z-Points
		Move HS-Lines(Idx-HS)		to Z-Lines
		Move HS-Entry(Idx-HS)		to Z-Entry
		Move HS-Level(Idx-HS)		to Z-Level
		Move HS-Bonus(Idx-HS)		to Z-Bonus
		Move HS-CPU(Idx-HS)		to Z-CPU
		Move Idx-HS			to Z-Num

		Move Space to Txt80
		String  Z-Num ". " 
			HS-NAME(Idx-HS) "    " Z-Points "    " Z-Lines "    "
			Z-Entry "   " Z-Level "    " Z-Bonus "     " Z-CPU
			Delimited By Size Into Txt80

		If Idx-HS = Idx
			Display Txt80 Bold Line LINX Column 1 No
		Else
			Display Txt80      Line LINX Column 1 No
		End-If

	End-Perform.
	
	Move All "-" to Txt80.
	Display Txt80 Line 22 Column 1.

***************************
EDIT-PLAYER-NAME SECTION.
***************************
BEGIN.

	Display "Congratulations. You will enter into HIGH SCORES." 
		Reversed Bold Line 23 Column 1.

	Display "ENTER YOUR NAME" Bold Blinking Line 24 Column 1 No.

	Move Space to PLAYER-NAME.
	Compute LinX = Idx + 1.
	Accept  PLAYER-NAME Reversed Protected Editing Line LinX Column 5
		Default Is SPACES.

	If PLAYER-NAME = Space Move "Anonymous" to PLAYER-NAME.

***************************
DRAW-DELIMITER-CHAR SECTION.
***************************
BEGIN.

	If Function MOD(TIndX, 2) = 0
				Move C-Delim-C2 To Delim-Char
				Compute Type-OUT = M_BOLD
	Else
				Move C-Delim-C1 To Delim-Char
				Compute Type-OUT = 0
	End-If
	Call "SMG$PUT_CHARS" Using DISP 
					     By Descriptor Delim-Char
					     By Reference LinX ColY
					     Omitted Type-OUT
	End-Call.
