   IDENTIFICATION DIVISION.
       PROGRAM-ID. CHECKERS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GNUCOBOL.
       OBJECT-COMPUTER. GNUCOBOL.
       SPECIAL-NAMES.
           CONSOLE IS CRT.
       REPOSITORY.
           FUNCTION ABS INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  X-KING CONSTANT AS -2.
       01  X-MAN  CONSTANT AS -1.
       01  EMPTY  CONSTANT AS 0.
       01  O-MAN  CONSTANT AS 1.
       01  O-KING CONSTANT AS 2.
      * Dimensions of S and R are base 0 in BASIC
       01  VECTOR.
           10  R OCCURS 5              PIC S99.
       01  BOARD.
           10  ROW OCCURS 8.
               20 S OCCURS 8           PIC S9.
       01  INITVALS PIC X(32)
           VALUE "+1+0+1+0+0+0-1+0+0+1+0+0+0-1+0-1".
       01  INITDATA REDEFINES INITVALS.
           10 VAL OCCURS 16            PIC S9 SIGN IS LEADING SEPARATE.
       77  A                           PIC S9.
       77  A1                          PIC 9.
       77  B                           PIC S9.
       77  B1                          PIC 9.
       77  C                           PIC S9.
       77  E                           PIC S9.
       77  G                           PIC S9.
       77  H                           PIC S9.
       77  I                           PIC S99.
       77  J                           PIC S99.
       77  L                           PIC 9.
       77  M                           PIC 9.
       77  Q                           PIC S99.
       77  P                           PIC XXX.
       77  T                           PIC 9.
       77  U                           PIC S99.
       77  V                           PIC S99.
       77  X                           PIC 99.
       77  Y                           PIC 9.
       77  Z                           PIC 9.

       SCREEN SECTION.
       01  MOVE-MASK LINE 2 COL 1.
           05  VALUE "COMPUTER MOVES FROM ".
           05  FROM-X                  PIC 9 FROM R(2).
           05  VALUE ",".
           05  FROM-Y                  PIC 9 FROM R(3).
           05  VALUE " TO ".
           05  TO-X                    PIC 9 FROM R(4).
           05  VALUE ",".
           05  TO-Y                    PIC 9 FROM R(5).
       01  EXTRA-TO-MASK.
           05  VALUE "TO ".
           05  TO-X                    PIC 9 FROM R(4).
           05  VALUE ",".
           05  TO-Y                    PIC 9 FROM R(5).
       01 FROM-ENTRY LINE 23 COL 1.
           05 VALUE "ENTER FROM: ".
           05 X-INPUT                  PIC 9 TO E AUTO-SKIP.
           05 VALUE ",".
           05 Y-INPUT                  PIC 9 TO H AUTO-SKIP.
       01 TO-ENTRY LINE 23 COL 16.
           05 VALUE " TO ".
           05 X-INPUT                  PIC 9 TO A AUTO-SKIP.
           05 VALUE ",".
           05 Y-INPUT                  PIC 9 TO B AUTO-SKIP.
       01 PLUS-TO-ENTRY.
           05 VALUE " +TO ".
           05 X-INPUT                  PIC 9 TO A1 AUTO-SKIP.
           05 VALUE ",".
           05 Y-INPUT                  PIC 9 TO B1 AUTO-SKIP.
       01  ENTRY-SPACER VALUE " "      LINE 23 COL 1 BLANK LINE.
       01 X-LEGEND VALUE "    1    2    3    4    5    6    7    8    "
               REVERSE-VIDEO.
       01 Y-SPACER VALUE " " REVERSE-VIDEO.
       01 Y-LEGEND                     PIC 9 FROM Y REVERSE-VIDEO.
       01 CLEAR-MSG-LINE VALUE " "     LINE 24 COL 10 BLANK LINE.
       01 MSG-ILLEGAL-MOVE VALUE "ILLEGAL MOVE" LINE 24 COL 10
               BLANK LINE.
       01 MSG-I-WIN VALUE "I WIN."     LINE 24 COL 10.
       01 MSG-YOU-WIN VALUE "YOU WIN." LINE 24 COL 10.

       PROCEDURE DIVISION.
       CHECKERS.
000005     DISPLAY "CHECKERS" AT 0136
000010     DISPLAY "CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY" AT 0220
000015     DISPLAY " " AT 0316
000020     DISPLAY "THIS IS THE GAME OF CHECKERS.  THE COMPUTER IS X,"
            AT 0416
000025     DISPLAY "AND YOU ARE O.  THE COMPUTER WILL MOVE FIRST."
            AT 0516
000030     DISPLAY "SQUARES ARE REFERRED TO BY A COORDINATE SYSTEM,"
            AT 0616
000035     DISPLAY "WHERE (1,1) IS THE LOWER LEFT CORNER." AT 0716
000040     DISPLAY "(1,8) IS THE UPPER LEFT CORNER" AT 0816
000045     DISPLAY "(8,1) IS THE LOWER RIGHT CORNER" AT 0916
000050     DISPLAY "(8,8) IS THE UPPER RIGHT CORNER" AT 1016
000055     DISPLAY "THE COMPUTER WILL TYPE '+TO' WHEN YOU HAVE ANOTHER"
            AT 1116
000060     DISPLAY "JUMP. TYPE TWO ZEROES IF YOU CANNOT JUMP." AT 1216
           DISPLAY "READY TO PLAY (Y/N)?" AT 1416
           ACCEPT P AT 1437
      * Kids these days -- they want everything, even lowercase letters
           IF P = "N" OR "n" STOP RUN.
000065     DISPLAY " " BLANK SCREEN
000080*    DIM R(5),S(7,7)
           MOVE -1 TO G.
           MOVE -99 TO R(1).
000090*    DATA 1,0,1,0,0,0,-1,0,0,1,0,0,0,-1,0,-1,15
           MOVE 1 TO I.
000120     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 8
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 8
                   MOVE VAL(I) TO S(X,Y)
                   ADD 1 TO I
                   IF I > 16 THEN MOVE 1 TO I END-IF
000200         END-PERFORM
           END-PERFORM.

      * Computer calculates next move
000230 LINE0230.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 8
               PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 8
                   IF S(X,Y) > -1 THEN EXIT PERFORM CYCLE END-IF
000310             IF S(X,Y) = X-MAN THEN
                       PERFORM VARYING A FROM -1 BY 2 UNTIL A > 1
                           MOVE G TO B
                           PERFORM LINE0650
                       END-PERFORM
                   END-IF
000330             IF S(X,Y) = X-KING THEN
                       PERFORM VARYING A FROM -1 BY 2 UNTIL A > 1
                           PERFORM VARYING B FROM -1 BY 2 UNTIL B > 1
                               PERFORM LINE0650
                           END-PERFORM
                       END-PERFORM
                   END-IF
               END-PERFORM
           END-PERFORM.
           GO TO LINE1140.

000650 LINE0650.
           ADD X TO A GIVING U
           ADD Y TO B GIVING V
           IF U < 1 OR U > 8 OR V < 1 OR V > 8 THEN EXIT PARAGRAPH.
000740     IF S(U,V) = EMPTY THEN
               PERFORM LINE0910
               EXIT PARAGRAPH.
000770     IF S(U,V) < 0 THEN EXIT PARAGRAPH.
000790     ADD A TO U.
           ADD B TO V.
           IF U < 1 OR V < 1 OR U > 8 OR V > 8 THEN EXIT PARAGRAPH.
000850     IF S(U,V) = EMPTY THEN PERFORM LINE0910.

000910 LINE0910.
           IF V=1 AND S(X,Y) = X-MAN THEN ADD 2 TO Q.
000920     IF ABS(Y - V) = 2 THEN ADD 5 TO Q.
000960     IF Y=8 THEN SUBTRACT 2 FROM Q.
000980     IF U=1 OR U=8 THEN ADD 1 TO Q.
001030     PERFORM VARYING C FROM -1 BY 2 UNTIL C > 1
               IF U + C < 1 OR U + C > 8 OR V + G < 1 THEN
                   EXIT PERFORM CYCLE
               END-IF
001035         IF S(U + C,V + G) < 0 THEN
                   ADD 1 TO Q
                   EXIT PERFORM CYCLE
               END-IF
001040         IF U - C < 1 OR U - C > 8 OR V - G > 8 THEN
                   EXIT PERFORM CYCLE
               END-IF
001045         IF S(U + C, V + G) > 0
                 AND (S(U - C, V - G) = 0 OR (U - C = X AND V - G = Y))
                   THEN SUBTRACT 2 FROM Q
               END-IF
001080     END-PERFORM
           IF Q > R(1) THEN
               MOVE Q TO R(1)
               MOVE X TO R(2)
               MOVE Y TO R(3)
               MOVE U TO R(4)
               MOVE V TO R(5)
           END-IF
001100     MOVE 0 TO Q.
      * Display computer move
001140 LINE1140.
           IF R(1) = -99 THEN GO TO LINE1880.
           DISPLAY SPACE AT 0201 BLANK LINE
001230     DISPLAY MOVE-MASK
           MOVE -99 TO R(1)
           MOVE 32 TO I.
001240 LINE1240.
           IF R(5) = 1 THEN
               MOVE X-KING TO S(R(4),R(5))
           ELSE
001250         MOVE S(R(2),R(3)) TO S(R(4),R(5)).
001310     MOVE EMPTY TO S(R(2),R(3))
           IF ABS(R(2) - R(4)) <> 2 THEN GO TO LINE1420.
001330     MOVE EMPTY TO S((R(2)+R(4))/2, (R(3)+R(5))/2).
001340     MOVE R(4) TO X.
           MOVE R(5) TO Y.
           IF S(X,Y) = X-MAN THEN
               MOVE -2 TO B
               PERFORM LINE1370
                   VARYING A FROM -2 BY 4 UNTIL A > 2
           ELSE
001350         IF S(X,Y) = X-KING THEN
                   PERFORM VARYING A FROM -2 BY 4 UNTIL A > 2
                       PERFORM LINE1370
                           VARYING B FROM -2 BY 4 UNTIL B > 2
001360             END-PERFORM
               END-IF
           END-IF
           IF R(1) <> -99 THEN
               DISPLAY EXTRA-TO-MASK AT LINE 02 COLUMN I
               ADD 7 TO I
               MOVE -99 TO R(1)
               GO TO LINE1240.
001365     GO TO LINE1420.
      * See if there is a piece to jump over.
001370 LINE1370.
           ADD X TO A GIVING U
           ADD Y TO B GIVING V
           IF U<1 OR U>8 OR V<1 OR V>8 THEN EXIT PARAGRAPH.
001380     IF S(U,V) = EMPTY AND S(X + A / 2, Y + B / 2) > 0 THEN
               PERFORM LINE0910.
      * Display board
001420 LINE1420.
           DISPLAY X-LEGEND AT LINE 4 COLUMN 19
           PERFORM VARYING Y FROM 8 BY -1 UNTIL Y < 1
               MULTIPLY Y BY 2 GIVING J
               SUBTRACT J FROM 21 GIVING J
               DISPLAY Y-LEGEND AT LINE J COLUMN 19
               PERFORM VARYING X FROM 1 BY 1 UNTIL X > 8
                   MULTIPLY X BY 5 GIVING I
                   ADD 18 TO I
001430             IF S(X,Y) = EMPTY THEN DISPLAY ". "
                                      AT LINE J COLUMN I
                   END-IF
001470             IF S(X,Y) = O-MAN THEN DISPLAY "O "
                                      AT LINE J COLUMN I
                   END-IF
001490             IF S(X,Y) = X-MAN THEN DISPLAY "X "
                                      AT LINE J COLUMN I
                   END-IF
001510             IF S(X,Y) = X-KING THEN DISPLAY "X*"
                                      AT LINE J COLUMN I
                   END-IF
001530             IF S(X,Y) = O-KING THEN DISPLAY "O*"
                                      AT LINE J COLUMN I
                   END-IF
001550         END-PERFORM
               ADD 4 TO I
               DISPLAY Y-LEGEND AT LINE J COLUMN I
               ADD 1 TO J
               DISPLAY Y-SPACER AT LINE J COLUMN 19
               DISPLAY Y-SPACER AT LINE J COLUMN I
               DISPLAY X-LEGEND AT LINE 21 COLUMN 19
           END-PERFORM
      * Check if one player has no pieces left
001552     PERFORM VARYING L FROM 1 BY 1 UNTIL L > 8
001554         PERFORM VARYING M FROM 1 BY 1 UNTIL M > 8
001556             IF S(L,M) = O-MAN OR S(L,M) = O-KING THEN
                       MOVE 1 TO Z
                   END-IF
001558             IF S(L,M) = X-MAN OR S(L,M) = X-KING THEN
                       MOVE 1 TO T
                   END-IF
001560         END-PERFORM
001562     END-PERFORM
001564     IF Z <> 1 THEN GO TO LINE1885.
001566     IF T <> 1 THEN GO TO LINE1880.
001570     MOVE 0 TO Z
           MOVE 0 TO T.
           DISPLAY CLEAR-MSG-LINE.
      * Ask for player move
001590 LINE1590.
           DISPLAY ENTRY-SPACER
           MOVE 0 TO X-INPUT OF FROM-ENTRY, Y-INPUT OF FROM-ENTRY
           ACCEPT FROM-ENTRY
           IF E = 0 THEN STOP RUN.
           MOVE E TO X.
           MOVE H TO Y.
           IF S(X,Y) <= 0 THEN
               DISPLAY MSG-ILLEGAL-MOVE
               GO TO LINE1590
           END-IF.
           DISPLAY CLEAR-MSG-LINE.
001670 LINE1670.
           MOVE 0 TO X-INPUT OF TO-ENTRY, Y-INPUT OF TO-ENTRY
           ACCEPT TO-ENTRY
           IF A = 0 THEN GO TO LINE1590.
           MOVE A TO X
           MOVE B TO Y
001680     IF S(X,Y) = EMPTY AND ABS(A - E) <= 2
                             AND ABS(A - E) = ABS(B - H)
                THEN NEXT SENTENCE
           ELSE
001690         DISPLAY MSG-ILLEGAL-MOVE
               GO TO LINE1670.
001700     MOVE 23 TO I.
001750 LINE1750.
           MOVE S(E,H) TO S(A,B)
           MOVE EMPTY TO S(E,H)
           IF ABS(E - A) <> 2 THEN GO TO LINE1810.
      * Erase jumped-over piece
001800     MOVE EMPTY TO S((E + A)/2,(H + B)/2).
001802 LINE1802.
      * Player jumped. Ask for second move
           DISPLAY CLEAR-MSG-LINE
           MOVE 0 TO X-INPUT OF PLUS-TO-ENTRY, Y-INPUT OF PLUS-TO-ENTRY
           ACCEPT PLUS-TO-ENTRY AT LINE 23 COLUMN I
           IF A1 < 1 THEN GO TO LINE1810.
001804     IF S(A1,B1) <> EMPTY OR ABS(A1 - A) <> 2 OR ABS(B1 - B) <> 2
                THEN GO TO LINE1802.
001806     MOVE A TO E.
           MOVE B TO H.
           MOVE A1 TO A.
           MOVE B1 TO B.
           ADD 8 TO I.
           GO TO LINE1750.
001810 LINE1810.
           IF B = 8 THEN MOVE O-KING TO S(A,B).
001830     GO TO LINE0230.
001880 LINE1880.
           DISPLAY MSG-YOU-WIN
           STOP RUN.
001885 LINE1885.
           DISPLAY MSG-I-WIN
           STOP RUN.
