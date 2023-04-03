000100      Identification Division.
000200
000300       Program-Id.    Program1.
000400       Author.        Author's Name.
000500       Installation.  IBM Toronto Lab
000600       Date-Written.  January 4, 1991.
000700       Date-Compiled. Will be replaced by compile date.
000900
001000
001100     *************************************************************
001200     * 01/04/91                  Sample COBOL file for S/390
001300     *
001400     * Description:
001500     *
001600     *
001700     *   Test COBOL tokenizer.
001800     *
001900     *************************************************************
002000
002100      Environment Division.
002200       Configuration Section.
002300        Source-Computer.   IBM-S/390.
002400        Object-Computer.   IBM-S/390.
002500        Special-Names.
002600
002700
002800
002900
003000
003100
003200      Data Division.
003300
003400       Working-Storage Section.
003500        01  itrap-vars.
003600          05  itrap100        Pic X(10) Value "ITRAP100".
003700          05  pgm-id          Pic X     Value "Program1".
003800
003900
004000
004100      Procedure Division.
004200
004300       Main-Program-Logic.
004400
004500          Perform Opening-Statements.
004600          Perform Variation-001 Through Variation-End.
004700          Perform Closing-Statements.
004800          Stop Run.
004900
