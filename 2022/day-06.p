
/*------------------------------------------------------------------------
    File        : day-06.p
    Purpose     : 

    Syntax      :

    Description : Tuning Trouble

    Author(s)   : Lucas Bicalho
    Created     : Mon Apr 03 13:25:12 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cSolution       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i               AS INTEGER     NO-UNDO.
DEFINE VARIABLE j               AS INTEGER     NO-UNDO.
DEFINE VARIABLE k               AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDataStream     AS LONGCHAR    NO-UNDO.
DEFINE VARIABLE cPiece          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE endTime         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtyCharacters1 AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtyCharacters2 AS INTEGER     NO-UNDO.
     
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fcIsUniqueLetters RETURNS LOGICAL (INPUT ipcText AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */

ETIME (YES).


COPY-LOB FILE "D:/workspace/AdventOfCode/2022/input/06.txt" TO cDatastream.
/*
/* --- PART 1 --- */
blkPart1:
DO i = 1 TO LENGTH(cDataStream):
    IF i - 4 < 1 THEN NEXT.
    cPiece = SUBSTRING(cDataStream, i - 4, 4).
    IF fcIsUniqueLetters(cPiece) THEN
    DO:
        iQtyCharacters1 = (i - 1). 
        LEAVE blkPart1.
    END.
END.
/* --- PART 2 --- */
blkPart2:
DO i = 1 TO LENGTH(cDataStream):
    IF i - 14 < 1 THEN NEXT.
    cPiece = SUBSTRING(cDataStream, i - 14, 14).
    IF fcIsUniqueLetters(cPiece) THEN
    DO:
        iQtyCharacters2 = (i - 1). 
        LEAVE blkPart2.
    END.
END.
*/
iQtyCharacters1 = 0.
iQtyCharacters2 = 0.
blk:
DO i = 1 TO LENGTH(cDataStream):
    
    j = i.
    k = i.

    IF iQtyCharacters1 > 0 AND iQtyCharacters2 > 0 THEN LEAVE blk.
         
    /* --- PART 1 --- */
    IF j > 4 AND iQtyCharacters1 = 0 THEN DO:
        cPiece = SUBSTRING(cDataStream, j - 4, 4).
        IF fcIsUniqueLetters(cPiece) THEN
        DO:
            iQtyCharacters1 = (j - 1). 
        END.
    END.
    /* --- PART 2 --- */
    IF k > 14 AND iQtyCharacters2 = 0 THEN DO:
        cPiece = SUBSTRING(cDataStream, k - 14, 14).
        IF fcIsUniqueLetters(cPiece) THEN
        DO:
            iQtyCharacters2 = (k - 1). 
        END.
    END.
END.

cSolution = SUBSTITUTE("[PART 1] Considering 4 unique characters, &1 characters were processed before the first start-of-packet marker is detected.&2[PART 2] Considering 14 unique characters, &3 characters were processed.", iQtyCharacters1, CHR(10), iQtyCharacters2).
endTime = ETIME.
MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", endTime) VIEW-AS ALERT-BOX.

/* ************************  Function Implementations ***************** */

FUNCTION fcIsUniqueLetters RETURNS LOGICAL 
    (INPUT ipcText AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Check if in a given INPUT text, all the digits is unique
 Notes:
------------------------------------------------------------------------------*/    

        DEFINE VARIABLE i           AS INTEGER              NO-UNDO.    
        DEFINE VARIABLE cTempText   AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE cLetter     AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE lUnique     AS LOGICAL INIT TRUE    NO-UNDO.    
        
        blkTest:
        DO i = 1 TO LENGTH(ipcText):
            cLetter = SUBSTRING(ipcText, i, 1).
            cTempText = cTempText + "," + cLetter.
            cTempText = TRIM(cTempText, ",").
            // If lookup result is less than the num-entries of cTempText, so this means a duplicated letter
            IF LOOKUP(cLetter, cTempText) < NUM-ENTRIES(cTempText) THEN
            DO:
                ASSIGN lUnique = FALSE.
                LEAVE blkTest.
            END.
        END.
        
        RETURN lUnique.

        
END FUNCTION.



