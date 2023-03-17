
/*------------------------------------------------------------------------
    File        : day-01.p
    Purpose     : Day 01 - Advent of Code
    Author(s)   : Lucas Bicalho
    Created     : Mon Mar 06 13:13:40 BRT 2023
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE iElf           AS INTEGER           NO-UNDO INIT 1.
DEFINE VARIABLE cNumber        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE dAmount        AS DECIMAL           NO-UNDO.
DEFINE VARIABLE cSolution      AS CHARACTER         NO-UNDO.
DEFINE VARIABLE dAmountOfThree AS DECIMAL EXTENT 3  NO-UNDO.
DEFINE VARIABLE k              AS INTEGER           NO-UNDO.

DEFINE TEMP-TABLE ttElfCarriage NO-UNDO
    FIELD iElf    AS INTEGER 
    FIELD dAmount AS DECIMAL
    INDEX dA dAmount.
    
/* ***************************  Main Block  *************************** */

ETIME (YES).

INPUT FROM "D:\workspace\AdventOfCode\2022\input\01.txt".

DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cNumber.
    
    dAmount = dAmount + DECIMAL (cNumber).
       
    IF cNumber = "" THEN DO:
        CREATE ttElfCarriage.
        ASSIGN ttElfCarriage.iElf    = iElf
               ttElfCarriage.dAmount = dAmount
               iElf                  = iElf + 1
               dAmount               = 0.
    END.
END.

// Get the elf with most calories
k = 0.
FOR EACH ttElfCarriage NO-LOCK BY dAmount DESC:
    CASE k:
        WHEN 0 THEN dAmountOfThree[1] = ttElfCarriage.dAmount.
        WHEN 1 THEN dAmountOfThree[2] = ttElfCarriage.dAmount.
        WHEN 2 THEN dAmountOfThree[3] = ttElfCarriage.dAmount.
    END.    
    k = k + 1.
    IF k = 3 THEN LEAVE .
END.

ASSIGN cSolution = SUBSTITUTE("[PART 1] Elf &1 has more than all others, an amount of &2 calories.&3[PART 2] The total of calories of the three top Elves is &4", 
                                ttElfCarriage.iElf, 
                                ttElfCarriage.dAmount,
                                CHR(10),
                                (dAmountOfThree[1] + dAmountOfThree[2] + dAmountOfThree[3])).

MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", ETIME) VIEW-AS ALERT-BOX.
