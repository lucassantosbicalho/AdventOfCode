
/*------------------------------------------------------------------------
    File        : day-01.p
    Purpose     : Day 01 - Advent of Code
    Author(s)   : Lucas Bicalho
    Created     : Mon Mar 06 13:13:40 BRT 2023
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE iElf      AS INTEGER   NO-UNDO INIT 1.
DEFINE VARIABLE cNumber   AS CHARACTER NO-UNDO.
DEFINE VARIABLE dAmount   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cSolution AS CHARACTER NO-UNDO.

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
FOR EACH ttElfCarriage NO-LOCK BY dAmount DESC:
    ASSIGN cSolution = SUBSTITUTE("Elf &1 has more than all others, an amount of &2 calories.", ttElfCarriage.iElf, ttElfCarriage.dAmount).
    LEAVE.
END.

MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", ETIME) VIEW-AS ALERT-BOX.
