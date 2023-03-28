
/*------------------------------------------------------------------------
    File        : day-04.p
    Purpose     : 

    Syntax      :

    Description : Camp Cleanup

    Author(s)   : Lucas Bicalho
    Created     : Tue Mar 28 12:55:44 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cLine          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSolution      AS CHARACTER NO-UNDO.
DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
DEFINE VARIABLE endTime        AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttAssignments NO-UNDO 
    FIELD iLine    AS INTEGER 
    FIELD iLL1     AS INTEGER 
    FIELD iUL1     AS INTEGER 
    FIELD iLL2     AS INTEGER 
    FIELD iUL2     AS INTEGER
    FIELD iContain AS INTEGER 
    INDEX idx IS UNIQUE IS PRIMARY iLine iLL1 iUL1 iLL2 iUL2.
     
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fcIsContainedOrFullyContains RETURNS INTEGER 
    (INPUT ipiLL1 AS INTEGER,
     INPUT ipiUL1 AS INTEGER,
     INPUT ipiLL2 AS INTEGER,
     INPUT ipiUL2 AS INTEGER) FORWARD.


/* ***************************  Main Block  *************************** */

ETIME (YES).

INPUT FROM "D:/workspace/AdventOfCode/2022/input/04.txt".

// In how many assignment pairs does one range fully contain the other?
i = 1.
DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    CREATE ttAssignments.
    ASSIGN 
        ttAssignments.iLine    = i
        ttAssignments.iLL1     = INTEGER(ENTRY(1, ENTRY(1 ,cLine, ","), "-"))
        ttAssignments.iUL1     = INTEGER(ENTRY(2, ENTRY(1 ,cLine, ","), "-"))
        ttAssignments.iLL2     = INTEGER(ENTRY(1, ENTRY(2 ,cLine, ","), "-"))
        ttAssignments.iUL2     = INTEGER(ENTRY(2, ENTRY(2 ,cLine, ","), "-"))
        ttAssignments.iContain = fcIsContainedOrFullyContains(ttAssignments.iLL1,
                                                              ttAssignments.iUL1,
                                                              ttAssignments.iLL2,
                                                              ttAssignments.iUL2).
    i = i + 1.
END.

FOR EACH ttAssignments NO-LOCK: 
    ACCUMULATE ttAssignments.iContain (SUM).
END.

cSolution = SUBSTITUTE("[PART 1] The total of assignment pairs that one range fully contains the other is &1", (ACCUM SUM ttAssignments.iContain)).
endTime = ETIME.
MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", endTime) VIEW-AS ALERT-BOX.

/* ************************  Function Implementations ***************** */

FUNCTION fcIsContainedOrFullyContains RETURNS INTEGER 
    (INPUT ipiLL1 AS INTEGER,
     INPUT ipiUL1 AS INTEGER,
     INPUT ipiLL2 AS INTEGER,
     INPUT ipiUL2 AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Check if the first pair is fully contained in the second or vice-versa. 
 Notes: @input LL1, UL1, LL2, UL2
        @output LOGICAL
       
    Each pair has a lower limit (LL) and an upper limit (UL).
        For each line, organize the data in LL1 and UL1 and LL2 and UL2
        Example:
         lineA    2-4,6-8
         lineB    2-8,3-7
         lineC    6-6,4-6
          ...      ...
         
         lineA: LL1 = 2, UL1 = 4, LL2 = 6 and UL2 = 8
         lineB: LL1 = 2, UL1 = 8, LL2 = 3 and UL2 = 7
         lineC: LL1 = 6, UL1 = 6, LL2 = 4 and UL2 = 6
        
        The test is simple:         
            IF (LL1 <= LL2 AND UL1 >= UL2) OR (LL1 >= LL2 AND UL1 <= UL2) THEN TRUE ELSE FALSE. 
        
        Test Example:
            lineA 
                (LL1 <= LL2 AND UL1 >= UL2) => (2 <= 6 AND 4 >= 8) := FALSE  | RESULT:
                (LL1 >= LL2 AND UL1 <= UL2) => (2 >= 6 AND 4 <= 8) := FALSE  | FALSE        
            lineB 
                (LL1 <= LL2 AND UL1 >= UL2) => (2 <= 3 AND 8 >= 7) := TRUE   | RESULT:
                (LL1 >= LL2 AND UL1 <= UL2) => (2 >= 3 AND 8 <= 7) := FALSE  | TRUE
            lineC 
                (LL1 <= LL2 AND UL1 >= UL2) => (6 <= 4 AND 6 >= 6) := FALSE  | RESULT:
                (LL1 >= LL2 AND UL1 <= UL2) => (6 >= 4 AND 6 <= 6) := TRUE   | TRUE
------------------------------------------------------------------------------*/    

        RETURN INTEGER((ipiLL1 <= ipiLL2 AND ipiUL1 >= ipiUL2) OR (ipiLL1 >= ipiLL2 AND ipiUL1 <= ipiUL2)).
        
END FUNCTION.




