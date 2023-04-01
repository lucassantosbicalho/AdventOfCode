
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
    FIELD iOverlap AS INTEGER 
    INDEX idx IS UNIQUE IS PRIMARY iLine iLL1 iUL1 iLL2 iUL2.
     
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fcIsContainedOrFullyContains RETURNS INTEGER 
    (INPUT ipiLL1 AS INTEGER,
     INPUT ipiUL1 AS INTEGER,
     INPUT ipiLL2 AS INTEGER,
     INPUT ipiUL2 AS INTEGER) FORWARD.

FUNCTION fcItHasOverlaps RETURNS INTEGER 
    (INPUT ipiLL1 AS INTEGER,
     INPUT ipiUL1 AS INTEGER,
     INPUT ipiLL2 AS INTEGER,
     INPUT ipiUL2 AS INTEGER) FORWARD.

/* ***************************  Main Block  *************************** */

ETIME (YES).

INPUT FROM "D:/workspace/AdventOfCode/2022/input/04.txt".

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
        /* ------ [ PART 1 ] ------ */
        ttAssignments.iContain = fcIsContainedOrFullyContains(ttAssignments.iLL1,
                                                              ttAssignments.iUL1,
                                                              ttAssignments.iLL2,
                                                              ttAssignments.iUL2)
        /* ------ [ PART 2 ] ------ */ 
        ttAssignments.iOverlap = fcItHasOverlaps(ttAssignments.iLL1,
                                                 ttAssignments.iUL1,
                                                 ttAssignments.iLL2,
                                                 ttAssignments.iUL2).
    i = i + 1.
END.
FOR EACH ttAssignments NO-LOCK: 
    ACCUMULATE ttAssignments.iContain (SUM).
    ACCUMULATE ttAssignments.iOverlap (SUM).
END.

cSolution = SUBSTITUTE("[PART 1] The total of assignment pairs that one range fully contains the other is &1.&2[PART 2] The total of overlapped pairs is &3", (ACCUM SUM ttAssignments.iContain), CHR(10), (ACCUM SUM ttAssignments.iOverlap)).
endTime = ETIME.
MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", endTime) VIEW-AS ALERT-BOX.

OUTPUT TO VALUE ("D:\workspace\AdventOfCode\README.md") APPEND.
/* Append some text to the end of the file */
PUT UNFORMATTED SUBSTITUTE ("~n~n**DAY 04** | Solved in &1 milliseconds.", endTime).
OUTPUT CLOSE.
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

FUNCTION fcItHasOverlaps RETURNS INTEGER 
    (INPUT ipiLL1 AS INTEGER,
     INPUT ipiUL1 AS INTEGER,
     INPUT ipiLL2 AS INTEGER,
     INPUT ipiUL2 AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
     
        The solution I found is to consider each pair of values as horizontal stacks in the air, below of lamp or the sun.
        If the stacks overlaps, the shadow formed on the ground has no gaps of light.
        If the stacks doesn't overlaps, the shadow formed on the ground has gaps of light between them.
        I think see the problem this way is funny!
        
        Example:
         lineA    2-4,6-8
         lineB    2-8,3-7
         lineC    6-6,4-6
         lineD    1-3,5-6
          ...      ...
          
         lineA:
                  234
                       678
                  ---  ---    (shadow on the ground with gap of light between them)
         
         lineB:
                  2345678
                   34567
                  -------     (shadown without gaps)
                 
         lineC:
                      6  
                    456  
                    ---       (shadown without gaps)
         lineD:
                  123
                      56
                  --- --      (shadow on the ground with gap of light between them)     
                 
         Mathematically, to solve the problem I can proceed this way:
             i) See if the (LL2 - UL1 > 0 OR LL1 - UL2 > 0), if yes, this is a gap of light
------------------------------------------------------------------------------*/    
        RETURN 1 - INTEGER((ipiLL2 - ipiUL1 > 0) OR (ipiLL1 - ipiUL2 > 0)).
        
END FUNCTION.

