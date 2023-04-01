
/*------------------------------------------------------------------------
    File        : day-05.p
    Purpose     : 

    Syntax      :

    Description : Supply Stacks

    Author(s)   : Lucas Bicalho
    Created     : Fri Mar 31 13:12:33 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE stack          AS CHARACTER EXTENT 9 NO-UNDO. 
DEFINE VARIABLE cLine          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSolution      AS CHARACTER NO-UNDO.
DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
DEFINE VARIABLE endTime        AS INTEGER   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fcLengthSubstring RETURNS INTEGER 
    (INPUT ipiIndex AS INTEGER) FORWARD.

FUNCTION getLastCrateOfStack RETURNS CHARACTER 
    (INPUT cStacks AS CHARACTER EXTENT 9) FORWARD.

// https://www.progresstalk.com/threads/find-an-element-in-an-array.143531/

/* ***************************  Main Block  *************************** */


ETIME (YES).

INPUT FROM "D:/workspace/AdventOfCode/2022/input/05.txt".


/*
    To solve this problem, I'm considering each stack as a position in an extent char variable, 
    with the crates apart from each other by a comma.
    This way, I can access the crate by the entry position in the given extent index of char variable.

    [T]     [D]         [L]                     Stack Column-Index (To manipulate on reading file):   
    [R]     [S] [G]     [P]         [H]         Stack[1] = index 2 
    [G]     [H] [W]     [R] [L]     [P]         Stack[2] = index 6 
    [W]     [G] [F] [H] [S] [M]     [L]         Stack[3] = index 10
    [Q]     [V] [B] [J] [H] [N] [R] [N]         Stack[4] = index 14
    [M] [R] [R] [P] [M] [T] [H] [Q] [C]         Stack[5] = index 18
    [F] [F] [Z] [H] [S] [Z] [T] [D] [S]         Stack[6] = index 22
    [P] [H] [P] [Q] [P] [M] [P] [F] [D]         Stack[7] = index 26
                                                Stack[8] = index 30
     1   2   3   4   5   6   7   8   9          Stack[9] = index 34
     
 */
 
// Populating the stacks with initial configuration of stacks and crates
DO i = 1 TO 8 ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    ASSIGN 
        stack[1] = SUBSTRING(cLine, 2,  1) + "," + stack[1] 
        stack[2] = SUBSTRING(cLine, 6,  1) + "," + stack[2] 
        stack[3] = SUBSTRING(cLine, 10, 1) + "," + stack[3] 
        stack[4] = SUBSTRING(cLine, 14, 1) + "," + stack[4] 
        stack[5] = SUBSTRING(cLine, 18, 1) + "," + stack[5] 
        stack[6] = SUBSTRING(cLine, 22, 1) + "," + stack[6] 
        stack[7] = SUBSTRING(cLine, 26, 1) + "," + stack[7] 
        stack[8] = SUBSTRING(cLine, 30, 1) + "," + stack[8] 
        stack[9] = SUBSTRING(cLine, 34, 1) + "," + stack[9] . 
END.

// Cleaning extra commas
stack[1] = TRIM(REPLACE(stack[1], ", ", ""), ",").
stack[2] = TRIM(REPLACE(stack[2], ", ", ""), ",").
stack[3] = TRIM(REPLACE(stack[3], ", ", ""), ",").
stack[4] = TRIM(REPLACE(stack[4], ", ", ""), ",").
stack[5] = TRIM(REPLACE(stack[5], ", ", ""), ",").
stack[6] = TRIM(REPLACE(stack[6], ", ", ""), ",").
stack[7] = TRIM(REPLACE(stack[7], ", ", ""), ",").
stack[8] = TRIM(REPLACE(stack[8], ", ", ""), ",").
stack[9] = TRIM(REPLACE(stack[9], ", ", ""), ",").

// Operating the rearrangement
DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    IF NOT cLine BEGINS "move" THEN NEXT.
    RUN prRearrange(cLine).
END.

// Cleaning extra commas
stack[1] = TRIM(REPLACE(stack[1], ", ", ""), ",").
stack[2] = TRIM(REPLACE(stack[2], ", ", ""), ",").
stack[3] = TRIM(REPLACE(stack[3], ", ", ""), ",").
stack[4] = TRIM(REPLACE(stack[4], ", ", ""), ",").
stack[5] = TRIM(REPLACE(stack[5], ", ", ""), ",").
stack[6] = TRIM(REPLACE(stack[6], ", ", ""), ",").
stack[7] = TRIM(REPLACE(stack[7], ", ", ""), ",").
stack[8] = TRIM(REPLACE(stack[8], ", ", ""), ",").
stack[9] = TRIM(REPLACE(stack[9], ", ", ""), ",").

cSolution = SUBSTITUTE("[PART 1] The crate that ends up on top of each stack is &1.", getLastCrateOfStack(stack)).
endTime = ETIME.
MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", endTime) VIEW-AS ALERT-BOX.

/* **********************  Internal Procedures  *********************** */

PROCEDURE prRearrange:
/*------------------------------------------------------------------------------
 Purpose: Executes the rearrangement accordingly to the instruction
 Notes:   Example of instruction: move 1 from 4 to 2
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInstruction AS CHARACTER NO-UNDO.
    DEFINE VARIABLE        k              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE        iQty           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE        iFrom          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE        iTo            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE        iIndex         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE        cValue         AS CHARACTER NO-UNDO.
    
    iQty  = INTEGER(ENTRY(2, ipcInstruction, " ")).
    iFrom = INTEGER(ENTRY(4, ipcInstruction, " ")).
    iTo   = INTEGER(ENTRY(6, ipcInstruction, " ")).
    
    DO k = 1 TO iQty:
        ASSIGN
            iIndex        = NUM-ENTRIES(stack[iFrom]).
            cValue        = ENTRY(iIndex, stack[iFrom]).
            stack[iTo]    = stack[iTo] + "," + cValue.
            stack[iFrom]  = SUBSTRING(stack[iFrom], 1, fcLengthSubstring(iIndex - 1)).
            stack[iFrom]  = TRIM(stack[iFrom], ",").
    END. 

END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fcLengthSubstring RETURNS INTEGER 
    (INPUT ipiIndex AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Return the length to use in substring to remove a crate from a stack.
 Notes:   It is not hard to see a function between the input index, and the output length:
           
            x (index)  y (lenght) | f(x) 
            1          2          | x = 1 => f(x) = 2. Otherwise, follows the rule below 
            2          3          | x = 2 => f(x) = 2 + (2 - 1) = 2 + 1 = 3
            3          5          | x = 3 => f(x) = 3 + (3 - 1) = 3 + 2 = 5
            4          7          | x = 4 => f(x) = 4 + (4 - 1) = 4 + 3 = 7
            5          9          | x = 5 => f(x) = 5 + (5 - 1) = 5 + 4 = 9
            
            To sum up, f(x) = 2,           x = 1 
                       f(x) = x + (x - 1), x > 1
                       f(x) = 0,           x < 1
------------------------------------------------------------------------------*/    
        DEFINE VARIABLE iResult AS INTEGER NO-UNDO.
        
        IF ipiIndex < 1 THEN iResult = 0.
        ELSE IF ipiIndex = 1 THEN iResult = 2.
        ELSE iResult = ipiIndex + (ipiIndex - 1).
        
        RETURN iResult.
        
END FUNCTION.

FUNCTION getLastCrateOfStack RETURNS CHARACTER 
    (INPUT ipcStacks AS CHARACTER EXTENT 9):
/*------------------------------------------------------------------------------
 Purpose: Print the last crate in each stack position
 Notes:
------------------------------------------------------------------------------*/    

        DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cValue  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE k       AS INTEGER   NO-UNDO.
        
        DO k = 1 TO 9:
            iIndex = NUM-ENTRIES(ipcStacks[k]).
            cValue = ENTRY(iIndex, ipcStacks[k]).   
            cResult = cResult + cValue.     
        END.

        RETURN cResult.
        
END FUNCTION.