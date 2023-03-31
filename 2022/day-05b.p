
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

DEFINE VARIABLE stack1         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE stack2         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stack3         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stack4         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stack5         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stack6         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stack7         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stack8         AS CHARACTER NO-UNDO.
DEFINE VARIABLE stack9         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLine          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSolution      AS CHARACTER NO-UNDO.
DEFINE VARIABLE i              AS INTEGER   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */
// https://www.progresstalk.com/threads/find-an-element-in-an-array.143531/

/* ***************************  Main Block  *************************** */


ETIME (YES).

INPUT FROM "D:/workspace/AdventOfCode/2022/input/05.txt".


/*
    To solve this problem, I'm considering each stack as char variable, 
    with the crates away from each other by a comma.
    This way, I can access the crate by the entry position in the char variable.

    [T]     [D]         [L]                     Stack Column-Index:   
    [R]     [S] [G]     [P]         [H]         Stack 1 = index 2 
    [G]     [H] [W]     [R] [L]     [P]         Stack 2 = index 6 
    [W]     [G] [F] [H] [S] [M]     [L]         Stack 3 = index 10
    [Q]     [V] [B] [J] [H] [N] [R] [N]         Stack 4 = index 14
    [M] [R] [R] [P] [M] [T] [H] [Q] [C]         Stack 5 = index 18
    [F] [F] [Z] [H] [S] [Z] [T] [D] [S]         Stack 6 = index 22
    [P] [H] [P] [Q] [P] [M] [P] [F] [D]         Stack 7 = index 26
                                                Stack 8 = index 30
     1   2   3   4   5   6   7   8   9          Stack 9 = index 34
     
 */
 
// Populating the stacks with initial configuration
DO i = 1 TO 8 ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    ASSIGN 
        stack1 = SUBSTRING(cLine, 2,  1) + "," + stack1 
        stack2 = SUBSTRING(cLine, 6,  1) + "," + stack2 
        stack3 = SUBSTRING(cLine, 10, 1) + "," + stack3 
        stack4 = SUBSTRING(cLine, 14, 1) + "," + stack4 
        stack5 = SUBSTRING(cLine, 18, 1) + "," + stack5 
        stack6 = SUBSTRING(cLine, 22, 1) + "," + stack6 
        stack7 = SUBSTRING(cLine, 26, 1) + "," + stack7 
        stack8 = SUBSTRING(cLine, 30, 1) + "," + stack8 
        stack9 = SUBSTRING(cLine, 34, 1) + "," + stack9 . 
END.

stack1 = TRIM(stack1, ",").
stack2 = TRIM(stack2, ",").
stack3 = TRIM(stack3, ",").
stack4 = TRIM(stack4, ",").
stack5 = TRIM(stack5, ",").
stack6 = TRIM(stack6, ",").
stack7 = TRIM(stack7, ",").
stack8 = TRIM(stack8, ",").
stack9 = TRIM(stack9, ",").

MESSAGE stack1
VIEW-AS ALERT-BOX.
// Operating the rearrangement
/*DO WHILE TRUE ON ENDKEY UNDO, LEAVE:     */
/*    IMPORT UNFORMATTED cLine.            */
/*                                         */
/*    IF NOT cLine BEGINS "move" THEN NEXT.*/
/*                                         */
/*    MESSAGE cLine                        */
/*    VIEW-AS ALERT-BOX.                   */
/*END.                                     */





/* **********************  Internal Procedures  *********************** */

PROCEDURE prRearrange:
/*------------------------------------------------------------------------------
 Purpose: Executes the rearrangement accordingly to the instruction
 Notes:   Example of instruction: move 1 from 4 to 2
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInstruction AS CHARACTER NO-UNDO.
    DEFINE VARIABLE        cStackFrom     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE        cStackTo       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE        iQty           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE        k              AS INTEGER   NO-UNDO.
    
    cStackFrom = SUBSTITUTE("stack&1", ENTRY(4, ipcInstruction, " ")).    
    cStackTo   = SUBSTITUTE("stack&1", ENTRY(6, ipcInstruction, " ")).    
    iQty       = INTEGER(ENTRY(2, ipcInstruction, " ")).
    
    
    DO k = 1 TO iQty:
                
    END. 

END PROCEDURE.