
/*------------------------------------------------------------------------
    File        : day-02.p
    Purpose     : 

    Syntax      :

    Description : Day 2: Rock Paper Scissors

    Author(s)   : Lucas Bicalho
    Created     : Thu Mar 16 21:21:37 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cLine          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSolution      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMyChoice      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTheirChoice   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMyInstruction AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTotalScorePt1 AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE iTotalScorePt2 AS INTEGER   NO-UNDO INIT 0.
DEFINE VARIABLE endTime        AS INTEGER   NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION getScorePt1 RETURNS INTEGER 
    (INPUT ipcPlayer AS CHARACTER, 
     INPUT ipcMe     AS CHARACTER) FORWARD.
     
FUNCTION getScorePt2 RETURNS INTEGER 
    (INPUT ipcPlayer AS CHARACTER, 
     INPUT ipcMe     AS CHARACTER) FORWARD.

FUNCTION getStandardNamePt1 RETURNS CHARACTER 
    (INPUT ipcLetter AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */
ETIME (YES).

INPUT FROM "D:/workspace/AdventOfCode/2022/input/02.txt".

DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    
    cTheirChoice   = getStandardNamePt1(ENTRY(1, cLine, " ")).
    cMyChoice      = getStandardNamePt1(ENTRY(2, cLine, " ")).
    cMyInstruction = ENTRY(2, cLine, " ").
    
    iTotalScorePt1 = iTotalScorePt1 + getScorePt1(cTheirChoice, cMyChoice).
    iTotalScorePt2 = iTotalScorePt2 + getScorePt2(cTheirChoice, cMyInstruction).
    
END.
endTime = ETIME.
cSolution = SUBSTITUTE("[PART 1] My total score is &1&2[PART 2] My total score is &3", iTotalScorePt1, CHR(10), iTotalScorePt2).
MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", endTime) VIEW-AS ALERT-BOX.

OUTPUT TO VALUE ("D:\workspace\AdventOfCode\README.md") APPEND.
/* Append some text to the end of the file */
PUT UNFORMATTED SUBSTITUTE ("~n~n**DAY 02**~n~nSolved in &1 milliseconds.", endTime).
OUTPUT CLOSE.

/* ************************  Function Implementations ***************** */

FUNCTION getScorePt1 RETURNS INTEGER 
    (INPUT ipcPlayer AS CHARACTER, 
     INPUT ipcMe     AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Calculate the score of the game
 Notes: Your total score is the sum of your scores for each round. 
        The score for a single round is the score for the shape you selected 
        (1 for Rock, 2 for Paper, and 3 for Scissors) 
        plus the score for the outcome of the round 
        (0 if you lost, 3 if the round was a draw, and 6 if you won).
        
        Rock beats scissors. Scissors beats paper. Paper beats Rock.
------------------------------------------------------------------------------*/    
    
        DEFINE VARIABLE iPointRock     AS INTEGER NO-UNDO INIT 1.
        DEFINE VARIABLE iPointPaper    AS INTEGER NO-UNDO INIT 2.
        DEFINE VARIABLE iPointScissors AS INTEGER NO-UNDO INIT 3.
        DEFINE VARIABLE iPointDraw     AS INTEGER NO-UNDO INIT 3.
        DEFINE VARIABLE iPointWin      AS INTEGER NO-UNDO INIT 6.
        DEFINE VARIABLE iFinalScore    AS INTEGER NO-UNDO INIT 0.
        
        CASE ipcMe:
            WHEN "Rock" THEN DO:
                iFinalScore = iPointRock.
                CASE ipcPlayer:
                    WHEN "Rock" THEN DO:
                        iFinalScore = iFinalScore + iPointDraw.
                    END.
                    WHEN "Scissors" THEN DO:
                        iFinalScore = iFinalScore + iPointWin.
                    END.
                END CASE.
            END.
            WHEN "Paper" THEN DO:
                iFinalScore = iPointPaper.
                CASE ipcPlayer:
                    WHEN "Paper" THEN DO:
                        iFinalScore = iFinalScore + iPointDraw.
                    END.
                    WHEN "Rock" THEN DO:
                        iFinalScore = iFinalScore + iPointWin.
                    END.
                END CASE.
            END.
            WHEN "Scissors" THEN DO:
                iFinalScore = iPointScissors.
                CASE ipcPlayer:
                    WHEN "Scissors" THEN DO:
                        iFinalScore = iFinalScore + iPointDraw.
                    END.
                    WHEN "Paper" THEN DO:
                        iFinalScore = iFinalScore + iPointWin.
                    END.
                END CASE.
            END.
        END CASE.
        
        RETURN iFinalScore.
        
END FUNCTION.

FUNCTION getScorePt2 RETURNS INTEGER 
    (INPUT ipcPlayer AS CHARACTER, 
     INPUT ipcMe     AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Calculate the score of the game
 Notes: Your total score is the sum of your scores for each round. 
        The score for a single round is the score for the shape you selected 
        (1 for Rock, 2 for Paper, and 3 for Scissors) 
        plus the score for the outcome of the round 
        (0 if you lost, 3 if the round was a draw, and 6 if you won).
        
        Rock beats scissors. Scissors beats paper. Paper beats Rock.
        
        Anyway, the second column says how the round needs to end: 
        X means you need to lose, 
        Y means you need to end the round in a draw, and  
        Z means you need to win. 
        Good luck!
------------------------------------------------------------------------------*/    
    
        DEFINE VARIABLE iPointRock     AS INTEGER NO-UNDO INIT 1.
        DEFINE VARIABLE iPointPaper    AS INTEGER NO-UNDO INIT 2.
        DEFINE VARIABLE iPointScissors AS INTEGER NO-UNDO INIT 3.
        DEFINE VARIABLE iPointDraw     AS INTEGER NO-UNDO INIT 3.
        DEFINE VARIABLE iPointWin      AS INTEGER NO-UNDO INIT 6.
        DEFINE VARIABLE iFinalScore    AS INTEGER NO-UNDO INIT 0.
        
        CASE ipcPlayer:
            WHEN "Rock" THEN DO:
                iFinalScore = iPointScissors. // need to lose
                CASE ipcMe:
                    WHEN "Y" THEN DO: // draw
                        iFinalScore = iPointRock + iPointDraw.
                    END.
                    WHEN "Z" THEN DO: // win
                        iFinalScore = iPointPaper + iPointWin.
                    END.
                END CASE.
            END.
            WHEN "Paper" THEN DO:
                iFinalScore = iPointRock. // need to lose
                CASE ipcMe:
                    WHEN "Y" THEN DO: // draw
                        iFinalScore = iPointPaper + iPointDraw.
                    END.
                    WHEN "Z" THEN DO: // win
                        iFinalScore = iPointScissors + iPointWin.
                    END.
                END CASE.
            END.
            WHEN "Scissors" THEN DO:
                iFinalScore = iPointPaper. // need to lose
                CASE ipcMe:
                    WHEN "Y" THEN DO: // draw
                        iFinalScore = iPointScissors + iPointDraw.
                    END.
                    WHEN "Z" THEN DO: // win
                        iFinalScore = iPointRock + iPointWin.
                    END.
                END CASE.
            END.
        END CASE.
        
        RETURN iFinalScore.
        
END FUNCTION.

FUNCTION getStandardNamePt1 RETURNS CHARACTER 
    (INPUT ipcLetter AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: INPUT A, B, C, X, Y, Z
 Notes:   OUTPUT Rock, Paper, Scissors
------------------------------------------------------------------------------*/    

        DEFINE VARIABLE cStdNames AS CHARACTER NO-UNDO INIT "Rock,Paper,Scissors".
        DEFINE VARIABLE cABC      AS CHARACTER NO-UNDO INIT "A,B,C".
        DEFINE VARIABLE cXYZ      AS CHARACTER NO-UNDO INIT "X,Y,Z".
        DEFINE VARIABLE iIndex    AS INTEGER NO-UNDO   INIT 0.
        
        iIndex = MAX(LOOKUP(ipcLetter, cABC), LOOKUP(ipcLetter, cXYZ)).
        RETURN ENTRY(iIndex, cStdNames).
        
END FUNCTION.



