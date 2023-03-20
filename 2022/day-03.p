
/*------------------------------------------------------------------------
    File        : day-03.p
    Purpose     : 

    Syntax      :

    Description : Day 3: Rucksack Reorganization

    Author(s)   : Lucas Bicalho
    Created     : Sat Mar 18 23:46:52 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cLine          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSolution      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItem          AS CHARACTER NO-UNDO CASE-SENSITIVE.
DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
DEFINE VARIABLE g              AS INTEGER   NO-UNDO.
DEFINE VARIABLE ch             AS CHARACTER NO-UNDO.
DEFINE VARIABLE pos            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPriors        AS CHARACTER NO-UNDO CASE-SENSITIVE.
DEFINE VARIABLE iSumPriors     AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttContents NO-UNDO
    FIELD i           AS INTEGER
    FIELD grp         AS INTEGER
    FIELD cItems      AS CHARACTER FORMAT "x(50)" CASE-SENSITIVE
    FIELD iLengh      AS INTEGER   
    FIELD iMiddle     AS INTEGER   
    FIELD cDuplicated AS CHARACTER CASE-SENSITIVE
    FIELD iPrior      AS INTEGER   
    INDEX iItem IS PRIMARY UNIQUE i cItems.
    
DEFINE TEMP-TABLE ttContentsGrp LIKE ttContents.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ETIME (YES).

INPUT FROM "D:/workspace/AdventOfCode/2022/input/03.txt".
cPriors = "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z".

i = 1. // record order
g = 1. // group index
DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    CREATE ttContents.
    ASSIGN 
        ttContents.i       = i
        ttContents.grp     = g
        ttContents.cItems  = cLine
        ttContents.iLengh  = LENGTH(cLine)
        ttContents.iMiddle = LENGTH(cLine) / 2.
    IF i MODULO 3 = 0 THEN g = g + 1.
    i = i + 1.
END.


FOR EACH ttContents NO-LOCK BY ttContents.i:
    DISPLAY ttContents.i ttContents.grp.
END.
// [PART 1] ----------------------------------------------------------------------
// Find the item type that appears in both compartments of each rucksack. 
// What is the sum of the priorities of those item types?
iSumPriors = 0.
foreachBlk1:
FOR EACH ttContents EXCLUSIVE-LOCK
    BY i:
    // For each item in the first compartment (1 to iMiddle)
    DO i = 1 TO ttContents.iMiddle:
        ch = SUBSTRING(ttContents.cItems, i, 1).
        // Check if exist the same item in the second compartment
        pos = INDEX (ttContents.cItems, ch, ttContents.iMiddle + 1).
        // If exists assign the priority and iterate the sum of the priorities
        IF (pos > 0) THEN DO: 
            ASSIGN 
                ttContents.cDuplicated = ch
                ttContents.iPrior = LOOKUP(ch, cPriors)
                iSumPriors = iSumPriors + ttContents.iPrior.
            NEXT foreachBlk1.
        END.
    END.
END.

cSolution = SUBSTITUTE("[PART 1] The sum of the priorities of those item types is &1", iSumPriors).
                        
MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", ETIME) VIEW-AS ALERT-BOX.

// [PART 2] ----------------------------------------------------------------------

ETIME (YES).
// Find the item type that corresponds to the badges of each three-Elf group. 
// What is the sum of the priorities of those item types?

FOR EACH ttContents NO-LOCK:
    
END.

/*
iSumPriors = 0.
foreachBlk2:
FOR EACH ttContents EXCLUSIVE-LOCK
    BY i:
    // For each item in the first compartment (1 to iMiddle)
    DO i = 1 TO ttContents.iMiddle:
        ch = SUBSTRING(ttContents.cItems, i, 1).
        // Check if exist the same item in the second compartment
        pos = INDEX (ttContents.cItems, ch, ttContents.iMiddle + 1).
        // If exists assign the priority and iterate the sum of the priorities
        IF (pos > 0) THEN DO: 
            ASSIGN 
                ttContents.cDuplicated = ch
                ttContents.iPrior = LOOKUP(ch, cPriors)
                iSumPriors = iSumPriors + ttContents.iPrior.
            NEXT foreachBlk2.
        END.
    END.
END.
*/