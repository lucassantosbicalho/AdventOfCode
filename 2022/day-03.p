
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
DEFINE VARIABLE j              AS INTEGER   NO-UNDO.
DEFINE VARIABLE g              AS INTEGER   NO-UNDO.
DEFINE VARIABLE ch             AS CHARACTER NO-UNDO.
DEFINE VARIABLE pos            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPriorities    AS CHARACTER NO-UNDO CASE-SENSITIVE.
DEFINE VARIABLE iSumPriorities AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttContents NO-UNDO
    FIELD i           AS INTEGER
    FIELD cItems      AS CHARACTER FORMAT "x(50)" CASE-SENSITIVE
    FIELD iLengh      AS INTEGER   
    FIELD iMiddle     AS INTEGER   
    FIELD cDuplicated AS CHARACTER CASE-SENSITIVE
    FIELD iPriority   AS INTEGER   
    INDEX iItem IS PRIMARY UNIQUE i cItems.
    
DEFINE TEMP-TABLE ttContentsGroup NO-UNDO 
    FIELD iGrp         AS INTEGER
    FIELD cContent     AS CHARACTER EXTENT 3 FORMAT "x(50)" CASE-SENSITIVE
    FIELD cDuplicated  AS CHARACTER 
    FIELD iPriority    AS INTEGER
    INDEX idGrp IS UNIQUE PRIMARY iGrp.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fcFindDuplicated RETURNS CHARACTER
    (INPUT str1 AS CHARACTER,
     INPUT str2 AS CHARACTER,
     INPUT str3 AS CHARACTER) FORWARD.
     
/* ***************************  Main Block  *************************** */


ETIME (YES).

INPUT FROM "D:/workspace/AdventOfCode/2022/input/03.txt".

// I defined cPriorities with the CASE-SENSITIVE property, 
// so the value of priority of each letter matches the 
// position of the letter in cPriorities used with a LOOKUP function.
cPriorities = "a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z".

i = 1. // record order (used in ttContents.i)
j = 1. // elf order (used in the index of ttContentsGroup.cContent[j]
g = 1. // group order (used in ttContentsGroup.iGrp)
DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    
    /* ------ [ PART 1 ] ------ */
    CREATE ttContents.
    ASSIGN 
        ttContents.i       = i
        ttContents.cItems  = cLine
        ttContents.iLengh  = LENGTH(cLine)
        ttContents.iMiddle = LENGTH(cLine) / 2.
    i = i + 1.
    
    // ------ [ PART 2 ] ------ */
    IF NOT CAN-FIND(FIRST ttContentsGroup
                    WHERE ttContentsGroup.iGrp = g 
                    NO-LOCK) THEN
    DO:
        CREATE ttContentsGroup.    
        ASSIGN ttContentsGroup.iGrp = g.
    END.
    ELSE FIND FIRST ttContentsGroup 
              WHERE ttContentsGroup.iGrp = g 
              EXCLUSIVE-LOCK
              NO-ERROR.
    ASSIGN 
        ttContentsGroup.cContent[j] = cLine.
    RELEASE ttContentsGroup.
        
    IF j MODULO 3 = 0 THEN ASSIGN j = 0 g = g + 1.
    j = j + 1.
    
END.

// [PART 1] ----------------------------------------------------------------------
// Find the item type that appears in both compartments of each rucksack. 
// What is the sum of the priorities of those item types?
iSumPriorities = 0.
foreachBlk1:
FOR EACH ttContents EXCLUSIVE-LOCK
    BY i:
    
    ASSIGN 
        ttContents.cDuplicated = fcFindDuplicated(SUBSTRING(ttContents.cItems, 1, ttContents.iMiddle), 
                                                  SUBSTRING(ttContents.cItems, ttContents.iMiddle + 1), 
                                                  "")
        ttContents.iPriority = LOOKUP(ttContents.cDuplicated, cPriorities)
        iSumPriorities = iSumPriorities + ttContents.iPriority.
END.

cSolution = SUBSTITUTE("[PART 1] The sum of the priorities of those item types is &1", iSumPriorities).


// [PART 2] ----------------------------------------------------------------------
// Find the item type that corresponds to the badges of each three-Elf group. 
// What is the sum of the priorities of those item types?
iSumPriorities = 0.
FOR EACH ttContentsGroup EXCLUSIVE-LOCK 
    BY ttContentsGroup.iGrp:
    
    ASSIGN 
        ttContentsGroup.cDuplicated = fcFindDuplicated(ttContentsGroup.cContent[1],
                                                       ttContentsGroup.cContent[2],
                                                       ttContentsGroup.cContent[3])
        ttContentsGroup.iPriority = LOOKUP(ttContentsGroup.cDuplicated, cPriorities)
        iSumPriorities = iSumPriorities + ttContentsGroup.iPriority.
END.

cSolution = cSolution + CHR(10) + SUBSTITUTE("[PART 2] The sum of the priorities of all groups is &1", iSumPriorities).

MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", ETIME) VIEW-AS ALERT-BOX.



/* ************************  Function Implementations ***************** */

FUNCTION fcFindDuplicated RETURNS CHARACTER
    (INPUT str1 AS CHARACTER,
     INPUT str2 AS CHARACTER,
     INPUT str3 AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Return duplicated letters, considering string inputs as case sensitive.
 Notes: @INPUT  str1, str2 [, str3]
        @OUTPUT duplicated letter present in those three inputs
------------------------------------------------------------------------------*/    
        
        DEFINE VARIABLE c1   AS CHARACTER NO-UNDO CASE-SENSITIVE.
        DEFINE VARIABLE c2   AS CHARACTER NO-UNDO CASE-SENSITIVE.
        DEFINE VARIABLE c3   AS CHARACTER NO-UNDO CASE-SENSITIVE.
        DEFINE VARIABLE i    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE ch   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE pos  AS INTEGER   NO-UNDO.
        
        DEFINE VARIABLE cDupLetters   AS CHARACTER EXTENT 3 NO-UNDO CASE-SENSITIVE.
        
        // At least str1 and str2 need to be informed
        IF ((LENGTH(str1) = 0 AND LENGTH(str2) = 0 AND LENGTH(str3) = 0) OR 
            (LENGTH(str1) = 0 AND LENGTH(str2) > 0 AND LENGTH(str3) = 0) OR   
            (LENGTH(str1) > 0 AND LENGTH(str2) = 0 AND LENGTH(str3) = 0)) THEN RETURN ''.  
        
        ASSIGN
            c1  = str1
            c2  = str2
            c3  = str3
            ch  = ''.
        
        /* Find duplicated between c1 and c2. 
         *   IF c3 is empty, then return duplicated found between c1 and c2.
         *   If c3 isn't empty, then check if duplicated between c1 and c2 exists in c3.
         *      If present, return. Else, next. 
         */
        
        blk:
        DO i = 1 TO LENGTH(c1):
            ch = SUBSTRING(c1, i, 1).
            // check if exists same letter in c2
            IF INDEX(c2, ch) > 0 THEN DO: 
                // If found, check if exists same letter in c3 (if c3 isn't empty)
                IF LENGTH(c3) = 0 THEN LEAVE blk.
                IF INDEX(c3, ch) > 0 THEN LEAVE blk.
                ELSE NEXT blk.
            END.
        END.
        
        RETURN ch.
        
END FUNCTION.