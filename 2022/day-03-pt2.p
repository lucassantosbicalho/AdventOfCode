// Core idea. To be finished yet


BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
DEFINE VARIABLE g             AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine         AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttContentsGroup NO-UNDO 
    FIELD iGrp     AS INTEGER
    FIELD cContent AS CHARACTER EXTENT 3
    FIELD cLetter  AS CHARACTER 
    FIELD iPrior   AS INTEGER
    INDEX idGrp IS UNIQUE PRIMARY iGrp.

INPUT FROM "D:\\workspace\\AdventOfCode\\2022\\input\\03.txt".
i = 1.
g = 1.
DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    IF NOT CAN-FIND(FIRST ttContentsGroup
        WHERE ttContentsGroup.iGrp = g NO-LOCK) THEN
    DO:
        CREATE ttContentsGroup.    
        ASSIGN ttContentsGroup.iGrp = g.
    END.
    ELSE FIND FIRST ttContentsGroup
        WHERE ttContentsGroup.iGrp = g EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN 
        ttContentsGroup.cContent[i] = cLine.
    RELEASE ttContentsGroup.
        
    IF i MODULO 3 = 0 THEN ASSIGN i = 0 g = g + 1.
    i = i + 1.
    
END.


FOR EACH ttContentsGroup NO-LOCK:
    DISP ttContentsGroup EXCEPT cLetter iPrior.
END.
