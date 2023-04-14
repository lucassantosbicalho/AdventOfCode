
/*------------------------------------------------------------------------
    File        : day-07.p
    Purpose     : 

    Syntax      :

    Description : No Space Left On Device

    Author(s)   : Lucas Bicalho
    Created     : Mon Apr 03 13:25:12 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE cLine           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSolution       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i               AS INTEGER     NO-UNDO.
DEFINE VARIABLE endTime         AS INTEGER     NO-UNDO.
DEFINE VARIABLE tempDir         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tempFile        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE tempSize        AS INT64       NO-UNDO.

DEFINE TEMP-TABLE ttDir NO-UNDO
    FIELD cPath      AS CHARACTER 
    FIELD lIsFile    AS LOGICAL INIT NO 
    FIELD cFileName  AS CHARACTER 
    FIELD cSize      AS INT64 
    INDEX idx IS PRIMARY IS UNIQUE cPath cFileName.
     
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fcReturnCurrentDir RETURNS CHARACTER 
    (INPUT ipPath AS CHARACTER,
     INPUT ipCmdLine AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */

ETIME (YES).

INPUT FROM "D:/workspace/AdventOfCode/2022/input/07-test.txt".

tempDir = "".

EMPTY TEMP-TABLE ttDir.


DO WHILE TRUE ON ENDKEY UNDO, LEAVE:
    IMPORT UNFORMATTED cLine.
    
    // Cleaning variables
    ASSIGN 
        tempFile = ""
        tempSize = 0.
        
    // Get the current dir. If the commands is '$ ls' then do not update tempDir.
    IF cLine <> "$ ls" THEN tempDir = fcReturnCurrentDir(tempDir, cLine).
    IF tempDir = "" THEN NEXT.
    
    // Get the file name and file size
    IF (NOT (cLine BEGINS "$" OR cLine BEGINS "dir")) AND  
       (INTEGER(ENTRY(1, cLine, " ")) > 0)
    THEN DO:
        ASSIGN 
            tempFile = ENTRY(2, cLine, " ")
            tempSize = INTEGER(ENTRY(1, cLine, " ")).
    END. 
    
    // If file name is empty, then next.
    IF tempFile = "" THEN NEXT.
    
    // Store information in temp-table    
    IF NOT CAN-FIND(FIRST ttDir NO-LOCK 
                    WHERE ttDir.cPath     = tempDir 
                      AND ttDir.cFileName = tempFile)
    THEN DO:
        
        CREATE ttDir.
        ASSIGN ttDir.cPath     = tempDir
               ttDir.cFileName = tempFile
               ttDir.lIsFile   = TRUE 
               ttDir.cSize     = tempSize.
        
        
    END.
/*    MESSAGE "cLine "cLine SKIP "tempDir "tempDir SKIP fcReturnCurrentDir(tempDir, cLine)*/
/*    VIEW-AS ALERT-BOX.                                                   */
       
END.

FOR EACH ttDir NO-LOCK:
    DISPLAY ttDir.
END.

cSolution = SUBSTITUTE("[PART 1] &1.&2[PART 2] &3 .", 0, CHR(10), 0).
endTime = ETIME.
MESSAGE cSolution SKIP SUBSTITUTE ("Took &1 msecs.", endTime) VIEW-AS ALERT-BOX.

/* ************************  Function Implementations ***************** */

FUNCTION fcReturnCurrentDir RETURNS CHARACTER 
    (INPUT ipPath AS CHARACTER,
     INPUT ipCmdLine AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Interpret Line, return the full path
 Notes:
        Give the current path (like the path the prompt are located).
        Then with the ipCmdLine, interpret if it is a command to enter in a 
        given directory or step back to the previous directory.
        
        Output the updated path 
        
        Instructions:        
        Within the terminal output, lines that begin with $ are commands you executed, very much like some modern computers:
            cd       means change directory. This changes which directory is the current directory, but the specific result depends on the argument:
            cd x     moves in one level: it looks in the current directory for the directory named x and makes it the current directory.
            cd ..    moves out one level: it finds the directory that contains the current directory, then makes that directory the current directory.
            cd /     switches the current directory to the outermost directory, /.
            ls       means list. It prints out all of the files and directories immediately contained by the current directory:
            123 abc  means that the current directory contains a file named abc with size 123.
            dir xyz  means that the current directory contains a directory named xyz.
            
            
        Consider ipCmdLine begining with $
------------------------------------------------------------------------------*/    
        DEFINE VARIABLE cCommand AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cReturn  AS CHARACTER NO-UNDO.
        
        IF NOT ipCmdLine BEGINS "$" THEN RETURN ipPath.
        
        IF ipCmdLine = "$ ls" THEN RETURN ipPath.
        
        cCommand = REPLACE (ipCmdLine, "$ ", "").
        
        // Moves to the root
        IF cCommand = "cd /" THEN cReturn = "/".
        // Moves out one level
        ELSE IF cCommand = "cd .." THEN DO:
            cReturn = SUBSTRING(ipPath, 1, R-INDEX(ipPath, "/" ) - 1). 
        END.
        // Moves in one level
        ELSE IF (cCommand <> "cd /" AND cCommand <> "cd /") THEN DO:
            cReturn = ipPath + "/" + REPLACE(cCommand, "cd ", ""). 
        END.
        IF cReturn BEGINS "//" THEN ASSIGN cReturn = REPLACE(cReturn, "//", "/").
        
        RETURN cReturn.
        
END FUNCTION.


