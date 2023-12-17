
/*------------------------------------------------------------------------
    File        : day-03.p
    Purpose     : 

    Syntax      : 

    Description : Day 3: Gear Ratios

    Author(s)   : lucas.sbicalho
    Created     : Wed Dec 12 22:47:10 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

block-level on error undo, throw.

define variable cThisProg            as character no-undo. 
define variable cSolution            as character no-undo.
define variable endTime              as integer   no-undo.
define variable cLine                as character no-undo.

define variable iSumPart1            as int64     no-undo.
define variable iSumPart2            as int64     no-undo.

define temp-table tt no-undo
    field iLine as integer
    field cString as character format 'x(140)'.
    
define temp-table ttS no-undo
    field iLine           as integer 
    field isSymbol        as logical
    field cSymbol         as character 
    field SymbX           as integer
    field SymbY           as integer
    index idx iLine
    index ixy SymbX SymbY.

define temp-table ttN no-undo
    field iLine           as integer 
    field isNumber        as logical
    field iNumber         as integer
    field NumbXi          as integer
    field NumbXf          as integer
    field NumbYi          as integer           
    field NumbYf          as integer
    index idx iLine
    index ixy NumbXi NumbXf NumbYi NumbYf.
               
/* ************************  Function Prototypes ********************** */

function isNumber returns logical 
    (input ipcValue as character) forward.

function isSymbol returns logical 
    (input ipcValue as character) forward.


/* ***************************  Main Block  *************************** */


/* ---- Read data ---- */
run pi-read-data.
/* ---- Run first and second part ---- */
run pi-part1n2.
/* ---- Ends ---- */
run pi-end.


catch e as Progress.Lang.Error:
    define variable i as integer no-undo.
    do i = 1 to e:NumMessages:
        message substitute("&1 - &2", e:GetMessageNum(i), e:GetMessage(i))
        view-as alert-box.
    end.
end catch.


/* **********************  Internal Procedures  *********************** */

procedure pi-read-data:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable iLine         as integer   no-undo init 1.
    define variable i             as integer   no-undo.
    define variable j             as integer   no-undo.
    define variable iFinPosNumber as integer   no-undo.
    define variable cStr          as character no-undo.
    define variable cOutNumber    as character no-undo.

    etime (yes).
    cThisProg = replace(substring(this-procedure:name, r-index(this-procedure:name, "\") + 1), ".p", "").
    
    input from 2023/input/03.txt.
    do while true on endkey undo, leave:
        import unformatted cLine.
        create tt.
        assign 
            tt.iLine   = iLine
            tt.cString = cLine
            iLine      = iLine + 1.
    end.
    
    for each tt:
        do i = 1 to length(tt.cString):
            cStr = substring(tt.cString, i, 1).
            if isSymbol(cStr) then do:   
                create ttS.
                assign 
                    ttS.iLine           = tt.iLine
                    ttS.isSymbol        = isSymbol(cStr)
                    ttS.cSymbol         = cStr
                    ttS.SymbX           = tt.iLine
                    ttS.SymbY           = i. 
            end.
        end.
    end.
    
    for each tt:
        iFinPosNumber = 0.
        do i = 1 to length(tt.cString):
            cStr = substring(tt.cString, i, 1).
            if isNumber(cStr) then do:
                if i < iFinPosNumber then next.
                create ttN.
                assign
                    ttN.iLine           = tt.iLine
                    ttN.isNumber        = isNumber(cStr)
                    ttN.NumbXi          = tt.iLine
                    ttN.NumbXf          = tt.iLine
                    ttN.NumbYi          = i.
                    
                j = i - 1.
                doBlk:
                do while true:
                    j = j + 1.
                    if j > length(tt.cString) then leave doBlk.
                    cStr = substring(tt.cString, j, 1).
                    if not isNumber(cStr) then leave doBlk.
                    assign
                        cOutNumber    = substitute("&1&2", cOutNumber, cStr)
                        iFinPosNumber = j.
                end.
                assign
                    ttN.iNumber         = integer(cOutNumber)
                    ttN.NumbYf          = iFinPosNumber
                    cOutNumber          = ""
                    i                   = j.
            end.
        end.
    end.
end procedure.

procedure pi-part1n2:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable isSecondNumber   as logical   no-undo.
    define variable c-SymbolLocation as character no-undo.
    define variable i-FirstNumber    as integer   no-undo.
    define variable i-SecondNumber   as integer   no-undo.
                                    
    /* ---- Part 1 ---- */
    for each ttN:
        for first ttS 
            where abs(ttS.iLine - ttN.iLine) <= 1 and
                  ttS.SymbY >= (ttN.NumbYi - 1)   and
                  ttS.SymbY <= (ttN.NumbYf + 1):
                assign 
                    iSumPart1 = iSumPart1 + ttN.iNumber.
        end. 
    end.
    
    /* ---- Part 2 ---- */
    for each ttS
        where ttS.cSymbol = "*":
        assign
            i-FirstNumber    = -1
            i-SecondNumber   = -1
            c-SymbolLocation = "".
        blk:
        for each ttN
            where abs(ttN.iLine - ttS.iLine) <= 1 and
                  ttS.SymbY >= (ttN.NumbYi - 1)   and
                  ttS.SymbY <= (ttN.NumbYf + 1):
            if i-FirstNumber = -1 then 
            assign
                i-FirstNumber    = ttN.iNumber
                c-SymbolLocation = substitute("&1,&2", ttS.SymbX, ttS.SymbY).
            else if substitute("&1,&2", ttS.SymbX, ttS.SymbY) = c-SymbolLocation then do:
                assign
                    i-SecondNumber   = ttN.iNumber.
                leave blk.
            end.
        end.
        if i-FirstNumber > 0 and i-SecondNumber > 0 then 
        assign
            iSumPart2 = iSumPart2 + (i-FirstNumber * i-SecondNumber). 
    end.

end procedure.

procedure pi-end:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    endTime = etime.
    cSolution = substitute("&2&1[PART 1] The total sum is &3.&1[PART 2] The total sum is &4.", chr(10), cThisProg, iSumPart1, iSumPart2).
    message cSolution skip substitute ("Took &1 msecs.", endTime) view-as alert-box.
    
    output to README.md append.
    put unformatted substitute ("~n~n**AOC 2023 DAY &2** | Solved in &1 milliseconds.", endTime, entry(2, cThisProg, "-")).
    output close.

end procedure.


/* ************************  Function Implementations ***************** */

function isNumber returns logical 
    (input ipcValue as character):
/*------------------------------------------------------------------------------
 Purpose: Returns if input value is a number between 0 and 9
 Notes: ASCII for 0 is 48, and ACSII for 9 is 57
------------------------------------------------------------------------------*/    

        return integer(asc(ipcValue)) > 47  and
               integer(asc(ipcValue)) < 58.
        
end function.

function isSymbol returns logical 
    (input ipcValue as character):
/*------------------------------------------------------------------------------
 Purpose:
 Notes: ASCII for . is 46
------------------------------------------------------------------------------*/    
        
        return asc(ipcValue) <> 46 and (not isNumber(ipcValue)). // lookup(string(asc(ipcValue)), "46,48,49,50,51,52,53,54,55,56,57") = 0.  
                     
end function.


