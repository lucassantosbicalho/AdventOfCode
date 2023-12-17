
/*------------------------------------------------------------------------
    File        : day-02.p
    Purpose     : 

    Syntax      :

    Description : Day 2: Cube Conundrum

    Author(s)   : lucas.sbicalho
    Created     : Wed Dec 11 00:22:13 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

block-level on error undo, throw.

define variable cThisProg            as character no-undo. 
define variable cSolution            as character no-undo.
define variable endTime              as integer   no-undo.
define variable cLine                as character no-undo.

define variable iSumPart1            as integer   no-undo.
define variable iSumPart2            as integer   no-undo.

define temp-table tt no-undo
    field cString     as character format 'x(100)'
    field iGame       as integer
    field iSet        as integer 
    field iRed        as integer
    field iRedLimit   as integer init 12
    field iGreen      as integer
    field iGreenLimit as integer init 13
    field iBlue       as integer
    field iBlueLimit  as integer init 14.
    

/* ***************************  Main Block  *************************** */
{2023/include/utils.i} /* functions */

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
    define variable k       as integer   no-undo.
    define variable m       as integer   no-undo.
    define variable cSet    as character no-undo.
    define variable iSets   as integer   no-undo.
    define variable cGame   as character no-undo.
    define variable cColors as character no-undo.
    define variable iColors as integer   no-undo.
    
    etime (yes).
    cThisProg = replace(substring(this-procedure:name, r-index(this-procedure:name, "\") + 1), ".p", "").
    
    input from 2023/input/02.txt.
    do while true on endkey undo, leave:
        import unformatted cLine.
        
        cGame = replace(entry(1, cLine, ":"), "Game ", "").
        iSets = num-entries(cLine, ";").
        do k = 1 to iSets:
            create tt.
            assign 
                tt.cString = trim(cLine)
                tt.iGame   = integer(cGame)   
                tt.iSet    = k
                cSet       = entry(k, entry(2, cLine, ":"), ";").
                iColors    = num-entries(cSet, ",").
                
            do m = 1 to iColors:
                cColors = trim((if iColors > 1 then entry(m, cSet, ",") else cSet)).
                if cColors matches "*red*" then 
                assign
                    tt.iRed   = integer(entry(1, cColors, " ")).
                else if cColors matches "*green*" then 
                assign
                    tt.iGreen = integer(entry(1, cColors, " ")).
                else  
                assign
                    tt.iBlue  = integer(entry(1, cColors, " ")).
            end.
        end.
    end.

end procedure.

procedure pi-part1n2:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable iMaxR as integer no-undo.
    define variable iMaxG as integer no-undo.
    define variable iMaxB as integer no-undo.

    for each tt no-lock
        break by tt.iGame:
        
        if first-of(tt.iGame) then 
        assign 
            iMaxR = 0 iMaxG = 0 iMaxB = 0.   
        
        assign
            iMaxR = max(iMaxR, tt.iRed)
            iMaxG = max(iMaxG, tt.iGreen)
            iMaxB = max(iMaxB, tt.iBlue).
                    
        if last-of(tt.iGame) then do:
            
            if iMaxR <= tt.iRedLimit   and 
               iMaxG <= tt.iGreenLimit and
               iMaxB <= tt.iBlueLimit 
            then
            assign 
                iSumPart1 = iSumPart1 + tt.iGame.
            iSumPart2 = iSumPart2 + (iMaxR * iMaxG * iMaxB).
        end.
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
    
    output to README.md APPEND.
    put unformatted substitute ("~n~n**AOC 2023 DAY &2** | Solved in &1 milliseconds.", endTime, entry(2, cThisProg, "-")).
    output close.

end procedure.

