
/*------------------------------------------------------------------------
    File        : day-04.p
    Purpose     : 

    Syntax      : 

    Description : Day 4: Scratchcards

    Author(s)   : lucas.sbicalho
    Created     : Wed Dec 12 23:49:39 BRT 2023
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

define temp-table ttWinningNumbers no-undo
    field iCard        as integer
    field cNumbers     as character format "x(60)"
    field iWinNumbers  as integer
    field iPoints      as integer
    index idx iCard cNumbers.

define temp-table ttMyNumbers no-undo
    field iCard    as integer
    field cNumbers as character format "x(60)"
    index idx iCard cNumbers.
    

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
    define variable iLine         as integer   no-undo.
    define variable cStr1         as character no-undo.
    define variable cStr2         as character no-undo.

    define variable i             as integer   no-undo.
    define variable j             as integer   no-undo.
    define variable iFinPosNumber as integer   no-undo.
    define variable cOutNumber    as character no-undo.

    etime (yes).
    cThisProg = replace(substring(this-procedure:name, r-index(this-procedure:name, "\") + 1), ".p", "").
    
    input from 2023/input/04.txt.
    do while true on endkey undo, leave:
        import unformatted cLine.
        
        assign        
            iLine = iLine + 1
            cLine = trim(replace(cLine, "  ", " "))
            cStr1 = entry(1, cLine, "|")
            cStr1 = trim(entry(2, cStr1, ":"))
            cStr2 = trim(entry(2, cLine, "|")).
            
        create ttWinningNumbers.
        assign 
            ttWinningNumbers.iCard    = iLine
            ttWinningNumbers.cNumbers = cStr1.

        create ttMyNumbers.
        assign 
            ttMyNumbers.iCard    = iLine
            ttMyNumbers.cNumbers = cStr2.
    end.
    
end procedure.

procedure pi-part1n2:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable i         as integer   no-undo.
    define variable cNum      as character no-undo.
    define variable iQtyFound as integer   no-undo.
    
    /* ---- Part 1 ---- */
    for each ttMyNumbers no-lock:
        for first ttWinningNumbers exclusive-lock
            where ttWinningNumbers.iCard = ttMyNumbers.iCard: 
            blk:
            do i = 1 to num-entries(ttMyNumbers.cNumbers, " "):
                cNum = entry(i, ttMyNumbers.cNumbers, " ").
                if lookup(cNum, ttWinningNumbers.cNumbers, " ") > 0 then 
                assign 
                    iQtyFound = iQtyFound + 1.
            end.
            
            assign 
                ttWinningNumbers.iWinNumbers = iQtyFound
                ttWinningNumbers.iPoints     = fcCalculatePoints(iQtyFound)
                iQtyFound                    = 0
                iSumPart1                    = iSumPart1 + ttWinningNumbers.iPoints.
            
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
    
    output to README.md append.
    put unformatted substitute ("~n~n**AOC 2023 DAY &2** | Solved in &1 milliseconds.", endTime, entry(2, cThisProg, "-")).
    output close.

end procedure.


