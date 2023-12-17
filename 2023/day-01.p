
/*------------------------------------------------------------------------
    File        : day-01.p
    Purpose     : 

    Syntax      :

    Description : Day 1: Trebuchet?!

    Author(s)   : lucas.sbicalho
    Created     : Wed Dec 06 22:35:45 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

block-level on error undo, throw.

define variable cThisProg            as character no-undo. 
define variable cSolution            as character no-undo.
define variable endTime              as integer   no-undo.
define variable cLine                as character no-undo.
define variable cNumbers             as character no-undo.
define variable cFirstAndLast        as character no-undo.
define variable iSumPart1            as integer   no-undo.
define variable iSumPart2            as integer   no-undo.
define variable lNumberAsWordIsValid as logical no-undo.

define temp-table tt no-undo
    field cString   as character format 'x(60)'.
    
/* ***************************  Main Block  *************************** */
{2023/include/utils.i} /* functions */


/* ---- Read data ---- */
run pi-read-data.
/* ---- Run first part ---- */
run pi-part1.
/* ---- Run second part ---- */
run pi-part2.
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
    cThisProg = replace(substring(this-procedure:name, r-index(this-procedure:name, "\") + 1), ".p", "").
    etime (yes).

    input from 2023/input/01.txt.
    do while true on endkey undo, leave:
        import unformatted cLine.
        create tt.
        assign 
            tt.cString = cLine.
    end.

end procedure.

procedure pi-part1:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    lNumberAsWordIsValid = false.
    for each tt no-lock:
        assign
            cNumbers      = fcGetAllNumbersFromString(tt.cString, lNumberAsWordIsValid)
            cFirstAndLast = fcExtractFirstAndLast(cNumbers)
            iSumPart1     = iSumPart1 + integer(cFirstAndLast).
    end.

end procedure.

procedure pi-part2:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    lNumberAsWordIsValid = true.
    for each tt no-lock:
        assign
            cNumbers      = fcGetAllNumbersFromString(tt.cString, lNumberAsWordIsValid)
            cFirstAndLast = fcExtractFirstAndLast(cNumbers)
            iSumPart2     = iSumPart2 + integer(cFirstAndLast).
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
    put unformatted substitute ("~n~n**AOC 2023 DAY 01** | Solved in &1 milliseconds.", endTime).
    output close.

end procedure.


