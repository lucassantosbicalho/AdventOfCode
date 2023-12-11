
/*------------------------------------------------------------------------
    File        : day-01.p
    Purpose     : 

    Syntax      :

    Description : 

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
    
define temp-table tt2 no-undo
    field cString   as character format 'x(60)'
    field cNumber   as character 
    field iPosition as integer.
    
/* ************************  Function Prototypes ********************** */


function fcExtractFirstAndLast returns character 
    (input ipcValue as character) forward.

function fcGetAllNumbersFromString returns character 
    (input ipcValue       as character,
     input iplIsWordValid as logical) forward.

function fcGetSpelledNumbersFromString returns character 
    (input        ipcValue       as character,
     input        iIndex         as integer,
     input-output ipcResult      as character) forward.

/* ***************************  Main Block  *************************** */


cThisProg = replace(substring(this-procedure:name, r-index(this-procedure:name, "\") + 1), ".p", "").

/* ---- Read data ---- */
etime (yes).
run pi-read-data.
/* ---- Run first part ---- */
run pi-part1.
/* ---- Run second part ---- */
run pi-part2.
/* ---- Ends ---- */
endTime = etime.
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
    cSolution = substitute("&2&1[PART 1] The total sum is &3.&1[PART 2] The total sum is &4.", chr(10), cThisProg, iSumPart1, iSumPart2).
    message cSolution skip substitute ("Took &1 msecs.", endTime) view-as alert-box.
    
    output to README.md APPEND.
    put unformatted substitute ("~n~n**AOC 2023 DAY 01** | Solved in &1 milliseconds.", endTime).
    output close.

end procedure.

/* ************************  Function Implementations ***************** */


function fcExtractFirstAndLast returns character 
    (input ipcValue as character):
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
------------------------------------------------------------------------------*/    

        if length(ipcValue) = 1 then
            return substitute("&1&1", ipcValue).
        else if length(ipcValue) = 2 then
            return ipcValue.
        else
            return substitute("&1&2", 
                              substring(ipcValue, 1, 1),
                              substring(ipcValue, length (ipcValue), 1)).
end function.

function fcGetSpelledNumbersFromString returns character 
    (input        ipcValue         as character,
     input        iIndex           as integer,
     input-output ipcResult        as character):
/*------------------------------------------------------------------------------
 Purpose: Get all spelled numbers. 
 Notes:
------------------------------------------------------------------------------*/    

        define variable cWords   as character no-undo
                                 initial "one,two,three,four,five,six,seven,eight,nine".
        define variable j        as integer   no-undo.
        
            // Prevent go to blk block if the first two letters are not on the list
            if lookup(substring(ipcValue, iIndex, 2), "on,tw,th,fo,fi,si,se,ei,ni") = 0 then return "".

            blk:
            do j = 1 to num-entries(cWords):
                
                if substring(ipcValue, iIndex) begins entry(j, cWords) then do:
                    assign 
                        ipcResult = ipcResult + string(j).
                    leave blk.     
                end.
                 
            end.
        
        return ipcResult.
        
end function.

function fcGetAllNumbersFromString returns character 
    (input ipcValue       as character,
     input iplIsWordValid as logical):
/*------------------------------------------------------------------------------
 Purpose: Get all numbers, spelled or in digit format. 
 Notes:
------------------------------------------------------------------------------*/    

        define variable i        as integer   no-undo.
        define variable c        as character no-undo.
        define variable iASCII   as integer   no-undo.
        define variable cResult  as character no-undo.
        
        do i = 1 to length(ipcValue):
            assign 
                c = substring(ipcValue, i, 1).
                iASCII = asc(c).
            
            if iASCII > 47 and iASCII < 58 then do:
                assign
                    cResult = cResult + c.
                next.
            end.
            
            if not iplIsWordValid then next.
            
            fcGetSpelledNumbersFromString(input ipcValue, input i, input-output cResult).
        end.                         
        
        return cResult.
        
end function.

