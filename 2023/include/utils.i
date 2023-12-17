
/*------------------------------------------------------------------------
    File        : utils.i
    Purpose     : Functions that may be utils among the days

    Syntax      :

    Description : 

    Author(s)   : lucas.sbicalho
    Created     : Sat Dec 16 23:51:54 BRT 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
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

function fcIsNumber returns logical 
    (input ipcValue as character):
/*------------------------------------------------------------------------------
 Purpose: Returns if input value is a number between 0 and 9
 Notes: ASCII for 0 is 48, and ACSII for 9 is 57
------------------------------------------------------------------------------*/    

        return integer(asc(ipcValue)) > 47  and
               integer(asc(ipcValue)) < 58.
        
end function.

function fcIsSymbol returns logical 
    (input ipcValue as character):
/*------------------------------------------------------------------------------
 Purpose:
 Notes: ASCII for . is 46
------------------------------------------------------------------------------*/    
        
        return asc(ipcValue) <> 46 and (not fcIsNumber(ipcValue)). // lookup(string(asc(ipcValue)), "46,48,49,50,51,52,53,54,55,56,57") = 0.  
                     
end function.

function fcCalculatePoints returns integer 
    (input ipcValue as integer):
/*------------------------------------------------------------------------------
 Purpose: 1 point for the first match, then double it for each match after the first
 Notes: ipcValue = number of matches to double it
 
        Ex: 1 match = 1 point. 2 matches = 2 points. 3 matches = 6 points.
------------------------------------------------------------------------------*/    
        define variable iResult as integer no-undo.
        
        if ipcValue = 1 then iResult = 1.
        if ipcValue > 1 then iResult = 2 * fcCalculatePoints(ipcValue - 1).
        
        return iResult.
end function.