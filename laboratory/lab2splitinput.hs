-- SPLITING the input by max. 30 characters
-- This solution is divided by 2 phases:
-- 1. splitter -> splits String by existing new lines
-- 2. cutter -> cuts splited input by 30 chars.

import Data.Char

split input = split' input "" []

split' ("") accC accL = 
    reverse( (reverse accC) : accL)

split' (c:res) accC accL = 
    if (c /= '\n') then 
        split' res (c:accC) accL
    else 
        split' res "" ((reverse accC):accL)

cutter len inp = cutter' len inp []

cutter' len inp res = 
    let pref = take len inp in 
    let suf = drop len inp  in
        if (suf == [])
            then reverse (pref : res)
        else
            cutter' len suf (pref:res)


divide len inp = 
    let lines = split inp in
        concat (map (cutter len) lines)

smain input = concat (map (++"\n") (divide 30 input))

main = interact smain

