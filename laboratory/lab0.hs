countdown n = 
    if n < 0 then
        error "Negative number"
    else
        if n == 0 then
            [0]
        else 
            n:(countdown (n-1))


collatz n =
    if mod n 2 == 0 then
        n:(collatz (div n 2))
    else 
        n:(collatz (3*n+1))

