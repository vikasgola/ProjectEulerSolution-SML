fun hello () = "Hello, World!"

fun isLeapYear year =
    if year mod 4 = 0 then
        if year mod 100 = 0 then
            if year mod 400 = 0 then
                true
            else
                false
        else
            true
    else
        false

fun isLeapYear year =
    let fun multiple n = year mod n =0;
    in    
        case (multiple 4, multiple 100, multiple 400) of
        (_,_,true) => true
        | (true,false,_) => true
        |_ => false
end
