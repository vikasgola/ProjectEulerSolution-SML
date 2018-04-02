(* ============problem statement===========
The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million. *)

(* required library *)
use "required/bignat.sml";

local
    fun checkprime(list , num) = if(list = []) then true
    else if(num mod hd(list) = 0) then (false)
    else checkprime(tl(list) , num)
in
    fun main(primes, next) = if(next > 2000000) then(
        "2"
    )else if(checkprime(primes , next)) then(
        Bignat.fromString(Int.toString(next))++(main(primes@[next] ,next+2 ))
    )else(
        main(primes , next+2)
    )
end

(* main([2] , 3) *)

val ans = 142913828922