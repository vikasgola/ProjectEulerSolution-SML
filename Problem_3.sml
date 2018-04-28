(*                                 ===========Probelm Statement=============
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ? 
 *)

(* required library *)
use "required/bignat.sml";

local
    fun checkprime(x) = (
    let
        fun prime(x,y)=
            if ((x mod y)=0 andalso y<>1 andalso y<>0)
                then false 
            else if (y=1 orelse x=2)
                then true
            else 
                prime(x,y-1)
        in	
            prime(x,x div 2)
        end
    )
    
    fun start(num ,d) = if((num mod d) = 0 ) then(
        if(checkprime(d)) then(
            d
        )else(
            start(num, d-1 )
        )
    )else(
        start(num , d-1)
    )

in
   fun main(num) = if(checkprime(num)) then(
       num
   )else start(num , num div 2)
 end