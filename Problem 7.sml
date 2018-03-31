(*                                 ===========Probelm Statement=============

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10001st prime number? *)

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

    fun start(a, numbered) = if(numbered = 0) then(
        a-2
    )else if(checkprime(a)) then(
        start(a+2 , numbered-1 )
    )else(
        start(a+2 , numbered)
    )
in
    fun main(numbered) = start(3 , numbered-1)
end