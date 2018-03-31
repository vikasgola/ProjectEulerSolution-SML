(*                                 ===========Probelm Statement=============

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 *)

local
    fun isdivisible(upto ,a) = if(upto = 1) then (
        true
    )else if(a mod upto = 0) then(
        isdivisible(upto-1 ,a)
    )else(
        false
    )

    fun start(upto , a) = if(isdivisible(upto ,a)) then(
        a
    )else(
        start(upto ,a+1)
    )
in
  fun main(upto) = start(upto , 1)
end