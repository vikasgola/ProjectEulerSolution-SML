(*                                 ===========Probelm Statement=============
A palindromic number reads the same both ways. 
The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
 *)

local
    fun checksymmetry([n]) = true
        | checksymmetry([]) = true
        | checksymmetry(n) = if(hd(n) = hd(rev(n)) ) then(
        checksymmetry( tl(rev(tl(rev(n)))) )
    )else(
        false
    )

    fun pow(a,b) = if(b <> 0) then(
        a*pow(a , b-1)
    )else(
        1
    )

    fun startone(constant , a) = if(checksymmetry(explode(Int.toString(a*constant)))) then(
        constant*a
    )else(
        startone(constant , a-1)
    )

    fun start(1 , max) = max
        | start(a,max) = if(startone(a ,a) > max) then(
        start(a-1 , startone(a ,a) )
    )else(
        start(a-1 , max)
    )
in
    fun main(d) = start(pow(10 ,d)-1 , pow(10 ,d)-1)
end