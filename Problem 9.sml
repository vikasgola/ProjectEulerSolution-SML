(*                                 ===========Probelm Statement============= 

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean trip let for which a + b + c = 1000.
Find the product abc. *)

fun start(a , b , c) = if(c = 1) then start(a , b-1 , 1000)
    else if(b = 1) then start(a-1 ,1000 ,1000)
    else if(a*a + b*b = c*c andalso a+b+c = 1000) then (a , b , c )
    else start(a , b ,c-1 )

val ans = main(1000 ,1000 , 1000)