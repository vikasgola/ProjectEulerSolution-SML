(*                                 ===========Probelm Statement=============
The sum of the squares of the first ten natural numbers is,

12 + 22 + ... + 102 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)2 = 552 = 3025
Hence the difference between the sum of the squares of the first ten 
natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first 
one hundred natural numbers and the square of the sum. *)

local
    fun sumsquare(upto ,sum) = if(upto = 1) then(
        1
    )else(
        upto*upto + sumsquare(upto-1 , sum)
    )

    fun squaresum(upto , sum) = if(upto = 1) then(
        1
    )else(
        upto + squaresum(upto-1 , sum)
    )
in
    fun main(upto) = squaresum(upto ,0)*squaresum(upto ,0) - sumsquare(upto , 0)
end