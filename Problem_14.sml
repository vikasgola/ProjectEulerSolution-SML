(*
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
*)

use "required/bignat.sml";

fun chainLength(1, length) = length
|   chainLength(n, length) = 
    if(n mod 2 = 0) then chainLength(n div 2, length + 1)
    else chainLength(3*n + 1, length + 1)

(*
- chainLength(13, 1);
val it = 10 : int
*)

fun longestChainLength(1, lengthMax, res) = res
|   longestChainLength(n, lengthMax, res) = 
    let 
        val length = chainLength(n, 1)
    in 
        if(length > lengthMax) then longestChainLength(n - 1, length, n)
        else longestChainLength(n - 1, lengthMax, res)
    end 
    
(*
- longestChainLength(10000, 1, 1)
val it = 6171 : int
    returns the number upto 10000 producing longest chain
    
- longestChainLength(100000, 1, 1)
uncaught exception Overflow [overflow]
  raised at: <file Problem_14.sml>
  
need to use BIGNAT library but first division operation has to be implemented!
*)