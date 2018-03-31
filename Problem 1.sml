
(* num --> number below which u want sum of multiples of 3 or 5 *)
(* sum --> intital value should be zero *)

fun main(0 , sum) = sum
    | main(num , sum) = if(num mod 3 = 0 orelse num mod 5 = 0) then (
    main(num-1 ,sum+num)
)else(
    main(num-1,sum)
)