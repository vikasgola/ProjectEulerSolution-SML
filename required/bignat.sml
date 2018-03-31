signature BIGNAT =
    sig
        type bignat
        type order = bignat * bignat
        exception overflow
        exception underflow
        exception division_by_zero
        exception emptyList
        
        val zero : bignat                       (* done *)
        val normalize : bignat -> bignat        (* done *)
        val fromString : string -> bignat       (* done *)
        val toString : bignat -> string         (* done *)
        val ++ : bignat * bignat -> bignat      (* done *)
        val succ : bignat -> bignat             (* done *)
        val min : bignat * bignat -> bignat     (* done *)
        val max : bignat * bignat -> bignat     (* done *)
        val ** : bignat * bignat -> bignat      (* done *)
        val compare : bignat * bignat -> order  (* done *)
        val << : bignat * bignat -> bool        (* done *)
        val <<= : bignat * bignat -> bool       (* done *)
        val >> : bignat * bignat -> bool        (* done *)
        val >>= : bignat * bignat -> bool       (* done *)
        val == : bignat * bignat -> bool        (* done *)
        val len : bignat -> int                 (* done *)
        val lenCompare : bignat * bignat -> order (* done *)
        val lenLt : bignat * bignat -> bool         (* done *)
        val lenLeq : bignat * bignat -> bool        (* done *)
        val lenGt : bignat * bignat -> bool         (* done *)
        val lenGeq : bignat * bignat -> bool        (* done *)
        val lenEq : bignat * bignat -> bool         (* done *)
        val -- : bignat * bignat -> bignat          (* done *)
        val pred : bignat -> bignat                 (* done *)
        val %% : bignat * bignat -> bignat * bignat
        val fact : bignat-> bignat
        (*val quo : bignat * bignat -> bignat*)
        (* val rem : bignat * bignat -> bignat *)
end

structure Bignat:BIGNAT =
    struct
        type bignat = string;
        type order = bignat * bignat;

        exception overflow
        exception underflow
        exception division_by_zero
        exception emptyList
        
        
        fun toString(n) = n;

        fun fromString(n) : bignat = n;

        val zero : bignat = fromString("0");

        local
            fun concat_str [] = ""
                | concat_str(s::e) = s^concat_str(e)
            fun toInts (s::e) = if (e = []) then [str(s)]
                            else str(s)::(toInts e)
        in
            fun toList(n) = toInts(explode(n));
            fun listtoBignat(n) = concat_str n; 
        end

        local
          fun start []  = ["0"]
                | start(s::e) = if(s = "0") then start(e)
                                else s::e;
                                    (* else [s]@["."]@[hd(e)]@["E"]@[Int.toString(len(listtoBignat(s::e))-1)] ;   *)
        in
          fun normalize(n): bignat = if(substring(n,0,1) <> "-") then listtoBignat(start(toList(n)))
                                        else "-"^listtoBignat(start(toList(substring(n,1,size(n)-1))));
        end

        fun len(n) = if(substring(n,0,1) <> "-") then size(normalize(n))
                else len( substring(n,1,size(normalize(n))-1) );


        local
            fun startsucc(s) = if(s <> []) then (
                                        if(valOf(Int.fromString(List.last(s))) <> 9 ) then List.take(s,(length s) - 1 )@[Int.toString(valOf(Int.fromString(List.last(s))) + 1)]
                                        else startsucc(List.take(s,(length s) - 1 ))@["0"] ) 
                            else raise emptyList
            fun startpred(s) = if(s <> []) then (
                        if(valOf(Int.fromString(List.last(s))) <> 0 ) then List.take(s,(length s) - 1 )@[Int.toString(valOf(Int.fromString(List.last(s))) - 1)]
                        else startpred(List.take(s,(length s) - 1 ))@["9"] ) 
            else raise emptyList

        in
            fun succ(n) : bignat = if(substring(n,0,1) <> "-") then listtoBignat( startsucc(toList(n)) )
                                    else if(n <> "-1") then "-"^pred(substring(n,1,size(n)-1))
                                    else "0"
                                    and
            pred(n) : bignat = if( n = "0") then "-1" 
                                    else if(substring(n,0,1) <> "-") then listtoBignat( startpred(toList(n)) )
                                    else "-"^succ(substring(n,1,size(n)-1));
        end

        infix ==;
        local
            fun isequal ([a],[b]) = (a = b)
                | isequal(a::e,b::e1) = if(a <> b) then false
                                        else if(e <> [] andalso e1 <> []) then isequal(e,e1)
                                        else false
        in
            fun op == (a: bignat,b: bignat) = isequal(toList(a),toList(b));
        end

        local
            fun start(a,b) =  if (size(normalize(a)) = size(normalize(b))) then (a,b)
                                    else if(size(normalize(a)) > size(normalize(b))) then (b,a)
                                    else (a,b)

        in
            fun lenCompare(a: bignat,b: bignat) : order = if(substring(a,0,1) = "-" andalso substring(b,0,1) = "-") then start(substring(a,1,size(a)-1) , substring(b,1,size(b)-1))
                                                else if(substring(a,0,1) = "-") then start(substring(a,1,size(a)-1),b)
                                                else if(substring(b,0,1) = "-") then start(a,substring(b,1,size(b)-1))
                                                else start(a,b);
        end

        local
            fun start([],[]) = []
                | start(a::e,b::e1) =if(length e > length e1) then b::e1
                                        else if(length e < length e1) then a::e
                                        else if( valOf(Int.fromString a) > valOf(Int.fromString b) ) then b::e1
                                        else if( valOf(Int.fromString a) < valOf(Int.fromString b) ) then a::e
                                        else a::start(e,e1);
        in
            fun min(a: bignat , b: bignat ) = 
                            if(substring(a,0,1) = "-" andalso substring(b,0,1) = "-") then 
                                if(listtoBignat("-"::start(toList(substring(b,1,size(b)-1)) , toList(substring(a,1,size(a)-1)) )) = a ) then b
                                else a 
                            else if(substring(a,0,1) = "-") then a
                            else if(substring(b,0,1) = "-") then b
                            else listtoBignat(start(toList(normalize(a)) , toList(normalize(b))));
        end

        fun max(a: bignat, b: bignat) = if(min(a,b) = a) then b
                                        else a;

        fun compare(a: bignat, b: bignat) = if(min(a,b) = a) then (a,b)
                                            else (b,a)

        fun lenLt(a: bignat, b: bignat) = if(len(a) < len(b)) then true
                                            else false

        fun lenGt(a: bignat, b: bignat) = if(len(a) > len(b)) then true
                                                    else false
        
        fun lenLeq(a: bignat, b: bignat) = if(len(a) < len(b) orelse len a = len b ) then true
                                            else false

        fun lenGeq(a: bignat, b: bignat) = if(len(a) > len(b) orelse len a = len b ) then true
                                            else false

        fun lenEq(a: bignat, b: bignat) = if(len a = len b ) then true
                                            else false

        infix <<;
        fun op << (a: bignat, b: bignat) = if( a == b ) then false
                                            else if(min(a,b) = a) then true
                                            else false

        infix <<=;
        fun op <<= (a: bignat, b: bignat) = if( a == b ) then true
                                            else if(min(a,b) = a) then true
                                            else false

        infix >>;
        fun op >> (a: bignat, b: bignat) = if( a == b ) then false
                                            else if(min(a,b) = b) then true
                                            else false

        infix >>=;
        fun op >>= (a: bignat, b: bignat) = if( a == b ) then true
                                            else if(min(a,b) = b) then true
                                            else false

        infix ++;
        (* local
            fun su(a,b) = Int.toString( valOf(Int.fromString(a)) + valOf(Int.fromString(b)));

            fun add(x,y) =  if( (length su(x,y) ) < 8 )
                                then ( su(x,y), "0")
                            else ( su(x,y) , substring(su(x,y),0 ,size(su(x,y)) - 8 ))

            fun start(a,b,c) = if(length a < 8 andalso length b < 8) then 
                                    if(c <> "0" ) then add(#1 add(a,b), #2 add(a,b) )
                                    else add(#1 add(a,b), "0" )
                            else if(length a < 8) then #2 add(a , substring( b , size(b)-8 ,size()) )^start()
        in
            fun op ++ (a: bignat, b: bignat) = if(substring(a,0,1) <> "-" andalso substring(b,0,1) <> "-" )
                                                            then normalize(start(substring(a,0,size(n)-1),substring(b,0,size(n)-1), 0 ));
        end *)


        (* only for positives *)

        local
            fun su(a,b,c) = Int.toString( valOf(Int.fromString(a)) + valOf(Int.fromString(b)) + valOf(Int.fromString(c)));        
            fun sum(a,b,c) = if(size(su(a,b,c)) <> 1) then (String.extract(su(a,b,c) ,size(su(a,b,c)) - 1, NONE ) ,  substring(su(a,b,c) ,0 , size(su(a,b,c)) - 1 ) )
                                else ( su(a,b,c), "0" );

            fun start([a] , [b] , c ) = let 
                                val (x,y) = sum(a,b,listtoBignat(c))
                                in 
                                    [y]@[x]
                                end
                |  start(a , [] , c ) = if(c = ["0"]) then a
                                        else start(a,c,["0"])
                |  start([] , b , c ) = if(c = ["0"]) then b
                                        else start(b,c,["0"])
                | start(a,b,c) = let 
                                val (x,y) = sum(List.last(a) , List.last(b) , listtoBignat(c) ) ;
                            in
                                start( List.take(a,length(a)-1 ), List.take(b,length(b)-1 ) , toList y  )@[x]
                            end
        in
            fun op ++ (a: bignat, b: bignat) = normalize(listtoBignat(start(toList(a),toList(b),["0"])))
        end
        
        infix --;
        local
            fun su(a,b,c) = valOf(Int.fromString(a)) - valOf(Int.fromString(b)) - valOf(Int.fromString(c));        
            fun sub(a,b,c) = if(su(a,b,c) < 0) then (Int.toString( 10 + su(a,b,c)) , "1" )
                                else ( Int.toString(su(a,b,c)), "0" );

            fun start([a] , [b] , c ) = let 
                                val (x,y) = sub(a,b,listtoBignat(c))
                                in 
                                    [x]
                                end
                |  start(a , [] , c ) = if(c = ["0"]) then a
                                        else toList(pred(listtoBignat(a)))
                | start(a,b,c) =  let 
                                val (x,y) = sub(List.last(a) , List.last(b) , listtoBignat(c) ) ;
                            in 
                                start( List.take(a,length(a)-1 ), List.take(b,length(b)-1 ), toList(y) )@[x]
                            end
        in
            fun op -- (a: bignat, b: bignat) = if(a == b) then zero
                                                else if(a << b) then raise underflow
                                                else normalize(listtoBignat(start(toList(a),toList(b) , ["0"])))
        end

        infix **;
        local
            fun su(a,b) = valOf(Int.fromString(a)) * valOf(Int.fromString(b));
            
            fun mul([],b,c) = c
                | mul(a,b,c) =  if(su(List.last(a),b) < 10) then 
                                        if(c = "0") then mul( List.take(a,length(a)-1 ) , b , c)^(Int.toString(su(List.last(a),b)) )
                                        else if( (Int.toString(su(List.last(a),b)) ++ c) << "10") then mul( List.take(a,length(a)-1 ) , b , "0")^(Int.toString(su(List.last(a),b)) ++ c )
                                        else mul( List.take(a,length(a)-1 ) , b , substring(Int.toString(su(List.last(a),b)) ++ c , 0 , 1 )  )^( substring(Int.toString(su(List.last(a),b)) ++ c , 1 , 1 )  )
                                else mul( List.take(a,length(a)-1 ) , b , substring(Int.toString(su(List.last(a),b)) ++ c , 0 , 1 )  )^( substring(Int.toString(su(List.last(a),b)) ++ c , 1 , 1 )  );

            fun start(a , [] ) = "0"
                |  start([] , b) = "0"
               | start(a,b) = if( listtoBignat(a) >> listtoBignat(b) ) then (start( a, List.take(b,length(b)-1 ))^"0") ++ mul(a,List.last(b),"0")
                                else (start(List.take(a,length(a)-1 ), b )^"0") ++ mul(b,List.last(a),"0")
        in
             fun op ** (a: bignat, b: bignat) = if( normalize(b) == zero orelse normalize(a) == zero ) then zero
                                                else normalize(start(toList(a),toList(b)))
        end

        infix %%;
        local
            fun divs(x,y,z) = if( y**z >> x ) then (x--(y**z) , z)
                                else divs(x,y,z++fromString("1"))

            fun start(a,b,q,r) : bignat * bignat = if(r << b andalso len(a) = 0) then (q,r)
                                else if(len(a) = 1) then ( let 
                                                                val (l,m) = divs(fromString(toString(r)^toString(a)),b, zero);
                                                            in 
                                                                (fromString(toString(q)^toString(m)), l)
                                                            end )
                                else if( fromString(toString(r)^substring(a,0,1)) >>= b ) then ( 
                                                                                let
                                                                                    val (l,m) = divs(fromString(toString(r)^substring(toString(a),0,1)), b , zero);
                                                                                in
                                                                                    start( listtoBignat( tl(toList(a)) ) , b , fromString(toString(q)^toString(m)) , l )
                                                                                end )
                                else start(listtoBignat( tl(toList(a)) ) , b , fromString(toString(q)^toString(zero)) , fromString(toString(r)^substring(a,0,1)) )
        in
            fun op %% (a: bignat, b: bignat) = if(normalize(b) = zero) then raise division_by_zero
                                                else if(len(a) >= len(b)) then start(fromString(substring(toString(a),len(b),len(a)-len(b) )) , b , fromString(zero) , fromString(substring(toString(a),0,len(b))) )
                                                else (fromString(zero) , a)
        end

        (* local
            fun start(a,b,s) = if(a<<b) then
                                else start(a,b*b, s)
        in
            fun rem(a: bignat, b: bignat): bignat = if(a << b) then a
                                                else start(a,b,b)
        end *)

        fun fact(a: bignat) = if(a = fromString("1")) then fromString("1")
                                else a**fact(a--fromString("1")) 

end
open Bignat;
    infix ==;
    infix <<;
    infix <<=;
    infix >>;
    infix >>=;
    infix ++;
    infix --;
    infix **;
    infix %%


