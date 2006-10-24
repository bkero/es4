structure Name : ORD_KEY = struct

type str = Ast.ustring
type N = { ns: str, prop: str }

fun cmpStr (s1:str) (s2:str) = 
    if s1 = s2 
    then EQUAL
    else (if s1 < s2
	  then LESS
	  else GREATER)

(* Implementation of ORD_KEY *)

type ord_key = N

fun compare (n1:N, n2:N) =
    let 
	val nsc = cmpStr (#ns n1) (#ns n2)
    in
	if nsc = EQUAL
	then cmpStr (#prop n1) (#prop n2)
	else nsc
    end

end
