structure LogErr = struct

val log_level = ref 0

fun log n ss = 
    if n <= (!log_level)
    then (List.app TextIO.print ss; TextIO.print "\n")
    else ()

fun trace ss = log 3 ss

fun error ss = log 0 ("**ERROR** " :: ss)

fun traceIn ss = trace (">>> " :: ss)

fun traceOut ss = trace ("<<< " :: ss)

fun withTrace (ss:string list) (f:('a -> 'b)) (x:'a) : 'b = 
    let 
	val _ = traceIn ss
	val r:'b = f x
	val _ = traceOut ss
    in
	r
    end

exception ParseError
exception DefnError
exception EvalError
exception HostError
exception UnimplError

fun parseError ss = 
    (error ("parseError " :: ss); 
     raise ParseError)

fun defnError ss = 
    (error ("defnError " :: ss); 
     raise DefnError)

fun evalError ss = 
    (error ("evalError " :: ss); 
     raise EvalError)

fun hostError ss = 
    (error ("hostError " :: ss); 
     raise HostError)

fun unimplError ss = 
    (error ("unimplError " :: ss); 
     raise UnimplError)

end
