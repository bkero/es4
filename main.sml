(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct
fun main (argv0:string, argvRest:string list) = 
    let 
	val asts = List.map Parser.parseFile argvRest
    in
	0
    end
end
