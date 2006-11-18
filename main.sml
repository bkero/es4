(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct
fun testTC argvRest =
    let val asts = List.map Parser.parseFile argvRest
    in
	TextIO.print "type checking ... \n";
        List.map TypeChk.tcProgram asts;
	TextIO.print "type checked! \n"
    end
    handle Match => TextIO.print "incomplete match\n"

fun main (argv0:string, argvRest:string list) = 
    ((case argvRest of
           ("-tc"::argvRest) => testTC argvRest
         | _ => (List.map Parser.parseFile argvRest; ()));
     0)

end
