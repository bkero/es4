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

fun testEV argvRest =
    let val asts = List.map Parser.parseFile argvRest
    in
	TextIO.print "evaluating ... \n";
        List.map Eval.evalProgram asts;
	TextIO.print "evaluated! \n"
    end

fun main (argv0:string, argvRest:string list) =
    BackTrace.monitor (fn () =>
                       ((case argvRest of
                              ("-tc"::argvRest) => testTC argvRest
	                    | ("-ev"::argvRest) => testEV argvRest
                            | _ => (List.map Parser.parseFile argvRest; ()));
                        0))

end
