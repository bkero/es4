(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct
fun printStackTrace e =
    let val ss = SMLofNJ.exnHistory e
        val s = General.exnMessage e
        val name = General.exnName e
        val details = if s = name then "" else (" [" ^ s ^ "]")
    in
        print ("uncaught exception " ^ name ^ details ^ "\n");
        app (fn s => TextIO.print ("  raised at: " ^ s ^ "\n")) ss
    end

fun testTC argvRest =
    let val asts = List.map Parser.parseFile argvRest
    in
	TextIO.print "type checking ... \n";
        List.map TypeChk.tcProgram asts;
	TextIO.print "type checked! \n"
    end

fun main (argv0:string, argvRest:string list) =
    BackTrace.monitor (fn () =>
                       ((case argvRest of
                              ("-tc"::argvRest) => testTC argvRest
                            | _ => (List.map Parser.parseFile argvRest; ()));
                        0))

end
