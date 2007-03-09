(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct

fun testTC argvRest =
    let val asts = List.map Parser.parseFile argvRest
    in
	TextIO.print "type checking ... \n";
    List.map Verify.verifyProgram asts;  
	TextIO.print "type checked! \n"
    end

fun testEV argvRest =
    let 
        val _ = Magic.registerNatives ()
        val _ = TextIO.print "parsing ... \n";
        val asts = List.map Parser.parseFile argvRest
        val _ = TextIO.print "defining ... \n";
        val dps = map Defn.defProgram asts
        val _ = TextIO.print "evaluating ... \n";
        val _ = map Eval.evalProgram dps
        val _ = TextIO.print "evaluated! \n"                
    in
    ()
    end

fun testDefn argvRest =
    let 
    	val _ = TextIO.print "parsing ... \n";
	    val asts = List.map Parser.parseFile argvRest
    	val _ = TextIO.print "defining ... \n";
	    val dps = map Defn.defProgram asts
    in
    	()
    end

fun consumeTraceOption (opt:string) : bool = 
    case opt of 
        "-Tlex" => (Lexer.UserDeclarations.doTrace := true; false)
      | "-Tparse" => (Parser.doTrace := true; false)
      | "-Tname" => (Multiname.doTrace := true; false)
      | "-Tdefn" => (Defn.doTrace := true; false)
      | "-Teval" => (Eval.doTrace := true; false)
      | "-Tmach" => (Mach.doTrace := true; false)
      | _ => true

fun main (argv0:string, argvRest:string list) =
    BackTrace.monitor (fn () =>
                          ((case List.filter consumeTraceOption argvRest of
                                ("-tc"::argvRest) => testTC argvRest
	                          | ("-ev"::argvRest) => testEV argvRest
                              | _ => testDefn argvRest);
                           0))

end
