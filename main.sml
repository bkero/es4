(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct

fun testTC argvRest =
    let 
    	val _ = TextIO.print "booting ... \n";
        val _ = Boot.boot (); 
        val asts = List.map Parser.parseFile argvRest
	    val _ = TextIO.print "type checking ... \n";
        val _ = List.map Verify.verifyProgram asts;  
	    val _ = TextIO.print "type checked! \n"
    in
        ()
    end

fun testEV argvRest =
    let 
    	val _ = TextIO.print "booting ... \n";
        val _ = Boot.boot (); 
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
    	val _ = TextIO.print "booting ... \n";
        val _ = Boot.boot (); 
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
      | "-Tdecimal" => (Decimal.doTrace := true; false)
      | "-Tnative" => (Native.doTrace := true; false)
      | "-Tboot" => (Boot.doTrace := true; false)
      | _ => true

fun main (argv0:string, argvRest:string list) =
    BackTrace.monitor (fn () =>                          
                          (
                           (case List.filter consumeTraceOption argvRest of
                                ("-tc"::argvRest) => testTC argvRest
	                          | ("-ev"::argvRest) => testEV argvRest
                              | _ => testDefn argvRest);
                           0))

end
