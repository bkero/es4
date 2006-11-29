(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct
fun exnDetails (Fail s)  = SOME ("Fail: " ^ s)
  | exnDetails Bind      = SOME "nonexhaustive binding failure"
  | exnDetails Match     = SOME "nonexhaustive match failure"
  | exnDetails Div       = SOME "divide by zero"
  | exnDetails Domain    = SOME "domain error"
  | exnDetails Overflow  = SOME "overflow"
  | exnDetails Size      = SOME "size"
  | exnDetails Subscript = SOME "subscript out of bounds"
  | exnDetails _         = NONE

fun printStackTrace e =
    let val ss = SMLofNJ.exnHistory e
    in
        TextIO.print ("uncaught exception " ^ (exnName e));
        (case (exnDetails e) of
              SOME s => TextIO.print (" [" ^ s ^ "]\n")
            | NONE => TextIO.print "\n");
        List.app (fn s => TextIO.print ("\t" ^ s ^ "\n")) ss
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
