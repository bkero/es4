(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct

fun findTraceOption (tname:string) 
    : (bool ref) option = 
    case tname of 
        "lex" => SOME (Lexer.UserDeclarations.doTrace)
      | "parse" => SOME (Parser.doTrace)
      | "name" => SOME (Multiname.doTrace)
      | "defn" => SOME (Defn.doTrace)
      | "verify" => SOME (Verify.doTrace)
      | "eval" => SOME (Eval.doTrace)
      | "mach" => SOME (Mach.doTrace)
      | "decimal" => SOME (Decimal.doTrace)
      | "native" => SOME (Native.doTrace)
      | "boot" => SOME (Boot.doTrace)
      | "stack" => SOME (Eval.traceStack)
      | _ => NONE

exception quitException
    
fun repl doPrompt = 
    let
        val doParse = ref true
        val doDefn = ref true
        val doEval = ref true

        fun toggleRef (n:string) (r:bool ref) = 
            (r := not (!r);
             print ("set " ^ n ^ " = " ^ (Bool.toString (!r)) ^ "\n"))

        fun doLine _ = 
            let 
                val _ = if doPrompt then print ">> " else ()
                val line = case TextIO.inputLine TextIO.stdIn of 
                               NONE => ""
                             | SOME s => s
                val toks = String.tokens Char.isSpace line
                fun help _ = (List.app print 
                                       [
                                        ":quit          - quit repl\n",
                                        ":trace <T>     - toggle tracing of <T>\n",
                                        ":help          - this message\n",
                                        ":reboot        - reload the boot environment\n",
                                        ":parse         - toggle parse stage\n",
                                        ":defn          - toggle defn stage\n",
                                        ":eval          - toggle evaluation stage\n" 
                                       ]; 
                              doLine())
            in
                case toks of 
                    [":quit"] => raise quitException
                  | [":q"] => raise quitException
                  | [":h"] => help ()
                  | [":help"] => help ()
                  | [":?"] => help ()
                  | ["?"] => help ()
                  | [":reboot"] => (Boot.boot(); doLine ())
                  | [":parse"] => toggleRef "parse" doParse
                  | [":defn"] => toggleRef "defn" doDefn
                  | [":eval"] => toggleRef "eval" doEval
                  | [":trace", t] => 
                    ((case findTraceOption t of 
                          NONE => 
                          (print ("unknown trace option " ^ t ^ "\n"))
                        | SOME r => toggleRef ("trace option " ^ t) r);
                     doLine())
                  | [] => doLine ()
                  | _ => 
                    if (!doParse)
                    then 
                        let 
                            val p = Parser.parseLines [line]
                        in
                            if (!doDefn)
                            then 
                                let
                                    val d = Defn.defProgram p
                                in
                                    if (!doEval)
                                    then 
                                        let 
                                            val res = Eval.evalProgram d
                                        in
                                            if res = Mach.Undef
                                            then ()
                                            else print (Ustring.toString (Eval.toUstring res) ^ "\n");
                                            doLine ()
                                        end
                                    else 
                                        doLine ()
                                end
                            else 
                                doLine ()
                        end
                    else
                        doLine ()
            end

        fun runUntilQuit _ = 
            ((doLine () 
              handle 
              (LogErr.LexError | LogErr.ParseError | LogErr.NameError 
            | LogErr.DefnError | LogErr.EvalError | LogErr.MachError 
            | LogErr.HostError | LogErr.UnimplError) => 
              Eval.resetStack (); ());
             runUntilQuit ())
    in
        Boot.boot();
        runUntilQuit ()
        handle quitException => print "bye\n"
    end
            

fun testTC argvRest =
    let 
    	val _ = TextIO.print "booting ... \n";
        val _ = Boot.boot (); 
        val asts = List.map Parser.parseFile argvRest
        val _ = TextIO.print "defining ... \n";
        val dps = map Defn.defProgram asts
	    val _ = TextIO.print "type checking ... \n";
        val _ = List.map Verify.verifyProgram dps;
	    val _ = TextIO.print "type checked! \n"
    in
        ()
    end

fun testEV argvRest =
    let 
        (* 10 seconds to run, then we get SIGALRM. *)
        val _ = Posix.Process.alarm (Time.fromReal 100.0)
    	val _ = TextIO.print "booting ... \n";
        val _ = Boot.boot (); 
        val _ = TextIO.print "parsing ... \n";
        val asts = List.map Parser.parseFile argvRest
        val _ = TextIO.print "defining ... \n";
        val dps = map Defn.defProgram asts
	    val _ = TextIO.print "type checking ... \n";
        val _ = List.map Verify.verifyProgram dps;
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
    case explode opt of
        (#"-" :: #"T" :: rest) => 
        (case findTraceOption (String.implode rest) of 
             SOME r => (r := true; false)
           | NONE => true)
      | _ => true

fun main (argv0:string, argvRest:string list) =
    BackTrace.monitor (fn () =>                          
                          (
                           (case List.filter consumeTraceOption argvRest of
                                ("-r"::argvRest) => repl true 
                              | ("-rq"::argvRest) => repl false 
                              | ("-tc"::argvRest) => testTC argvRest
	                          | ("-ev"::argvRest) => testEV argvRest
                              | _ => testDefn argvRest);
                           0))

end
