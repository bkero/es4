(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct

fun findTraceOption (tname:string) 
    : (bool ref) option = 
    case tname of 
        "lex" => SOME (Lexer.doTrace)
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
        val beStrict = ref false

        fun toggleRef (n:string) (r:bool ref) = 
            (r := not (!r);
             print ("set " ^ n ^ " = " ^ (Bool.toString (!r)) ^ "\n"))

        fun doLine _ = 
            let 
                val _ = if doPrompt then print ">> " else print "<SMLREADY>\n"
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
                                        ":strict        - toggle strict verification\n",
                                        ":eval          - toggle evaluation stage\n",
                                        ":profile <N>   - toggle profiling at depth <N>\n"
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
                  | [":strict"] => toggleRef "strict" beStrict
                  | [":trace", t] => 
                    ((case findTraceOption t of 
                          NONE => 
                          (print ("unknown trace option " ^ t ^ "\n"))
                        | SOME r => toggleRef ("trace option " ^ t) r);
                     doLine())

                  | [":profile", n] => 
                    ((case Int.fromString n of 
                          NONE => Eval.doProfile := NONE
                        | SOME 0 => Eval.doProfile := NONE
                        | SOME n => Eval.doProfile := SOME n);
                     doLine())
               
                  | [] => doLine ()
                  | _ => 
                    if (!doParse)
                    then
                        let
                            val p = Parser.parseLines [Ustring.fromSource line]
                        in
                            if (!doDefn)
                            then
                                let
                                    val d = Defn.defProgram p
                                    val vd = Verify.verifyProgram d
                                in
                                    if (!doEval)
                                    then
                                        let 
                                            val res = Eval.evalProgram vd
                                        in
                                            if res = Mach.Undef
                                            then ()
                                            else print (Ustring.toAscii (Eval.toUstring res) ^ "\n");
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
            | LogErr.VerifyError | LogErr.HostError | LogErr.UnimplError) => 
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

fun testDump dumpfile =
    let 
        (* 10 seconds to run, then we get SIGALRM. *)
        (* val _ = Posix.Process.alarm (Time.fromReal 100.0) *)
    	val _ = TextIO.print "booting ... \n";
        val _ = Boot.boot (); 
    in
        SMLofNJ.exportFn (dumpfile, 
                          (fn (arg0, argvRest) => 
                              let 
                                  val _ = Posix.Process.alarm (Time.fromReal 300.0)
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
                                  0
                              end)) ;
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
                              | ("-dump"::filename::argvRest) => testDump filename
                              | _ => testDefn argvRest);
                           0))

end
