(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* 
 * This is the main entry point for the ES4 reference evaluator.
 *)

structure Main = struct

val quiet = ref false

fun findTraceOption (tname:string) 
    : (bool ref) option = 
    case tname of 
        "lex" => SOME (Lexer.doTrace)
      | "parse" => SOME (Parser.doTrace)
      | "name" => SOME (Multiname.doTrace)
      | "defn" => SOME (Defn.doTrace)
      | "verify" => SOME (Verify.doTrace)
      | "verified" => SOME (Verify.doTraceProg)
      | "eval" => SOME (Eval.doTrace)
      | "mach" => SOME (Mach.doTrace)
      | "decimal" => SOME (Decimal.doTrace)
      | "native" => SOME (Native.doTrace)
      | "boot" => SOME (Boot.doTrace)
      | "stack" => SOME (Eval.traceStack)
      | _ => NONE

fun consumeOption (opt:string) : bool = 
    case explode opt of
        (#"-" :: #"T" :: rest) => 
        (case findTraceOption (String.implode rest) of 
             SOME r => (r := true; false)
           | NONE => true)
      | ([#"-", #"q"]) => 
        (quiet := true; false)
      | (#"-" :: #"P" :: rest) => 
        (case Int.fromString (String.implode rest) of 
            NONE => false
          | SOME 0 => false
          | SOME n => (Eval.doProfile := SOME n; false))
      | _ => true

exception quitException

fun startup doBoot argvRest = 
    let
        val argvRest = List.filter consumeOption argvRest
    in
        if doBoot then 
    	    (TextIO.print "booting ... \n";
             Boot.boot ();
             argvRest)
        else
            argvRest
    end
    
fun repl doBoot argvRest = 
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
                val _ = if !quiet then print "<SMLREADY>\n" else print ">> " 
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
                                            val res = Eval.evalTopProgram vd
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
              LogErr.LexError e => (print ("**ERROR** LexError: " ^ e ^ "\n"); Eval.resetStack(); ())
            | LogErr.ParseError e => (print ("**ERROR** ParseError: " ^ e ^ "\n"); Eval.resetStack(); ())
            | LogErr.EofError => (print ("**ERROR* EofError: Unexpected end of file\n"); Eval.resetStack(); ())
            | LogErr.NameError e => (print ("**ERROR** NameError: " ^ e ^ "\n"); Eval.resetStack(); ())
            | LogErr.DefnError e => (print ("**ERROR** DefnError: " ^ e ^ "\n"); Eval.resetStack(); ())
            | LogErr.EvalError e => (print ("**ERROR** EvalError: " ^ e ^ "\n"); Eval.resetStack(); ())
            | LogErr.MachError e => (print ("**ERROR** MachError: " ^ e ^ "\n"); Eval.resetStack(); ())
            | LogErr.VerifyError e => (print ("**ERROR** VerifyError: " ^ e ^ "\n"); Eval.resetStack(); ())
            | LogErr.HostError e => (print ("**ERROR** HostError: " ^ e ^ "\n"); Eval.resetStack(); ())
            | LogErr.UnimplError e => (print ("**ERROR** UnimplError: " ^ e ^ "\n"); Eval.resetStack(); ()));
             runUntilQuit ())
    in
        startup doBoot argvRest;
        runUntilQuit ()
        handle quitException => print "bye\n"
    end

fun parse doBoot argvRest = 
    let 
        val argvRest = startup doBoot argvRest
    in
        TextIO.print "parsing ... \n";
        List.map Parser.parseFile argvRest
    end

fun define doBoot argvRest =
    let 
        val parsed = parse doBoot argvRest
    in
        TextIO.print "defining ... \n";
        map Defn.defProgram parsed
    end
            
fun verify doBoot argvRest =
    let 
        val defined = define doBoot argvRest
    in
        TextIO.print "verifying ... \n";
        List.map Verify.verifyProgram defined
    end

fun eval doBoot argvRest =
    let 
        val verified = verify doBoot argvRest 
    in
        Posix.Process.alarm (Time.fromReal 300.0);
	    TextIO.print "evaluating ... \n";
        (map Eval.evalTopProgram verified; 0)
        handle 
        LogErr.LexError e => (print ("**ERROR** LexError: " ^ e ^ "\n");  1)
      | LogErr.ParseError e => (print ("**ERROR** ParseError: " ^ e ^ "\n"); 1)
      | LogErr.NameError e => (print ("**ERROR** NameError: " ^ e ^ "\n"); 1)
      | LogErr.DefnError e => (print ("**ERROR** DefnError: " ^ e ^ "\n"); 1)
      | LogErr.EvalError e => (print ("**ERROR** EvalError: " ^ e ^ "\n"); 1)
      | LogErr.MachError e => (print ("**ERROR** MachError: " ^ e ^ "\n"); 1)
      | LogErr.VerifyError e => (print ("**ERROR** VerifyError: " ^ e ^ "\n"); 1)
      | LogErr.HostError e => (print ("**ERROR** HostError: " ^ e ^ "\n"); 1)
      | LogErr.UnimplError e => (print ("**ERROR** UnimplError: " ^ e ^ "\n"); 1)
    end


fun dumpEval dumpfile =
    (startup true [];
     SMLofNJ.exportFn 
         (dumpfile, (fn (arg0, argvRest) => 
                        BackTrace.monitor
                            (fn () => eval false argvRest))))

fun dumpRepl dumpfile =
    (startup true [];
     SMLofNJ.exportFn 
         (dumpfile, (fn (arg0, argvRest) => 
                        BackTrace.monitor 
                            (fn () => (repl false argvRest; 0)))))

fun main (argv0:string, argvRest:string list) =
    BackTrace.monitor 
        (fn () =>                          
            (case argvRest of
                 ("-r"::argvRest) => (repl true argvRest; 0)
               | ("-p"::argvRest) => (parse true argvRest; 0)
               | ("-d"::argvRest) => (define true argvRest; 0)
               | ("-v"::argvRest) => (verify true argvRest; 0)
	           | ("-e"::argvRest) => eval true argvRest
               | ("-dumpEval"::filename::argvRest) => (dumpEval filename; 0)
               | ("-dumpRepl"::filename::argvRest) => (dumpRepl filename; 0)
               | _ => (define true argvRest; 0))
            handle 
            LogErr.LexError e => (print ("**ERROR** LexError: " ^ e ^ "\n"); 1)
          | LogErr.ParseError e => (print ("**ERROR** ParseError: " ^ e ^ "\n"); 1)
          | LogErr.EofError => (print ("**ERROR* EofError: Unexpected end of file\n"); 1)
          | LogErr.NameError e => (print ("**ERROR** NameError: " ^ e ^ "\n"); 1)
          | LogErr.DefnError e => (print ("**ERROR** DefnError: " ^ e ^ "\n"); 1)
          | LogErr.EvalError e => (print ("**ERROR** EvalError: " ^ e ^ "\n"); 1)
          | LogErr.MachError e => (print ("**ERROR** MachError: " ^ e ^ "\n"); 1)
          | LogErr.VerifyError e => (print ("**ERROR** VerifyError: " ^ e ^ "\n"); 1)
          | LogErr.HostError e => (print ("**ERROR** HostError: " ^ e ^ "\n"); 1)
          | LogErr.UnimplError e => (print ("**ERROR** UnimplError: " ^ e ^ "\n"); 1))

end
