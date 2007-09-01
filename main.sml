(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 *
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 *
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
 *
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * End of Terms and Conditions
 *
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *)

(*
 * This is the basic entry point for the ES4 reference evaluator.
 * Platform-specific wrappers for different SML implementations
 * may explicitly call the main function from custom entry points.
 *)

structure Main = struct

val interactive = ref true

fun findTraceOption (tname:string)
    : (bool ref) option =
    case tname of
        "lex" => SOME (Lexer.doTrace)
      | "parse" => SOME (Parser.doTrace)
      | "name" => SOME (Multiname.doTrace)
      | "defn" => SOME (Defn.doTrace)
      (* | "verify" => SOME (Verify.doTrace) *)
      (* | "verified" => SOME (Verify.doTraceProg) *)
      | "eval" => SOME (Eval.doTrace)
      | "mach" => SOME (Mach.doTrace)
      | "decimal" => SOME (DecimalParams.doTrace)
      | "native" => SOME (Native.doTrace)
      | "boot" => SOME (Boot.doTrace)
      (* | "stack" => SOME (Eval.traceStack) *)
      (* FIXME: add "fixture" and "type" *)
      | _ => NONE

fun consumeOption (opt:string) : bool =
    case explode opt of
        (#"-" :: #"T" :: rest) =>
        (case findTraceOption (String.implode rest) of
             SOME r => (r := true; false)
           | NONE => true)
      | ([#"-", #"I"]) =>
        (interactive := false; false)
(*
      | (#"-" :: #"P" :: rest) =>
        (case Int.fromString (String.implode rest) of
            NONE => false
          | SOME 0 => false
          | SOME n => (Eval.doProfile := SOME n; false))
*)
      | _ => true

exception quitException

(* FIXME: should use more portable OS.Process.exit *)
fun withEofHandler thunk =
    (thunk (); 0)
    handle LogErr.EofError => (print ("**ERROR* EofError: Unexpected end of file\n"); 1)

fun withHandlers thunk =
    (thunk (); 0)
    handle
    LogErr.LexError e => (print ("**ERROR** LexError: " ^ e ^ "\n"); 1)
  | LogErr.ParseError e => (print ("**ERROR** ParseError: " ^ e ^ "\n"); 1)
  | LogErr.NameError e => (print ("**ERROR** NameError: " ^ e ^ "\n"); 1)
  | LogErr.TypeError e => (print ("**ERROR** TypeError: " ^ e ^ "\n"); 1)
  | LogErr.FixtureError e => (print ("**ERROR** FixtureError: " ^ e ^ "\n"); 1)
  | LogErr.DefnError e => (print ("**ERROR** DefnError: " ^ e ^ "\n"); 1)
  | LogErr.EvalError e => (print ("**ERROR** EvalError: " ^ e ^ "\n"); 1)
  | LogErr.MachError e => (print ("**ERROR** MachError: " ^ e ^ "\n"); 1)
  | LogErr.VerifyError e => (print ("**ERROR** VerifyError: " ^ e ^ "\n"); 1)
  | LogErr.HostError e => (print ("**ERROR** HostError: " ^ e ^ "\n"); 1)
  | LogErr.UnimplError e => (print ("**ERROR** UnimplError: " ^ e ^ "\n"); 1)

fun startup (regsOpt:Mach.REGS option)
            (argvRest:string list)
    : (Mach.REGS * string list) =
    let
        val argvRest = List.filter consumeOption argvRest
    in
        case regsOpt of 
            SOME r => (r, argvRest)
          | NONE => 
            let 
                val _ = TextIO.print "booting ... \n"
                val regs = ref NONE
                fun bootAndCaptureRegs _ = (regs := SOME (Boot.boot ()))
            in
                withEofHandler (fn () => withHandlers bootAndCaptureRegs);
                (valOf (!regs), argvRest)
            end
    end

fun repl regsOpt argvRest =
    let
        val (regs, argvRest) = startup regsOpt argvRest                               

        val doParse = ref true
        val doDefn = ref true
        val doEval = ref true
        val beStrict = ref false
        val regsCell = ref regs

        fun toggleRef (n:string) (r:bool ref) =
            (r := not (!r);
             print ("set " ^ n ^ " = " ^ (Bool.toString (!r)) ^ "\n"))

        fun doLine _ =
            let
                val _ = if !interactive then print ">> " else print "<SMLREADY>\n"
                val line = case TextIO.inputLine TextIO.stdIn of
                               NONE => raise quitException
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
                  | [":reboot"] => (regsCell := Boot.boot(); doLine ())
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
(*
                  | [":profile", n] =>
                    ((case Int.fromString n of
                          NONE => Eval.doProfile := NONE
                        | SOME 0 => Eval.doProfile := NONE
                        | SOME n => Eval.doProfile := SOME n);
                     doLine())
*)

                  | [] => doLine ()
                  | _ =>
                    if (!doParse)
                    then
                        let
                            val frag = Parser.parseLines [Ustring.fromSource line]
                        in
                            if (!doDefn)
                            then
                                let
                                    val (prog, frag) = Defn.defTopFragment (#prog (!regsCell)) frag
                                    (* val vd = Verify.verifyProgram d *)
                                in
                                    regsCell := Eval.withProg regs prog;
                                    if (!doEval)
                                    then
                                        let
                                            val res = Eval.evalTopFragment (!regsCell) frag
                                        in
                                            (case res of
                                                 Mach.Undef => ()
                                               | _ => print (Ustring.toAscii 
                                                                 (Eval.toUstring (!regsCell) res) ^ "\n"));
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
            (withEofHandler (fn () => withHandlers doLine);
             runUntilQuit ())
    in
        runUntilQuit ()
        handle quitException => print "bye\n"
    end

fun parse regsOpt argvRest =
    let
        val (regs, argvRest) = startup regsOpt argvRest
    in
        TextIO.print "parsing ... \n";
        (regs, List.map Parser.parseFile argvRest)
    end

fun define regsOpt argvRest =
    let
        val (regs, frags) = parse regsOpt argvRest
        fun f prog accum (frag::frags) = 
            let 
                val (prog', frag') = Defn.defTopFragment prog frag
            in
                f prog' (frag'::accum) frags
            end
          | f prog accum _ = (prog, List.rev accum)
        val _ = TextIO.print "defining ... \n";
        val (prog, frags) = f (#prog regs) [] frags
    in
        (Eval.withProg regs prog, frags)
    end

fun verify regsOpt argvRest =
    define regsOpt argvRest
(*
    let
        val defined = define argvRest
    in
        TextIO.print "verifying ... \n";
        List.map Verify.verifyProgram defined
    end
*)

fun eval regsOpt argvRest =
    let
        val (regs, frags) = verify regsOpt argvRest
    in
(*        Posix.Process.alarm (Time.fromReal 300.0);
*)
	    TextIO.print "evaluating ... \n";
        withHandlers (fn () => map (Eval.evalTopFragment regs) frags)
    end

fun usage () =
    (List.app TextIO.print
              ["usage: es4 [-h|-r|-p file ...|-d file ...|-v file ...|-e file ...] [-Pn] [-Tmod] ...\n",
               "    -h            display this help message and exit\n",
               "(*) -r            start the interactive read-eval-print loop\n",
               "    -p            run given files through parse phase and exit\n",
               "    -d            run given files through definition phase and exit\n",
               "    -v            run given files through verification phase and exit\n",
               "    -e            evaluate given files and exit\n",
               "\n",
               "(*) default\n",
               "\n",
               "    -Pn           turn on profiling for stack depth {n}\n",
               "    -Tmod         turn on tracing for module {mod}\n",
               "\n",
               "    mod:\n",
               "        lex       lexing\n",
               "        parse     parsing\n",
               "        name      name resolution\n",
               "        defn      definition phase\n",
               "        verify    verification phase\n",
               "        verified  show post-verification AST alone\n",
               "        eval      evaluator\n",
               "        mach      abstract machine operations\n",
               "        decimal   decimal arithmetic\n",
               "        native    native operations\n",
               "        boot      standard library boot sequence\n",
               "        stack     stack operations\n"])

fun main (regsOpt:Mach.REGS option, argv0:string, argvRest:string list) =
    withEofHandler
        (fn () =>
            withHandlers
                (fn () =>
                    (case argvRest of
                         ("-h"::argvRest) => (usage (); 0)
                       | ("-r"::argvRest) => (repl regsOpt argvRest; 0)
                       | ("-p"::argvRest) => (parse regsOpt argvRest; 0)
                       | ("-d"::argvRest) => (define regsOpt argvRest; 0)
                       | ("-v"::argvRest) => (verify regsOpt argvRest; 0)
                       | ("-e"::argvRest) => (eval regsOpt argvRest)
                       | _ => (repl regsOpt argvRest; 0))))

end
