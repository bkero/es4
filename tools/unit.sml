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

structure UnitTests =
struct

open TextIO Utils StringUtils ListUtils List

datatype STAGE = Parse
               | Verify
               | VerifyEval
               | Eval

datatype INPUT_SOURCE = Raw of Ustring.SOURCE list
                      | File of string

type TEST_CASE = { name: string option,
                   stage: STAGE,
                   arg: bool,
                   source: INPUT_SOURCE }

type TEST_RESULT = TEST_CASE * bool

type HEADER = { name: string option, tokens: string list }

datatype LINE = Header of HEADER
              | SourceLine of string
              | Link of string

type 'a DICT = (string * 'a) list

exception BadTestParameter of string * int
exception MixedContent of int
exception ExtraLink of int

(* **********************************************************************************************
 * USER FEEDBACK
 * ********************************************************************************************** *)

fun warn msg filename lineNum =
    print (filename ^ ": Warning: " ^ msg ^ " on line " ^ (Int.toString lineNum) ^ "\n")

fun error msg filename lineNum =
    print (filename ^ ": Error: " ^ msg ^ " on line " ^ (Int.toString lineNum) ^ "\n")

fun unexpectedExn e =
(
    print "\n----------------------------------------\n";
    print "UNEXPECTED EXCEPTION:\n";
    PrintTrace.printStackTrace e;
    print   "----------------------------------------\n"
)

fun logTestCase name =
(
    print "\n****************************************\n";
    print "*** TEST CASE";
    (case name of
          NONE => ()
        | SOME s => print (" [" ^ s ^ "]..."));
    print "\n****************************************\n\n"
)

fun report (results : TEST_RESULT list) : int =
    let val successes = map #1 (filter (fn (_, b) => b) results)
        val failures = map #1 (filter (fn (_, b) => not b) results)
    in
        print ("\n****************************************\n");
        unless (null failures)
            (fn _ => print ("Failed tests include: " ^ (join ", " (mapPartial #name failures)) ^ "\n\n"));
        print ("TOTAL PASSED: " ^ (Int.toString (length successes)) ^ "\n");
        print ("TOTAL FAILED: " ^ (Int.toString (length failures)) ^ "\n");
        print ("****************************************\n");
        if null failures then 0 else 1
    end

(* **********************************************************************************************
 * SYNTAX
 * ********************************************************************************************** *)

fun isHeader (s : string) : bool =
    String.isPrefix "%%" (stripLeft s)

fun isLink (s : string) : bool =
    String.isPrefix "@" (stripLeft s)

fun isComment (s : string) : bool =
    String.isPrefix "//" (stripLeft s)

fun isBlank (s : string) : bool =
    ((String.size (strip s)) = 0)

val stages = [("parse", Parse),
              ("verify", Verify),
              ("verifyeval", VerifyEval),
              ("eval", Eval)]

val parseArgMeanings = [("parse", true), ("fail", false)]
val verifyArgMeanings = [("pass", true), ("fail", false)]
(* TODO: do these too *)
val verifyEvalArgMeanings = []
val evalArgMeanings = []

(* **********************************************************************************************
 * READER
 * ********************************************************************************************** *)

fun readHeader (s : string) : LINE =
    let val contents = String.extract ((strip s), 3, NONE)
    in
        if (String.isPrefix "[" contents) andalso (String.isSubstring "]" contents) then
            case split [#"]"] (String.extract (contents, 1, NONE)) of
                 [] => Header { name=NONE, tokens=[] } (* can't happen *)
               | (name::pieces) =>
                     let val paramSrc = join "]" pieces
                     in
                         Header { name=SOME name,
                                  tokens=map strip (split [#","] paramSrc) }
                     end
        else
            Header { name=NONE,
                     tokens=map strip (split [#","] contents) }
    end

fun readLine (s : string) : LINE option =
    if (isBlank s) orelse (isComment s) then
        NONE
    else if (isHeader s) then
        SOME (readHeader s)
    else if (isLink s) then
        SOME (Link (String.extract ((strip s), 1, NONE)))
    else
        SOME (SourceLine s)

fun nextLine (input : instream, lineNum : int) : (LINE * int) option =
(
    case (inputLine input) of
         NONE => NONE
       | SOME s => (case (readLine s) of
                         NONE => nextLine (input, lineNum + 1)
                       | SOME x => SOME (x, lineNum + 1))
)

(* **********************************************************************************************
 * PARSER
 * ********************************************************************************************** *)

fun checkNull (ls : string list) (lineNum : int) : unit =
    case ls of
         [] => ()
       | (s::_) =>
             raise (BadTestParameter ("extra parameter \"" ^ s ^ "\"", lineNum))

fun lookup (meanings : 'a DICT, s : string, lineNum : int) : 'a =
    case find (fn (s', x) => (toLower s) = (toLower s')) meanings of
         NONE => raise (BadTestParameter (s, lineNum))
       | SOME (_, x) => x

fun parseHeader (header : HEADER)
                (lineNum : int)
    : (string option * STAGE * bool) =
    case header of
         { name, tokens=[] } => (name, Eval, true)
       | { name, tokens=[s1] } => (name, lookup (stages, s1, lineNum), true)
       | { name, tokens=(s1::(s2::ls)) } =>
             let val stage = lookup (stages, s1, lineNum)
                 val argMeanings = case stage of
                                        Parse => parseArgMeanings
                                      | Verify => verifyArgMeanings
                                      | VerifyEval => verifyEvalArgMeanings
                                      | Eval => evalArgMeanings
                 val arg = lookup (argMeanings, s2, lineNum)
             in
                 checkNull ls lineNum;
                 (name, stage, arg)
             end

fun parseTestCase (h : HEADER)
                  (source : INPUT_SOURCE)
                  (lineNum : int)
    : TEST_CASE =
    let val (name, stage, arg) = parseHeader h lineNum
    in
        { name=name, stage=stage, arg=arg, source=source }
    end

fun parseScript (filename : string) : TEST_CASE list =
    let
        val input = openIn filename

        fun parse (lineNum : int) : TEST_CASE list =
        (
            case nextLine (input, lineNum) of
                 NONE => []
               | SOME ((Header h), lineNum') => parseTests h lineNum' (fn (x, _) => x)
               | SOME (_, lineNum') => (warn "ignoring line" filename (lineNum' - 1);
                                        parse lineNum')
        )

        and parseTests (thisHeader : HEADER)
                       (lineNum : int)
                       (k : (TEST_CASE list * int) -> TEST_CASE list)
            : TEST_CASE list =
        (
            parseContents []
                          lineNum
                          (fn NONE => k ([], lineNum)
                            | SOME (source, nextHeader, lineNum') =>
                                  let val test1 = parseTestCase thisHeader
                                                                source
                                                                (lineNum - 1)
                                  in
                                      case nextHeader of
                                           NONE => k ([test1], lineNum')
                                         | SOME header =>
                                               parseTests header
                                                          lineNum'
                                                          (fn (tests, lineNum'') =>
                                                              k (test1::tests, lineNum''))
                                  end)
        )

        and parseContents (accum : string list)
                          (lineNum : int)
                          (k : (INPUT_SOURCE * HEADER option * int) option -> TEST_CASE list)
            : TEST_CASE list =
        (
            case nextLine (input, lineNum) of
                 NONE => if null accum then
                             k NONE
                         else
                             k (SOME (Raw (map Ustring.fromSource (rev accum)), NONE, lineNum))
               | SOME (SourceLine s, lineNum') => parseContents (s::accum) lineNum' k
               | SOME (Link s, lineNum') =>
                 if null accum then
                     finishLink s lineNum' k
                 else
                     raise (MixedContent (lineNum' - 1))
               | SOME (Header h, lineNum') => k (SOME (Raw (map Ustring.fromSource (rev accum)), SOME h, lineNum'))
        )

        and finishLink (link : string)
                       (lineNum : int)
                       (k : (INPUT_SOURCE * HEADER option * int) option -> TEST_CASE list)
            : TEST_CASE list =
        (
            case nextLine (input, lineNum) of
                 NONE => k (SOME (File link, NONE, lineNum))
               | SOME (SourceLine s, lineNum') =>
                     raise (MixedContent (lineNum' - 1))
               | SOME (Link s, lineNum') =>
                     raise (ExtraLink (lineNum' - 1))
               | SOME (Header h, lineNum') =>
                     k (SOME (File link, SOME h, lineNum'))
        )
    in
        parse 1
        handle e => (closeIn input; raise e)
    end

fun parse (source : INPUT_SOURCE) : Ast.FRAGMENT =
    case source of
         Raw s => Parser.parseLines s
       | File f => Parser.parseFile f

(* **********************************************************************************************
 * TEST RUNNER
 * ********************************************************************************************** *)

fun runTestCase (regs:Mach.REGS) (test : TEST_CASE) : TEST_RESULT =
(
    logTestCase (#name test);
    case test of

	{ name, stage=Parse, arg, source } =>
	 (let
	      val frag = parse source
	  in
	      (test, if arg then true else false)
	  end
	  handle LogErr.ParseError _ => (test, if arg then false else true) 
	       | e => (unexpectedExn e; (test, false)))

       | { name, stage=Verify, arg, source } =>
	 (let
	      val frag = parse source
	      val (prog, frag) = Defn.defTopFragment (#prog regs) frag
	      val frag = Verify.verifyTopFragment prog true frag
	  in
	      (test, if arg then true else false)
	  end
	  handle LogErr.VerifyError _ => (test, if arg then false else true) 
	       | e => (unexpectedExn e; (test, false)))

       | { name, stage=Eval, arg, source } =>
	 (let
	      val frag = parse source
	      val (prog, frag) = Defn.defTopFragment (#prog regs) frag
	      val frag = Verify.verifyTopFragment prog true frag
	      val res = Eval.evalTopFragment regs frag
	  in
	      (test, if arg then true else false)
	  end
	  handle LogErr.EvalError _ => (test, if arg then false else true) 
	       | e => (unexpectedExn e; (test, false)))
)

fun run (filename : string) : TEST_RESULT list =
    let
	val regs = Boot.boot ()
    in
	map (runTestCase regs) (parseScript filename)
	handle e as (BadTestParameter (s, lineNum)) =>
               (error ("bad test parameter (" ^ s ^ ")") filename lineNum;
		raise e)
             | e as (MixedContent lineNum) =>
               (error ("combined external and inline source") filename lineNum;
		raise e)
             | e as (ExtraLink lineNum) =>
               (error ("multiple external sources") filename lineNum;
		raise e)
    end

fun exitCode (b : bool) : int = if b then 0 else 1

fun consumeTraceOption (opt:string) : bool =
    case opt of
        "-Tlex" => (Lexer.doTrace := true; false)
      | "-Tparse" => (Parser.doTrace := true; false)
      | "-Tname" => (Multiname.doTrace := true; false)
      | "-Tdefn" => (Defn.doTrace := true; false)
      | "-Teval" => (Eval.doTrace := true; false)
      | "-Tmach" => (Mach.doTrace := true; false)
      | "-Tdecimal" => (Decimal.doTrace := true; false)
      | "-Tnative" => (Native.doTrace := true; false)
      | "-Tboot" => (Boot.doTrace := true; false)
      | _ => true

fun main (arg0 : string, args : string list) : int =
    let
	val nonTraceArgs = List.filter consumeTraceOption args
    in
	BackTrace.monitor (fn _ => (report (concat (map run nonTraceArgs)))
                              handle (BadTestParameter _ | MixedContent _ | ExtraLink _) => 1
				   | e => (unexpectedExn e; 1))
    end

end
