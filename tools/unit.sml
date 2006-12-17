structure UnitTests =
struct

open TextIO StringUtils ListUtils List

datatype STAGE = Parse
               | Verify
               | VerifyEval
               | Eval

datatype INPUT_SOURCE = Raw of string list
                      | File of string

type TEST_CASE = { name: string option,
                   stage: STAGE,
                   arg: bool,
                   source: INPUT_SOURCE }

type TEST_RESULT = TEST_CASE * bool

datatype LINE = Header of string option * string list
              | SourceLine of string
              | Link of string

type LOC = { filename: string, line: int }

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

fun logTestCase name =
    (
        print "\n****************************************\n";
        print "*** TEST CASE";
        (case name of
              NONE => ()
            | SOME s => print (" [" ^ s ^ "]"));
        print "\n****************************************\n\n"
    )

fun report (results : TEST_RESULT list) : int =
    let val successes = List.map #1 (filter (fn (_, b) => b) results)
        val failures = List.map #1 (filter (fn (_, b) => not b) results)
    in
        print ("\n****************************************\n");
        print ("Failed tests include: " ^ (join ", " (List.mapPartial #name failures)) ^ "\n\n");
        print ("TOTAL PASSED: " ^ (Int.toString (length successes)) ^ "\n");
        print ("TOTAL FAILED: " ^ (Int.toString (length failures)) ^ "\n");
        print ("****************************************\n");
        if (length failures) = 0 then 0 else 1
    end

(* **********************************************************************************************
 * SYNTAX
 * ********************************************************************************************** *)

fun isHeader s = String.isPrefix "%%" (stripLeft s)

fun isLink s = String.isPrefix "@" (stripLeft s)

fun isComment s = String.isPrefix "//" (stripLeft s)

fun isBlank s = ((String.size (strip s)) = 0)

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
                 [] => Header (NONE, []) (* can't happen *)
               | (name::pieces) =>
                     let val paramSrc = join "]" pieces
                     in
                         Header (SOME name, map strip (split [#","] paramSrc))
                     end
        else
            Header (NONE, map strip (split [#","] contents))
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

fun checkNull [] lineNum = ()
  | checkNull (s::ss) lineNum = raise (BadTestParameter ("extra parameter \"" ^ s ^ "\"", lineNum))

fun interp meanings s lineNum =
    case List.find (fn (s', x) => (toLower s) = (toLower s')) meanings of
         NONE => raise (BadTestParameter (s, lineNum))
       | SOME (_, x) => x

fun parseHeader (name, []) lineNum = (name, Eval, true)
  | parseHeader (name, [s1]) lineNum = (name, interp stages s1 lineNum, true)
  | parseHeader (name, (s1::(s2::ls))) lineNum =
        let val stage = interp stages s1 lineNum
            val argMeanings = case stage of
                                   Parse => parseArgMeanings
                                 | Verify => verifyArgMeanings
                                 | VerifyEval => verifyEvalArgMeanings
                                 | Eval => evalArgMeanings
            val arg = interp argMeanings s2 lineNum
        in
            checkNull ls lineNum;
            (name, stage, arg)
        end

fun parseTestCase h source lineNum =
    let val (name, stage, arg) = parseHeader h lineNum
    in
        { name=name, stage=stage, arg=arg, source=source }
    end

fun parseScript filename =
    let val input = openIn filename
        fun parse lineNum =
            (case nextLine (input, lineNum) of
                  NONE => []
                | SOME ((Header h), lineNum') => parseTests h lineNum' (fn (x, _) => x)
                | SOME (_, lineNum') => (warn "ignoring line" filename (lineNum' - 1);
                                         parse lineNum'))
        and parseTests thisHeader lineNum k =
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
        and parseContents accum lineNum k =
            (case nextLine (input, lineNum) of
                  NONE => if null accum then
                              k NONE
                          else
                              k (SOME (Raw (rev accum), NONE, lineNum))
                | SOME (SourceLine s, lineNum') => parseContents (s::accum) lineNum' k
                | SOME (Link s, lineNum') =>
                  if null accum then
                      finishLink s lineNum' k
                  else
                      raise (MixedContent (lineNum' - 1))
                | SOME (Header h, lineNum') => k (SOME (Raw (rev accum), SOME h, lineNum')))
        and finishLink link lineNum k =
            (case nextLine (input, lineNum) of
                  NONE => k (SOME (File link, NONE, lineNum))
                | SOME (SourceLine s, lineNum') =>
                      raise (MixedContent (lineNum' - 1))
                | SOME (Link s, lineNum') =>
                      raise (ExtraLink (lineNum' - 1))
                | SOME (Header h, lineNum') =>
                      k (SOME (File link, SOME h, lineNum')))
    in
        parse 1
        handle e => (closeIn input; raise e)
    end

fun parse (Raw s) = Parser.parseLines s
  | parse (File f) = Parser.parseFile f

(* **********************************************************************************************
 * TEST RUNNER
 * ********************************************************************************************** *)

fun runTestCase (test as { name, stage=Parse, arg=true, source=source }) =
        ((logTestCase name; (parse source); (test, true))
         handle _ => (test, false))
  | runTestCase (test as { name, stage=Parse, arg=false, source }) =
        ((logTestCase name; (parse source); (test, false))
         handle Parser.ParseError => (test, true)
              | _ => (test, false))
  | runTestCase (test as { name, stage=Verify, arg=true, source }) =
        ((logTestCase name; (TypeChk.tcProgram (parse source); (test, true)))
         handle _ => (test, false))
  | runTestCase (test as { name, stage=Verify, arg=false, source }) =
        ((logTestCase name; (TypeChk.tcProgram (parse source); (test, false)))
         handle TypeChk.IllTypedException _ => (test, true)
              | _ => (test, false))

fun run (filename : string) : TEST_RESULT list =
    (map runTestCase (parseScript filename))
    handle e as (BadTestParameter (s, lineNum)) =>
           (error ("bad test parameter (" ^ s ^ ")") filename lineNum;
            raise e)
         | e as (MixedContent lineNum) =>
           (error ("combined external and inline source") filename lineNum;
            raise e)
         | e as (ExtraLink lineNum) =>
           (error ("multiple external sources") filename lineNum;
            raise e)

fun exitCode (b:bool) = if b then 0 else 1

fun main (arg0:string, args:string list) =
    BackTrace.monitor (fn _ => (report (concat (map run args)))
                               handle (BadTestParameter _ | MixedContent _ | ExtraLink _) => 1)

end
