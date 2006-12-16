structure UnitTests =
struct

open TextIO StringUtils ListUtils List

datatype TEST_PARAMS = Parse of bool
                     | Verify of bool
                     | VerifyEval of bool
                     | Eval of bool

type TEST_CASE = string option * TEST_PARAMS * (string list)

type TEST_RESULT = TEST_CASE * bool

datatype LINE = Header of string option * string list
              | SourceLine of string

exception BadTestParameter of string

fun isHeader s = String.isPrefix "%%" (stripLeft s)

fun isComment s = String.isPrefix "//" (stripLeft s)

fun isBlank s = ((String.size (strip s)) = 0)

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
    else
        SOME (SourceLine s)

fun nextLine (input : instream) : LINE option =
(
    case (inputLine input) of
         NONE => NONE
       | SOME s => (case (readLine s) of
                         NONE => nextLine input
                       | x => x)
)

fun checkNull [] = ()
  | checkNull (s::ss) = raise (BadTestParameter (s ^ " (extra parameter)"))

fun parseArg [] arg = raise (BadTestParameter arg)
  | parseArg ((s, v)::map) arg = if ((toLower arg) = (toLower s)) then v else parseArg map arg

fun parseParse [] = Parse true
  | parseParse (s::ss) = let val result = parseArg [("pass", Parse true), ("fail", Parse false)] s
                         in
                             checkNull ss;
                             result
                         end

fun parseVerify [] = Verify true
  | parseVerify (s::ss) = let val result = parseArg [("pass", Verify true), ("fail", Verify false)] s
                          in
                              checkNull ss;
                              result
                          end

(* TODO: do these too *)
fun parseVerifyEval _ = VerifyEval true
fun parseEval _ = Eval true

fun parseHeader (name, []) = (name, Eval true)
  | parseHeader (name, (s::ls)) =
        (name, if (toLower s) = "parse" then
                   parseParse ls
               else if (toLower s) = "verify" then
                   parseVerify ls
               else if (toLower s) = "verifyeval" then
                   parseVerifyEval ls
               else if (toLower s) = "eval" then
                   parseEval ls
               else
                   raise (BadTestParameter s))

fun parseTestCase h src = let val (name, params) = parseHeader h
                          in
                              (name, params, src)
                          end

fun parseScript f =
    let val input = openIn f
        fun parse () =
            (case nextLine input of
                  NONE => []
                | SOME (SourceLine _) => parse ()
                | SOME (Header h) => parseTests h (fn x => x))
        and parseTests thisHeader k =
            parseContents [] (fn NONE => k []
                               | SOME (lines, nextHeader) =>
                                     let val test1 = parseTestCase thisHeader lines
                                     in
                                         case nextHeader of
                                              NONE => k [test1]
                                            | SOME header => parseTests header (fn tests => k (test1::tests))
                                     end)
        and parseContents accum k =
            (case nextLine input of
                  NONE => if null accum then
                              k NONE
                          else
                              k (SOME (rev accum, NONE))
                | SOME (SourceLine s) => parseContents (s::accum) k
                | SOME (Header h) => k (SOME (rev accum, SOME h)))
    in
        parse ()
        handle e => (closeIn input; raise e)
    end

fun printTest (header, lines) =
(
    print ("header: " ^ "?" ^ "\n");
    app (fn s => print ("    [" ^ s ^ "]\n")) lines
)

fun logTestCase name =
    (
        print "\n****************************************\n";
        print "*** TEST CASE";
        (case name of
              NONE => ()
            | SOME s => print (" [" ^ s ^ "]"));
        print "\n****************************************\n\n"
    )

fun runTestCase (test as (name, Parse true, src)) =
        ((logTestCase name; (Parser.parseLines src); (test, true))
         handle _ => (test, false))
  | runTestCase (test as (name, Parse false, src)) =
        ((logTestCase name; (Parser.parseLines src); (test, false))
         handle Parser.ParseError => (test, true)
              | _ => (test, false))
  | runTestCase (test as (name, Verify true, src)) =
        ((logTestCase name; (TypeChk.tcProgram (Parser.parseLines src); (test, true)))
         handle _ => (test, false))
  | runTestCase (test as (name, Verify false, src)) =
        ((logTestCase name; (TypeChk.tcProgram (Parser.parseLines src); (test, false)))
         handle TypeChk.IllTypedException _ => (test, true)
              | _ => (test, false))

fun run (filename : string) : TEST_RESULT list =
    map runTestCase (parseScript filename)

fun report (results : TEST_RESULT list) : int =
    let val successes = filter (fn (_, b) => b) results
        val failures = filter (fn (_, b) => not b) results
    in
        print ("\n****************************************\n");
        print ("PASSED TESTS: " ^ (Int.toString (length successes)) ^ "\n");
        print ("FAILED TESTS: " ^ (Int.toString (length failures)) ^ "\n");
        print ("****************************************\n");
        if (length failures) = 0 then 0 else 1
    end

fun exitCode (b:bool) = if b then 0 else 1

fun main (arg0:string, args:string list) =
    BackTrace.monitor (fn _ => (report (concat (map run args)))
                               handle BadTestParameter s => (print ("bad test parameter (\"" ^ s ^ "\")\n"); 1))

end
