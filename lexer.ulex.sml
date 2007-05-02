structure Lexer  = struct

    datatype yystart_state = 
XML | REGEXP_CHARSET | REGEXP | STRING | MULTI_LINE_COMMENT | INITIAL | SINGLE_LINE_COMMENT
    structure UserDeclarations = 
      struct

 
open Token

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[lex] " :: ss) else ()

fun error ss = LogErr.lexError ss

type lex_result = TOKEN

fun eof _ = Eof

fun chopTrailing (s:string) 
    : string = 
    String.substring (s, 0, ((String.size s) - 1))

val (found_newline : bool ref) = ref false
val (curr_quote    : char ref) = ref #"\000"
val (curr_chars    : UTF8.wchar list ref) = ref []


      end

    local
    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of ULexBuffer.stream * action * yymatch
    withtype action = ULexBuffer.stream * yymatch -> UserDeclarations.lex_result

    fun innerLex (yystrm_, yyss_, yysm) = let
        (* current start state *)
          val yyss = ref yyss_
	  fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
          val yystrm = ref yystrm_
	  fun yygetPos() = ULexBuffer.getpos (!yystrm)
        (* start position of token -- can be updated via skip() *)
	  val yystartPos = ref (yygetPos())
	(* get one char of input *)
	  fun yygetc strm = (case UTF8.getu ULexBuffer.getc strm
                of (SOME (0w10, s')) => 
		     (StreamPos.markNewLine yysm (ULexBuffer.getpos strm);
		      SOME (0w10, s'))
		 | x => x)
          fun yygetList getc strm = let
            val get1 = UTF8.getu getc
            fun iter (strm, accum) = 
	        (case get1 strm
	          of NONE => rev accum
	           | SOME (w, strm') => iter (strm', w::accum)
	         (* end case *))
          in
            iter (strm, [])
          end
	(* create yytext *)
	  fun yymksubstr(strm) = ULexBuffer.subtract (strm, !yystrm)
	  fun yymktext(strm) = Substring.string (yymksubstr strm)
	  fun yymkunicode(strm) = yygetList Substring.getc (yymksubstr strm)
          open UserDeclarations
          fun lex () = let
            fun yystuck (yyNO_MATCH) = raise Fail "lexer reached a stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yygetPos()
	    fun yygetlineNo strm = StreamPos.lineNo yysm (ULexBuffer.getpos strm)
	    fun yygetcolNo  strm = StreamPos.colNo  yysm (ULexBuffer.getpos strm)
	    fun continue() = 
let
fun yyAction0 (strm, lastMatch) = (yystrm := strm;  Eol)
fun yyAction1 (strm, lastMatch) = (yystrm := strm;  Minus)
fun yyAction2 (strm, lastMatch) = (yystrm := strm;  MinusMinus)
fun yyAction3 (strm, lastMatch) = (yystrm := strm;  Not)
fun yyAction4 (strm, lastMatch) = (yystrm := strm;  NotEquals)
fun yyAction5 (strm, lastMatch) = (yystrm := strm;  StrictNotEquals)
fun yyAction6 (strm, lastMatch) = (yystrm := strm;  Modulus)
fun yyAction7 (strm, lastMatch) = (yystrm := strm;  ModulusAssign)
fun yyAction8 (strm, lastMatch) = (yystrm := strm;  BitwiseAnd)
fun yyAction9 (strm, lastMatch) = (yystrm := strm;  LogicalAnd)
fun yyAction10 (strm, lastMatch) = (yystrm := strm;  LogicalAndAssign)
fun yyAction11 (strm, lastMatch) = (yystrm := strm;  BitwiseAndAssign)
fun yyAction12 (strm, lastMatch) = (yystrm := strm;  LeftParen)
fun yyAction13 (strm, lastMatch) = (yystrm := strm;  RightParen)
fun yyAction14 (strm, lastMatch) = (yystrm := strm;  Mult)
fun yyAction15 (strm, lastMatch) = (yystrm := strm;  MultAssign)
fun yyAction16 (strm, lastMatch) = (yystrm := strm;  Comma)
fun yyAction17 (strm, lastMatch) = (yystrm := strm;  Dot)
fun yyAction18 (strm, lastMatch) = (yystrm := strm;  DoubleDot)
fun yyAction19 (strm, lastMatch) = (yystrm := strm;  TripleDot)
fun yyAction20 (strm, lastMatch) = (yystrm := strm;  LeftDotAngle)
fun yyAction21 (strm, lastMatch) = (yystrm := strm;
       LexBreakDiv       { lex_initial = fn _ => [], lex_regexp = fn _ => [] })
fun yyAction22 (strm, lastMatch) = (yystrm := strm;
       LexBreakDivAssign { lex_initial = fn _ => [], lex_regexp = fn _ => [] })
fun yyAction23 (strm, lastMatch) = (yystrm := strm;  Colon)
fun yyAction24 (strm, lastMatch) = (yystrm := strm;  DoubleColon)
fun yyAction25 (strm, lastMatch) = (yystrm := strm;  SemiColon)
fun yyAction26 (strm, lastMatch) = (yystrm := strm;  QuestionMark)
fun yyAction27 (strm, lastMatch) = (yystrm := strm;  At)
fun yyAction28 (strm, lastMatch) = (yystrm := strm;  LeftBracket)
fun yyAction29 (strm, lastMatch) = (yystrm := strm;  RightBracket)
fun yyAction30 (strm, lastMatch) = (yystrm := strm;  BitwiseXor)
fun yyAction31 (strm, lastMatch) = (yystrm := strm;  BitwiseXorAssign)
fun yyAction32 (strm, lastMatch) = (yystrm := strm;  LeftBrace)
fun yyAction33 (strm, lastMatch) = (yystrm := strm;  BitwiseOr)
fun yyAction34 (strm, lastMatch) = (yystrm := strm;  LogicalOr)
fun yyAction35 (strm, lastMatch) = (yystrm := strm;  LogicalOrAssign)
fun yyAction36 (strm, lastMatch) = (yystrm := strm;  BitwiseOrAssign)
fun yyAction37 (strm, lastMatch) = (yystrm := strm;  RightBrace)
fun yyAction38 (strm, lastMatch) = (yystrm := strm;  BitwiseNot)
fun yyAction39 (strm, lastMatch) = (yystrm := strm;  Plus)
fun yyAction40 (strm, lastMatch) = (yystrm := strm;  PlusPlus)
fun yyAction41 (strm, lastMatch) = (yystrm := strm;  PlusAssign)
fun yyAction42 (strm, lastMatch) = (yystrm := strm;
       LexBreakLessThan { lex_initial = fn _ => [], lex_xml = fn _ => [] })
fun yyAction43 (strm, lastMatch) = (yystrm := strm;  LeftShift)
fun yyAction44 (strm, lastMatch) = (yystrm := strm;  LeftShiftAssign)
fun yyAction45 (strm, lastMatch) = (yystrm := strm;  LessThanOrEquals)
fun yyAction46 (strm, lastMatch) = (yystrm := strm;  Assign)
fun yyAction47 (strm, lastMatch) = (yystrm := strm;  MinusAssign)
fun yyAction48 (strm, lastMatch) = (yystrm := strm;  Equals)
fun yyAction49 (strm, lastMatch) = (yystrm := strm;  StrictEquals)
fun yyAction50 (strm, lastMatch) = (yystrm := strm;  GreaterThan)
fun yyAction51 (strm, lastMatch) = (yystrm := strm;  GreaterThanOrEquals)
fun yyAction52 (strm, lastMatch) = (yystrm := strm;  RightShift)
fun yyAction53 (strm, lastMatch) = (yystrm := strm;  RightShiftAssign)
fun yyAction54 (strm, lastMatch) = (yystrm := strm;  UnsignedRightShift)
fun yyAction55 (strm, lastMatch) = (yystrm := strm;  UnsignedRightShiftAssign)
fun yyAction56 (strm, lastMatch) = (yystrm := strm;  As)
fun yyAction57 (strm, lastMatch) = (yystrm := strm;  Break)
fun yyAction58 (strm, lastMatch) = (yystrm := strm;  Case)
fun yyAction59 (strm, lastMatch) = (yystrm := strm;  Cast)
fun yyAction60 (strm, lastMatch) = (yystrm := strm;  Catch)
fun yyAction61 (strm, lastMatch) = (yystrm := strm;  Class)
fun yyAction62 (strm, lastMatch) = (yystrm := strm;  Const)
fun yyAction63 (strm, lastMatch) = (yystrm := strm;  Continue)
fun yyAction64 (strm, lastMatch) = (yystrm := strm;  Default)
fun yyAction65 (strm, lastMatch) = (yystrm := strm;  Delete)
fun yyAction66 (strm, lastMatch) = (yystrm := strm;  Do)
fun yyAction67 (strm, lastMatch) = (yystrm := strm;  Else)
fun yyAction68 (strm, lastMatch) = (yystrm := strm;  Enum)
fun yyAction69 (strm, lastMatch) = (yystrm := strm;  Extends)
fun yyAction70 (strm, lastMatch) = (yystrm := strm;  False)
fun yyAction71 (strm, lastMatch) = (yystrm := strm;  Finally)
fun yyAction72 (strm, lastMatch) = (yystrm := strm;  For)
fun yyAction73 (strm, lastMatch) = (yystrm := strm;  Function)
fun yyAction74 (strm, lastMatch) = (yystrm := strm;  If)
fun yyAction75 (strm, lastMatch) = (yystrm := strm;  Implements)
fun yyAction76 (strm, lastMatch) = (yystrm := strm;  Import)
fun yyAction77 (strm, lastMatch) = (yystrm := strm;  In)
fun yyAction78 (strm, lastMatch) = (yystrm := strm;  InstanceOf)
fun yyAction79 (strm, lastMatch) = (yystrm := strm;  Interface)
fun yyAction80 (strm, lastMatch) = (yystrm := strm;  Internal)
fun yyAction81 (strm, lastMatch) = (yystrm := strm;  Intrinsic)
fun yyAction82 (strm, lastMatch) = (yystrm := strm;  Is)
fun yyAction83 (strm, lastMatch) = (yystrm := strm;  Let)
fun yyAction84 (strm, lastMatch) = (yystrm := strm;  New)
fun yyAction85 (strm, lastMatch) = (yystrm := strm;  Null)
fun yyAction86 (strm, lastMatch) = (yystrm := strm;  Package)
fun yyAction87 (strm, lastMatch) = (yystrm := strm;  Precision)
fun yyAction88 (strm, lastMatch) = (yystrm := strm;  Private)
fun yyAction89 (strm, lastMatch) = (yystrm := strm;  Protected)
fun yyAction90 (strm, lastMatch) = (yystrm := strm;  Public)
fun yyAction91 (strm, lastMatch) = (yystrm := strm;  Return)
fun yyAction92 (strm, lastMatch) = (yystrm := strm;  Super)
fun yyAction93 (strm, lastMatch) = (yystrm := strm;  Switch)
fun yyAction94 (strm, lastMatch) = (yystrm := strm;  This)
fun yyAction95 (strm, lastMatch) = (yystrm := strm;  Throw)
fun yyAction96 (strm, lastMatch) = (yystrm := strm;  To)
fun yyAction97 (strm, lastMatch) = (yystrm := strm;  True)
fun yyAction98 (strm, lastMatch) = (yystrm := strm;  Try)
fun yyAction99 (strm, lastMatch) = (yystrm := strm;  TypeOf)
fun yyAction100 (strm, lastMatch) = (yystrm := strm;  Use)
fun yyAction101 (strm, lastMatch) = (yystrm := strm;  Var)
fun yyAction102 (strm, lastMatch) = (yystrm := strm;  Void)
fun yyAction103 (strm, lastMatch) = (yystrm := strm;  While)
fun yyAction104 (strm, lastMatch) = (yystrm := strm;  With)
fun yyAction105 (strm, lastMatch) = (yystrm := strm;  Call)
fun yyAction106 (strm, lastMatch) = (yystrm := strm;  Debugger)
fun yyAction107 (strm, lastMatch) = (yystrm := strm;  Decimal)
fun yyAction108 (strm, lastMatch) = (yystrm := strm;  Double)
fun yyAction109 (strm, lastMatch) = (yystrm := strm;  Dynamic)
fun yyAction110 (strm, lastMatch) = (yystrm := strm;  Each)
fun yyAction111 (strm, lastMatch) = (yystrm := strm;  Final)
fun yyAction112 (strm, lastMatch) = (yystrm := strm;  Get)
fun yyAction113 (strm, lastMatch) = (yystrm := strm;  Goto)
fun yyAction114 (strm, lastMatch) = (yystrm := strm;  Has)
fun yyAction115 (strm, lastMatch) = (yystrm := strm;  Include)
fun yyAction116 (strm, lastMatch) = (yystrm := strm;  Int)
fun yyAction117 (strm, lastMatch) = (yystrm := strm;  Namespace)
fun yyAction118 (strm, lastMatch) = (yystrm := strm;  Native)
fun yyAction119 (strm, lastMatch) = (yystrm := strm;  Number)
fun yyAction120 (strm, lastMatch) = (yystrm := strm;  Override)
fun yyAction121 (strm, lastMatch) = (yystrm := strm;  Prototype)
fun yyAction122 (strm, lastMatch) = (yystrm := strm;  Rounding)
fun yyAction123 (strm, lastMatch) = (yystrm := strm;  Standard)
fun yyAction124 (strm, lastMatch) = (yystrm := strm;  Strict)
fun yyAction125 (strm, lastMatch) = (yystrm := strm;  UInt)
fun yyAction126 (strm, lastMatch) = (yystrm := strm;  Set)
fun yyAction127 (strm, lastMatch) = (yystrm := strm;  Static)
fun yyAction128 (strm, lastMatch) = (yystrm := strm;  Type)
fun yyAction129 (strm, lastMatch) = (yystrm := strm;  Undefined)
fun yyAction130 (strm, lastMatch) = (yystrm := strm;  Token.Xml)
fun yyAction131 (strm, lastMatch) = (yystrm := strm;  Yield)
fun yyAction132 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction133 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;  Identifier (Vector.fromList yyunicode)
      end
fun yyAction134 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         case Int32.fromString (chopTrailing yytext) of
					  SOME i => ExplicitIntLiteral i
					| NONE => error ["unexpected input in {explicitIntLiteral}: '", yytext, "'"]
      end
fun yyAction135 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         case LargeInt.fromString (chopTrailing yytext) of
					  SOME i => ExplicitUIntLiteral (Word32.fromLargeInt i)
					| NONE => error ["unexpected input in {explicitUIntDecLiteral}: '", yytext, "'"]
      end
fun yyAction136 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         case Word32.fromString (chopTrailing yytext) of
					  SOME i => ExplicitUIntLiteral i
					| NONE => error ["unexpected input in {explicitUIntHexLiteral}: '", yytext, "'"]
      end
fun yyAction137 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         case Real64.fromString (chopTrailing yytext) of
					  SOME i => ExplicitDoubleLiteral i
					| NONE => error ["unexpected input in {explicitDoubleLiteral}: '", yytext, "'"]
      end
fun yyAction138 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         case Decimal.fromStringDefault (chopTrailing yytext) of
					  SOME i => ExplicitDecimalLiteral i
					| NONE => error ["unexpected input in {explicitDecimalLiteral}: '", yytext, "'"]
      end
fun yyAction139 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  DecimalIntegerLiteral yytext
      end
fun yyAction140 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  HexIntegerLiteral yytext
      end
fun yyAction141 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;  DecimalLiteral yytext
      end
fun yyAction142 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN SINGLE_LINE_COMMENT; continue())
fun yyAction143 (strm, lastMatch) = (yystrm := strm;  YYBEGIN INITIAL; Eol)
fun yyAction144 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction145 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN MULTI_LINE_COMMENT; continue())
fun yyAction146 (strm, lastMatch) = (yystrm := strm;
       YYBEGIN INITIAL; continue())
fun yyAction147 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction148 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction149 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         let
				    val x_flag = String.isSubstring "x" yytext;
				    val re = rev (!curr_chars) @ yyunicode
				in
				    if !found_newline andalso (not x_flag)
				    then error ["Illegal newline in regexp"]
				    else
				       (curr_chars := [];
					found_newline := false;
					YYBEGIN INITIAL;
					RegexpLiteral (Vector.fromList re))
				end
      end
fun yyAction150 (strm, lastMatch) = (yystrm := strm;
       curr_chars := (UTF8.fromAscii #"[") :: !curr_chars;
				YYBEGIN REGEXP_CHARSET;
				continue())
fun yyAction151 (strm, lastMatch) = (yystrm := strm;
       found_newline := true; continue())
fun yyAction152 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction153 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;
         curr_chars := List.nth(yyunicode,1) :: (UTF8.fromAscii #"\\") :: !curr_chars;
				continue()
      end
fun yyAction154 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;
         curr_chars := List.nth(yyunicode,0) :: !curr_chars;
				continue()
      end
fun yyAction155 (strm, lastMatch) = (yystrm := strm;
       curr_chars := (UTF8.fromAscii #"]") :: !curr_chars;
				YYBEGIN REGEXP;
				continue())
fun yyAction156 (strm, lastMatch) = (yystrm := strm;
       found_newline := true; continue())
fun yyAction157 (strm, lastMatch) = (yystrm := strm;  continue())
fun yyAction158 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;
         curr_chars := List.nth(yyunicode,1) :: (UTF8.fromAscii #"\\") :: !curr_chars;
				continue()
      end
fun yyAction159 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;
         curr_chars := List.nth(yyunicode,0) :: !curr_chars;
				continue()
      end
fun yyAction160 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         curr_quote := String.sub (yytext,0); 
				curr_chars := [];
				YYBEGIN STRING;
				continue()
      end
fun yyAction161 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         if 
				    (!curr_quote) = String.sub (yytext,0)
				then 
				    let 
					val str = rev (!curr_chars)
					(* val str_span = *)
				    in
					curr_quote := #"\000";
					curr_chars := [];
					YYBEGIN INITIAL;
					(* (StringLiteral str, str_span) *)
					StringLiteral (Vector.fromList str)
				    end
				else
				    (curr_chars := (hd yyunicode) :: (!curr_chars);
				    continue())
      end
fun yyAction162 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (case Char.fromCString yytext of
				    NONE => error ["unexpected input in <STRING>{charEscape}: '", yytext, "'"]
				  | SOME c => curr_chars := (UTF8.fromAscii c) :: (!curr_chars));
				continue()
      end
fun yyAction163 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;
         curr_chars := (List.nth(yyunicode,1)) :: (!curr_chars);
				continue()
      end
fun yyAction164 (strm, lastMatch) = let
      val yyunicode = yymkunicode(strm)
      in
        yystrm := strm;
         curr_chars := (List.nth(yyunicode,0)) :: (!curr_chars);
				continue()
      end
fun yyAction165 (strm, lastMatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
         error [
					"unexpected input: '",
					yytext,
					"' -- char code: ",
					Int.toString (Char.ord (String.sub (yytext,0)))
				      ]
      end
fun yyQ429 (strm, lastMatch) = yyAction143(strm, yyNO_MATCH)
fun yyQ430 (strm, lastMatch) = yyAction144(strm, yyNO_MATCH)
fun yyQ6 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxA
              then yyQ429(strm', lastMatch)
              else yyQ430(strm', lastMatch)
      (* end case *))
fun yyQ35 (strm, lastMatch) = yyAction0(strm, yyNO_MATCH)
fun yyQ427 (strm, lastMatch) = yyAction2(strm, yyNO_MATCH)
fun yyQ428 (strm, lastMatch) = yyAction47(strm, yyNO_MATCH)
fun yyQ36 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yyAction1(strm, yyNO_MATCH)
            else if inp < 0wx2E
              then if inp = 0wx2D
                  then yyQ427(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = 0wx3D
              then yyQ428(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ426 (strm, lastMatch) = yyAction5(strm, yyNO_MATCH)
fun yyQ425 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ426(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ425(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ424 (strm, lastMatch) = yyAction7(strm, yyNO_MATCH)
fun yyQ38 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ424(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ423 (strm, lastMatch) = yyAction10(strm, yyNO_MATCH)
fun yyQ421 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ423(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ422 (strm, lastMatch) = yyAction11(strm, yyNO_MATCH)
fun yyQ39 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx27
              then yyAction8(strm, yyNO_MATCH)
            else if inp < 0wx27
              then if inp = 0wx26
                  then yyQ421(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = 0wx3D
              then yyQ422(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch) = yyAction12(strm, yyNO_MATCH)
fun yyQ41 (strm, lastMatch) = yyAction13(strm, yyNO_MATCH)
fun yyQ420 (strm, lastMatch) = yyAction15(strm, yyNO_MATCH)
fun yyQ42 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ420(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch) = yyAction16(strm, yyNO_MATCH)
fun yyQ419 (strm, lastMatch) = yyAction19(strm, yyNO_MATCH)
fun yyQ417 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2E
              then yyQ419(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ418 (strm, lastMatch) = yyAction20(strm, yyNO_MATCH)
fun yyQ386 (strm, lastMatch) = yyAction137(strm, yyNO_MATCH)
fun yyQ387 (strm, lastMatch) = yyAction138(strm, yyNO_MATCH)
fun yyQ396 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction141(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx64
              then yyQ386(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx30
                  then yyQ396(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction141(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ396(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
                  else yyAction141(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ387(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
              else yyAction141(strm, yyNO_MATCH)
      (* end case *))
fun yyQ395 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ396(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ396(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ388 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2D
              then yyQ395(strm', lastMatch)
            else if inp < 0wx2D
              then if inp = 0wx2B
                  then yyQ395(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx30
              then yyQ396(strm', lastMatch)
            else if inp < 0wx30
              then yystuck(lastMatch)
            else if inp <= 0wx39
              then yyQ396(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ389 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction141(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx64
              then yyQ386(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx3A
                  then yyAction141(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction141(strm, yyNO_MATCH)
                      else yyQ389(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
                else if inp = 0wx45
                  then yyQ388(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
                  else yyAction141(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ387(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx65
                  then yyQ388(strm', yyMATCH(strm, yyAction141, yyNO_MATCH))
                  else yyAction141(strm, yyNO_MATCH)
              else yyAction141(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ389(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx30
              then if inp = 0wx2E
                  then yyQ417(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = 0wx3C
              then yyQ418(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < 0wx3C
              then if inp <= 0wx39
                  then yyQ389(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ414 (strm, lastMatch) = yyAction22(strm, yyNO_MATCH)
fun yyQ415 (strm, lastMatch) = yyAction142(strm, yyNO_MATCH)
fun yyQ416 (strm, lastMatch) = yyAction145(strm, yyNO_MATCH)
fun yyQ45 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ415(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < 0wx2F
              then if inp = 0wx2A
                  then yyQ416(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = 0wx3D
              then yyQ414(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ413 (strm, lastMatch) = yyAction24(strm, yyNO_MATCH)
fun yyQ46 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3A
              then yyQ413(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch) = yyAction25(strm, yyNO_MATCH)
fun yyQ48 (strm, lastMatch) = yyAction26(strm, yyNO_MATCH)
fun yyQ49 (strm, lastMatch) = yyAction27(strm, yyNO_MATCH)
fun yyQ50 (strm, lastMatch) = yyAction28(strm, yyNO_MATCH)
fun yyQ51 (strm, lastMatch) = yyAction29(strm, yyNO_MATCH)
fun yyQ412 (strm, lastMatch) = yyAction31(strm, yyNO_MATCH)
fun yyQ52 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ412(strm', yyMATCH(strm, yyAction30, yyNO_MATCH))
              else yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch) = yyAction32(strm, yyNO_MATCH)
fun yyQ411 (strm, lastMatch) = yyAction35(strm, yyNO_MATCH)
fun yyQ409 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ411(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ410 (strm, lastMatch) = yyAction36(strm, yyNO_MATCH)
fun yyQ54 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyAction33(strm, yyNO_MATCH)
            else if inp < 0wx3E
              then if inp = 0wx3D
                  then yyQ410(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp = 0wx7C
              then yyQ409(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch) = yyAction37(strm, yyNO_MATCH)
fun yyQ56 (strm, lastMatch) = yyAction38(strm, yyNO_MATCH)
fun yyQ407 (strm, lastMatch) = yyAction40(strm, yyNO_MATCH)
fun yyQ408 (strm, lastMatch) = yyAction41(strm, yyNO_MATCH)
fun yyQ57 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2C
              then yyAction39(strm, yyNO_MATCH)
            else if inp < 0wx2C
              then if inp = 0wx2B
                  then yyQ407(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
                  else yyAction39(strm, yyNO_MATCH)
            else if inp = 0wx3D
              then yyQ408(strm', yyMATCH(strm, yyAction39, yyNO_MATCH))
              else yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ406 (strm, lastMatch) = yyAction44(strm, yyNO_MATCH)
fun yyQ404 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ406(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ405 (strm, lastMatch) = yyAction45(strm, yyNO_MATCH)
fun yyQ58 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ405(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp < 0wx3D
              then if inp = 0wx3C
                  then yyQ404(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ403 (strm, lastMatch) = yyAction49(strm, yyNO_MATCH)
fun yyQ402 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ403(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
              else yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ402(strm', yyMATCH(strm, yyAction46, yyNO_MATCH))
              else yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ397 (strm, lastMatch) = yyAction51(strm, yyNO_MATCH)
fun yyQ399 (strm, lastMatch) = yyAction53(strm, yyNO_MATCH)
fun yyQ401 (strm, lastMatch) = yyAction55(strm, yyNO_MATCH)
fun yyQ400 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3D
              then yyQ401(strm', yyMATCH(strm, yyAction54, yyNO_MATCH))
              else yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ398 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ400(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
            else if inp < 0wx3E
              then if inp = 0wx3D
                  then yyQ399(strm', yyMATCH(strm, yyAction52, yyNO_MATCH))
                  else yyAction52(strm, yyNO_MATCH)
              else yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx3E
              then yyQ398(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
            else if inp < 0wx3E
              then if inp = 0wx3D
                  then yyQ397(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
                  else yyAction50(strm, yyNO_MATCH)
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch) = yyAction160(strm, yyNO_MATCH)
fun yyQ62 (strm, lastMatch) = yyAction165(strm, yyNO_MATCH)
fun yyQ384 (strm, lastMatch) = yyAction134(strm, yyNO_MATCH)
fun yyQ385 (strm, lastMatch) = yyAction135(strm, yyNO_MATCH)
fun yyQ391 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction139(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ388(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx3A
                  then yyAction139(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx2F
                      then yyAction139(strm, yyNO_MATCH)
                    else if inp < 0wx2F
                      then if inp = 0wx2E
                          then yyQ389(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                          else yyAction139(strm, yyNO_MATCH)
                      else yyQ391(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                else if inp = 0wx46
                  then yyAction139(strm, yyNO_MATCH)
                else if inp < 0wx46
                  then if inp = 0wx45
                      then yyQ388(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                      else yyAction139(strm, yyNO_MATCH)
                else if inp = 0wx64
                  then yyQ386(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                  else yyAction139(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ387(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx69
                  then yyQ384(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                  else yyAction139(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ385(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
              else yyAction139(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction139(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ388(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx3A
                  then yyAction139(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx2F
                      then yyAction139(strm, yyNO_MATCH)
                    else if inp < 0wx2F
                      then if inp = 0wx2E
                          then yyQ389(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                          else yyAction139(strm, yyNO_MATCH)
                      else yyQ391(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                else if inp = 0wx46
                  then yyAction139(strm, yyNO_MATCH)
                else if inp < 0wx46
                  then if inp = 0wx45
                      then yyQ388(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                      else yyAction139(strm, yyNO_MATCH)
                else if inp = 0wx64
                  then yyQ386(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                  else yyAction139(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ387(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx69
                  then yyQ384(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                  else yyAction139(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ385(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
              else yyAction139(strm, yyNO_MATCH)
      (* end case *))
fun yyQ393 (strm, lastMatch) = yyAction136(strm, yyNO_MATCH)
fun yyQ392 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction140(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ392(strm', yyMATCH(strm, yyAction140, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx3A
                  then yyAction140(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp <= 0wx2F
                      then yyAction140(strm, yyNO_MATCH)
                      else yyQ392(strm', yyMATCH(strm, yyAction140, yyNO_MATCH))
                else if inp = 0wx41
                  then yyQ392(strm', yyMATCH(strm, yyAction140, yyNO_MATCH))
                else if inp < 0wx41
                  then yyAction140(strm, yyNO_MATCH)
                else if inp <= 0wx46
                  then yyQ392(strm', yyMATCH(strm, yyAction140, yyNO_MATCH))
                  else yyAction140(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyAction140(strm, yyNO_MATCH)
            else if inp < 0wx6A
              then if inp = 0wx67
                  then yyAction140(strm, yyNO_MATCH)
                else if inp < 0wx67
                  then yyQ392(strm', yyMATCH(strm, yyAction140, yyNO_MATCH))
                else if inp = 0wx69
                  then yyQ384(strm', yyMATCH(strm, yyAction140, yyNO_MATCH))
                  else yyAction140(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ393(strm', yyMATCH(strm, yyAction140, yyNO_MATCH))
              else yyAction140(strm, yyNO_MATCH)
      (* end case *))
fun yyQ390 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ392(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ392(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ392(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ392(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ392(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ392(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ64 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction139(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx65
              then yyQ388(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx45
                  then yyQ388(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                else if inp < 0wx45
                  then if inp = 0wx2F
                      then yyAction139(strm, yyNO_MATCH)
                    else if inp < 0wx2F
                      then if inp = 0wx2E
                          then yyQ389(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                          else yyAction139(strm, yyNO_MATCH)
                    else if inp <= 0wx39
                      then yyQ391(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                      else yyAction139(strm, yyNO_MATCH)
                else if inp = 0wx59
                  then yyAction139(strm, yyNO_MATCH)
                else if inp < 0wx59
                  then if inp = 0wx58
                      then yyQ390(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                      else yyAction139(strm, yyNO_MATCH)
                else if inp = 0wx64
                  then yyQ386(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                  else yyAction139(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyAction139(strm, yyNO_MATCH)
            else if inp < 0wx6E
              then if inp = 0wx6A
                  then yyAction139(strm, yyNO_MATCH)
                else if inp < 0wx6A
                  then if inp = 0wx69
                      then yyQ384(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                      else yyAction139(strm, yyNO_MATCH)
                else if inp = 0wx6D
                  then yyQ387(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                  else yyAction139(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyAction139(strm, yyNO_MATCH)
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ385(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
                  else yyAction139(strm, yyNO_MATCH)
            else if inp = 0wx78
              then yyQ390(strm', yyMATCH(strm, yyAction139, yyNO_MATCH))
              else yyAction139(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ383 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction132(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyQ383(strm', yyMATCH(strm, yyAction132, yyNO_MATCH))
            else if inp < 0wxD
              then if inp = 0wx9
                  then yyQ383(strm', yyMATCH(strm, yyAction132, yyNO_MATCH))
                  else yyAction132(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ383(strm', yyMATCH(strm, yyAction132, yyNO_MATCH))
              else yyAction132(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction132(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxD
              then yyQ383(strm', yyMATCH(strm, yyAction132, yyNO_MATCH))
            else if inp < 0wxD
              then if inp = 0wx9
                  then yyQ383(strm', yyMATCH(strm, yyAction132, yyNO_MATCH))
                  else yyAction132(strm, yyNO_MATCH)
            else if inp = 0wx20
              then yyQ383(strm', yyMATCH(strm, yyAction132, yyNO_MATCH))
              else yyAction132(strm, yyNO_MATCH)
      (* end case *))
fun yyQ382 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction131(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction131, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction131(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction131, yyNO_MATCH))
                      else yyAction131(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction131, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction131(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction131, yyNO_MATCH))
                  else yyAction131(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction131(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction131(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction131, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction131, yyNO_MATCH))
                  else yyAction131(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction131, yyNO_MATCH))
              else yyAction131(strm, yyNO_MATCH)
      (* end case *))
fun yyQ381 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ382(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ380 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ381(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ379 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ380(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ379(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ378 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction130(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction130, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction130(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction130, yyNO_MATCH))
                      else yyAction130(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction130, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction130(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction130, yyNO_MATCH))
                  else yyAction130(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction130(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction130(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction130, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction130, yyNO_MATCH))
                  else yyAction130(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction130, yyNO_MATCH))
              else yyAction130(strm, yyNO_MATCH)
      (* end case *))
fun yyQ377 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ378(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ377(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ376 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction120(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction120, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction120(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction120, yyNO_MATCH))
                      else yyAction120(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction120, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction120(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction120, yyNO_MATCH))
                  else yyAction120(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction120(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction120(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction120, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction120, yyNO_MATCH))
                  else yyAction120(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction120, yyNO_MATCH))
              else yyAction120(strm, yyNO_MATCH)
      (* end case *))
fun yyQ375 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ376(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ374 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ375(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ373 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ374(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ372 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ373(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ371 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ372(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ370 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ371(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx77
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx77
              then if inp = 0wx76
                  then yyQ370(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ369 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction114(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction114, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction114(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction114, yyNO_MATCH))
                      else yyAction114(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction114, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction114(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction114, yyNO_MATCH))
                  else yyAction114(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction114(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction114(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction114, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction114, yyNO_MATCH))
                  else yyAction114(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction114, yyNO_MATCH))
              else yyAction114(strm, yyNO_MATCH)
      (* end case *))
fun yyQ368 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ369(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ368(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ367 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction113(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction113, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction113(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction113, yyNO_MATCH))
                      else yyAction113(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction113, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction113(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction113, yyNO_MATCH))
                  else yyAction113(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction113(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction113(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction113, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction113, yyNO_MATCH))
                  else yyAction113(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction113, yyNO_MATCH))
              else yyAction113(strm, yyNO_MATCH)
      (* end case *))
fun yyQ366 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ367(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ363 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ366(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ365 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction112(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction112, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction112(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction112, yyNO_MATCH))
                      else yyAction112(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction112, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction112(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction112, yyNO_MATCH))
                  else yyAction112(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction112(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction112(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction112, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction112, yyNO_MATCH))
                  else yyAction112(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction112, yyNO_MATCH))
              else yyAction112(strm, yyNO_MATCH)
      (* end case *))
fun yyQ364 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ365(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx65
                  then yyQ364(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ363(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ362 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction104(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction104, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction104(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction104, yyNO_MATCH))
                      else yyAction104(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction104, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction104(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction104, yyNO_MATCH))
                  else yyAction104(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction104(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction104(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction104, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction104, yyNO_MATCH))
                  else yyAction104(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction104, yyNO_MATCH))
              else yyAction104(strm, yyNO_MATCH)
      (* end case *))
fun yyQ361 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx68
                  then yyQ362(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ356 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ361(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ360 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction103(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction103, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction103(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction103, yyNO_MATCH))
                      else yyAction103(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction103, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction103(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction103, yyNO_MATCH))
                  else yyAction103(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction103(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction103(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction103, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction103, yyNO_MATCH))
                  else yyAction103(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction103, yyNO_MATCH))
              else yyAction103(strm, yyNO_MATCH)
      (* end case *))
fun yyQ359 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ360(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ358 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ359(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ357 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ358(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ356(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx68
                  then yyQ357(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ355 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction102(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction102, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction102(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction102, yyNO_MATCH))
                      else yyAction102(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction102, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction102(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction102, yyNO_MATCH))
                  else yyAction102(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction102(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction102(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction102, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction102, yyNO_MATCH))
                  else yyAction102(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction102, yyNO_MATCH))
              else yyAction102(strm, yyNO_MATCH)
      (* end case *))
fun yyQ354 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ355(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ351 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ354(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ353 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction101(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction101, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction101(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction101, yyNO_MATCH))
                      else yyAction101(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction101, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction101(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction101, yyNO_MATCH))
                  else yyAction101(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction101(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction101(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction101, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction101, yyNO_MATCH))
                  else yyAction101(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction101, yyNO_MATCH))
              else yyAction101(strm, yyNO_MATCH)
      (* end case *))
fun yyQ352 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ353(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ351(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx61
                  then yyQ352(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx60
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ350 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction129(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction129, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction129(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction129, yyNO_MATCH))
                      else yyAction129(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction129, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction129(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction129, yyNO_MATCH))
                  else yyAction129(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction129(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction129(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction129, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction129, yyNO_MATCH))
                  else yyAction129(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction129, yyNO_MATCH))
              else yyAction129(strm, yyNO_MATCH)
      (* end case *))
fun yyQ349 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ350(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ348 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ349(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ347 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ348(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ346 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ347(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ345 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx66
                  then yyQ346(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ344 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ345(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ338 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ344(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ343 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction125(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction125, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction125(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction125, yyNO_MATCH))
                      else yyAction125(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction125, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction125(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction125, yyNO_MATCH))
                  else yyAction125(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction125(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction125(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction125, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction125, yyNO_MATCH))
                  else yyAction125(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction125, yyNO_MATCH))
              else yyAction125(strm, yyNO_MATCH)
      (* end case *))
fun yyQ342 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ343(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ339 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ342(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ341 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction100(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction100, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction100(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction100, yyNO_MATCH))
                      else yyAction100(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction100, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction100(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction100, yyNO_MATCH))
                  else yyAction100(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction100(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction100(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction100, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction100, yyNO_MATCH))
                  else yyAction100(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction100, yyNO_MATCH))
              else yyAction100(strm, yyNO_MATCH)
      (* end case *))
fun yyQ340 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ341(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx6A
                  then if inp = 0wx69
                      then yyQ339(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx6E
                  then yyQ338(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ340(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ322 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction96(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction96, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction96(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction96, yyNO_MATCH))
                      else yyAction96(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction96, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction96(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction96, yyNO_MATCH))
                  else yyAction96(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction96(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction96(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction96, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction96, yyNO_MATCH))
                  else yyAction96(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction96, yyNO_MATCH))
              else yyAction96(strm, yyNO_MATCH)
      (* end case *))
fun yyQ337 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction99(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction99, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction99(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction99, yyNO_MATCH))
                      else yyAction99(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction99, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction99(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction99, yyNO_MATCH))
                  else yyAction99(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction99(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction99(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction99, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction99, yyNO_MATCH))
                  else yyAction99(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction99, yyNO_MATCH))
              else yyAction99(strm, yyNO_MATCH)
      (* end case *))
fun yyQ336 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx66
                  then yyQ337(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ335 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction128(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction128(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
                      else yyAction128(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction128(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction128(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
                  else yyAction128(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ336(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction128, yyNO_MATCH))
              else yyAction128(strm, yyNO_MATCH)
      (* end case *))
fun yyQ334 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ335(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ323 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx71
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx71
              then if inp = 0wx70
                  then yyQ334(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ331 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction98(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction98, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction98(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction98, yyNO_MATCH))
                      else yyAction98(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction98, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction98(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction98, yyNO_MATCH))
                  else yyAction98(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction98(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction98(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction98, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction98, yyNO_MATCH))
                  else yyAction98(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction98, yyNO_MATCH))
              else yyAction98(strm, yyNO_MATCH)
      (* end case *))
fun yyQ333 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction97(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction97, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction97(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction97, yyNO_MATCH))
                      else yyAction97(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction97, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction97(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction97, yyNO_MATCH))
                  else yyAction97(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction97(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction97(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction97, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction97, yyNO_MATCH))
                  else yyAction97(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction97, yyNO_MATCH))
              else yyAction97(strm, yyNO_MATCH)
      (* end case *))
fun yyQ332 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ333(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ324 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx75
                  then yyQ332(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx7A
              then if inp = 0wx79
                  then yyQ331(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ330 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction95(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction95, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction95(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction95, yyNO_MATCH))
                      else yyAction95(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction95, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction95(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction95, yyNO_MATCH))
                  else yyAction95(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction95(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction95(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction95, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction95, yyNO_MATCH))
                  else yyAction95(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction95, yyNO_MATCH))
              else yyAction95(strm, yyNO_MATCH)
      (* end case *))
fun yyQ329 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx78
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx78
              then if inp = 0wx77
                  then yyQ330(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ326 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ329(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ328 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction94(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction94, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction94(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction94, yyNO_MATCH))
                      else yyAction94(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction94, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction94(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction94, yyNO_MATCH))
                  else yyAction94(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction94(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction94(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction94, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction94, yyNO_MATCH))
                  else yyAction94(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction94, yyNO_MATCH))
              else yyAction94(strm, yyNO_MATCH)
      (* end case *))
fun yyQ327 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ328(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ325 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx69
                  then yyQ327(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ326(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ324(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx69
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx69
                  then if inp = 0wx68
                      then yyQ325(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx6F
                  then yyQ322(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx7A
              then if inp = 0wx79
                  then yyQ323(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ321 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction126(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction126, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction126(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction126, yyNO_MATCH))
                      else yyAction126(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction126, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction126(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction126, yyNO_MATCH))
                  else yyAction126(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction126(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction126(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction126, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction126, yyNO_MATCH))
                  else yyAction126(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction126, yyNO_MATCH))
              else yyAction126(strm, yyNO_MATCH)
      (* end case *))
fun yyQ297 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ321(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ320 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction124(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction124, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction124(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction124, yyNO_MATCH))
                      else yyAction124(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction124, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction124(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction124, yyNO_MATCH))
                  else yyAction124(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction124(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction124(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction124, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction124, yyNO_MATCH))
                  else yyAction124(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction124, yyNO_MATCH))
              else yyAction124(strm, yyNO_MATCH)
      (* end case *))
fun yyQ319 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ320(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ318 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ319(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ308 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ318(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ317 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction127(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction127, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction127(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction127, yyNO_MATCH))
                      else yyAction127(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction127, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction127(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction127, yyNO_MATCH))
                  else yyAction127(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction127(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction127(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction127, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction127, yyNO_MATCH))
                  else yyAction127(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction127, yyNO_MATCH))
              else yyAction127(strm, yyNO_MATCH)
      (* end case *))
fun yyQ316 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ317(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ310 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ316(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ315 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction123(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction123, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction123(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction123, yyNO_MATCH))
                      else yyAction123(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction123, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction123(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction123, yyNO_MATCH))
                  else yyAction123(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction123(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction123(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction123, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction123, yyNO_MATCH))
                  else yyAction123(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction123, yyNO_MATCH))
              else yyAction123(strm, yyNO_MATCH)
      (* end case *))
fun yyQ314 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ315(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ313 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ314(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ312 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ313(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ311 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ312(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ309 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx6E
                  then yyQ311(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ310(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ298 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx72
              then yyQ308(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx72
              then if inp = 0wx61
                  then yyQ309(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx60
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ307 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction93(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction93, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction93(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction93, yyNO_MATCH))
                      else yyAction93(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction93, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction93(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction93, yyNO_MATCH))
                  else yyAction93(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction93(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction93(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction93, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction93, yyNO_MATCH))
                  else yyAction93(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction93, yyNO_MATCH))
              else yyAction93(strm, yyNO_MATCH)
      (* end case *))
fun yyQ306 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx68
                  then yyQ307(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ305 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ306(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ304 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ305(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ299 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ304(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ303 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction92(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction92, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction92(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction92, yyNO_MATCH))
                      else yyAction92(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction92, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction92(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction92, yyNO_MATCH))
                  else yyAction92(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction92(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction92(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction92, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction92, yyNO_MATCH))
                  else yyAction92(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction92, yyNO_MATCH))
              else yyAction92(strm, yyNO_MATCH)
      (* end case *))
fun yyQ302 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ303(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ301 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ302(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ300 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx71
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx71
              then if inp = 0wx70
                  then yyQ301(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ300(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx66
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx66
                  then if inp = 0wx65
                      then yyQ297(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx74
                  then yyQ298(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx78
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx78
              then if inp = 0wx76
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ299(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ296 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction122(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction122, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction122(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction122, yyNO_MATCH))
                      else yyAction122(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction122, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction122(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction122, yyNO_MATCH))
                  else yyAction122(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction122(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction122(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction122, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction122, yyNO_MATCH))
                  else yyAction122(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction122, yyNO_MATCH))
              else yyAction122(strm, yyNO_MATCH)
      (* end case *))
fun yyQ295 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx67
                  then yyQ296(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ294 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ295(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ293 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ294(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ292 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ293(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ291 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ292(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ285 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ291(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ290 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction91(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction91, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction91(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction91, yyNO_MATCH))
                      else yyAction91(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction91, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction91(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction91, yyNO_MATCH))
                  else yyAction91(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction91(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction91(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction91, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction91, yyNO_MATCH))
                  else yyAction91(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction91, yyNO_MATCH))
              else yyAction91(strm, yyNO_MATCH)
      (* end case *))
fun yyQ289 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ290(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ288 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ289(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ287 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ288(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ286 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ287(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx65
                  then yyQ286(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ285(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ284 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction90(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction90, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction90(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction90, yyNO_MATCH))
                      else yyAction90(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction90, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction90(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction90, yyNO_MATCH))
                  else yyAction90(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction90(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction90(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction90, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction90, yyNO_MATCH))
                  else yyAction90(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction90, yyNO_MATCH))
              else yyAction90(strm, yyNO_MATCH)
      (* end case *))
fun yyQ283 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ284(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ282 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ283(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ281 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ282(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ249 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx63
              then yyQ281(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ280 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction121(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction121, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction121(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction121, yyNO_MATCH))
                      else yyAction121(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction121, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction121(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction121, yyNO_MATCH))
                  else yyAction121(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction121(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction121(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction121, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction121, yyNO_MATCH))
                  else yyAction121(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction121, yyNO_MATCH))
              else yyAction121(strm, yyNO_MATCH)
      (* end case *))
fun yyQ279 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ280(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ278 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx71
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx71
              then if inp = 0wx70
                  then yyQ279(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ277 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx7A
              then if inp = 0wx79
                  then yyQ278(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ271 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ277(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ276 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction89(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction89, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction89(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction89, yyNO_MATCH))
                      else yyAction89(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction89, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction89(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction89, yyNO_MATCH))
                  else yyAction89(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction89(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction89(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction89, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction89, yyNO_MATCH))
                  else yyAction89(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction89, yyNO_MATCH))
              else yyAction89(strm, yyNO_MATCH)
      (* end case *))
fun yyQ275 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ276(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ274 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ275(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ273 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ274(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ272 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ273(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ270 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx65
                  then yyQ272(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ271(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ257 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ270(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ269 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction88(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction88, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction88(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction88, yyNO_MATCH))
                      else yyAction88(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction88, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction88(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction88, yyNO_MATCH))
                  else yyAction88(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction88(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction88(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction88, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction88, yyNO_MATCH))
                  else yyAction88(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction88, yyNO_MATCH))
              else yyAction88(strm, yyNO_MATCH)
      (* end case *))
fun yyQ268 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ269(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ267 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ268(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ266 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ267(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ258 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx77
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx77
              then if inp = 0wx76
                  then yyQ266(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ265 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction87(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction87, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction87(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction87, yyNO_MATCH))
                      else yyAction87(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction87, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction87(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction87, yyNO_MATCH))
                  else yyAction87(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction87(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction87(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction87, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction87, yyNO_MATCH))
                  else yyAction87(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction87, yyNO_MATCH))
              else yyAction87(strm, yyNO_MATCH)
      (* end case *))
fun yyQ264 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ265(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ263 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ264(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ262 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ263(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ261 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ262(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ260 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ261(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ259 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ260(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ250 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx66
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx66
                  then if inp = 0wx65
                      then yyQ259(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx69
                  then yyQ258(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ257(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ256 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction86(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction86, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction86(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction86, yyNO_MATCH))
                      else yyAction86(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction86, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction86(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction86, yyNO_MATCH))
                  else yyAction86(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction86(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction86(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction86, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction86, yyNO_MATCH))
                  else yyAction86(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction86, yyNO_MATCH))
              else yyAction86(strm, yyNO_MATCH)
      (* end case *))
fun yyQ255 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ256(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ254 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx67
                  then yyQ255(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ253 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ254(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ252 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6C
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx6B
                  then yyQ253(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ251 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ252(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx62
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx62
                  then yyQ251(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx72
                  then yyQ250(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ249(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ248 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction118(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction118, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction118(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction118, yyNO_MATCH))
                      else yyAction118(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction118, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction118(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction118, yyNO_MATCH))
                  else yyAction118(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction118(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction118(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction118, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction118, yyNO_MATCH))
                  else yyAction118(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction118, yyNO_MATCH))
              else yyAction118(strm, yyNO_MATCH)
      (* end case *))
fun yyQ247 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ248(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ246 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx77
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx77
              then if inp = 0wx76
                  then yyQ247(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ238 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ246(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ245 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction117(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction117, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction117(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction117, yyNO_MATCH))
                      else yyAction117(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction117, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction117(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction117, yyNO_MATCH))
                  else yyAction117(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction117(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction117(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction117, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction117, yyNO_MATCH))
                  else yyAction117(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction117, yyNO_MATCH))
              else yyAction117(strm, yyNO_MATCH)
      (* end case *))
fun yyQ244 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ245(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ243 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ244(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ242 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ243(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ241 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx71
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx71
              then if inp = 0wx70
                  then yyQ242(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ240 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ241(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ239 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ240(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ228 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx6D
                  then yyQ239(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ238(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ237 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction119(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction119, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction119(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction119, yyNO_MATCH))
                      else yyAction119(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction119, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction119(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction119, yyNO_MATCH))
                  else yyAction119(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction119(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction119(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction119, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction119, yyNO_MATCH))
                  else yyAction119(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction119, yyNO_MATCH))
              else yyAction119(strm, yyNO_MATCH)
      (* end case *))
fun yyQ236 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ237(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ235 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ236(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ232 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx63
              then yyQ235(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ234 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction85(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction85, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction85(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction85, yyNO_MATCH))
                      else yyAction85(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction85, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction85(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction85, yyNO_MATCH))
                  else yyAction85(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction85(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction85(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction85, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction85, yyNO_MATCH))
                  else yyAction85(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction85, yyNO_MATCH))
              else yyAction85(strm, yyNO_MATCH)
      (* end case *))
fun yyQ233 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ234(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ229 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ232(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx6C
                  then yyQ233(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ231 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction84(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction84, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction84(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction84, yyNO_MATCH))
                      else yyAction84(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction84, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction84(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction84, yyNO_MATCH))
                  else yyAction84(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction84(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction84(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction84, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction84, yyNO_MATCH))
                  else yyAction84(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction84, yyNO_MATCH))
              else yyAction84(strm, yyNO_MATCH)
      (* end case *))
fun yyQ230 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx78
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx78
              then if inp = 0wx77
                  then yyQ231(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx62
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx62
                  then yyQ228(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx65
                  then yyQ230(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ229(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ227 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction83(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction83, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction83(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction83, yyNO_MATCH))
                      else yyAction83(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction83, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction83(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction83, yyNO_MATCH))
                  else yyAction83(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction83(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction83(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction83, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction83, yyNO_MATCH))
                  else yyAction83(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction83, yyNO_MATCH))
              else yyAction83(strm, yyNO_MATCH)
      (* end case *))
fun yyQ226 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ227(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ226(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ182 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction74(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                      else yyAction74(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction74(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyAction74(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction74(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction74(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
                  else yyAction74(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction74, yyNO_MATCH))
              else yyAction74(strm, yyNO_MATCH)
      (* end case *))
fun yyQ225 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction115(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction115, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction115(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction115, yyNO_MATCH))
                      else yyAction115(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction115, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction115(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction115, yyNO_MATCH))
                  else yyAction115(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction115(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction115(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction115, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction115, yyNO_MATCH))
                  else yyAction115(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction115, yyNO_MATCH))
              else yyAction115(strm, yyNO_MATCH)
      (* end case *))
fun yyQ224 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ225(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ223 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ224(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ222 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ223(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ197 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ222(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ221 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction81(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction81, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction81(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction81, yyNO_MATCH))
                      else yyAction81(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction81, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction81(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction81, yyNO_MATCH))
                  else yyAction81(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction81(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction81(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction81, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction81, yyNO_MATCH))
                  else yyAction81(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction81, yyNO_MATCH))
              else yyAction81(strm, yyNO_MATCH)
      (* end case *))
fun yyQ220 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ221(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ219 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ220(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ218 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ219(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ217 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ218(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ207 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ217(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ216 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction80(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction80, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction80(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction80, yyNO_MATCH))
                      else yyAction80(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction80, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction80(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction80, yyNO_MATCH))
                  else yyAction80(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction80(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction80(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction80, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction80, yyNO_MATCH))
                  else yyAction80(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction80, yyNO_MATCH))
              else yyAction80(strm, yyNO_MATCH)
      (* end case *))
fun yyQ215 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ216(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ210 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ215(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ214 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction79(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction79, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction79(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction79, yyNO_MATCH))
                      else yyAction79(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction79, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction79(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction79, yyNO_MATCH))
                  else yyAction79(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction79(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction79(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction79, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction79, yyNO_MATCH))
                  else yyAction79(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction79, yyNO_MATCH))
              else yyAction79(strm, yyNO_MATCH)
      (* end case *))
fun yyQ213 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ214(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ212 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ213(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ211 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ212(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ209 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx66
                  then yyQ211(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ210(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ208 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ209(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ198 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction116(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
                      else yyAction116(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
                      else yyAction116(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
                  else yyAction116(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction116(strm, yyNO_MATCH)
                else if inp = 0wx65
                  then yyQ208(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ207(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction116, yyNO_MATCH))
              else yyAction116(strm, yyNO_MATCH)
      (* end case *))
fun yyQ206 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction78(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction78, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction78(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction78, yyNO_MATCH))
                      else yyAction78(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction78, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction78(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction78, yyNO_MATCH))
                  else yyAction78(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction78(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction78(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction78, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction78, yyNO_MATCH))
                  else yyAction78(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction78, yyNO_MATCH))
              else yyAction78(strm, yyNO_MATCH)
      (* end case *))
fun yyQ205 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx66
                  then yyQ206(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ204 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ205(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ203 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ204(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ202 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ203(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ201 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ202(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ200 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ201(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ199 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ200(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ183 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction77(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction77(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx3A
                  then yyAction77(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction77(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
                          else yyAction77(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction77(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction77(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction77(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
                  else yyAction77(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ199(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx63
                  then yyQ197(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
            else if inp < 0wx75
              then yyQ198(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction77, yyNO_MATCH))
              else yyAction77(strm, yyNO_MATCH)
      (* end case *))
fun yyQ184 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction82(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction82, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction82(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction82, yyNO_MATCH))
                      else yyAction82(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction82, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction82(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction82, yyNO_MATCH))
                  else yyAction82(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction82(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction82(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction82, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction82, yyNO_MATCH))
                  else yyAction82(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction82, yyNO_MATCH))
              else yyAction82(strm, yyNO_MATCH)
      (* end case *))
fun yyQ196 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction76(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction76(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                      else yyAction76(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction76(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                  else yyAction76(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction76(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction76(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
                  else yyAction76(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction76, yyNO_MATCH))
              else yyAction76(strm, yyNO_MATCH)
      (* end case *))
fun yyQ195 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ196(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ187 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ195(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ194 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction75(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction75, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction75(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction75, yyNO_MATCH))
                      else yyAction75(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction75, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction75(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction75, yyNO_MATCH))
                  else yyAction75(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction75(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction75(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction75, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction75, yyNO_MATCH))
                  else yyAction75(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction75, yyNO_MATCH))
              else yyAction75(strm, yyNO_MATCH)
      (* end case *))
fun yyQ193 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ194(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ192 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ193(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ191 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ192(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ190 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ191(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ189 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ190(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ188 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ189(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ186 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx6C
                  then yyQ188(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ187(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ185 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx71
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx71
              then if inp = 0wx70
                  then yyQ186(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ183(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx67
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx67
                  then if inp = 0wx66
                      then yyQ182(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx6D
                  then yyQ185(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ184(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ181 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction73(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction73(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                      else yyAction73(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction73(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                  else yyAction73(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction73(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction73(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
                  else yyAction73(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction73, yyNO_MATCH))
              else yyAction73(strm, yyNO_MATCH)
      (* end case *))
fun yyQ180 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ181(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ179 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ180(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ178 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ179(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ177 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ178(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ176 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ177(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ163 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ176(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ175 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction72(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction72(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                      else yyAction72(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction72(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyAction72(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction72(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction72(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
                  else yyAction72(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction72, yyNO_MATCH))
              else yyAction72(strm, yyNO_MATCH)
      (* end case *))
fun yyQ164 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ175(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ174 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction71(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction71(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                      else yyAction71(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction71(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                  else yyAction71(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction71(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction71(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
                  else yyAction71(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction71, yyNO_MATCH))
              else yyAction71(strm, yyNO_MATCH)
      (* end case *))
fun yyQ173 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx7A
              then if inp = 0wx79
                  then yyQ174(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ172 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction111(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction111(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
                      else yyAction111(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction111(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction111(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
                  else yyAction111(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ173(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction111, yyNO_MATCH))
              else yyAction111(strm, yyNO_MATCH)
      (* end case *))
fun yyQ171 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ172(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ170 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ171(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ165 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ170(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ169 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction70(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction70(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                      else yyAction70(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction70(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                  else yyAction70(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction70(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction70(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
                  else yyAction70(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction70, yyNO_MATCH))
              else yyAction70(strm, yyNO_MATCH)
      (* end case *))
fun yyQ168 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ169(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ167 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ168(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ166 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ167(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ166(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx6A
                  then if inp = 0wx69
                      then yyQ165(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx6F
                  then yyQ164(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ163(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ162 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction110(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction110, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction110(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction110, yyNO_MATCH))
                      else yyAction110(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction110, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction110(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction110, yyNO_MATCH))
                  else yyAction110(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction110(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction110(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction110, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction110, yyNO_MATCH))
                  else yyAction110(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction110, yyNO_MATCH))
              else yyAction110(strm, yyNO_MATCH)
      (* end case *))
fun yyQ161 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx68
                  then yyQ162(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ148 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ161(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ160 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction69(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction69(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                      else yyAction69(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction69(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                  else yyAction69(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction69(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction69(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
                  else yyAction69(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction69, yyNO_MATCH))
              else yyAction69(strm, yyNO_MATCH)
      (* end case *))
fun yyQ159 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ160(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ158 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx65
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx65
              then if inp = 0wx64
                  then yyQ159(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ157 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ158(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ156 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ157(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ149 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ156(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ155 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction68(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction68(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                      else yyAction68(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction68(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyAction68(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction68(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction68(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
                  else yyAction68(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction68, yyNO_MATCH))
              else yyAction68(strm, yyNO_MATCH)
      (* end case *))
fun yyQ154 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ155(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ150 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ154(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ153 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction67(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                      else yyAction67(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction67(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction67(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction67(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
                  else yyAction67(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction67, yyNO_MATCH))
              else yyAction67(strm, yyNO_MATCH)
      (* end case *))
fun yyQ152 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ153(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ151 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ152(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ148(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6D
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx6D
                  then if inp = 0wx6C
                      then yyQ151(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ150(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx79
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx79
              then if inp = 0wx78
                  then yyQ149(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ147 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction108(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction108, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction108(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction108, yyNO_MATCH))
                      else yyAction108(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction108, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction108(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction108, yyNO_MATCH))
                  else yyAction108(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction108(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction108(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction108, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction108, yyNO_MATCH))
                  else yyAction108(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction108, yyNO_MATCH))
              else yyAction108(strm, yyNO_MATCH)
      (* end case *))
fun yyQ146 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ147(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ145 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ146(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ144 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx63
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx63
              then yyQ145(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction66(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction66(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                      else yyAction66(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction66(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction66(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyAction66(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ144(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction66, yyNO_MATCH))
              else yyAction66(strm, yyNO_MATCH)
      (* end case *))
fun yyQ143 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction109(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction109, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction109(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction109, yyNO_MATCH))
                      else yyAction109(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction109, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction109(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction109, yyNO_MATCH))
                  else yyAction109(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction109(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction109(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction109, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction109, yyNO_MATCH))
                  else yyAction109(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction109, yyNO_MATCH))
              else yyAction109(strm, yyNO_MATCH)
      (* end case *))
fun yyQ142 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ143(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ141 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ142(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ140 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ141(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ139 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ140(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ139(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ138 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction107(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction107, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction107(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction107, yyNO_MATCH))
                      else yyAction107(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction107, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction107(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction107, yyNO_MATCH))
                  else yyAction107(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction107(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction107(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction107, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction107, yyNO_MATCH))
                  else yyAction107(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction107, yyNO_MATCH))
              else yyAction107(strm, yyNO_MATCH)
      (* end case *))
fun yyQ137 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ138(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ136 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ137(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ135 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6E
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6E
              then if inp = 0wx6D
                  then yyQ136(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ135(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ134 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction106(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction106, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction106(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction106, yyNO_MATCH))
                      else yyAction106(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction106, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction106(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction106, yyNO_MATCH))
                  else yyAction106(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction106(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction106(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction106, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction106, yyNO_MATCH))
                  else yyAction106(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction106, yyNO_MATCH))
              else yyAction106(strm, yyNO_MATCH)
      (* end case *))
fun yyQ133 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ134(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ132 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ133(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ131 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx67
                  then yyQ132(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ130 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx68
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx68
              then if inp = 0wx67
                  then yyQ131(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ130(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ129 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction65(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction65(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                      else yyAction65(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction65(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction65(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction65(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
                  else yyAction65(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction65, yyNO_MATCH))
              else yyAction65(strm, yyNO_MATCH)
      (* end case *))
fun yyQ128 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ129(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ127 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ128(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ127(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction64(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                      else yyAction64(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction64(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction64(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction64(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
                  else yyAction64(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction64, yyNO_MATCH))
              else yyAction64(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ126(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ125(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ124(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ123(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx67
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx67
              then if inp = 0wx64
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx64
                  then if inp = 0wx62
                      then yyQ120(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ119(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx66
                  then yyQ122(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ121(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx66
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx66
                  then if inp = 0wx65
                      then yyQ118(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx6F
                  then yyQ116(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx7A
              then if inp = 0wx79
                  then yyQ117(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction63(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                      else yyAction63(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction63(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction63(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction63(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
                  else yyAction63(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction63, yyNO_MATCH))
              else yyAction63(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ115(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ113 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx76
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx76
              then if inp = 0wx75
                  then yyQ114(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ113(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6A
              then if inp = 0wx69
                  then yyQ112(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction62(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction62(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                      else yyAction62(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction62(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction62(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction62(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
                  else yyAction62(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction62, yyNO_MATCH))
              else yyAction62(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ111(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ109(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx73
                  then yyQ110(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6F
              then if inp = 0wx6E
                  then yyQ108(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction61(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction61(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                      else yyAction61(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction61(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction61(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction61(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
                  else yyAction61(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction61, yyNO_MATCH))
              else yyAction61(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ107(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ106(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ105(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction105(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction105, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction105(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction105, yyNO_MATCH))
                      else yyAction105(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction105, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction105(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction105, yyNO_MATCH))
                  else yyAction105(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction105(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction105(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction105, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction105, yyNO_MATCH))
                  else yyAction105(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction105, yyNO_MATCH))
              else yyAction105(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx6C
                  then yyQ104(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction60(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction60(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                      else yyAction60(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction60(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                  else yyAction60(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction60(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction60(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
                  else yyAction60(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction60, yyNO_MATCH))
              else yyAction60(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx69
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx69
              then if inp = 0wx68
                  then yyQ103(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx64
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx64
              then if inp = 0wx63
                  then yyQ102(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction58(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                      else yyAction58(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction58(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                  else yyAction58(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction58(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction58(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
                  else yyAction58(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction59(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction59(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                      else yyAction59(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction59(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction59(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction59(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
                  else yyAction59(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction59, yyNO_MATCH))
              else yyAction59(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5F
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx5F
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx41
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx41
                  then if inp <= 0wx39
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp <= 0wx5A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx61
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx61
                  then yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx65
                  then yyQ100(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then if inp = 0wx74
                  then yyQ101(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ99(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx6C
                  then yyQ97(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx75
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx75
              then yyQ98(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx60
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then if inp = 0wx25
                      then yyAction133(strm, yyNO_MATCH)
                    else if inp < 0wx25
                      then if inp = 0wx24
                          then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                          else yyAction133(strm, yyNO_MATCH)
                    else if inp <= 0wx2F
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5B
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then if inp <= 0wx40
                      then yyAction133(strm, yyNO_MATCH)
                      else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6D
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6D
              then if inp = 0wx62
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx62
                  then yyQ96(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp = 0wx6C
                  then yyQ95(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx70
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx70
              then if inp = 0wx6F
                  then yyQ94(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction57(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                      else yyAction57(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction57(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction57(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction57(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
                  else yyAction57(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction57, yyNO_MATCH))
              else yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx6C
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx6C
              then if inp = 0wx6B
                  then yyQ93(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ92(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx66
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx65
                  then yyQ91(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx73
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx73
              then if inp = 0wx72
                  then yyQ90(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ89(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx25
                  then yyAction56(strm, yyNO_MATCH)
                else if inp < 0wx25
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                      else yyAction56(strm, yyNO_MATCH)
                else if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction56(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ89(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp = 0wx60
              then yyAction56(strm, yyNO_MATCH)
            else if inp < 0wx60
              then if inp = 0wx5B
                  then yyAction56(strm, yyNO_MATCH)
                else if inp < 0wx5B
                  then yyQ89(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                else if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
                  else yyAction56(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction56, yyNO_MATCH))
              else yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction133(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction133(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp = 0wx30
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx24
                      then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                      else yyAction133(strm, yyNO_MATCH)
                else if inp = 0wx3A
                  then yyAction133(strm, yyNO_MATCH)
                else if inp < 0wx3A
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                else if inp <= 0wx40
                  then yyAction133(strm, yyNO_MATCH)
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx61
              then if inp = 0wx5F
                  then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyAction133(strm, yyNO_MATCH)
            else if inp = 0wx74
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx73
                  then yyQ88(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
                  else yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
            else if inp <= 0wx7A
              then yyQ89(strm', yyMATCH(strm, yyAction133, yyNO_MATCH))
              else yyAction133(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx5D
              then yyQ51(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx2B
                  then yyQ57(strm', lastMatch)
                else if inp < 0wx2B
                  then if inp = 0wx22
                      then yyQ61(strm', lastMatch)
                    else if inp < 0wx22
                      then if inp = 0wxD
                          then yyQ66(strm', lastMatch)
                        else if inp < 0wxD
                          then if inp = 0wxA
                              then yyQ35(strm', lastMatch)
                            else if inp < 0wxA
                              then if inp = 0wx9
                                  then yyQ66(strm', lastMatch)
                                  else yyQ62(strm', lastMatch)
                              else yyQ62(strm', lastMatch)
                        else if inp = 0wx20
                          then yyQ66(strm', lastMatch)
                        else if inp = 0wx21
                          then yyQ37(strm', lastMatch)
                          else yyQ62(strm', lastMatch)
                    else if inp = 0wx27
                      then yyQ61(strm', lastMatch)
                    else if inp < 0wx27
                      then if inp = 0wx25
                          then yyQ38(strm', lastMatch)
                        else if inp < 0wx25
                          then if inp = 0wx23
                              then yyQ62(strm', lastMatch)
                              else yyQ65(strm', lastMatch)
                          else yyQ39(strm', lastMatch)
                    else if inp = 0wx29
                      then yyQ41(strm', lastMatch)
                    else if inp = 0wx28
                      then yyQ40(strm', lastMatch)
                      else yyQ42(strm', lastMatch)
                else if inp = 0wx3C
                  then yyQ58(strm', lastMatch)
                else if inp < 0wx3C
                  then if inp = 0wx30
                      then yyQ64(strm', lastMatch)
                    else if inp < 0wx30
                      then if inp = 0wx2E
                          then yyQ44(strm', lastMatch)
                        else if inp < 0wx2E
                          then if inp = 0wx2C
                              then yyQ43(strm', lastMatch)
                              else yyQ36(strm', lastMatch)
                          else yyQ45(strm', lastMatch)
                    else if inp = 0wx3A
                      then yyQ46(strm', lastMatch)
                    else if inp = 0wx3B
                      then yyQ47(strm', lastMatch)
                      else yyQ63(strm', lastMatch)
                else if inp = 0wx40
                  then yyQ49(strm', lastMatch)
                else if inp < 0wx40
                  then if inp = 0wx3E
                      then yyQ60(strm', lastMatch)
                    else if inp = 0wx3D
                      then yyQ59(strm', lastMatch)
                      else yyQ48(strm', lastMatch)
                else if inp = 0wx5B
                  then yyQ50(strm', lastMatch)
                else if inp = 0wx5C
                  then yyQ62(strm', lastMatch)
                  else yyQ65(strm', lastMatch)
            else if inp = 0wx6F
              then yyQ69(strm', lastMatch)
            else if inp < 0wx6F
              then if inp = 0wx66
                  then yyQ82(strm', lastMatch)
                else if inp < 0wx66
                  then if inp = 0wx62
                      then yyQ86(strm', lastMatch)
                    else if inp < 0wx62
                      then if inp = 0wx60
                          then yyQ62(strm', lastMatch)
                        else if inp < 0wx60
                          then if inp = 0wx5E
                              then yyQ52(strm', lastMatch)
                              else yyQ65(strm', lastMatch)
                          else yyQ87(strm', lastMatch)
                    else if inp = 0wx64
                      then yyQ84(strm', lastMatch)
                    else if inp = 0wx63
                      then yyQ85(strm', lastMatch)
                      else yyQ83(strm', lastMatch)
                else if inp = 0wx6A
                  then yyQ65(strm', lastMatch)
                else if inp < 0wx6A
                  then if inp = 0wx68
                      then yyQ70(strm', lastMatch)
                    else if inp = 0wx67
                      then yyQ71(strm', lastMatch)
                      else yyQ81(strm', lastMatch)
                else if inp = 0wx6D
                  then yyQ65(strm', lastMatch)
                else if inp < 0wx6D
                  then if inp = 0wx6C
                      then yyQ80(strm', lastMatch)
                      else yyQ65(strm', lastMatch)
                  else yyQ79(strm', lastMatch)
            else if inp = 0wx78
              then yyQ68(strm', lastMatch)
            else if inp < 0wx78
              then if inp = 0wx74
                  then yyQ75(strm', lastMatch)
                else if inp < 0wx74
                  then if inp = 0wx72
                      then yyQ77(strm', lastMatch)
                    else if inp < 0wx72
                      then if inp = 0wx70
                          then yyQ78(strm', lastMatch)
                          else yyQ65(strm', lastMatch)
                      else yyQ76(strm', lastMatch)
                else if inp = 0wx76
                  then yyQ73(strm', lastMatch)
                else if inp = 0wx75
                  then yyQ74(strm', lastMatch)
                  else yyQ72(strm', lastMatch)
            else if inp = 0wx7C
              then yyQ54(strm', lastMatch)
            else if inp < 0wx7C
              then if inp = 0wx7A
                  then yyQ65(strm', lastMatch)
                else if inp = 0wx79
                  then yyQ67(strm', lastMatch)
                  else yyQ53(strm', lastMatch)
            else if inp = 0wx7E
              then yyQ56(strm', lastMatch)
            else if inp = 0wx7D
              then yyQ55(strm', lastMatch)
              else yyQ62(strm', lastMatch)
      (* end case *))
fun yyQ31 (strm, lastMatch) = yyAction147(strm, yyNO_MATCH)
fun yyQ32 (strm, lastMatch) = yyAction148(strm, yyNO_MATCH)
fun yyQ34 (strm, lastMatch) = yyAction146(strm, yyNO_MATCH)
fun yyQ33 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction148(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ34(strm', yyMATCH(strm, yyAction148, yyNO_MATCH))
              else yyAction148(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyQ32(strm', lastMatch)
            else if inp < 0wxB
              then if inp = 0wxA
                  then yyQ31(strm', lastMatch)
                  else yyQ32(strm', lastMatch)
            else if inp = 0wx2A
              then yyQ33(strm', lastMatch)
              else yyQ32(strm', lastMatch)
      (* end case *))
fun yyQ21 (strm, lastMatch) = yyAction161(strm, yyNO_MATCH)
fun yyQ22 (strm, lastMatch) = yyAction164(strm, yyNO_MATCH)
fun yyQ24 (strm, lastMatch) = yyAction162(strm, yyNO_MATCH)
fun yyQ25 (strm, lastMatch) = yyAction163(strm, yyNO_MATCH)
fun yyQ29 (strm, lastMatch) = yyAction162(strm, yyNO_MATCH)
fun yyQ30 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ29(strm', lastMatch)
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ29(strm', lastMatch)
                else if inp < 0wx30
                  then yystuck(lastMatch)
                else if inp <= 0wx39
                  then yyQ29(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = 0wx61
              then yyQ29(strm', lastMatch)
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ29(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp <= 0wx66
              then yyQ29(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ26 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction163(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx41
              then yyQ30(strm', yyMATCH(strm, yyAction163, yyNO_MATCH))
            else if inp < 0wx41
              then if inp = 0wx30
                  then yyQ30(strm', yyMATCH(strm, yyAction163, yyNO_MATCH))
                else if inp < 0wx30
                  then yyAction163(strm, yyNO_MATCH)
                else if inp <= 0wx39
                  then yyQ30(strm', yyMATCH(strm, yyAction163, yyNO_MATCH))
                  else yyAction163(strm, yyNO_MATCH)
            else if inp = 0wx61
              then yyQ30(strm', yyMATCH(strm, yyAction163, yyNO_MATCH))
            else if inp < 0wx61
              then if inp <= 0wx46
                  then yyQ30(strm', yyMATCH(strm, yyAction163, yyNO_MATCH))
                  else yyAction163(strm, yyNO_MATCH)
            else if inp <= 0wx66
              then yyQ30(strm', yyMATCH(strm, yyAction163, yyNO_MATCH))
              else yyAction163(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction162(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ29(strm', yyMATCH(strm, yyAction162, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction162(strm, yyNO_MATCH)
            else if inp <= 0wx37
              then yyQ29(strm', yyMATCH(strm, yyAction162, yyNO_MATCH))
              else yyAction162(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction162(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx30
              then yyQ28(strm', yyMATCH(strm, yyAction162, yyNO_MATCH))
            else if inp < 0wx30
              then yyAction162(strm, yyNO_MATCH)
            else if inp <= 0wx37
              then yyQ28(strm', yyMATCH(strm, yyAction162, yyNO_MATCH))
              else yyAction162(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction164(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx66
              then yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
            else if inp < 0wx66
              then if inp = 0wx30
                  then yyQ27(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                else if inp < 0wx30
                  then if inp = 0wx23
                      then yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                    else if inp < 0wx23
                      then if inp = 0wx22
                          then yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                          else yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                    else if inp = 0wx27
                      then yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                      else yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                else if inp = 0wx5D
                  then yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                else if inp < 0wx5D
                  then if inp = 0wx38
                      then yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                    else if inp < 0wx38
                      then yyQ27(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                    else if inp = 0wx5C
                      then yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                      else yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                else if inp = 0wx62
                  then yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
            else if inp = 0wx74
              then yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
            else if inp < 0wx74
              then if inp = 0wx6F
                  then yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                else if inp < 0wx6F
                  then if inp = 0wx6E
                      then yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                      else yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                else if inp = 0wx72
                  then yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                  else yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
            else if inp = 0wx77
              then yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
            else if inp < 0wx77
              then if inp = 0wx75
                  then yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
            else if inp = 0wx78
              then yyQ26(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
              else yyQ25(strm', yyMATCH(strm, yyAction164, yyNO_MATCH))
      (* end case *))
fun yyQ3 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx27
              then yyQ21(strm', lastMatch)
            else if inp < 0wx27
              then if inp = 0wx22
                  then yyQ21(strm', lastMatch)
                  else yyQ22(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ23(strm', lastMatch)
              else yyQ22(strm', lastMatch)
      (* end case *))
fun yyQ13 (strm, lastMatch) = yyAction150(strm, yyNO_MATCH)
fun yyQ14 (strm, lastMatch) = yyAction151(strm, yyNO_MATCH)
fun yyQ15 (strm, lastMatch) = yyAction154(strm, yyNO_MATCH)
fun yyQ19 (strm, lastMatch) = yyAction152(strm, yyNO_MATCH)
fun yyQ20 (strm, lastMatch) = yyAction153(strm, yyNO_MATCH)
fun yyQ16 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction154(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyQ20(strm', yyMATCH(strm, yyAction154, yyNO_MATCH))
            else if inp < 0wxB
              then if inp = 0wxA
                  then yyQ19(strm', yyMATCH(strm, yyAction154, yyNO_MATCH))
                  else yyQ20(strm', yyMATCH(strm, yyAction154, yyNO_MATCH))
            else if inp = 0wxD
              then yyQ19(strm', yyMATCH(strm, yyAction154, yyNO_MATCH))
              else yyQ20(strm', yyMATCH(strm, yyAction154, yyNO_MATCH))
      (* end case *))
fun yyQ18 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction149(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction149(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yyAction149(strm, yyNO_MATCH)
                  else yyQ18(strm', yyMATCH(strm, yyAction149, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ18(strm', yyMATCH(strm, yyAction149, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction149(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ18(strm', yyMATCH(strm, yyAction149, yyNO_MATCH))
              else yyAction149(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction149(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wx5B
              then yyAction149(strm, yyNO_MATCH)
            else if inp < 0wx5B
              then if inp <= 0wx40
                  then yyAction149(strm, yyNO_MATCH)
                  else yyQ18(strm', yyMATCH(strm, yyAction149, yyNO_MATCH))
            else if inp = 0wx61
              then yyQ18(strm', yyMATCH(strm, yyAction149, yyNO_MATCH))
            else if inp < 0wx61
              then yyAction149(strm, yyNO_MATCH)
            else if inp <= 0wx7A
              then yyQ18(strm', yyMATCH(strm, yyAction149, yyNO_MATCH))
              else yyAction149(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wx2F
              then yyQ17(strm', lastMatch)
            else if inp < 0wx2F
              then if inp = 0wxB
                  then yyQ15(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ14(strm', lastMatch)
                      else yyQ15(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ14(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
            else if inp = 0wx5C
              then yyQ16(strm', lastMatch)
            else if inp < 0wx5C
              then if inp = 0wx5B
                  then yyQ13(strm', lastMatch)
                  else yyQ15(strm', lastMatch)
              else yyQ15(strm', lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch) = yyAction155(strm, yyNO_MATCH)
fun yyQ8 (strm, lastMatch) = yyAction156(strm, yyNO_MATCH)
fun yyQ9 (strm, lastMatch) = yyAction159(strm, yyNO_MATCH)
fun yyQ11 (strm, lastMatch) = yyAction157(strm, yyNO_MATCH)
fun yyQ12 (strm, lastMatch) = yyAction158(strm, yyNO_MATCH)
fun yyQ10 (strm, lastMatch) = (case (yygetc(strm))
       of NONE => yyAction159(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = 0wxB
              then yyQ12(strm', yyMATCH(strm, yyAction159, yyNO_MATCH))
            else if inp < 0wxB
              then if inp = 0wxA
                  then yyQ11(strm', yyMATCH(strm, yyAction159, yyNO_MATCH))
                  else yyQ12(strm', yyMATCH(strm, yyAction159, yyNO_MATCH))
            else if inp = 0wxD
              then yyQ11(strm', yyMATCH(strm, yyAction159, yyNO_MATCH))
              else yyQ12(strm', yyMATCH(strm, yyAction159, yyNO_MATCH))
      (* end case *))
fun yyQ1 (strm, lastMatch) = (case (yygetc(strm))
       of NONE =>
            if ULexBuffer.eof(strm)
              then UserDeclarations.eof(())
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = 0wxE
              then yyQ9(strm', lastMatch)
            else if inp < 0wxE
              then if inp = 0wxB
                  then yyQ9(strm', lastMatch)
                else if inp < 0wxB
                  then if inp = 0wxA
                      then yyQ8(strm', lastMatch)
                      else yyQ9(strm', lastMatch)
                else if inp = 0wxD
                  then yyQ8(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
            else if inp = 0wx5D
              then yyQ7(strm', lastMatch)
            else if inp < 0wx5D
              then if inp = 0wx5C
                  then yyQ10(strm', lastMatch)
                  else yyQ9(strm', lastMatch)
              else yyQ9(strm', lastMatch)
      (* end case *))
fun yyQ0 (strm, lastMatch) = if ULexBuffer.eof(strm)
        then UserDeclarations.eof(())
        else yystuck(lastMatch)
in
  (case (!(yyss))
   of XML => yyQ0(!(yystrm), yyNO_MATCH)
    | REGEXP_CHARSET => yyQ1(!(yystrm), yyNO_MATCH)
    | REGEXP => yyQ2(!(yystrm), yyNO_MATCH)
    | STRING => yyQ3(!(yystrm), yyNO_MATCH)
    | MULTI_LINE_COMMENT => yyQ4(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ5(!(yystrm), yyNO_MATCH)
    | SINGLE_LINE_COMMENT => yyQ6(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            and skip() = (yystartPos := yygetPos(); continue())
	    in (continue(), (!yystartPos, yygetPos()), !yystrm, !yyss) end
          in 
            lex()
          end
  in
    type pos = StreamPos.pos
    type span = StreamPos.span
    type tok = UserDeclarations.lex_result

    datatype prestrm = STRM of ULexBuffer.stream * 
		(yystart_state * tok * span * prestrm * yystart_state) option ref
    type strm = (prestrm * yystart_state)

    fun lex sm (STRM (yystrm, memo), ss) = (case !memo
	  of NONE => let
	     val (tok, span, yystrm', ss') = innerLex (yystrm, ss, sm)
	     val strm' = STRM (yystrm', ref NONE);
	     in 
	       memo := SOME (ss, tok, span, strm', ss');
	       (tok, span, (strm', ss'))
	     end
	   | SOME (ss', tok, span, strm', ss'') => 
	       if ss = ss' then
		 (tok, span, (strm', ss''))
	       else (
		 memo := NONE;
		 lex sm (STRM (yystrm, memo), ss))
         (* end case *))

    fun streamify input = (STRM (ULexBuffer.mkStream input, ref NONE), 
			   INITIAL)

    fun streamifyReader readFn strm = let
          val s = ref strm
	  fun iter(strm, n, accum) = 
	        if n > 1024 then (String.implode (rev accum), strm)
		else (case readFn strm
		       of NONE => (String.implode (rev accum), strm)
			| SOME(c, strm') => iter (strm', n+1, c::accum))
          fun input() = let
	        val (data, strm) = iter(!s, 0, [])
	        in
	          s := strm;
		  data
	        end
          in
            streamify input
          end

    fun streamifyInstream strm = streamify (fn ()=>TextIO.input strm)

    fun getPos (STRM (strm, _), _) = ULexBuffer.getpos strm

  end
end
