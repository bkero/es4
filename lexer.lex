
(* 
 * This lexer is a sketch, confined to ASCII and covering only a subset of
 * the actual lexical grammar of ES4. 
 *)

open Token

type lexresult = token

fun eof _ = Eof

fun log ss = 
    (TextIO.print "log: "; 
     List.app TextIO.print ss; 
     TextIO.print "\n")

val line_breaks : int list ref = ref []
val token_count : int ref = ref 0

fun token_list (token_fn : unit -> token) =
let
    val t = ref [] 
    fun add tok = t := tok :: !t
    fun add_lb offset = line_breaks := offset :: !line_breaks
    fun stop _ = (token_count := length (!t); rev (!t))
    fun step _ = 
        let 
	        val tok = token_fn ()
	    in 
	        (* log ["lexed ", tokenname tok]; *)

	  (*
	   * The lexbreak tokens represent choice points for the parser. We
	   * return two thunks to it: one for each lexer start state
	   * it might wish to resume lexing in.
	   *)
	    case tok of 
            LexBreakDiv _ => (add tok; stop ())
	      | LexBreakDivAssign _ => (add DivAssign; add tok; stop ())
	      | LexBreakLessThan _ => (add LessThan; add tok; stop ())
	      | Eof => (add Eof; stop ())
	      | Eol => (add_lb (length (!t)); step ())
	      | x => (add x; step ())
	end
in
    line_breaks := [];
    step ()
end


fun followsLineBreak (ts) =
	let val _ = log(["followsLineBreak"])
	    val offset = length ts
	    val max_offset = !token_count
	    fun findBreak lbs =
            case lbs of
                [] => false
              | _ => 
	              (log(["token_count=",Int.toString(max_offset),
						" offset=",Int.toString(max_offset-offset),
						" break=",Int.toString(hd lbs)]);
                  if (hd lbs) = (max_offset - offset) then true else findBreak (tl lbs))
	in
		findBreak (!line_breaks)
	end

val (curr_quote : char ref) = ref #"\000"
val (curr_chars : (char list) ref) = ref []

%%

%structure Lexer

%s REGEXP XML SINGLE_LINE_COMMENT MULTI_LINE_COMMENT STRING;

whitespace      = [\032\t\r\013]+;

identifierStart = [a-zA-Z_$];
identifierPart  = [a-zA-Z_$0-9];
identifier      = ({identifierStart} {identifierPart}*);

hexDigit           = [0-9a-fA-F];
decimalDigit       = [0-9];
nonZeroDigit       = [1-9];
exponentIndicator  = [eE];

decimalDigits      = ({decimalDigit}+);
signedInteger      = ([-+]? {decimalDigits});
exponentPart       = ({exponentIndicator} {signedInteger});

decimalIntegerLiteral = ("0" | ({nonZeroDigit} {decimalDigits}?));
decimalLiteral_1      = ({decimalIntegerLiteral} "." {decimalDigits}? {exponentPart}?);
decimalLiteral_2      = ("." {decimalDigits} {exponentPart}?);
decimalLiteral_3      = ({decimalIntegerLiteral} {exponentPart}?);
decimalLiteral        = ({decimalLiteral_1} | {decimalLiteral_2} | {decimalLiteral_3});

hexIntegerLiteral     = ("0" [xX] {hexDigit}+);

charEscape            = "\\" ([btnvfr\"\'\\]|"x"{hexDigit}{2}|[0-7]{1,3});

%%

<INITIAL>"\n"              => (Eol);

<INITIAL>"-"               => (Minus);
<INITIAL>"--"              => (MinusMinus);
<INITIAL>"!"               => (Not);
<INITIAL>"!="              => (NotEquals);
<INITIAL>"!=="             => (StrictNotEquals);
<INITIAL>"%"               => (Modulus);
<INITIAL>"%="              => (ModulusAssign);
<INITIAL>"&"               => (BitwiseAnd);
<INITIAL>"&&"              => (LogicalAnd);
<INITIAL>"&&="             => (LogicalAndAssign);
<INITIAL>"&="              => (BitwiseAndAssign);
<INITIAL>"("               => (LeftParen);
<INITIAL>")"               => (RightParen);
<INITIAL>"*"               => (Mult);
<INITIAL>"*="              => (MultAssign);
<INITIAL>","               => (Comma);
<INITIAL>"."               => (Dot);
<INITIAL>".."              => (DoubleDot);
<INITIAL>"..."             => (TripleDot);
<INITIAL>".<"              => (LeftDotAngle);

<INITIAL>"/"               => (LexBreakDiv
				   { lex_initial = 
				     (fn _ => Div :: token_list 
						  (fn _ => (YYBEGIN INITIAL; lex ()))),
				     lex_regexp = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN REGEXP; lex ()))) });

<INITIAL>"/="              => (LexBreakDivAssign
				   { lex_initial = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN INITIAL; lex ()))),
				     lex_regexp = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN REGEXP; lex ()))) });
<INITIAL>":"               => (Colon);
<INITIAL>"::"              => (DoubleColon);
<INITIAL>";"               => (SemiColon);
<INITIAL>"?"               => (QuestionMark);
<INITIAL>"@"               => (At);
<INITIAL>"["               => (LeftBracket);
<INITIAL>"]"               => (RightBracket);
<INITIAL>"^"               => (BitwiseXor);
<INITIAL>"^="              => (BitwiseXorAssign);
<INITIAL>"{"               => (LeftBrace);
<INITIAL>"|"               => (BitwiseOr);
<INITIAL>"||"              => (LogicalOr);
<INITIAL>"||="             => (LogicalOrAssign);
<INITIAL>"|="              => (BitwiseOrAssign);
<INITIAL>"}"               => (RightBrace);
<INITIAL>"~"               => (BitwiseNot);
<INITIAL>"+"               => (Plus);
<INITIAL>"++"              => (PlusPlus);
<INITIAL>"+="              => (PlusAssign);

<INITIAL>"<"               => (LexBreakLessThan
				   { lex_initial = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN INITIAL; lex ()))),
				     lex_xml = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN XML; lex ()))) });

<INITIAL>"<<"              => (LeftShift);
<INITIAL>"<<="             => (LeftShiftAssign);
<INITIAL>"<="              => (LessThanOrEquals);
<INITIAL>"="               => (Assign);
<INITIAL>"-="              => (MinusAssign);
<INITIAL>"=="              => (Equals);
<INITIAL>"==="             => (StrictEquals);
<INITIAL>">"               => (GreaterThan);
<INITIAL>">="              => (GreaterThanOrEquals);
<INITIAL>">>"              => (RightShift);
<INITIAL>">>="             => (RightShiftAssign);
<INITIAL>">>>"             => (UnsignedRightShift);
<INITIAL>">>>="            => (UnsignedRightShiftAssign);

<INITIAL>"as"              => (As);
<INITIAL>"break"           => (Break);
<INITIAL>"case"            => (Case); 
<INITIAL>"cast"            => (Cast); 
<INITIAL>"catch"           => (Catch); 
<INITIAL>"class"           => (Class); 
<INITIAL>"const"           => (Const); 
<INITIAL>"continue"        => (Continue); 
<INITIAL>"default"         => (Default); 
<INITIAL>"delete"          => (Delete); 
<INITIAL>"do"              => (Do); 
<INITIAL>"else"            => (Else); 
<INITIAL>"enum"            => (Enum); 
<INITIAL>"extends"         => (Extends); 
<INITIAL>"false"           => (False); 
<INITIAL>"finally"         => (Finally); 
<INITIAL>"for"             => (For); 
<INITIAL>"function"        => (Function); 
<INITIAL>"if"              => (If);
<INITIAL>"implements"      => (Implements);
<INITIAL>"import"          => (Import);
<INITIAL>"in"              => (In);
<INITIAL>"instanceof"      => (InstanceOf);
<INITIAL>"interface"       => (Interface);
<INITIAL>"internal"        => (Internal);
<INITIAL>"intrinsic"       => (Intrinsic);
<INITIAL>"is"              => (Is);
<INITIAL>"let"             => (Let);
<INITIAL>"new"             => (New);
<INITIAL>"null"            => (Null);
<INITIAL>"package"         => (Package);
<INITIAL>"precision"       => (Precision);
<INITIAL>"private"         => (Private);
<INITIAL>"protected"       => (Protected);
<INITIAL>"public"          => (Public);
<INITIAL>"return"          => (Return);
<INITIAL>"super"           => (Super);
<INITIAL>"switch"          => (Switch);
<INITIAL>"this"            => (This);
<INITIAL>"throw"           => (Throw);
<INITIAL>"to"              => (To);
<INITIAL>"true"            => (True);
<INITIAL>"try"             => (Try);
<INITIAL>"typeof"          => (TypeOf);
<INITIAL>"use"             => (Use);
<INITIAL>"var"             => (Var);
<INITIAL>"void"            => (Void);
<INITIAL>"while"           => (While);
<INITIAL>"with"            => (With);

<INITIAL>"call"            => (Call);
<INITIAL>"debugger"        => (Debugger);
<INITIAL>"decimal"         => (Decimal);
<INITIAL>"double"          => (Double);
<INITIAL>"dynamic"         => (Dynamic);
<INITIAL>"each"            => (Each);
<INITIAL>"final"           => (Final);
<INITIAL>"get"             => (Get);
<INITIAL>"goto"            => (Goto);
<INITIAL>"include"         => (Include);
<INITIAL>"int"             => (Int);
<INITIAL>"namespace"       => (Namespace);
<INITIAL>"native"          => (Native);
<INITIAL>"number"          => (Number);
<INITIAL>"override"        => (Override);
<INITIAL>"prototype"       => (Prototype);
<INITIAL>"rounding"        => (Rounding);
<INITIAL>"standard"        => (Standard);
<INITIAL>"strict"          => (Strict);
<INITIAL>"uint"            => (UInt);
<INITIAL>"set"             => (Set);
<INITIAL>"static"          => (Static);
<INITIAL>"type"            => (Type);
<INITIAL>"xml"             => (Token.Xml);
<INITIAL>"yield"           => (Yield);

<INITIAL>{whitespace}        => (lex());
<INITIAL>{identifier}        => (Identifier yytext);
                   
<INITIAL>{decimalLiteral}    => (case Real.fromString yytext of 
                                     SOME r => NumberLiteral r 
                                   | NONE   => raise LexError);

<INITIAL>{hexIntegerLiteral} => (case Int.fromString yytext of
                                     SOME i => NumberLiteral (Real.fromInt i)
                                   | NONE => raise LexError);


<INITIAL>"//"                => (YYBEGIN SINGLE_LINE_COMMENT; lex());
<SINGLE_LINE_COMMENT>"\n"    => (YYBEGIN INITIAL; lex());
<SINGLE_LINE_COMMENT>.       => (lex());

<INITIAL>"/*"                => (YYBEGIN MULTI_LINE_COMMENT; lex());
<MULTI_LINE_COMMENT>"*/"     => (YYBEGIN INITIAL; lex());
<MULTI_LINE_COMMENT>.|"\n"   => (lex());

<INITIAL>"'"|"\""            => (curr_quote := String.sub (yytext,0); 
                                 curr_chars := [];
                                 YYBEGIN STRING;
                                 lex());

<STRING>"'"|"\""             => (if 
                                     (!curr_quote) = String.sub (yytext,0)
                                 then 
                                     let 
                                         val str = (String.implode (rev (!curr_chars)))
                                     in
                                         curr_quote := #"\000";
                                         curr_chars := [];
                                         YYBEGIN INITIAL;
                                         StringLiteral str
                                     end
                                 else
                                     (curr_chars := (String.sub (yytext,0)) :: (!curr_chars);
                                     lex()));

<STRING>{charEscape}          => ((case Char.fromCString yytext of
				       NONE => raise LexError
				     | SOME c => curr_chars := c :: (!curr_chars));
				  lex());

<STRING>"\\".                 => (curr_chars := (String.sub (yytext,1)) :: (!curr_chars);
				  lex());

<STRING>.                     => (curr_chars := (String.sub (yytext,0)) :: (!curr_chars);
				  lex());

<INITIAL>.                    => (log ["unexpected input: '", yytext, "'"]; raise LexError);
