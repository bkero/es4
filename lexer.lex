
(* 
 * This lexer is a sketch, confined to ASCII and covering only a subset of
 * the actual lexical grammar of ES4. 
 *)

open Token

type lexresult = token

fun eof _ = EOF

fun log ss = 
    (TextIO.print "log: "; 
     List.app TextIO.print ss; 
     TextIO.print "\n")

fun token_list (token_fn : unit -> token) =
let
    val t = ref [] 
    fun add tok = t := tok :: !t
    fun stop _ = rev (!t)
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
		LEXBREAK_DIV _ => (add DIV; add tok; stop ())
	      | LEXBREAK_DIVASSIGN _ => (add DIVASSIGN; add tok; stop ())
	      | LEXBREAK_LESSTHAN _ => (add LESSTHAN; add tok; stop ())
	      | EOF => (add EOF; stop ())
	      | x => (add x; step ())
	end
in
    step ()
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

charEscape            = "\\" ([abtnvfr\"\'\\]|"x"{hexDigit}{2}|[0-7]{1}{3});

%%

<INITIAL>"\n"              => (EOL);

<INITIAL>"-"               => (MINUS);
<INITIAL>"--"              => (MINUSMINUS);
<INITIAL>"!"               => (NOT);
<INITIAL>"!="              => (NOTEQUALS);
<INITIAL>"!=="             => (STRICTNOTEQUALS);
<INITIAL>"%"               => (MODULUS);
<INITIAL>"%="              => (MODULUSASSIGN);
<INITIAL>"&"               => (BITWISEAND);
<INITIAL>"&&"              => (LOGICALAND);
<INITIAL>"&&="             => (LOGICALANDASSIGN);
<INITIAL>"&="              => (BITWISEANDASSIGN);
<INITIAL>"("               => (LEFTPAREN);
<INITIAL>")"               => (RIGHTPAREN);
<INITIAL>"*"               => (MULT);
<INITIAL>"*="              => (MULTASSIGN);
<INITIAL>","               => (COMMA);
<INITIAL>"."               => (DOT);
<INITIAL>".."              => (DOUBLEDOT);
<INITIAL>"..."             => (TRIPLEDOT);
<INITIAL>".<"              => (LEFTDOTANGLE);

<INITIAL>"/"               => (LEXBREAK_DIV 
				   { lex_initial = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN INITIAL; lex ()))),
				     lex_regexp = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN REGEXP; lex ()))) });

<INITIAL>"/="              => (LEXBREAK_DIVASSIGN 
				   { lex_initial = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN INITIAL; lex ()))),
				     lex_regexp = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN REGEXP; lex ()))) });
<INITIAL>":"               => (COLON);
<INITIAL>"::"              => (DOUBLECOLON);
<INITIAL>";"               => (SEMICOLON);
<INITIAL>"?"               => (QUESTIONMARK);
<INITIAL>"@"               => (AT);
<INITIAL>"["               => (LEFTBRACKET);
<INITIAL>"]"               => (RIGHTBRACKET);
<INITIAL>"^"               => (BITWISEXOR);
<INITIAL>"^^"              => (LOGICALXOR);
<INITIAL>"^^="             => (LOGICALXORASSIGN);
<INITIAL>"^="              => (BITWISEXORASSIGN);
<INITIAL>"{"               => (LEFTBRACE);
<INITIAL>"|"               => (BITWISEOR);
<INITIAL>"||"              => (LOGICALOR);
<INITIAL>"||="             => (LOGICALORASSIGN);
<INITIAL>"|="              => (BITWISEORASSIGN);
<INITIAL>"}"               => (RIGHTBRACE);
<INITIAL>"~"               => (BITWISENOT);
<INITIAL>"+"               => (PLUS);
<INITIAL>"++"              => (PLUSPLUS);
<INITIAL>"+="              => (PLUSASSIGN);

<INITIAL>"<"               => (LEXBREAK_LESSTHAN
				   { lex_initial = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN INITIAL; lex ()))),
				     lex_xml = 
				     (fn _ => token_list 
						  (fn _ => (YYBEGIN XML; lex ()))) });

<INITIAL>"<<"              => (LEFTSHIFT);
<INITIAL>"<<="             => (LEFTSHIFTASSIGN);
<INITIAL>"<="              => (LESSTHANOREQUALS);
<INITIAL>"="               => (ASSIGN);
<INITIAL>"-="              => (MINUSASSIGN);
<INITIAL>"=="              => (EQUALS);
<INITIAL>"==="             => (STRICTEQUALS);
<INITIAL>">"               => (GREATERTHAN);
<INITIAL>">="              => (GREATERTHANOREQUALS);
<INITIAL>">>"              => (RIGHTSHIFT);
<INITIAL>">>="             => (RIGHTSHIFTASSIGN);
<INITIAL>">>>"             => (UNSIGNEDRIGHTSHIFT);
<INITIAL>">>>="            => (UNSIGNEDRIGHTSHIFTASSIGN);

<INITIAL>"as"              => (AS);
<INITIAL>"break"           => (BREAK);
<INITIAL>"case"            => (CASE); 
<INITIAL>"cast"            => (CAST); 
<INITIAL>"catch"           => (CATCH); 
<INITIAL>"class"           => (CLASS); 
<INITIAL>"const"           => (CONST); 
<INITIAL>"continue"        => (CONTINUE); 
<INITIAL>"default"         => (DEFAULT); 
<INITIAL>"delete"          => (DELETE); 
<INITIAL>"do"              => (DO); 
<INITIAL>"else"            => (ELSE); 
<INITIAL>"enum"            => (ENUM); 
<INITIAL>"extends"         => (EXTENDS); 
<INITIAL>"false"           => (FALSE); 
<INITIAL>"finally"         => (FINALLY); 
<INITIAL>"for"             => (FOR); 
<INITIAL>"function"        => (FUNCTION); 
<INITIAL>"if"              => (IF);
<INITIAL>"implements"      => (IMPLEMENTS);
<INITIAL>"import"          => (IMPORT);
<INITIAL>"in"              => (IN);
<INITIAL>"instanceof"      => (INSTANCEOF);
<INITIAL>"interface"       => (INTERFACE);
<INITIAL>"internal"        => (INTERNAL);
<INITIAL>"intrinsic"       => (INTRINSIC);
<INITIAL>"is"              => (IS);
<INITIAL>"let"             => (LET);
<INITIAL>"new"             => (NEW);
<INITIAL>"null"            => (NULL);
<INITIAL>"package"         => (PACKAGE);
<INITIAL>"private"         => (PRIVATE);
<INITIAL>"protected"       => (PROTECTED);
<INITIAL>"public"          => (PUBLIC);
<INITIAL>"return"          => (RETURN);
<INITIAL>"super"           => (SUPER);
<INITIAL>"switch"          => (SWITCH);
<INITIAL>"this"            => (THIS);
<INITIAL>"throw"           => (THROW);
<INITIAL>"to"              => (TO);
<INITIAL>"true"            => (TRUE);
<INITIAL>"try"             => (TRY);
<INITIAL>"typeof"          => (TYPEOF);
<INITIAL>"use"             => (USE);
<INITIAL>"var"             => (VAR);
<INITIAL>"void"            => (VOID);
<INITIAL>"while"           => (WHILE);
<INITIAL>"with"            => (WITH);

<INITIAL>"call"            => (CALL);
<INITIAL>"debugger"        => (DEBUGGER);
<INITIAL>"DECIMAL"         => (DECIMAL);
<INITIAL>"DOUBLE"          => (DOUBLE);
<INITIAL>"DYNAMIC"         => (DYNAMIC);
<INITIAL>"EACH"            => (EACH);
<INITIAL>"final"           => (FINAL);
<INITIAL>"get"             => (GET);
<INITIAL>"goto"            => (GOTO);
<INITIAL>"include"         => (INCLUDE);
<INITIAL>"int"             => (INT);
<INITIAL>"namespace"       => (NAMESPACE);
<INITIAL>"native"          => (NATIVE);
<INITIAL>"number"          => (NUMBER);
<INITIAL>"override"        => (OVERRIDE);
<INITIAL>"prototype"       => (PROTOTYPE);
<INITIAL>"rounding"        => (ROUNDING);
<INITIAL>"standard"        => (STANDARD);
<INITIAL>"strict"          => (STRICT);
<INITIAL>"uint"            => (UINT);
<INITIAL>"set"             => (SET);
<INITIAL>"static"          => (STATIC);
<INITIAL>"type"            => (TYPE);
<INITIAL>"xml"             => (Token.XML);
<INITIAL>"yield"           => (YIELD);

<INITIAL>{whitespace}        => (lex());
<INITIAL>{identifier}        => (IDENTIFIER yytext);
                   
<INITIAL>{decimalLiteral}    => (case Real.fromString yytext of 
                                     SOME r => NUMBERLITERAL r 
                                   | NONE   => raise LexError);

<INITIAL>{hexIntegerLiteral} => (case Int.fromString yytext of
                                     SOME i => NUMBERLITERAL (Real.fromInt i)
                                   | NONE => raise LexError);


<INITIAL>"//"                => (YYBEGIN SINGLE_LINE_COMMENT; lex());
<SINGLE_LINE_COMMENT>"\n"    => (YYBEGIN INITIAL; lex());
<SINGLE_LINE_COMMENT>.       => (lex());

<INITIAL>"/*"                => (YYBEGIN MULTI_LINE_COMMENT; lex());
<MULTI_LINE_COMMENT>"*/"     => (YYBEGIN INITIAL; lex());
<MULTI_LINE_COMMENT>.        => (lex());

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
                                         STRINGLITERAL str
                                     end
                                 else
                                     lex());

<STRING>{charEscape}          => ((case Char.fromCString yytext of
				       NONE => raise LexError
				     | SOME c => curr_chars := c :: (!curr_chars));
				  lex());

<STRING>"\\".                 => (curr_chars := (String.sub (yytext,1)) :: (!curr_chars);
				  lex());

<STRING>.                     => (curr_chars := (String.sub (yytext,0)) :: (!curr_chars);
				  lex());

<INITIAL>.                    => (log ["unexpected input: '", yytext, "'"]; raise LexError);
