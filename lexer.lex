
(* 
 * This lexer is a sketch, confined to ASCII and covering only a subset of
 * the actual lexical grammar of ES4. 
 *)

open Token

type lexresult = token

fun eof _ = EMPTY

%%

%structure Lexer

whitespace      = [ \t\r\013]+;

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

%%

"\n"              => (EOL);

"-"               => (MINUS);
"--"              => (MINUSMINUS);
"!"               => (NOT);
"!="              => (NOTEQUALS);
"!=="             => (STRICTNOTEQUALS);
"%"               => (MODULUS);
"%="              => (MODULUSASSIGN);
"&"               => (BITWISEAND);
"&&"              => (LOGICALAND);
"&&="             => (LOGICALANDASSIGN);
"&="              => (BITWISEANDASSIGN);
"("               => (LEFTPAREN);
")"               => (RIGHTPAREN);
"*"               => (MULT);
"*="              => (MULTASSIGN);
","               => (COMMA);
"."               => (DOT);
".."              => (DOUBLEDOT);
"..."             => (TRIPLEDOT);
".<"              => (LEFTDOTANGLE);
"/"               => (DIV);
"/="              => (DIVASSIGN);
":"               => (COLON);
"::"              => (DOUBLECOLON);
";"               => (SEMICOLON);
"?"               => (QUESTIONMARK);
"@"               => (AT);
"["               => (LEFTBRACKET);
"]"               => (RIGHTBRACKET);
"^"               => (BITWISEXOR);
"^^"              => (LOGICALXOR);
"^^="             => (LOGICALXORASSIGN);
"^="              => (BITWISEXORASSIGN);
"{"               => (LEFTBRACE);
"|"               => (BITWISEOR);
"||"              => (LOGICALOR);
"||="             => (LOGICALORASSIGN);
"|="              => (BITWISEORASSIGN);
"}"               => (RIGHTBRACE);
"~"               => (BITWISENOT);
"+"               => (PLUS);
"++"              => (PLUSPLUS);
"+="              => (PLUSASSIGN);
"<"               => (LESSTHAN);
"<<"              => (LEFTSHIFT);
"<<="             => (LEFTSHIFTASSIGN);
"<="              => (LESSTHANOREQUAL);
"="               => (ASSIGN);
"-="              => (MINUSASSIGN);
"=="              => (EQUALS);
"==="             => (STRICTEQUALS);
">"               => (GREATERTHAN);
">="              => (GREATERTHANOREQUAL);
">>"              => (RIGHTSHIFT);
">>="             => (RIGHTSHIFTASSIGN);
">>>"             => (UNSIGNEDRIGHTSHIFT);
">>>="            => (UNSIGNEDRIGHTSHIFTASSIGN);

"as"              => (AS);
"break"           => (BREAK);
"case"            => (CASE); 
"cast"            => (CAST); 
"catch"           => (CATCH); 
"class"           => (CLASS); 
"const"           => (CONST); 
"continue"        => (CONTINUE); 
"default"         => (DEFAULT); 
"delete"          => (DELETE); 
"do"              => (DO); 
"else"            => (ELSE); 
"enum"            => (ENUM); 
"extends"         => (EXTENDS); 
"false"           => (FALSE); 
"finally"         => (FINALLY); 
"for"             => (FOR); 
"function"        => (FUNCTION); 
"if"              => (IF);
"implements"      => (IMPLEMENTS);
"import"          => (IMPORT);
"in"              => (IN);
"instanceof"      => (INSTANCEOF);
"interface"       => (INTERFACE);
"internal"        => (INTERNAL);
"intrinsic"       => (INTRINSIC);
"is"              => (IS);
"let"             => (LET);
"new"             => (NEW);
"null"            => (NULL);
"package"         => (PACKAGE);
"private"         => (PRIVATE);
"protected"       => (PROTECTED);
"public"          => (PUBLIC);
"return"          => (RETURN);
"super"           => (SUPER);
"switch"          => (SWITCH);
"this"            => (THIS);
"throw"           => (THROW);
"to"              => (TO);
"true"            => (TRUE);
"try"             => (TRY);
"type"            => (TYPE);
"typeof"          => (TYPEOF);
"use"             => (USE);
"var"             => (VAR);
"void"            => (VOID);
"while"           => (WHILE);
"with"            => (WITH);


{whitespace}        => (lex());
{identifier}        => (IDENTIFIER yytext);
		   
{decimalLiteral}    => (case Real.fromString yytext of 
		            SOME r => NUMBERLITERAL r 
			  | NONE   => raise LexError);

{hexIntegerLiteral} => (case Int.fromString yytext of
		            SOME i => NUMBERLITERAL (Real.fromInt i)
			  | NONE => raise LexError);
