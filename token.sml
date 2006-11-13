structure Token = struct

datatype token = 

    (* punctuators *)

      MINUS
    | MINUSMINUS
    | NOT
    | NOTEQUALS
    | STRICTNOTEQUALS
    | MODULUS
    | MODULUSASSIGN
    | BITWISEAND
    | LOGICALAND
    | LOGICALANDASSIGN
    | BITWISEANDASSIGN
    | LEFTPAREN
    | RIGHTPAREN
    | MULT
    | MULTASSIGN
    | COMMA
    | DOT
    | DOUBLEDOT
    | TRIPLEDOT
    | LEFTDOTANGLE
    | DIV
    | DIVASSIGN
    | COLON
    | DOUBLECOLON
    | SEMICOLON
    | QUESTIONMARK
    | AT
    | LEFTBRACKET
    | RIGHTBRACKET
    | BITWISEXOR
    | LOGICALXOR
    | LOGICALXORASSIGN
    | BITWISEXORASSIGN
    | LEFTBRACE
    | BITWISEOR
    | LOGICALOR
    | LOGICALORASSIGN
    | BITWISEORASSIGN
    | RIGHTBRACE
    | BITWISENOT
    | PLUS
    | PLUSPLUS
    | PLUSASSIGN
    | LESSTHAN
    | LEFTSHIFT
    | LEFTSHIFTASSIGN
    | LESSTHANOREQUALS
    | ASSIGN
    | MINUSASSIGN
    | EQUALS
    | STRICTEQUALS
    | GREATERTHAN
    | GREATERTHANOREQUALS
    | RIGHTSHIFT
    | RIGHTSHIFTASSIGN
    | UNSIGNEDRIGHTSHIFT
    | UNSIGNEDRIGHTSHIFTASSIGN

    (* reserved identifiers *)

    | AS
    | BREAK
    | CASE
    | CAST
    | CATCH
    | CLASS
    | CONST
    | CONTINUE
    | DEFAULT
    | DELETE
    | DO
    | ELSE
    | ENUM
    | EXTENDS
    | FALSE
    | FINALLY
    | FOR
    | FUNCTION
    | IF
    | IMPLEMENTS
    | IMPORT
    | IN
    | INSTANCEOF
    | INTERFACE
    | INTERNAL
    | INTRINSIC
    | IS
    | LET
    | NEW
    | NULL
    | PACKAGE
    | PRIVATE
    | PROTECTED
    | PUBLIC
    | RETURN
    | SUPER
    | SWITCH
    | THIS
    | THROW
    | TO
    | TRUE
    | TRY
    | TYPEOF
    | USE
    | VAR
    | VOID
    | WHILE
    | WITH

	(* contextually reserved identifiers *)

    | CALL
    | DEBUGGER
    | DECIMAL
    | DOUBLE
    | DYNAMIC
    | EACH
    | FINAL
    | GET
    | GOTO
    | INCLUDE
    | INT
    | NAMESPACE
    | NATIVE
    | NUMBER
    | OVERRIDE
    | PROTOTYPE
    | ROUNDING
    | STANDARD
    | STRICT
    | UINT
    | SET
    | STATIC
    | TYPE
    | XML
    | YIELD


    (* literals *)

    | ATTRIBUTEIDENTIFIER
    | BLOCKCOMMENT
    | DOCCOMMENT
    | EOL
    | IDENTIFIER of string
    | NUMBERLITERAL of real
    | PACKAGEIDENTIFIER
    | REGEXPLITERAL
    | SLASHSLASHCOMMENT
    | STRINGLITERAL of string
    | WHITESPACE
    | XMLLITERAL
    | XMLPART
    | XMLMARKUP
    | XMLTEXT
    | XMLTAGENDEND
    | XMLTAGSTARTEND

    (* meta *)

    | ERROR
    | LEXBREAK_DIV of { lex_initial: unit -> (token list),
			lex_regexp: unit -> (token list) }
    | LEXBREAK_DIVASSIGN of { lex_initial: unit -> (token list),
			      lex_regexp: unit -> (token list) }
    | LEXBREAK_LESSTHAN of { lex_initial: unit -> (token list),
			     lex_xml: unit -> (token list) }
    | EOF

exception TokenError

fun isreserved t = 
	case t of
    ( AS
    | BREAK
    | CASE
    | CAST
    | CATCH
    | CLASS
    | CONST
    | CONTINUE
    | DEFAULT
    | DELETE
    | DO
    | ELSE
    | ENUM
    | EXTENDS
    | FALSE
    | FINALLY
    | FOR
    | FUNCTION
    | IF
    | IMPLEMENTS
    | IMPORT
    | IN
    | INSTANCEOF
    | INTERFACE
    | INTERNAL
    | INTRINSIC
    | IS
    | LET
    | NEW
    | NULL
    | PACKAGE
    | PRIVATE
    | PROTECTED
    | PUBLIC
    | RETURN
    | SUPER
    | SWITCH
    | THIS
    | THROW
    | TO
    | TRUE
    | TRY
    | TYPEOF
    | USE
    | VAR
    | VOID
    | WHILE
    | WITH ) => true
	| _ => false

fun tokenname t =
    case t of

	(* punctuators *)

      MINUS => "-"
    | MINUSMINUS => "--"
    | NOT => "!"
    | NOTEQUALS => "!="
    | STRICTNOTEQUALS => "!=="
    | MODULUS => "%"
    | MODULUSASSIGN => "%="
    | BITWISEAND => "&"
    | LOGICALAND => "&&"
    | LOGICALANDASSIGN => "&&="
    | BITWISEANDASSIGN => "&="
    | LEFTPAREN => "("
    | RIGHTPAREN => ")"
    | MULT => "*"
    | MULTASSIGN => "*="
    | COMMA => ","
    | DOT => "."
    | DOUBLEDOT => ".."
    | TRIPLEDOT => "..."
    | LEFTDOTANGLE => ".<"
    | DIV => "/"
    | DIVASSIGN => "/="
    | COLON => ":"
    | DOUBLECOLON => "::"
    | SEMICOLON => ";"
    | QUESTIONMARK => "?"
    | AT => "@"
    | LEFTBRACKET => "["
    | RIGHTBRACKET => "]"
    | BITWISEXOR => "^"
    | LOGICALXOR => "^^"
    | LOGICALXORASSIGN => "^^="
    | BITWISEXORASSIGN => "^="
    | LEFTBRACE => "{"
    | BITWISEOR => "|"
    | LOGICALOR => "||"
    | LOGICALORASSIGN => "||="
    | BITWISEORASSIGN => "|="
    | RIGHTBRACE => "}"
    | BITWISENOT => "~"
    | PLUS => "+"
    | PLUSPLUS => "++"
    | PLUSASSIGN => "+="
    | LESSTHAN => "<"
    | LEFTSHIFT => "<<"
    | LEFTSHIFTASSIGN => "<<="
    | LESSTHANOREQUALS => "<="
    | ASSIGN => "="
    | MINUSASSIGN => "*="
    | EQUALS => "=="
    | STRICTEQUALS => "==="
    | GREATERTHAN => ">"
    | GREATERTHANOREQUALS => ">="
    | RIGHTSHIFT => ">>"
    | RIGHTSHIFTASSIGN => ">>="
    | UNSIGNEDRIGHTSHIFT => ">>>"
    | UNSIGNEDRIGHTSHIFTASSIGN => ">>>="

    (* reserved identifiers *)

    | AS => "as"
    | BREAK => "break"
    | CASE => "case"
    | CAST => "cast"
    | CATCH => "catch"
    | CLASS => "class"
    | CONST => "const"
    | CONTINUE => "continue"
    | DEFAULT => "default"
    | DELETE => "delete"
    | DO => "do"
    | ELSE => "else"
    | ENUM => "enum"
    | EXTENDS => "extends"
    | FALSE => "false"
    | FINALLY => "finally"
    | FOR => "for"
    | FUNCTION => "function"
    | IF => "if"
    | IMPLEMENTS => "implements"
    | IMPORT => "import"
    | IN => "in"
    | INSTANCEOF => "instanceof"
    | INTERFACE => "interface"
    | INTERNAL => "internal"
    | INTRINSIC => "intrinsic"
    | IS => "is"
    | LET => "let"
    | NEW => "new"
    | NULL => "null"
    | PACKAGE => "package"
    | PRIVATE => "private"
    | PROTECTED => "protected"
    | PUBLIC => "public"
    | RETURN => "return"
    | SUPER => "super"
    | SWITCH => "switch"
    | THIS => "this"
    | THROW => "throw"
    | TO => "to"
    | TRUE => "true"
    | TRY => "try"
    | TYPEOF => "typeof"
    | USE => "use"
    | VAR => "var"
    | VOID => "void"
    | WHILE => "while"
    | WITH => "with"

    (* contextually reserved identifiers *)

    | CALL => "call"
    | DEBUGGER => "debugger"
    | DECIMAL => "decimal"
    | DOUBLE => "double"
    | DYNAMIC => "dynamic"
    | EACH => "each"
    | FINAL => "final"
    | GET => "get"
    | GOTO => "goto"
    | INCLUDE => "include"
    | INT => "int"
    | NAMESPACE => "namespace"
    | NATIVE => "native"
    | NUMBER => "number"
    | OVERRIDE => "override"
    | PROTOTYPE => "prototype"
    | ROUNDING => "rounding"
    | STANDARD => "standard"
    | STRICT => "strict"
    | UINT => "uint"
    | SET => "set"
    | STATIC => "static"
    | TYPE => "type"
    | XML => "xml"
    | YIELD => "yield"

    (* literals *)

    | ATTRIBUTEIDENTIFIER => "@id"
    | BLOCKCOMMENT => ""
    | DOCCOMMENT => ""
    | EOL => "eol"
    | IDENTIFIER x => "identifier("^x^")"
    | NUMBERLITERAL x => Real.toString(x)
    | PACKAGEIDENTIFIER => "packageidentifier(x)"
    | REGEXPLITERAL => "regexp(x)"
    | SLASHSLASHCOMMENT => ""
    | STRINGLITERAL x => "string("^x^")"
    | WHITESPACE => "<ws>"
    | XMLLITERAL => "xmlliteral()"
    | XMLPART => "xmlpart()"
    | XMLMARKUP => "xmlmarkup()"
    | XMLTEXT => "xmltext()"
    | XMLTAGENDEND => "xmltagendend()"
    | XMLTAGSTARTEND => "xmltagstartend()"

	(* meta tokens *)

    | ERROR => "error"
    | LEXBREAK_DIV x => "lexbreak_div"
    | LEXBREAK_DIVASSIGN x => "lexbreak_divassign"
    | LEXBREAK_LESSTHAN x => "lexbreak_lessthan"
    | EOF => "eof"

end
