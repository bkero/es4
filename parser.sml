structure Parser = struct

(* Terminal symbols *)

datatype 
    node = 
        Node
      | IdentifierNode of { name: string }
      | NamespaceNode of { name : string }
      | QualifiedIdentifierNode of { ns : node, ident : node }

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
    | NUMBERLITERAL
    | PACKAGEIDENTIFIER
    | REGEXPLITERAL
    | SLASHSLASHCOMMENT
    | STRINGLITERAL
    | WHITESPACE
    | XMLLITERAL
    | XMLPART
    | XMLMARKUP
    | XMLTEXT
    | XMLTAGENDEND
    | XMLTAGSTARTEND

    (* meta *)

    | EMPTY
    | ERROR

    (* end token definitions *)

(* Scanner *)

exception ScanError of string

(* Character classes *)

fun isblank c = (c = #" " orelse c = #"\t" orelse c = #"\n")
fun isdigit c = (#"0" <= c andalso c <= #"9")
fun islower c = (#"a" <= c andalso c <= #"z")
fun isupper c = (#"A" <= c andalso c <= #"Z")
fun isletter c = (islower c orelse isupper c)
fun isletterdigit c = (isletter c orelse isdigit c)

(* Token classes *)

fun isreserved tk = ( tk = AS )

fun tokenname tk = 
    case tk of 
        AS => "as"
      | IDENTIFIER name => name
      | DOUBLECOLON => "::"
      | _ => raise ScanError "invalid name in tokenname"

fun scanname (cs, value) =
    case cs of
        c :: cr => if isletterdigit c then scanname(cr, c :: value)
                   else (cs, implode (rev value))
      | []      => (cs, implode (rev value))


fun scan s =
    let fun sc cs =
        case cs of
            [] => []
          | #"-" :: cr => MINUS :: sc cr
          | #":" :: #":" :: cr => DOUBLECOLON :: sc cr
          | #"1" :: cr => NUMBERLITERAL :: sc cr
          | c :: cr => if isblank c then sc cr
                       else if isletter c then
                           let 
                               val (cs1, n) = scanname(cr,[c])
                           in
                               case n of
                                   "private" => PRIVATE :: sc cs1
                                 | _         => IDENTIFIER n :: sc cs1
                           end
                       else (print ("invalid character in " ^ (implode cs)); raise ScanError "invalid character")
    in sc (explode s) end

(* Parser *)

exception ParseError

(*

Identifier	
	Identifier
	ContextuallyReservedIdentifier
*)

fun Identifier ts =
    case ts of
        CALL :: tr        => (tr,IdentifierNode {name="call"})
      | DEBUGGER :: tr    => (tr,Node)
      | DECIMAL :: tr     => (tr,Node)
      | DOUBLE :: tr      => (tr,Node)
      | DYNAMIC :: tr     => (tr,Node)
      | EACH :: tr        => (tr,Node)
      | FINAL :: tr       => (tr,Node)
      | GET :: tr         => (tr,Node)
      | GOTO :: tr        => (tr,Node)
      | INCLUDE :: tr     => (tr,Node)
      | INT :: tr         => (tr,Node)
      | NAMESPACE :: tr   => (tr,Node)
      | NATIVE :: tr      => (tr,Node)
      | NUMBER :: tr      => (tr,Node)
      | OVERRIDE :: tr    => (tr,Node)
      | PROTOTYPE :: tr   => (tr,Node)
      | ROUNDING :: tr    => (tr,Node)
      | STANDARD :: tr    => (tr,Node)
      | STRICT :: tr      => (tr,Node)
      | UINT :: tr        => (tr,Node)
      | SET :: tr         => (tr,Node)
      | STATIC :: tr      => (tr,Node)
      | TYPE :: tr        => (tr,Node)
      | XML :: tr         => (tr,Node)
      | YIELD :: tr       => (tr,Node)
      | IDENTIFIER(nm) :: tr  => (tr,IdentifierNode {name=nm})
      | _                 => raise ParseError

(*
    PropertyIdentifier ->
        Identifier
        *
*)

and PropertyIdentifier ts =
    case ts of
        MULT :: tr => (tr,Node)
      | _          => Identifier ts

(*
    Qualifier
        ReservedNamespace
        PropertyIdentifier
*)

and Qualifier ts =
    case ts of
        (INTERNAL :: _ | INTRINSIC :: _ | PRIVATE :: _ | PROTECTED :: _ | PUBLIC :: _) => 
            ReservedNamespace ts
      | _ =>
            PropertyIdentifier ts

and ReservedNamespace ts =
    case ts of
        INTERNAL :: tr   => (tr, NamespaceNode {name="internal"})
      | INTRINSIC :: tr  => (tr, NamespaceNode {name="intrinsic"})
      | PRIVATE :: tr    => (tr, NamespaceNode {name="private"})
      | PROTECTED :: tr  => (tr, NamespaceNode {name="protected"})
      | PUBLIC :: tr     => (tr, NamespaceNode {name="public"})
      | _                => raise ParseError

(*

SimpleQualifiedIdentifier	
	PropertyIdentifier
	Qualifier  ::  PropertyIdentifier
	Qualifier  ::  ReservedIdentifier
	Qualifier  ::  Brackets

left factored: 

SimpleQualifiedIdentifier
    ReservedNamespace :: SimpleQualifiedIdentifierPrime
    PropertyIdentifier :: SimpleQualifiedIdentifierPrime
    PropertyIdentifier

SimpleQualifiedIdentifierPrime
    PropertyIdentifier
    ReservedIdentifier
    Brackets

*)

and SimpleQualifiedIdentifier ts =
    case ts of
        (INTERNAL :: _ | INTRINSIC :: _ | PRIVATE :: _ | PROTECTED :: _ | PUBLIC :: _) => 
            let 
                val (ts1, nd1) = ReservedNamespace(ts)
                val (ts2, nd2) = if hd ts1 = DOUBLECOLON then SimpleQualifiedIdentifierPrime(tl ts1, nd1) 
                                 else raise ParseError
            in
                (ts2, nd2)
            end
      | _ => 
            let
                val (ts1, nd1) = PropertyIdentifier(ts)
                val (ts2, nd2) = if hd ts1 = DOUBLECOLON then SimpleQualifiedIdentifierPrime(tl ts1, nd1) 
                                 else (ts1, nd1)
            in
                (ts2, nd2)
            end

and SimpleQualifiedIdentifierPrime (ts, nd1:node) =
    case ts of
        LEFTBRACKET :: _ => (ts,Node) (* Brackets ts *)
      | tk  :: ts1 => 
            let
                val (ts2,nd2) =	if isreserved(tk) then (ts1, IdentifierNode {name=(tokenname tk)})
                                else PropertyIdentifier(ts)
            in
                (ts2,QualifiedIdentifierNode({ns=nd1, ident=nd2}) )
            end

(*

*)


and Program ts =
    case ts of
        PACKAGE :: tr => PackageDefinition tr
      | _             => SimpleQualifiedIdentifier ts

(*

*)

and PackageDefinition ts = let in (ts,Node) end

(*

*)

and Directives ts = let in (ts,Node) end

fun parse ts =
    case (Program ts) of
        ([],result) => result
      | _  => raise ParseError

(* File I/O *)

fun readfile ( filename : string ) : string =
    let open TextIO
        val infile = openIn filename
        val str = inputAll infile
    in closeIn infile; str end

(* Tester *)

exception TestError

fun test n =
    case n of
        0 => parse (scan ("foo::bar"))
      | 1 => parse (scan ("foo::call"))
      | 2 => parse (scan ("private::call"))
      | _ => raise TestError

end (* Parser *)