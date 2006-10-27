structure Parser = struct

open Token

exception ParseError

fun log ss = 
    (TextIO.print "log: "; 
     List.app TextIO.print ss; 
     TextIO.print "\n")

(*

Identifier      
        Identifier
        ContextuallyReservedIdentifier

PropertyIdentifier ->
        Identifier
        *

Qualifier
        ReservedNamespace
        PropertyIdentifier

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

fun builtinNSQualifiedIdentifier ns ts = 
    let
        val (ts2, ident) = simpleQualifiedIdentifierPrime ts
        val qual = SOME (Ast.Expr 
                             (Ast.LiteralExpr 
                                  (Ast.LiteralNamespace ns)))
        val node = Ast.QualIdent { lhs = qual,
                                   rhs = ident,
                                   openNamespaces = [] }
    in
        (node, ts2)
    end

and simpleQualifiedIdentifierPrime (LEFTBRACKET :: ts) = raise ParseError
  | simpleQualifiedIdentifierPrime ((IDENTIFIER id) :: ts) = (ts, Ast.Ident id)
  | simpleQualifiedIdentifierPrime (MULT :: ts) = (ts, Ast.Ident "*")
  | simpleQualifiedIdentifierPrime _ = raise ParseError

 (* Are ReservedIdentifiers actually supposed to be legal here too?
    It looks like a bug in the grammar to me -- graydon *)
        

and simpleQualifiedIdentifier (PUBLIC :: DOUBLECOLON :: ts) = 
    builtinNSQualifiedIdentifier Ast.PUBLIC ts
    
  | simpleQualifiedIdentifier (PRIVATE :: DOUBLECOLON :: ts) = 
    builtinNSQualifiedIdentifier Ast.PRIVATE ts
      
  | simpleQualifiedIdentifier (PROTECTED :: DOUBLECOLON :: ts) = 
    builtinNSQualifiedIdentifier Ast.PROTECTED ts

  | simpleQualifiedIdentifier (INTERNAL :: DOUBLECOLON :: ts) = 
    builtinNSQualifiedIdentifier Ast.INTERNAL ts

  | simpleQualifiedIdentifier ((IDENTIFIER id) :: DOUBLECOLON :: ts) =
    let 
        fun litNS n = Ast.LiteralExpr (Ast.LiteralNamespace n) 
        val (ts2, ie) = simpleQualifiedIdentifierPrime ts 
        val node = Ast.QualIdent { lhs = SOME (Ast.Ident id),
                                   rhs = ie,
                                   openNamespaces = [] }
    in
        (node, ts2)
    end

  | simpleQualifiedIdentifier _ = raise ParseError


and program ts =
    case ts of
        PACKAGE :: tr => packageDefinition tr
      | _             => simpleQualifiedIdentifier ts

(*

*)

and packageDefinition ts = raise ParseError

(*

*)

and directives ts = raise ParseError


fun mkReader filename = 
    let
        val stream = TextIO.openIn filename
    in
        fn _ => case TextIO.inputLine stream of
                    SOME line => (log ["read line ", line]; line)
                  | NONE => ""
    end

fun lexFile (filename : string) : (token list) = 
    let 
        val tmp = ref []
        val lexer = Lexer.makeLexer (mkReader filename)
        fun step _ = case lexer () of 
                         EMPTY => (tmp := EMPTY :: (!tmp); List.rev (!tmp))
		       | other => (tmp := other :: (!tmp); step ())
	val result = step ()
		     handle Lexer.LexError => (log ["lex error"]; 
					       raise Lexer.LexError)
    in
	log ["lexed ", filename];
	result
    end

fun parse ts =
    case (program ts) of
        (result, [EOL, EMPTY]) => result
      | _  => raise ParseError

fun parseFile filename = 
    (log ["scanning ", filename];
     (parse (lexFile filename)
      handle ParseError => (log ["parse error"]; 
			    raise ParseError));
     log ["parsed ", filename])


end (* Parser *)
