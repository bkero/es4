(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Parser = struct

open Token

exception ParseError

datatype alpha =
    ALLOWLIST
  | NOLIST

datatype beta =
    ALLOWIN
  | NOIN

datatype gamma =
    ALLOWEXPR
  | NOEXPR

datatype omega =
    ABBREV
  | NOSHORTIF
  | FULL

datatype tau =
    GLOBAL
  | CLASS
  | INTERFACE
  | LOCAL

(*

    PATTERNS to BINDINGS to FIXTURES
    EXPRS to INITS
*)

datatype PATTERN = 
         ObjectPattern of FIELD_PATTERN list
       | ArrayPattern of PATTERN list
       | SimplePattern of Ast.EXPR
       | IdentifierPattern of Ast.IDENT

withtype FIELD_PATTERN =
         { ident: Ast.IDENT_EXPR, 
           pattern : PATTERN }
         
type PATTERN_BINDING_PART =
     { kind:Ast.VAR_DEFN_TAG,
       ty:Ast.TYPE_EXPR,
       prototype:bool,
       static:bool }



fun log ss = 
    (TextIO.print "log: "; 
     List.app TextIO.print ss;
     TextIO.print "\n")

val trace_on = true

fun trace ss =
    if trace_on then log ss else ()

val currentClassName : Ast.IDENT ref = ref ""

fun error ss =
    (log ("*syntax error: " :: ss); raise ParseError)

fun newline ts = Lexer.UserDeclarations.followsLineBreak ts

val defaultNamespace : Ast.EXPR list ref = ref [Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Internal ""))]  (* todo: implement dns pragma *)

val defaultAttrs = 
    { 
        ns = Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Internal "")),
        override = false,
           static = false,
        final = false,
           dynamic = false,
        prototype = false,
        native = false,
        rest = false }

val defaultRestAttrs = 
    { 
        ns = Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Internal "")),
        override = false,
           static = false,
        final = false,
           dynamic = false,
        prototype = false,
        native = false,
        rest = true }

(*
    PATTERN

    Patterns in binding contexts (e.g. after 'var') cause fixtures
    to be created. Other patterns de-sugar into assignment expressions
    with the value of the right side stored in a temporary to avoid
    multiple evaluation.

    When the definition phase is complete, the only kind of patterns
    that remain are SimplePatterns which are wrappers for the property
    references that are the targets of assignments

    Example:

        ns var {i:x,j:y} : {i:int,j:string} = o

    gets rewritten by the parser as,

        ns var {i:x,j:y} : {i:int,j:string}
        ns <init> {i:x,j:y} = o

    which gets rewritten by 'defBinding' and 'defInit' as,

        ns var x:int
        ns var y:string

    and 

        <temp> t = o
        ns::x = t["i"]
        ns::y = t["j"]

    respectively, where the name shown as 't' is guaranteed not to 
    conflict with or shadow any other name in scope.

    Example:

        [ns::x, ns::y] = o

    gets rewriten by 'defAssignment' as,

        <temp> t = o
        ns::x = t[0]
        ns::y = t[1]

    Note: the difference between defineAssignment and defInit is that
    defInit creates qualified identifiers using the namespace and the
    identifiers on the left side of each assignment.
*)

(* 
 * Patterns get desugared into FIXTURES (covering all temporaries) and
 * lists of INIT_STEPs.
 * 
 * Depending on context, the INIT_STEPs might be restricted to 
 * initializing single idents, thus of the form "Init (...)". This is
 * true of patterns in 4 contexts:
 * 
 *   - function parameters
 *   - class member initializers (including proto and static inits)
 *   - class constructor settings
 *   - variable binding forms like "var" and "let" in imperative blocks
 * 
 * In other contexts, a pattern represents just a sequence of general
 * assignments, and any expression that can evaluate to an object ref
 * is valid for the pattern LHS. In these cases the desugared form is
 * Assign (e1,e2) and e1 must be a "ref expr" (see eval.sml).
 *
 * INIT_STEP is required as input to the definer so that the target of
 * the initialisation can be known. The definer turns on INIT_STEPS into
 * INITS or STMTS.
 *)


(****

fun needInitSteps (pl:Ast.INIT_STEP list)
                  (targ:Ast.INIT_TARGET)
    : Ast.INITS =
    case pl of
        [] => []
      | (Ast.AssignStep _)::xs => LogErr.defnError ["deep pattern assignment ",
                                            "in binding-initialization context"]
      | (Ast.InitStep (t, f, e))::xs =>
        if t = targ
        then (f, e)::(needInitSteps pl targ xs)
        else LogErr.defnError ["unexpected initialization target ",
                               "in binding-initialization context"]
and defInit (env:ENV)
            (ns:Ast.NAMESPACE)
            (prototype:bool)
            (static:bool)
            (init:Ast.EXPR)
    : Ast.EXPR list =
    let
    in case init of
        (Ast.SetExpr (_,pattern,expr)) =>
            defPatternAssign env ns pattern expr prototype static 0
      | _ => LogErr.defnError ["internal definition error in defInit"]
    end

and defAssignment (env:ENV)
                  (pattern:PATTERN)
                  (expr:Ast.EXPR)
    : Ast.EXPR list =
    let
        val level = 0  (* to start with *)
        val ns = Ast.Intrinsic  (* unused since there are no IdentifierPatterns in this context *)
    in
        defPatternAssign env ns pattern expr false false level
    end
****)

(*
    var x : int = 10
    
    Binding {ident="x",ty=int}
    InitStep ("x",10)

    Turn a pattern, type and initialiser into lists of bindings, inits, and statements.
    The outer defn will wrap both with the attributes

    
*)
                            
fun desugarPattern (pattern:PATTERN)
                   (ty:Ast.TYPE_EXPR option)
                   (expr:Ast.EXPR option)
    : (Ast.BINDING list * Ast.INIT_STEP list) =
    let
        fun desugarIdentifierPattern (id:Ast.IDENT)
            : (Ast.BINDING list * Ast.INIT_STEP list) =

            (*

                Binding (id,ty)
                InitStep (id,expr)
                
            *)

            let
                val ident = Ast.PropIdent id
                val bind = Ast.Binding {ident=ident,ty=ty}
            in case expr of
                SOME e => ([bind],[Ast.InitStep (ident,e)])
              | NONE => ([bind],[])
            end

        fun desugarSimplePattern (patternExpr:Ast.EXPR)
            : (Ast.BINDING list * Ast.INIT_STEP list) =

            (*
                [],
                AssignStep (patternExpr,expr)
                
            *)

            let
            in case expr of
                SOME e => ([],[Ast.AssignStep (patternExpr,e)])
              | NONE => error ["simple pattern without initialiser"]
            end
                
        (*
            [x,y] = o
            let [i,s]:[int,String] = o

            ISSUE: Are partial type annotations allowed? let [i,s]:[int]=...
                   If so, what is the type of 's' here? int or *?
        *)

        fun desugarArrayPattern (element_ptrns: PATTERN list) 
                                (element_types: Ast.TYPE_EXPR option)
                                (temp:Ast.EXPR) 
                                (n:int)
            : (Ast.BINDING list * Ast.INIT_STEP list) =
            let
            in case element_ptrns of
                p::plist =>
                    let
                        val id = Int.toString n
                        val str = Ast.LiteralString id
                        val ident = Ast.ExpressionIdentifier (Ast.LiteralExpr (str))
                        val e = SOME (Ast.ObjectRef {base=temp, ident=ident})
                    in case element_types of
                        SOME ty =>
                            let
                                val t = SOME (Ast.TypeRef (ty,id))
                                val (binds, inits) = desugarPattern p t e
                                val (binds', inits') = desugarArrayPattern plist element_types temp (n+1)
                             in
                                ((binds @ binds'), (inits @ inits'))
                             end
                      | _ =>
                            let
                                val (binds, inits) = desugarPattern p (SOME (Ast.SpecialType Ast.Any)) e
                                val (binds', inits') = desugarArrayPattern plist element_types temp (n+1)  
                            in
                                ((binds @ binds'), (inits @ inits'))
                            end
                    end
              | [] => ([], [])
            end

        
        (*
                {i:x,j:y}:{i:t,j:u} = o

            becomes

                <temp> tmp = o
                Bind x,t
                Init x = tmp["i"]
                Bind y,u
                Init y = tmp["j"]
        *)
            
        fun desugarObjectPattern (field_ptrns:FIELD_PATTERN list)
                                 (field_types:Ast.TYPE_EXPR option) 
                                 (temp:Ast.EXPR)
            : (Ast.BINDING list * Ast.INIT_STEP list) =
            case field_ptrns of
                fp::fps =>
                    let
                        val (binds,inits) = desugarFieldPattern fp field_types temp
                        val (binds',inits') = desugarObjectPattern fps field_types temp
                    in
                        ((binds @ binds'), (inits @ inits'))
                    end
              | [] => ([],[])  
            
        (*
            Use the field name to get the field type and associate that field type 
            with the field's pattern. Deference the given expression with the field
            name to get the value of the field.
        *)

        and desugarFieldPattern (field_pattern: FIELD_PATTERN) 
                                (field_types: Ast.TYPE_EXPR option)
                                (temp: Ast.EXPR)
            : (Ast.BINDING list * Ast.INIT_STEP list) =
            let
                val {ident,pattern=p} = field_pattern
            in
                case (field_types,ident) of
                    (SOME ty, Ast.Identifier {ident=id,...}) =>
                        (* if the field pattern is typed, it must have a identifier for
                           its name so we can do the mapping to its field type *)
                        let
                            val t = SOME (Ast.TypeRef (ty,id))
                            val e = SOME (Ast.ObjectRef {base=temp, ident=ident})
                        in
                            desugarPattern p t e
                        end
                  | (_,_) =>
                        let
                            val t = SOME (Ast.SpecialType Ast.Any)
                            val e = SOME (Ast.ObjectRef {base=temp, ident=ident})
                        in
                            desugarPattern p t e
                        end
            end

    in 
        case (pattern,expr) of
            (SimplePattern expr,_) => desugarSimplePattern expr
          | (IdentifierPattern id,_) => desugarIdentifierPattern id
          | (_,_) =>
                let
                    val e = case expr of SOME e => e | _ => (Ast.GetTemp 0) (* FIXME: get arg *)
                    val temp_counter = ref 0
                    val temp_n = !temp_counter
                    val temp_id = Ast.TempIdent temp_n
                    val temp_binding = Ast.Binding {ident=temp_id,ty=ty}
                    val temp_step = Ast.InitStep (temp_id, e)
                    val temp_expr = Ast.GetTemp temp_n                        
                in case pattern of
                    ObjectPattern fields =>
                        let
                           val (bindings, steps) = desugarObjectPattern fields ty temp_expr
                        in
                           (temp_binding::bindings, temp_step::steps)
                        end
                  | ArrayPattern elements => 
                        let
                            val temp_index = 0
                            val (bindings, steps) = desugarArrayPattern elements ty temp_expr temp_index
                        in
                            (temp_binding::bindings, temp_step::steps)
                        end
                  | _ => error ["won't get here"]
                end
    end

(*

Identifier    
    Identifier
    ContextuallyReservedIdentifier
*)

and identifier ts =
    let
    in case ts of
        Identifier(str) :: tr => (tr,str)
      | Call :: tr => (tr,"call")
      | Construct :: tr => (tr,"construct")
      | Debugger :: tr => (tr,"debugger")
      | Decimal :: tr => (tr,"decimal")
      | Double :: tr => (tr,"double")
      | Dynamic :: tr => (tr,"dynamic")
      | Each :: tr => (tr,"each")
      | Final :: tr => (tr,"final")
      | Get :: tr => (tr,"get")
      | Goto :: tr => (tr,"goto")
      | Include :: tr => (tr,"include")
      | Int :: tr => (tr,"int")
      | Namespace :: tr => (tr,"namespace")
      | Native :: tr => (tr,"native")
      | Number :: tr => (tr,"number")
      | Override :: tr => (tr,"override")
      | Precision :: tr => (tr,"precision")
      | Prototype :: tr => (tr,"prototype")
      | Rounding :: tr => (tr,"rounding")
      | Standard :: tr => (tr,"standard")
      | Strict :: tr => (tr,"strict")
      | UInt :: tr => (tr,"uint")
      | Set :: tr => (tr,"set")
      | Static :: tr => (tr,"static")
      | Type :: tr => (tr,"type")
      | Undefined :: tr => (tr,"undefined")
      | Xml :: tr => (tr,"xml")
      | Yield :: tr => (tr,"yield")
      | _ => error(["expecting 'identifier' before '",tokenname(hd ts),"'"])
    end

(*
    PropertyIdentifier ->
        Identifier
        *
*)

and propertyIdentifier ts =
    let 
        val _ = trace([">> propertyIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        Mult :: tr => (tr,"*")
      | _ => 
            let
                val (ts1,nd1) = identifier ts
            in
                (trace(["<< propertyIdentifier with next=",tokenname(hd(ts1))]);(ts1,nd1)) 
            end
    end

(*
    Qualifier
        ReservedNamespace
        PropertyIdentifier
*)

and qualifier ts =
    let
    in case ts of
        (Internal :: _ | Intrinsic :: _ | Private :: _ | Protected :: _ | Public :: _) => 
          let
              val (ts1,nd1) = reservedNamespace (ts)
          in
              (ts1,Ast.LiteralExpr(Ast.LiteralNamespace nd1))
          end
      | _ => 
          let
              val (ts1,nd1) = propertyIdentifier (ts)
          in
              (ts1,Ast.LexicalRef{ident=Ast.Identifier {ident=nd1, openNamespaces=[]}})
          end
    end

and reservedNamespace ts =
    let val _ = trace([">> reservedNamespace with next=",tokenname(hd(ts))])
    in case ts of
        Internal :: tr => 
            (tr, Ast.Internal "")
      | Intrinsic :: tr => 
            (tr, Ast.Intrinsic)
      | Private :: tr => 
            (tr, Ast.Private "class name here")
      | Protected :: tr => 
            (tr, Ast.Protected "class name here")
      | Public :: tr => 
            (tr, Ast.Public "")
      | _ => raise ParseError
    end

(*
    SimpleQualifiedIdentifier    
        PropertyIdentifier
        Qualifier  ::  PropertyIdentifier
        Qualifier  ::  ReservedIdentifier
        Qualifier  ::  Brackets

    ExpressionQualifiedIdentifer    
        ParenListExpression  ::  PropertyIdentifier
        ParenListExpression  ::  ReservedIdentifier
        ParenListExpression  ::  Brackets

    left factored: 

    SimpleQualifiedIdentifier
        ReservedNamespace :: QualifiedIdentifierPrime
        PropertyIdentifier :: QualifiedIdentifierPrime
        PropertyIdentifier

    ExpressionQualifiedIdentifer    
        ParenListExpression  ::  QualifiedIdentifierPrime

    QualifiedIdentifierPrime
        PropertyIdentifier
        ReservedIdentifier
        Brackets
*)

and simpleQualifiedIdentifier ts =
    let val _ = trace([">> simpleQualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        (Internal :: _ | Intrinsic :: _ | Private :: _ | Protected :: _ | Public :: _) => 
          let 
              val (ts1, nd1) = reservedNamespace(ts)
          in case ts1 of
              DoubleColon :: ts2 => qualifiedIdentifier'(ts2,Ast.LiteralExpr(Ast.LiteralNamespace nd1))
            | _ => raise ParseError
          end
      | _ => 
              let
                  val (ts1, nd1) = propertyIdentifier(ts)
                val id = Ast.Identifier {ident=nd1, openNamespaces=[]}
              in case ts1 of
                  DoubleColon :: _ => 
                    qualifiedIdentifier'(tl ts1,Ast.LexicalRef ({ident=id}))
              | _ => 
                    (trace(["<< simpleQualifiedIdentifier with next=",tokenname(hd(ts1))]);
                    (ts1,id))
          end
    end

and expressionQualifiedIdentifier (ts) =
    let 
        val (ts1,nd1) = parenListExpression(ts)
    in case ts1 of
        DoubleColon :: _ => 
            let
                val (ts2,nd2) = qualifiedIdentifier'(tl ts1,nd1) 
(* todo: make qualifier be an EXPR list *) 
            in
                (ts2,nd2)
            end

      | _ => raise ParseError
    end

and reservedOrPropertyIdentifier ts =
    case isreserved(hd ts) of
        true => (tl ts, tokenname(hd ts))
      | false => propertyIdentifier(ts)

and reservedIdentifier ts =
    case isreserved(hd ts) of
        true => (tl ts, tokenname(hd ts))
      | false => raise ParseError

and qualifiedIdentifier' (ts1, nd1) : (token list * Ast.IDENT_EXPR) =
    let val _ = trace([">> qualifiedIdentifier' with next=",tokenname(hd(ts1))]) 
    in case ts1 of
        LeftBracket :: ts => 
            let
                val (ts2,nd2) = brackets (ts1)
                val (ts3,nd3) = (ts2,Ast.QualifiedExpression({qual=nd1,expr=nd2}))

            in
                (ts3,nd3)
            end
      | tk :: ts =>
            let
                val (ts2,nd2) = reservedOrPropertyIdentifier(ts1)
                val qid = Ast.QualifiedIdentifier({qual=nd1, ident=nd2})
                val (ts3,nd3) = (ts2,qid)
            in
                (ts3,nd3)
            end
      | _ => raise ParseError
    end

(*
    NonAttributeQualifiedIdentifier    
        SimpleQualifiedIdentifier
        ExpressionQualifiedIdentifier
*)

and nonAttributeQualifiedIdentifier ts =
    let val _ = trace([">> nonAttributeQualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftParen :: _ => expressionQualifiedIdentifier(ts)
      | _ => simpleQualifiedIdentifier(ts)
    end

(*
    AttributeIdentifier    
        @  Brackets
        @  NonAttributeQualifiedIdentifier
*)

and attributeIdentifier ts =
    let val _ = trace([">> attributeIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        At :: LeftBracket :: _ => 
            let
                val (ts1,nd1) = brackets(tl ts)
            in
                (ts1,Ast.AttributeIdentifier (Ast.ExpressionIdentifier nd1))
            end
      | At :: _ => 
            let
                val (ts1,nd1) = nonAttributeQualifiedIdentifier(tl ts)
            in
                (ts1,Ast.AttributeIdentifier nd1)
            end
      | _ => 
            raise ParseError
    end

(*

    QualifiedIdentifier    
        AttributeIdentifier
        NonAttributeQualifiedIdentifier

*)

and qualifiedIdentifier ts =
    let val _ = trace([">> qualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        At :: _ => attributeIdentifier(ts)
      | _ => nonAttributeQualifiedIdentifier(ts)
    end

(*
    SimpleTypeIdentifier    
        PackageIdentifier  .  Identifier
        NonAttributeQualifiedIdentifier
*)

and simpleTypeIdentifier ts =
    let val _ = trace([">> simpleTypeIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        PackageIdentifier p :: Dot :: ts1 => 
            let val (ts2,nd2) = identifier(ts1) 
                val nd' = Ast.QualifiedIdentifier(
                           {qual=Ast.LiteralExpr(Ast.LiteralNamespace(Ast.Public(p))),
                            ident=nd2})
            in 
                (ts2,nd') 
            end
      | _ => nonAttributeQualifiedIdentifier(ts)
    end

(*
    TypeIdentifier    
        SimpleTypeIdentifier
        SimpleTypeIdentifier  .<  TypeExpressionList  >
*)

and typeIdentifier ts =
    let val _ = trace([">> typeIdentifier with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = simpleTypeIdentifier ts
    in case ts1 of
        LeftDotAngle :: _ => 
            let
                val (ts2,nd2) = typeExpressionList (tl ts1)
            in case ts2 of
                GreaterThan :: _ =>
                    (trace(["<< typeIdentifier with next=",tokenname(hd(tl ts2))]); 
                    (tl ts2,Ast.TypeIdentifier {ident=nd1,typeParams=nd2}))
              | _ => raise ParseError
            end
      | _ =>
            (trace(["<< typeIdentifier with next=",tokenname(hd(ts1))]); 
            (ts1, nd1))
    end

(*
    ParenExpression    
        (  AssignmentExpressionallowLet, allowIn  )
*)

and parenExpression ts =
    let val _ = trace([">> parenExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftParen :: ts1 => 
            let
                val (ts2,nd2:Ast.EXPR) = assignmentExpression (ts1,ALLOWLIST,ALLOWIN)
            in case ts2 of
                RightParen :: ts3 => (ts3,nd2)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    ParenListExpression    
        (  ListExpression(ALLOWIN)  )
*)

and parenListExpression (ts) : (token list * Ast.EXPR) =
    let val _ = trace([">> parenListExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftParen :: _ => 
            let
                val (ts1,nd1) = listExpression (tl ts,ALLOWIN)
            in case ts1 of
                RightParen :: _ => 
                    (trace(["<< parenListExpression with next=",tokenname(hd(ts1))]);
                    (tl ts1,nd1))
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    FunctionExpression(allowList, b)    
        function  FunctionSignature  Block
        function  FunctionSignature  ListExpressionb
        function  Identifier  FunctionSignature  Block
        function  Identifier  FunctionSignature  ListExpressionb
        
    FunctionExpression(noList, b)
        function  FunctionSignature  Block
        function  Identifier  FunctionSignature  Block
*)

and functionExpression (ts,a:alpha,b:beta) =
    let val _ = trace([">> functionExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        Function :: ts1 => 
            let
            in case ts1 of
                (LeftDotAngle | LeftParen) :: _ => 
                    let
                        val (ts3,nd3) = functionSignature ts1
                    in case (ts3,a) of
                        (LeftBrace :: _,_) => 
                            let
                                val (ts4,nd4) = block (ts3,LOCAL)
                            in
                                (ts4,Ast.LiteralExpr 
                                         (Ast.LiteralFunction 
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=""},
                                                         fsig=nd3,
                                                         block=nd4,
                                                         defaults=[],
                                                         param=([],[]),
                                                         ty=functionTypeFromSignature nd3})))
                            end
                      | (_,ALLOWLIST) => 
                            let
                                val (ts4,nd4) = listExpression (ts3,b)
                            in
                                (ts4,Ast.LiteralExpr 
                                         (Ast.LiteralFunction 
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=""},
                                                         fsig=nd3,
                                                         block=Ast.Block {pragmas=[],
                                                                          defns=[],
                                                                          body=[Ast.ReturnStmt nd4],
                                                                          head=NONE},
                                                         param=([],[]),
                                                         defaults=[],
                                                         ty=functionTypeFromSignature nd3})))
                                
                            end
                      | _ => raise ParseError
                    end
              | _ => 
                    let
                        val (ts2,nd2) = identifier ts1
                        val (ts3,nd3) = functionSignature ts2
                    in case (ts3,a) of
                        (LeftBrace :: _,_) => 
                            let
                                val (ts4,nd4) = block (ts3,LOCAL)
                            in
                                (ts4,Ast.LiteralExpr 
                                         (Ast.LiteralFunction 
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=nd2},
                                                         fsig=nd3,
                                                         block=nd4,
                                                         param=([],[]),
                                                         defaults=[],
                                                         ty=functionTypeFromSignature nd3})))
                            end
                      | (_,ALLOWLIST) => 
                            let
                                val (ts4,nd4) = listExpression (ts3,b)
                            in
                                (ts4,Ast.LiteralExpr 
                                         (Ast.LiteralFunction 
                                              (Ast.Func 
                                                   {name={kind=Ast.Ordinary,ident=nd2},
                                                    fsig=nd3,
                                                    block=Ast.Block 
                                                              {pragmas=[],
                                                               defns=[],
                                                               body=[Ast.ReturnStmt nd4],
                                                               head=NONE },
                                                    param=([],[]),
                                                    defaults=[],
                                                    ty=functionTypeFromSignature nd3})))
                            end
                      | _ => raise ParseError
                    end
            end
      | _ => raise ParseError
    end

(*
    FunctionSignature    
        TypeParameters  (  Parameters  )  ResultType
        TypeParameters  (  this  :  TypeIdentifier  ,  Parameters  )  ResultType
*)

and needType (nd:Ast.IDENT_EXPR,nullable:bool option) = 
    case nd of
        Ast.Identifier {ident,...} =>
            if (ident="*")
                then Ast.SpecialType Ast.Any
                else if( ident="Object" )  (* FIXME: check for *the* object name *)
                    then Ast.TypeName nd
                    else Ast.TypeName nd
      | _ => Ast.TypeName nd
    
and functionSignature (ts) : (token list * Ast.FUNC_SIG) =
    let val _ = trace([">> functionSignature with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = typeParameters ts
    in case ts1 of
        LeftParen :: This :: Colon ::  _ =>
            let
                val (ts2,nd2) = typeIdentifier (tl (tl (tl ts1)))
            in case ts2 of
                Comma :: _ =>
                    let
                           val (ts3,nd3) = nonemptyParameters (tl ts2)  
                       in case ts3 of
                           RightParen :: _ =>
                               let
                                   val (ts4,nd4) = resultType (tl ts3)
                               in
                                (log(["<< functionSignature with next=",tokenname(hd ts4)]);
                                (ts4,Ast.FunctionSignature
                                     {typeParams=nd1,
                                      thisType=SOME (needType (nd2,SOME false)),
                                      params=nd3,
                                      returnType=nd4,
                                      settings=NONE,
                                      hasRest=false })) (* do we need this *)
                               end
                         | _ => raise ParseError
                    end
                 | RightParen :: _ =>
                       let
                              val (ts3,nd3) = resultType (tl ts2)
                          in
                        (log(["<< functionSignature with next=",tokenname(hd ts3)]);
                        (ts3,Ast.FunctionSignature
                                { typeParams=nd1,
                                  thisType=SOME (needType (nd2,SOME false)),
                                  params=([],[]),
                                  returnType=nd3,
                                  settings=NONE,
                                  hasRest=false })) (* do we need this *)
                          end
                 | _ => raise ParseError
            end
      | LeftParen :: _ =>
               let
                   val (ts2, nd2) = parameters (tl ts1)
               in case ts2 of
                   RightParen :: _ =>
                       let
                           val (ts3,nd3) = resultType (tl ts2)
                       in
                        (log(["<< functionSignature with next=",tokenname(hd ts3)]);
                        (ts3,Ast.FunctionSignature
                                    {typeParams=nd1,
                                     params=nd2,
                                     returnType=nd3,
                                     settings=NONE,
                                     thisType=NONE,  (* todo *)
                                     hasRest=false })) (* do we need this *)
                       end
                 | _ => raise ParseError
            end
      | _ => raise ParseError
    end

and functionSignatureType (ts) =
    let val _ = trace([">> functionSignatureType with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = typeParameters ts
    in case ts1 of
        LeftParen :: This :: Colon ::  _ =>
            let
                val (ts2,nd2) = typeIdentifier (tl (tl (tl ts1)))
            in case ts2 of
                Comma :: _ =>
                    let
                        val (ts3,nd3) = nonemptyParametersType (tl ts2)  
                    in case ts3 of
                        RightParen :: _ =>
                           let
                               val (ts4,nd4) = resultType (tl ts3)
                           in
                               (log(["<< functionSignature with next=",tokenname(hd ts4)]);
                               (ts4,Ast.FunctionSignature
                                        { typeParams=nd1,
                                          thisType=SOME (needType (nd2,SOME false)),
                                          params=nd3,
                                          returnType=nd4,
                                          settings=NONE,
                                          hasRest=false })) (* do we need this *)
                               end
                      | _ => raise ParseError
                    end
              | RightParen :: _ =>
                    let
                        val (ts3,nd3) = resultType (tl ts2)
                    in
                        (log(["<< functionSignature with next=",tokenname(hd ts3)]);
                        (ts3,Ast.FunctionSignature
                                { typeParams=nd1,
                                  thisType=SOME (needType (nd2,SOME false)),
                                  params=([],[]),
                                  returnType=nd3,
                                  settings=NONE,
                                  hasRest=false })) (* do we need this *)
                          end
              | _ => raise ParseError
            end
      | LeftParen :: _ =>
            let
                val (ts2, nd2) = parametersType (tl ts1)
            in case ts2 of
                   RightParen :: _ =>
                       let
                           val (ts3,nd3) = resultType (tl ts2)
                       in
                        (log(["<< functionSignature with next=",tokenname(hd ts3)]);
                        (ts3,Ast.FunctionSignature
                                { typeParams=nd1,
                                  params=nd2,
                                  returnType=nd3,
                                  settings=NONE,
                                  thisType=NONE,  (* todo *)
                                  hasRest=false })) (* do we need this *)
                       end
                 | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    TypeParameters    
        empty
        .<  TypeParameterList  >  
*)

and typeParameters ts =
    let val _ = trace([">> typeParameters with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftDotAngle :: _ => 
            let
                val (ts1,nd1) = typeParameterList (tl ts)
            in case ts1 of
                GreaterThan :: _ => 
                    let
                    in
                        trace(["<< typeParameters with next=",tokenname(hd(tl ts1))]);
                        (tl ts1,nd1)
                    end
              | _ => raise ParseError
            end
      | _ => 
            (trace(["<< typeParameters with next=",tokenname(hd(ts))]);
            (ts,[]))
    end

(*
    TypeParametersList    
        Identifier
        Identifier  ,  TypeParameterList

    left factored:

    TypeParameterList
        Identifier TypeParameterListPrime

    TypeParameterListPrime
        empty
        ,  Identififier  TypeParameterListPrime
*)

and typeParameterList (ts) : token list * string list =
    let val _ = trace([">> typeParameterList with next=",tokenname(hd(ts))]) 
        fun typeParameterList' (ts) =
            let
            in case ts of
                Comma :: _ =>
                       let
                           val (ts1,nd1) = identifier(tl ts)
                           val (ts2,nd2) = typeParameterList' (ts1)
                       in
                         (ts2,nd1::nd2)
                       end
              | _ => (ts,[])
    end
        val (ts1,nd1) = identifier ts
        val (ts2,nd2) = typeParameterList' (ts1)
    in
        trace(["<< typeParameterList with next=",tokenname(hd ts2)]);
        (ts2,nd1::nd2)
    end

(*
    Parameters    
        empty
        NonemptyParameters(ALLOWLIST)

    NonemptyParameters    
        ParameterInit
        ParameterInit  ,  NonemptyParameters
        RestParameter
*)

and nonemptyParameters (ts)
    : (token list * Ast.BINDINGS) = 
    let
    in case ts of
        TripleDot :: _ => 
            let
                val (ts1,nd1) = restParameter ts
            in case ts1 of
                RightParen :: _ => (ts1,nd1)
              | _ => raise ParseError
            end
      | _ => 
            let
                val (ts1,(b1,i1)) = parameterInit ts
            in case ts1 of
                RightParen :: _ => (ts1,(b1,i1))
              | Comma :: _ =>
                    let
                        val (ts2,(b2,i2)) = nonemptyParameters (tl ts1)
                    in
                        (ts2,(b1@b2,i1@i2))
                    end
              | _ => raise ParseError
            end
    end

and nonemptyParametersType (ts)
    : (token list * Ast.BINDINGS) = 
    let
    in case ts of
        TripleDot :: _ => 
            let
                val (ts1,nd1) = restParameterType ts
            in case ts1 of
                RightParen :: _ => (ts1,nd1)
              | _ => raise ParseError
            end
      | _ => 
            let
                val (ts1,(b1,i1)) = parameterInitType ts
            in case ts1 of
                RightParen :: _ => (ts1,(b1,i1))
              | Comma :: ts2 =>
                    let
                        val (ts3,(b3,i3)) = nonemptyParametersType ts2
                    in
                        (ts3,(b1@b3,i1@i3))
                    end
              | _ => raise ParseError
            end
    end

and parameters (ts) 
    : (token list * Ast.BINDINGS) =
    let val _ = trace([">> parameters with next=",tokenname(hd(ts))]) 
    in case ts of 
        RightParen :: ts1 => (ts,([],[]))
      | _ => nonemptyParameters ts
    end

and parametersType (ts) 
    : (token list * Ast.BINDINGS)=
    let val _ = trace([">> parameters with next=",tokenname(hd(ts))]) 
    in case ts of 
        RightParen :: ts1 => (ts,([],[]))
      | _ => nonemptyParametersType ts
    end

(*
    ParameterInit
        Parameter
        Parameter  =  NonAssignmentExpression(ALLOWIN)
*)

and parameterInit (ts) 
    : (token list * Ast.BINDINGS) = 
    let val _ = trace([">> parameterInit with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = parameter ts
    in case ts1 of
        Assign :: _ => 
            let
                val {pattern,ty,...} = nd1
                val (ts2,nd2) = nonAssignmentExpression (tl ts1,NOLIST,ALLOWIN)
            in 
                trace(["<< parameterInit with next=",tokenname(hd(ts))]);
                (ts2, desugarPattern pattern ty (SOME nd2))
            end
      | _ => 
            let
                val {pattern,ty,...} = nd1
            in
                trace(["<< parameterInit with next=",tokenname(hd(ts))]);
                (ts1, desugarPattern pattern ty NONE)
            end
    end

and parameterInitType (ts) 
    : (token list * Ast.BINDINGS) = 
    let val _ = trace([">> parameterInit with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = parameterType ts
    in case ts1 of
        Assign :: _ =>
            let
                val {pattern,ty,...} = nd1
                val (ts2,nd2) = (tl ts1,Ast.LiteralExpr Ast.LiteralUndefined)
            in
                trace(["<< parameterInit with next=",tokenname(hd(ts))]);
                (ts2, ([Ast.Binding {ident=Ast.PropIdent "", ty = ty}],
                       [Ast.InitStep (Ast.PropIdent "",nd2)]))
            end
      | _ =>
            let
                val {pattern,ty,...} = nd1
            in
                trace(["<< parameterInit with next=",tokenname(hd(ts))]);
                (ts1, ([Ast.Binding {ident=Ast.PropIdent "", ty = NONE}],
                       []))
            end
    end

(*
    Parameter    
        ParameterKind TypedIdentifier(ALLOWIN)
        ParameterKind TypedPattern

    ParameterKind    
        empty
        const
*)

and parameter (ts) =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = parameterKind (ts)
        val (ts2,{p,t}) = typedPattern (ts1,NOLIST,ALLOWIN)
    in
        trace(["<< parameter with next=",tokenname(hd(ts2))]);
        (ts2,{pattern=p,ty=t,init=NONE})
    end

and parameterType (ts) =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))])
        val (ts2,t) = nullableTypeExpression ts
    in
        trace(["<< parameter with next=",tokenname(hd(ts2))]);
        (ts2,{pattern=IdentifierPattern "",ty=SOME t,init=NONE})
    end

and parameterKind (ts) 
    : (token list * Ast.VAR_DEFN_TAG)  = 
    let val _ = trace([">> parameterKind with next=",tokenname(hd(ts))]) 
    in case ts of
        Const :: ts1 => (ts1,Ast.Const)
      | ts1 => (ts1,Ast.Var)
    end

(*
    RestParameter
        ...
        ...  ParameterKind TypedIdentifier
        ...  ParameterKind TypedPattern
*)

and restParameter (ts) : (token list * Ast.BINDINGS) =
    let val _ = trace([">> restParameter with next=",tokenname(hd(ts))])
    in case ts of
        DOTDOTDOT :: _ =>
            let
            in case tl ts of
                RightParen :: _ => 
                    (tl ts, ([Ast.Binding{ident=Ast.PropIdent "",ty=NONE}],[]))
              | _ =>
                    let
                        val (ts1,{pattern,ty,...}) = parameter (tl ts)
                    in
                        (ts1, desugarPattern pattern ty NONE)
                    end
            end
      | _ => raise ParseError
    end

and restParameterType (ts) : (token list * Ast.BINDINGS) =
    let val _ = trace([">> restParameter with next=",tokenname(hd(ts))])
    in case ts of
        DOTDOTDOT :: _ =>
            let
            in case tl ts of
                RightParen :: _ => 
                    (tl ts,([Ast.Binding {ident=Ast.PropIdent "",ty=NONE}],[])) 
              | _ =>
                    let
                        val (ts1:token list,{pattern,ty,...}) = parameterType (tl ts)
                    in
                        (ts1,([Ast.Binding {ident=Ast.PropIdent "", ty=ty}],[]))
                    end
            end
      | _ => raise ParseError
    end

(*
    ResultType
        empty
        :  void
        :  TypeExpression
*)

and resultType ts = 
    let val _ = trace([">> resultType with next=",tokenname(hd(ts))]) 
    in case ts of
        Colon :: Void :: ts1 => (ts1,Ast.SpecialType(Ast.VoidType))
      | Colon :: _ => 
            let
                val (ts1,nd1) = nullableTypeExpression (tl ts)
            in
                log(["<< resultType with next=",tokenname(hd ts1)]);
                (ts1,nd1)
            end
      | ts1 => (ts1,Ast.SpecialType(Ast.Any))
    end

(*
    ObjectLiteral    
        {  FieldList  }
        {  FieldList  }  :  TypeExpression
*)

and objectLiteral ts = 
    let val _ = trace([">> objectLiteral with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftBrace :: _ => 
            let
                val (ts1,nd1) = fieldList (tl ts)
            in case ts1 of
                RightBrace :: Colon :: _ => 
                    let
                        val (ts2,nd2) = typeExpression (tl (tl ts1))
                    in
                        (ts2,Ast.LiteralObject {expr=nd1,ty=SOME nd2})
                    end
              | RightBrace :: _ => 
                    (tl ts1,Ast.LiteralObject {expr=nd1,ty=NONE})
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    FieldList    
        empty
        NonemptyFieldList(allowLet)

    NonemptyFieldList(a)
        LiteralField(a)
        LiteralField(noLet)  ,  NonemptyFieldList(a)
*)

and fieldList ts =
    let val _ = trace([">> fieldList with next=",tokenname(hd(ts))]) 
        fun nonemptyFieldList (ts) =
            let
                val (ts1,nd1) = literalField(ts)
            in case ts1 of
                Comma :: ts2 => 
                    let
                        val (ts3,nd3) = nonemptyFieldList (ts2)
                    in
                        (ts3,nd1::nd3)
                    end
              | _ => (ts1,nd1::[])
            end
    in case ts of
        RightBrace :: _ => 
            (trace(["<< fieldList with next=",tokenname(hd(ts))]);
            (ts,[]))
      | _ => 
        let
            val (ts1,nd1) = nonemptyFieldList (ts)
        in
            trace(["<< fieldList with next=",tokenname(hd(ts))]);
            (ts1,nd1)
         end
    end

(*
    LiteralField    
        FieldKind  FieldName  :  AssignmentExpression(NOLIST, ALLOWIN)
        get  Identifier  FunctionCommon
        set  Identifier  FunctionCommon

    FieldKind    
        empty
        const

    FieldName    
        PropertyIdentifier
        StringLiteral
        NumberLiteral
        ReservedIdentifier
*)

and literalField (ts) =
    let val _ = trace([">> literalField with next=",tokenname(hd(ts))]) 
    in case ts of
        (Get | Set) :: Colon :: _ =>  (* special case for fields with name 'get' or 'set' *)
            let
                val (ts1,nd1) = fieldKind ts
                val (ts2,nd2) = fieldName ts1
            in case ts2 of
                Colon :: _ =>
                    let
                        val (ts3,nd3) = assignmentExpression (tl ts2,NOLIST,ALLOWIN)
                    in
                        (ts3,{kind=nd1,name=nd2,init=nd3})
                    end
              | _ => raise ParseError
            end
      | Get :: _ =>
            let
                val (ts1,nd1) = fieldName (tl ts)
                val (ts2,{fsig,block}) = functionCommon (ts1)
            in
                (ts2,{kind=Ast.Var,
                      name=nd1,
                      init=Ast.LiteralExpr 
                               (Ast.LiteralFunction 
                                    (Ast.Func {name={kind=Ast.Get, ident=""},
                                               fsig=fsig,
                                               block=block,
                                               param=([],[]),
                                               defaults=[],
                                               ty=functionTypeFromSignature fsig}))})
            end
      | Set :: _ =>
            let
                val (ts1,nd1) = fieldName (tl ts)
                val (ts2,{fsig,block}) = functionCommon (ts1)
            in
                (ts2,{kind=Ast.Var,
                      name=nd1,
                      init=Ast.LiteralExpr 
                               (Ast.LiteralFunction 
                                    (Ast.Func {name={kind=Ast.Get,ident=""},
                                               fsig=fsig,
                                               block=block,
                                               param=([],[]),
                                               defaults=[],
                                               ty=functionTypeFromSignature fsig}))})
            end
      | _ => 
            let
                val (ts1,nd1) = fieldKind ts
                val (ts2,nd2) = fieldName ts1
            in case ts2 of
                Colon :: _ =>
                    let
                        val (ts3,nd3) = assignmentExpression (tl ts2,NOLIST,ALLOWIN)
                    in
                        (ts3,{kind=nd1,name=nd2,init=nd3})
                    end
              | _ => raise ParseError
            end
    end

and fieldKind (ts) : (token list * Ast.VAR_DEFN_TAG)  = 
    let val _ = trace([">> fieldKind with next=",tokenname(hd(ts))]) 
    in case ts of
        (Const | Get | Set ) :: Colon :: _ => (ts,Ast.Var)
      | Const :: _ => (tl ts,Ast.Const)
      | _ => (ts,Ast.Var)
    end

and fieldName (ts) : token list * Ast.IDENT_EXPR =
    let val _ = trace([">> fieldName with next=",tokenname(hd(ts))]) 
    in case ts of
        StringLiteral s :: ts1 => (ts1,Ast.Identifier {ident=s,openNamespaces=[]})
(*      | NumberLiteral n :: ts1 => (ts1,n)  *) (* todo: convert number to string *)
      | _ => 
            let
                val (ts1,nd1) = reservedOrPropertyIdentifier (ts)
            in
                (ts1,Ast.Identifier {ident=nd1,openNamespaces=[]})  (* todo: allow qualified identifier *)
            end
    end

and functionCommon ts =
    let val _ = trace([">> functionCommon with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = functionSignature ts
    in case ts1 of
        LeftBrace :: _ => 
            let
                val (ts2,nd2) = block (ts1,LOCAL)
            in
                (ts2,{fsig=nd1,block=nd2})
            end
      | _ => (error(["expecting {"]); raise ParseError)
    end

(*
    ArrayLiteral    
        [  ElementList  ]
        [  ElementList  ]  :  ArrayType
*)

and arrayLiteral (ts) =
    let val _ = trace([">> arrayLiteral with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftBracket :: _ => 
            let
                val (ts1,nd1) = elementList (tl ts)
            in case ts1 of
                RightBracket :: Colon :: _ => 
                    let
                        val (ts2,nd2) = typeExpression (tl (tl ts1))
                    in
                        (ts2,Ast.LiteralArray {exprs=nd1,ty=SOME nd2})
                    end
              | RightBracket :: _ => 
                    (tl ts1,Ast.LiteralArray {exprs=nd1,ty=NONE})
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    ElementList
        empty
        LiteralElement
        ,  ElementList
        LiteralElement  ,  ElementList

    LiteralElement
        AssignmentExpression(NOLIST, ALLOWIN)
*)

and elementList (ts) =
    let val _ = trace([">> elementList with next=",tokenname(hd(ts))]) 
    in case ts of
        RightBracket :: _ => (ts,[])
      | Comma :: _ => 
            let
                val (ts1,nd1) = elementList (tl ts)
            in
                (ts1,Ast.LiteralExpr(Ast.LiteralUndefined) :: nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = assignmentExpression (ts,NOLIST,ALLOWIN)
            in case ts1 of
                Comma :: _ =>
                    let
                        val (ts2,nd2) = elementList (tl ts1)
                    in
                        (ts2,nd1::nd2)
                    end
              | _ => (ts1,nd1::[])
            end
    end

(*
    XMLInitialiser    
        XMLMarkup
        XMLElement
        <  >  XMLElementContent  </  >

    XMLElementContent    
        XMLMarkup  XMLElementContentopt
        XMLText  XMLElementContentopt
        XMLElement  XMLElementContentopt
        {  ListExpressionallowIn  }  XMLElementContentopt
    
    XMLElement    
        <  XMLTagContent  XMLWhitespaceopt  />
        <  XMLTagContent  XMLWhitespaceopt  >  XMLElementContent
                  </  XMLTagName  XMLWhitespaceopt  >
    
    XMLTagContent    
        XMLTagName  XMLAttributes
    
    XMLTagName    
        {  ListExpressionallowIn  }
        XMLName
    
    XMLAttributes    
        XMLWhitespace  {  ListExpressionallowIn  }
        XMLAttribute  XMLAttributes
        empty
    
    XMLAttribute    
        XMLWhitespace  XMLName  XMLWhitespaceopt  =  XMLWhitespaceopt  {  ListExpressionallowIn  }
        XMLWhitespace  XMLName  XMLWhitespaceopt  =  XMLWhitespaceopt  XMLAttributeValue
    
    XMLElementContent    
        {  ListExpressionallowIn  }  XMLElementContent
        XMLMarkup  XMLElementContent
        XMLText  XMLElementContent
        XMLElement  XMLElementContent
        empty
*)

(*
    PrimaryExpression(a,b)
        null
        true
        false
        NumberLiteral
        StringLiteral
        this
        RegularExpression
        XMLInitialiser
        ParenListExpression
        ArrayLiteral
        ObjectLiteral
        FunctionExpression(a,b)
        AttributeIdentifier
        TypeIdentifier
*)

and primaryExpression (ts,a,b) =
    let val _ = trace([">> primaryExpression with next=",tokenname(hd ts)])
    in case ts of
        Null :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNull))
      | True :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean true))
      | False :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean false))
      | NumberLiteral n :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNumber n))
      | StringLiteral s :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralString s))
      | This :: ts1 => (ts1, Ast.ThisExpr)
      | LeftParen :: _ => 
            parenListExpression ts
      | LeftBracket :: _ => 
            let 
                val (ts1,nd1) = arrayLiteral ts
            in 
                (ts1,Ast.LiteralExpr nd1) 
            end
      | LeftBrace :: _ => 
            let 
                val (ts1,nd1) = objectLiteral ts 
            in 
                (ts1,Ast.LiteralExpr nd1) 
            end
      | Function :: _ => functionExpression (ts,a,b)
      | LexBreakDiv x :: _ => 
            let 
                val ts1 = (#lex_regexp x)()
            in case ts1 of
                RegExp :: _ => 
                    let 
                    in case ts1 of
                        RegexpLiteral str :: _ =>
                            (tl ts1,Ast.LiteralExpr(Ast.LiteralRegExp {str=str}))
                      | _ => raise ParseError
                    end
              | _ => raise ParseError
            end
(* todo:
      | (XmlMarkup | LessThan ) :: _ => xmlInitializer ts
*)
(* what's this?      | Eol :: ts1 => primaryExpression (ts1,a,b) *)
      | At :: _ => 
            let
                val (ts1,nd1) = attributeIdentifier ts
            in
                (ts1,Ast.LexicalRef {ident=nd1})
            end
      | _ => 
            let
                val (ts1,nd1) = typeIdentifier ts
            in
                (ts1,Ast.LexicalRef {ident=nd1})
            end
    end

(*
    SuperExpression    
        super
        super  ParenExpression
*)

and superExpression ts =
    let val _ = trace([">> superExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        Super :: _ =>
            let
            in case tl ts of
                LeftParen :: _ => 
                    let
                           val (ts1,nd1) = parenExpression(tl (tl ts))
                    in
                        (ts1,Ast.SuperExpr(SOME(nd1)))
                    end
                | _ => 
                    (tl ts,Ast.SuperExpr(NONE))
            end
      | _ => raise ParseError
    end

(*
    MemberExpression    
        PrimaryExpression
        new  MemberExpression  Arguments
        SuperExpression  PropertyOperator
        MemberExpression  PropertyOperator

    Refactored:

    MemberExpression :    
        PrimaryExpression MemberExpressionPrime
        new MemberExpression Arguments MemberExpressionPrime
        SuperExpression  PropertyOperator  MemberExpressionPrime
    
    MemberExpressionPrime :    
        PropertyOperator MemberExpressionPrime
        empty
*)

and memberExpression (ts,a,b) =
    let val _ = trace([">> memberExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: _ =>
            let
                val (ts1,nd1) = memberExpression(tl ts,a,b)
            in case ts1 of
                LeftParen :: _ =>
                    let
                        val (ts2,nd2) = arguments(ts1)
                        val (ts3,nd3) = memberExpressionPrime(ts2,Ast.NewExpr {obj=nd1,actuals=nd2},a,b)
                    in
                        (ts3,nd3)
                    end
              | _ => 
                    let
                    in
                        (ts1,Ast.NewExpr {obj=nd1,actuals=[]}) (* short new, we're done *)
                    end
            end
      | Super :: _ =>
            let
                val (ts2,nd2) = superExpression(ts)
                val (ts3,nd3) = propertyOperator(ts2,nd2)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3,a,b)
            in
                (ts4,nd4)
            end
      | _ =>
            let
                val (ts3,nd3) = primaryExpression(ts,a,b)
                val (ts4,nd4) = memberExpressionPrime(ts3,nd3,a,b)
            in
                (trace(["<< memberExpression with next=",tokenname(hd ts4)]);(ts4,nd4))
            end
    end

and memberExpressionPrime (ts,nd,a,b) =
    let val _ = trace([">> memberExpressionPrime with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBracket :: _ | Dot :: _) =>
            let
                val (ts1,nd1) = propertyOperator(ts,nd)
                val (ts2,nd2) = memberExpressionPrime(ts1,nd1,a,b)
            in
                trace(["<< memberExpressionPrime with next=",tokenname(hd(ts2))]);
                (ts2,nd2)
            end
      | _ => 
            let
            in
                trace(["<< memberExpressionPrime with next=",tokenname(hd(ts))]);
                (ts,nd)
            end
    end

(*
    CallExpression    
        MemberExpression  Arguments
        CallExpression  Arguments
        CallExpression  PropertyOperator

    refactored:

    CallExpression :    
        MemberExpression Arguments CallExpressionPrime
    
    CallExpressionPrime :    
        Arguments CallExpressionPrime
        PropertyOperator CallExpressionPrime
        empty
*)

and callExpression (ts,a,b) =
    let val _ = trace([">> callExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = memberExpression(ts,a,b)
        val (ts2,nd2) = arguments(ts1)
        val (ts3,nd3) = callExpressionPrime(ts2,Ast.CallExpr({func=nd1,actuals=nd2}),a,b)
    in 
        trace(["<< callExpression with next=",tokenname(hd(ts2))]);
        (ts3,nd3)
    end

and callExpressionPrime (ts,nd,a,b) =
    let val _ = trace([">> callExpressionPrime with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBracket :: _ | Dot :: _) =>
            let
                val (ts1,nd1) = propertyOperator(ts,nd)
                val (ts2,nd2) = callExpressionPrime(ts1,nd1,a,b)
            in
                trace(["<< callExpressionPrime with next=",tokenname(hd(ts2))]);
                (ts2,nd2)
            end
      | LeftParen :: _ => 
            let
                val (ts1,nd1) = arguments(ts)
                val (ts2,nd2) = callExpressionPrime(ts1,Ast.CallExpr({func=nd,actuals=nd1}),a,b)
            in
                trace(["<< callExpressionPrime with next=",tokenname(hd(ts2))]);
                (ts2,nd2)
            end
      | _ => 
            let
            in
                trace(["<< callExpressionPrime with next=",tokenname(hd(ts))]);
                (ts,nd)
            end
    end

(*
    NewExpression(a,b)
        MemberExpression(a,b)
        new  NewExpression(a,b)
*)

and newExpression (ts,a,b) =
    let val _ = trace([">> newExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: New :: _ =>
            let
                val (ts1,nd1) = newExpression(tl ts,a,b)  (* eat only the first new *)
            in
                trace(["<< newExpression new new with next=",tokenname(hd(ts1))]);
                (ts1,Ast.NewExpr({obj=nd1,actuals=[]}))
            end
      | New :: _ =>
            let
                val (ts1,nd1) = memberExpression(ts,a,b)  (* don't eat new, let memberExpr eat it *)
            in
                trace(["<< newExpression with next=",tokenname(hd(ts1))]);
                (ts1,nd1)
            end
      | _ => 
            let
                val (ts1,nd1) = memberExpression(ts,a,b)
            in case ts1 of
                LeftParen :: _ =>
                    let
                        val (ts2,nd2) = callExpressionPrime (ts,nd1,a,b)
                    in
                        (ts2,nd2)
                    end
              | _ =>
                    let
                    in
                        (ts1,nd1)
                    end
            end
    end

(*
    Arguments
        (  )
        (  ArgumentList(ALLOWLIST)  )
*)

and arguments (ts) : (token list * Ast.EXPR list)  =
    let val _ = trace([">> arguments with next=",tokenname(hd(ts))])
    in case ts of
        LeftParen :: RightParen :: _ => 
            let
            in
                trace(["<< arguments with next=",tokenname(hd(tl (tl ts)))]);
                (tl (tl ts),[]) 
            end
      | LeftParen :: _ => 
            let
                val (ts1,nd1) = argumentList(tl ts)
            in case ts1 of
                RightParen :: _ => 
                    let
                    in
                        trace(["<< arguments with next=",tokenname(hd(tl ts1))]);
                        (tl ts1,nd1)
                    end
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    ArgumentList
        AssignmentExpression(NOLIST, ALLOWIN)
        ArgumentList  ,  AssignmentExpression(NOLIST, ALLOWIN)

    refactored:

    ArgumentList
        AssignmentExpression(NOLIST,ALLOWIN) ArgumentListPrime

    ArgumentListPrime
        empty
        , AssignmentExpression(NOLIST,ALLOWIN) ArgumentListPrime
*)

and argumentList (ts) : (token list * Ast.EXPR list)  =
    let val _ = trace([">> argumentList with next=",tokenname(hd(ts))])
        fun argumentList' (ts) : (token list * Ast.EXPR list) =
            let val _ = trace([">> argumentList' with next=",tokenname(hd(ts))])
            in case ts of
                Comma :: _ => 
                    let
                        val (ts1,nd1) = assignmentExpression(tl ts,NOLIST,ALLOWIN)
                        val (ts2,nd2) = argumentList'(ts1)
                    in
                        (ts2,nd1::nd2)
                    end
              | RightParen :: _ => 
                    (ts,[])
              | _ => 
                    (log(["*syntax error*: expect '",tokenname RightParen, "' before '",tokenname(hd ts),"'"]);
                    raise ParseError)
            end
        val (ts1,nd1) = assignmentExpression(ts,NOLIST,ALLOWIN)
        val (ts2,nd2) = argumentList'(ts1)
    in
        (ts2,nd1::nd2)
    end

(*
    PropertyOperator    
        .. QualifiedIdentifier
        .  ReservedIdentifier   <- might be private, public, etc
        .  QualifiedIdentifier
        .  ParenListExpression
        .  ParenListExpression  ::  PropertyIdentifier
        .  ParenListExpression  ::  ReservedIdentifier
        .  ParenListExpression  ::  Brackets
        Brackets
*)

and propertyOperator (ts, nd) =
    let val _ = trace([">> propertyOperator with next=",tokenname(hd(ts))]) 
    in case ts of
        Dot :: LeftParen :: _ =>
                    let
                        val (ts1,nd1) = parenListExpression(tl ts)
                    in case ts1 of
                        DoubleColon :: LeftBracket :: _ => 
                               let
                                val (ts2,nd2) = brackets(tl ts1)
                               in
                                   (ts2,Ast.ObjectRef {base=nd,ident=Ast.QualifiedExpression {
                                            qual=nd1, expr=nd2}})
                            end
                      | DoubleColon :: _ => 
                            let
                                val (ts2,nd2) = reservedOrPropertyIdentifier(tl ts1)
                            in
                                (ts2,Ast.ObjectRef({base=nd,ident=Ast.QualifiedIdentifier {
                                            qual=nd1, ident=nd2}}))
                            end
                      | _ => raise ParseError (* e4x filter expr *)
                    end
      | Dot :: _ => 
                    let
                     in case (isreserved(hd (tl ts)),tl ts) of
                        ((true,(Intrinsic | Private | Public | Protected | Internal) ::_) |
                         (false,_)) => 
                            let
                                val (ts1,nd1) = qualifiedIdentifier(tl ts)
                            in
                                (ts1,Ast.ObjectRef {base=nd,ident=nd1})
                            end
                      | (true,_) =>
                            let
                                val (ts1,nd1) = reservedIdentifier (tl ts)
                            in
                                (ts1,Ast.ObjectRef {base=nd,ident=Ast.Identifier {ident=nd1,
                                                    openNamespaces=[]}})
                            end 
                    end
      | LeftBracket :: _ => 
            let
                val (ts1,nd1) = brackets(ts)
            in
                (ts1,Ast.ObjectRef({base=nd,ident=Ast.ExpressionIdentifier(nd1)}))
            end
      | _ => raise ParseError
    end

(*
    Brackets    
        [  ListExpression(allowIn)  ]
        [  SliceExpression   ]
        
    SliceExpression    
        OptionalExpression  :  OptionalExpression
        OptionalExpression  :  OptionalExpression  :  OptionalExpression
        
    OptionalExpression    
        ListExpression(allowIn)
        empty

    TODO: implement SliceExpression
*)

and brackets (ts) : (token list * Ast.EXPR) =
    let val _ = trace([">> brackets with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftBracket :: ts' =>
            let
                val (ts1,nd1) = listExpression (ts',ALLOWIN)
            in case ts1 of
                Colon :: ts'' => 
                    let
                        val (ts2,nd2) = listExpression (ts'',ALLOWIN)
                    in case ts2 of
                        RightBracket :: ts'' => (ts'',Ast.SliceExpr (nd1,nd2,Ast.ListExpr [])) 
                      | _ => raise ParseError
                    end
              | RightBracket :: ts'' => (ts'',nd1) 
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    LeftHandSideExpression(a, b)   
        NewExpression(a, b)
        CallExpression(a, b)
    
    refactored:

    LeftHandSideExpression(a, b)
        new NewExpression(a, b)
        MemberExpression(a,b) Arguments CallExpressionPrime(a, b)
        MemberExpression(a, b)
*)

and leftHandSideExpression (ts,a,b) =
    let val _ = trace([">> leftHandSideExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        New :: _ =>
            let
                val (ts1,nd1) = newExpression(ts,a,b)
            in
                (ts1,nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = memberExpression(ts,a,b)
            in case ts1 of
                LeftParen :: _ =>
                    let
                        val (ts2,nd2) = arguments(ts1)
                        val (ts3,nd3) = callExpressionPrime(ts2,Ast.CallExpr {func=nd1,actuals=nd2},a,b)
                    in
                        (trace(["<< leftHandSideExpression with next=",tokenname(hd(ts3))]);(ts3,nd3))
                    end
              | _ => 
                    (trace(["<< leftHandSideExpression with next=",tokenname(hd(ts1))]);
                    (ts1,nd1))
            end
    end

(*
    PostfixExpression(a, b)    
        LeftHandSideExpression(a, b)
        LeftHandSideExpression(a, b)  [no line break]  ++
        LeftHandSideExpression(a, b)  [no line break]  --
*)

and postfixExpression (ts,a,b) =
    let val _ = trace([">> postfixExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = leftHandSideExpression(ts,a,b)
    in case ts1 of
        PlusPlus :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostIncrement,nd1))
      | MinusMinus :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostDecrement,nd1))
      | _ => (trace(["<< postfixExpression"]);(ts1,nd1))
    end

(*
    UnaryExpression(a, b)    
        PostfixExpression(a, b)
        delete  PostfixExpression(a, b)
        void  UnaryExpression(a, b)
        typeof  UnaryExpression(a, b)
        ++   PostfixExpression(a, b)
        --  PostfixExpression(a, b)
        +  UnaryExpression(a, b)
        -  UnaryExpression(a, b)
        ~  UnaryExpression(a, b)
        !  UnaryExpression(a, b)
        type TypeExpression
*)

and unaryExpression (ts,a,b) =
    let val _ = trace([">> unaryExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        Delete :: ts1 => 
            let 
                val (ts2,nd2) = postfixExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.Delete,nd2)) 
            end
      | Void :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.Void,nd2)) 
            end
      | TypeOf :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.Typeof,nd2)) 
            end
      | PlusPlus :: ts1 => 
            let 
                val (ts2,nd2) = postfixExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.PreIncrement,nd2)) 
            end
      | MinusMinus :: ts1 => 
            let 
                val (ts2,nd2) = postfixExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.PreDecrement,nd2)) 
            end
      | Plus :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.UnaryPlus,nd2)) 
            end
      | Minus :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.UnaryMinus,nd2)) 
            end
      | BitwiseNot :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.BitwiseNot,nd2)) 
            end
      | Not :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.LogicalNot,nd2)) 
            end
      | Type :: ts1 => 
            let 
                val (ts2,nd2) = nullableTypeExpression (ts1)
            in 
                (ts2,Ast.TypeExpr(nd2)) 
            end
      | _ => 
            postfixExpression (ts,a,b)
    end
    
(*
    MultiplicativeExpression    
        UnaryExpression
        MultiplicativeExpression  *  UnaryExpression
        MultiplicativeExpression  /  UnaryExpression
        MultiplicativeExpression  %  UnaryExpression

    right recursive:

    MultiplicativeExpression    
        UnaryExpression MultiplicativeExpressionPrime
    
    MultiplicativeExpression'    
        *  UnaryExpression MultiplicativeExpressionPrime
        /  UnaryExpression MultiplicativeExpressionPrime
        %  UnaryExpression MultiplicativeExpressionPrime
        empty
*)

and multiplicativeExpression (ts,a,b) =
    let val _ = trace([">> multiplicativeExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = unaryExpression (ts,a,b)
        fun multiplicativeExpression' (ts1, nd1,a,b) =
            case ts1 of
                Mult :: ts2 => 
                    let 
                        val (ts3,nd3) = unaryExpression (ts2,a,b) 
                    in 
                        multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Times NONE,nd1,nd3),a,b) 
                    end

              | LexBreakDiv x :: _ =>
                    let    
                    in case (#lex_initial x)() of
                        Div :: ts2 => 
                            let 
                                val (ts3,nd3) = unaryExpression (ts2,a,b) 
                            in 
                                multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Divide NONE,nd1,nd3),a,b) 
                            end
                      | _ => raise ParseError
                    end

              | Modulus :: ts2 => 
                    let 
                        val (ts3,nd3) = unaryExpression (ts2,a,b) 
                    in 
                        multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Remainder NONE,nd1,nd3),a,b) 
                    end
              | _ => (trace(["<< multiplicative"]);(ts1,nd1))
    in
        multiplicativeExpression' (ts1,nd1,a,b)
    end

(*
    AdditiveExpression    
        MultiplicativeExpression
        AdditiveExpression  +  MultiplicativeExpression
        AdditiveExpression  -  MultiplicativeExpression

    right recursive: (see pattern of MultiplicativeExpression)
*)

and additiveExpression (ts,a,b) =
    let val _ = trace([">> additiveExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = multiplicativeExpression (ts,a,b)
        fun additiveExpression' (ts1, nd1,a,b) =
            case ts1 of
                Plus :: ts2 => 
                    let 
                        val (ts3,nd3) = multiplicativeExpression (ts2,a,b) 
                    in 
                        additiveExpression' (ts3,Ast.BinaryExpr(Ast.Plus NONE,nd1,nd3),a,b) 
                    end
              | Minus :: ts2 => 
                    let 
                        val (ts3,nd3) = multiplicativeExpression (ts2,a,b) 
                    in 
                        additiveExpression' (ts3,Ast.BinaryExpr(Ast.Minus NONE,nd1,nd3),a,b) 
                    end
              | _ => 
                    (trace(["<< additiveExpression"]);
                    (ts1,nd1))
    in
        additiveExpression' (ts1,nd1,a,b)
    end

(*
    ShiftExpression    
        AdditiveExpression
        ShiftExpression  <<  AdditiveExpression
        ShiftExpression  >>  AdditiveExpression
        ShiftExpression  >>>  AdditiveExpression
    
    right recursive: (see pattern of MultiplicativeExpression)
*)

and shiftExpression (ts,a,b) =
    let val _ = trace([">> shiftExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = additiveExpression (ts,a,b)
        fun shiftExpression' (ts1,nd1,a,b) =
            case ts1 of
                LeftShift :: ts2 => 
                    let 
                        val (ts3,nd3) = additiveExpression (ts2,a,b) 
                    in 
                        shiftExpression' (ts3,Ast.BinaryExpr(Ast.LeftShift,nd1,nd3),a,b) 
                    end
              | RightShift :: ts2 => 
                    let 
                        val (ts3,nd3) = additiveExpression (ts2,a,b)
                    in 
                        shiftExpression' (ts3,Ast.BinaryExpr(Ast.RightShift,nd1,nd3),a,b) 
                    end
              | UnsignedRightShift :: ts2 => 
                    let 
                        val (ts3,nd3) = additiveExpression (ts2,a,b) 
                    in 
                        shiftExpression' (ts3,Ast.BinaryExpr(Ast.RightShiftUnsigned,nd1,nd3),a,b) 
                    end
              | _ => (trace(["<< shiftExpression"]);(ts1,nd1))
    in
        shiftExpression' (ts1,nd1,a,b)
    end

(*
    RelationalExpression(ALLOWIN)
        ShiftExpression
        RelationalExpressionallowIn  <  ShiftExpression
        RelationalExpressionallowIn  >  ShiftExpression
        RelationalExpressionallowIn  <=  ShiftExpression
        RelationalExpressionallowIn  >=  ShiftExpression
        RelationalExpressionallowIn  in  ShiftExpression
        RelationalExpressionallowIn  instanceof  ShiftExpression
        RelationalExpressionallowIn  is  TypeExpression
        RelationalExpressionallowIn  to  TypeExpression
        RelationalExpressionallowIn  cast  TypeExpression

    RelationalExpression(NOIN)
        ShiftExpression
        RelationalExpressionallowIn  <  ShiftExpression
        RelationalExpressionallowIn  >  ShiftExpression
        RelationalExpressionallowIn  <=  ShiftExpression
        RelationalExpressionallowIn  >=  ShiftExpression
        RelationalExpressionallowIn  instanceof  ShiftExpression
        RelationalExpressionallowIn  is  TypeExpression
        RelationalExpressionallowIn  to  TypeExpression
        RelationalExpressionallowIn  cast  TypeExpression

    right recursive: (see pattern of MultiplicativeExpression)
*)

and relationalExpression (ts,a, b)=
    let val _ = trace([">> relationalExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = shiftExpression (ts,a,b)
        fun relationalExpression' (ts1,nd1,a,b) =
            case (ts1,b) of
                ((LexBreakLessThan x) :: _,_) =>
                    let    
                    in case (#lex_initial x)() of
                        LessThan :: ts2 => 
                            let 
                                val (ts3,nd3) = shiftExpression (ts2,a,b) 
                            in 
                                relationalExpression' (ts3,Ast.BinaryExpr(Ast.Less NONE,nd1,nd3),a,ALLOWIN) 
                            end
                      | _ => raise ParseError
                    end

              | (GreaterThan :: ts2,_) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.Greater NONE,nd1,nd3),a,ALLOWIN) 
                    end
              | (LessThanOrEquals :: ts2, _) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.LessOrEqual NONE,nd1,nd3),a,ALLOWIN) 
                    end
              | (GreaterThanOrEquals :: ts2, _) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.GreaterOrEqual NONE,nd1,nd3),a,ALLOWIN) 
                    end
              | (In :: ts2, ALLOWIN) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.In,nd1,nd3),a,ALLOWIN) 
                    end
              | (InstanceOf :: ts2, _) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.InstanceOf,nd1,nd3),a,ALLOWIN) 
                    end
              | (Cast :: ts2, _) => 
                    let 
                        val (ts3,nd3) = typeExpression (ts2) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Cast,nd1,nd3),a,ALLOWIN) 
                    end
              | (To :: ts2, _) => 
                    let 
                        val (ts3,nd3) = typeExpression (ts2) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.To,nd1,nd3),a,ALLOWIN) 
                    end
              | (Is :: ts2, _) => 
                    let 
                        val (ts3,nd3) = typeExpression (ts2) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Is,nd1,nd3),a,ALLOWIN) 
                    end
              | (_,_) => 
                    (trace(["<< relationalExpression"]);(ts1,nd1))
    in
        relationalExpression' (ts1,nd1,a,b)
    end

(*
    EqualityExpression(b)
        RelationalExpression(b)
        EqualityExpression(b)  ==  RelationalExpression(b)
        EqualityExpression(b)  !=  RelationalExpression(b)
        EqualityExpression(b)  ===  RelationalExpression(b)
        EqualityExpression(b)  !==  RelationalExpression(b)

    right recursive: (see pattern of MultiplicativeExpression)

*)

and equalityExpression (ts,a,b)=
    let val _ = trace([">> equalityExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = relationalExpression (ts,a,b)
        fun equalityExpression' (ts1,nd1) =
            case ts1 of
                Equals :: ts2 => 
                    let 
                        val (ts3,nd3) = relationalExpression (ts2,a,b) 
                    in 
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.Equals NONE,nd1,nd3)) 
                    end
              | NotEquals :: ts2 => 
                    let 
                        val (ts3,nd3) = relationalExpression (ts2,a,b) 
                    in 
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.NotEquals NONE,nd1,nd3)) 
                    end
              | StrictEquals :: ts2 => 
                    let 
                        val (ts3,nd3) = relationalExpression (ts2,a,b) 
                    in 
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictEquals NONE,nd1,nd3)) 
                    end
              | StrictNotEquals :: ts2 => 
                    let 
                        val (ts3,nd3) = relationalExpression (ts2,a,b) 
                    in 
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictNotEquals NONE,nd1,nd3)) 
                    end
              | _ => 
                    (trace(["<< equalityExpression"]);(ts1,nd1))
        
    in
        equalityExpression' (ts1,nd1)
    end

(*
    BitwiseAndExpression(b)    
        EqualityExpression(b)
        BitwiseAndExpressionr(b)  &  EqualityExpression(b)

    right recursive: (see pattern of MultiplicativeExpression)
*)

and bitwiseAndExpression (ts,a,b)=
    let val _ = trace([">> bitwiseAndExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = equalityExpression (ts,a,b)
        fun bitwiseAndExpression' (ts1,nd1) =
            case ts1 of
                BitwiseAnd :: ts2 => 
                    let 
                        val (ts3,nd3) = equalityExpression (ts2,a,b) 
                    in 
                        bitwiseAndExpression' (ts3,Ast.BinaryExpr(Ast.BitwiseAnd,nd1,nd3)) 
                    end
              | _ => (trace(["<< bitwiseAnd"]);(ts1,nd1))
        
    in
        bitwiseAndExpression' (ts1,nd1)
    end

(*
    BitwiseXorExpressionb    
        BitwiseAndExpressionb
        BitwiseXorExpressionb  ^  BitwiseAndExpressionb

    right recursive: (see pattern of MultiplicativeExpression)
*)

and bitwiseXorExpression (ts,a,b)=
    let val _ = trace([">> bitwiseXorExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = bitwiseAndExpression (ts,a,b)
        fun bitwiseXorExpression' (ts1,nd1) =
            case ts1 of
                BitwiseXor :: ts2 => 
                    let 
                        val (ts3,nd3) = bitwiseAndExpression (ts2,a,b) 
                    in 
                        bitwiseXorExpression' (ts3,Ast.BinaryExpr(Ast.BitwiseXor,nd1,nd3)) 
                    end
              | _ => (trace(["<< bitwiseXor"]);(ts1,nd1))
    in
        bitwiseXorExpression' (ts1,nd1)
    end

(*
    BitwiseOrExpressionb    
        BitwiseXorExpressionb
        BitwiseOrExpressionb  |  BitwiseXorExpressionb
*)

and bitwiseOrExpression (ts,a,b)=
    let val _ = trace([">> bitwiseOrExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = bitwiseXorExpression (ts,a,b)
        fun bitwiseOrExpression' (ts1,nd1) =
            case ts1 of
                BitwiseOr :: ts2 => 
                    let 
                        val (ts3,nd3) = bitwiseXorExpression (ts2,a,b) 
                    in 
                        bitwiseOrExpression' (ts3,Ast.BinaryExpr(Ast.BitwiseOr,nd1,nd3)) 
                    end
              | _ => (trace(["<< bitwiseAnd"]);(ts1,nd1))
        
    in
        bitwiseOrExpression' (ts1,nd1)
    end

(*
    LogicalAndExpression(b)    
        BitwiseOrExpression(b)
        LogicalAndExpression(b)  &&  BitwiseOrExpression(b)
*)

and logicalAndExpression (ts,a,b)=
    let val _ = trace([">> logicalAndExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = bitwiseOrExpression (ts,a,b)
        fun logicalAndExpression' (ts1,nd1) =
            case ts1 of
                LogicalAnd :: ts2 => 
                    let 
                        val (ts3,nd3) = bitwiseOrExpression (ts2,a,b) 
                    in 
                        logicalAndExpression' (ts3,Ast.BinaryExpr(Ast.LogicalAnd,nd1,nd3)) 
                    end
              | _ => 
                    (trace(["<< logicalAndExpression"]);
                    (ts1,nd1))
        
    in
        logicalAndExpression' (ts1,nd1)
    end

(*
    LogicalOrExpression(b)
        LogicalAndExpression(b)
        LogicalOrExpression(b)  ||  LogicalAndExpression(b)

*)

and logicalOrExpression (ts,a,b) =
    let val _ = trace([">> logicalOrExpression with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = logicalAndExpression (ts,a,b)
        fun logicalOrExpression' (ts1,nd1) =
            case ts1 of
                LogicalOr :: ts2 => 
                    let 
                        val (ts3,nd3) = logicalAndExpression (ts2,a,b) 
                    in 
                        logicalOrExpression' (ts3,Ast.BinaryExpr(Ast.LogicalOr,nd1,nd3)) 
                    end
              | _ => 
                    (trace(["<< logicalOrExpression"]);
                    (ts1,nd1))
        
    in
        logicalOrExpression' (ts1,nd1)
    end

(*
    ConditionalExpression(ALLOWLIST, b)    
        LetExpression(b)
        YieldExpression(b)
        LogicalOrExpression(b)
        LogicalOrExpression(b)  ?  AssignmentExpression(ALLOWLIST,b)  
                                   :  AssignmentExpression(ALLOWLIST,b)

    ConditionalExpression(NOLIST, b)    
        SimpleYieldExpression
        LogicalOrExpression(b)
        LogicalOrExpression(b)  ?  AssignmentExpression(ALLOWLIST,b) 
                                   :  AssignmentExpression(NOLIST,b)
    
*)

and conditionalExpression (ts,ALLOWLIST,b) =
    let val _ = trace([">> conditionalExpression ALLOWLIST with next=",tokenname(hd(ts))])
    in case ts of
        Let :: _ => letExpression(ts,b)
      | Yield :: _ => yieldExpression(ts,b)
      | _ => 
            let
                val (ts2,nd2) = logicalOrExpression(ts,ALLOWLIST,b)
            in case ts2 of
                QuestionMark :: ts3 => 
                    let
                        val (ts4,nd4) = assignmentExpression(ts3,ALLOWLIST,b)
                    in case ts4 of
                        Colon :: ts5 =>
                            let
                                val (ts6,nd6) = assignmentExpression(ts5,ALLOWLIST,b)
                            in
                                (ts6,nd6)
                            end
                      | _ => raise ParseError                            
                    end
              | _ => 
                    (trace(["<< conditionalExpression ALLOWLIST with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end
 
  | conditionalExpression (ts,NOLIST,b) =
    let val _ = trace([">> conditionalExpression NOLIST with next=",tokenname(hd(ts))])
    in case ts of
        Yield :: _ => simpleYieldExpression ts
      | _ => 
            let
                val (ts2,nd2) = logicalOrExpression(ts,NOLIST,b)
            in case ts2 of
                QuestionMark :: ts3 => 
                    let
                        val (ts4,nd4) = assignmentExpression(ts3,NOLIST,b)
                    in case ts4 of
                        Colon :: ts5 =>
                            let
                                val (ts6,nd6) = assignmentExpression(ts5,NOLIST,b)
                            in
                                (ts6,nd6)
                            end
                      | _ => raise ParseError                            
                    end
              | _ => 
                    (trace(["<< conditionalExpression NOLIST with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end

(*
    NonAssignmentExpression(allowList, b)   
        LetExpression(b)
        YieldExpression(b)
        LogicalOrExpression(b)
        LogicalOrExpression(b)  ?  NonAssignmentExpression(allowLet, b)  
                               :  NonAssignmentExpression(allowLet, b)
    
    NonAssignmentExpression(noList, b)    
        SimpleYieldExpression
        LogicalOrExpression(b)
        LogicalOrExpression(b)  ?  NonAssignmentExpression(allowLet, b)  
                               :  NonAssignmentExpression(noLet, b)
*)

and nonAssignmentExpression (ts,ALLOWLIST,b) =
    let val _ = trace([">> nonAssignmentExpression ALLOWLIST with next=",tokenname(hd(ts))])
    in case ts of
        Let :: _ => letExpression(ts,b)
      | Yield :: _ => yieldExpression(ts,b)
      | _ => 
            let
                val (ts2,nd2) = logicalOrExpression(ts,ALLOWLIST,b)
            in case ts2 of
                QuestionMark :: ts3 => 
                    let
                        val (ts4,nd4) = nonAssignmentExpression(ts3,ALLOWLIST,b)
                    in case ts4 of
                        Colon :: ts5 =>
                            let
                                val (ts6,nd6) = nonAssignmentExpression(ts5,ALLOWLIST,b)
                            in
                                (ts6,nd6)
                            end
                      | _ => raise ParseError                            
                    end
              | _ => 
                    (trace(["<< nonAssignmentExpression ALLOWLIST with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end
 
  | nonAssignmentExpression (ts,NOLIST,b) =
    let val _ = trace([">> nonAssignmentExpression NOLIST with next=",tokenname(hd(ts))])
    in case ts of
        Yield :: _ => simpleYieldExpression ts
      | _ => 
            let
                val (ts2,nd2) = logicalOrExpression(ts,NOLIST,b)
            in case ts2 of
                QuestionMark :: ts3 => 
                    let
                        val (ts4,nd4) = nonAssignmentExpression(ts3,NOLIST,b)
                    in case ts4 of
                        Colon :: ts5 =>
                            let
                                val (ts6,nd6) = nonAssignmentExpression(ts5,NOLIST,b)
                            in
                                (ts6,nd6)
                            end
                      | _ => raise ParseError                            
                    end
              | _ => 
                    (trace(["<< nonAssignmentExpression NOLIST with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end


(*
    LetExpression(b)    
        let  (  LetBindingList  )  ListExpression(b)
*)

and letExpression (ts,b) =
    let val _ = trace([">> letExpression with next=",tokenname(hd(ts))])
    in case ts of
        Let :: LeftParen :: ts1 => 
            let
                val (ts2,nd2) = letBindingList (ts1)
            in 
            case ts2 of
                RightParen :: ts3 =>
                    let
                        val (ts4,nd4) = listExpression(ts3,b)
                    in
                        (trace(["<< letExpression with next=",tokenname(hd(ts4))]);
                        (ts4,Ast.LetExpr{defs=nd2,body=nd4,head=NONE}))
                    end
               |    _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    LetBindingList    
        empty
        NonemptyLetBindingList

    NonemptyLetBindingList    
        VariableBindingallowIn
        VariableBindingallowIn  ,  NonemptyLetBindingList
*)

and letBindingList (ts) : (token list * Ast.BINDINGS) =
    let val _ = trace([">> letBindingList with next=",tokenname(hd(ts))]) 
        fun nonemptyLetBindingList (ts) : (token list * Ast.BINDINGS) = 
            let
                val (ts1,(b1,i1)) = variableBinding (ts,NOLIST,ALLOWIN)
            in case ts1 of
                RightParen :: _ => (ts1,(b1,i1))
              | Comma :: _ =>
                    let
                        val (ts2,(b2,i2)) = nonemptyLetBindingList (tl ts1)
                    in
                        (trace(["<< nonemptyLetBindingList with next=",tokenname(hd ts2)]);
                        (ts2,(b1@b2,i1@i2)))
                    end
              | _ => raise ParseError
            end
    in case ts of 
        RightParen :: _ => 
            (trace(["<< nonemptyLetBindingList with next=",tokenname(hd ts)]);
            (ts,([],[])))
      | _ => 
            let
                val (ts1,nd1) = nonemptyLetBindingList ts
            in
                (trace(["<< letBindingList with next=",tokenname(hd ts1)]);
                (ts1,nd1))
            end
    end

(*
    YieldExpressionb    
        yield
        yield  [no line break]  ListExpressionb
*)

and yieldExpression (ts,b) =
    let val _ = trace([">> yieldExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        Yield :: ts1 => 
            let
            in case ts1 of
                (SemiColon :: _ | RightBrace :: _ | RightParen :: _) => (ts1,Ast.YieldExpr NONE)
              | _ => 
                    let
                        val (ts2,nd2) = listExpression(ts1,b)
                    in
                        (ts2,Ast.YieldExpr(SOME nd2))
                    end
            end
      | _ => raise ParseError
    end

(*
    SimpleYieldExpression    
        yield
*)

and simpleYieldExpression ts =
    case ts of
        Yield :: ts1 => (ts1,Ast.YieldExpr NONE)
      | _ => raise ParseError

(*
    AssignmentExpression(a, b)    
        ConditionalExpression(a, b)
        Pattern(a, b, allowExpr)  =  AssignmentExpression(a, b)
        SimplePattern(a, b, allowExpr)  CompoundAssignmentOperator  AssignmentExpression(a, b)
        SimplePattern(a, b, allowExpr)  LogicalAssignmentOperator  AssignmentExpression(a, b)
        
    CompoundAssignmentOperator    
        *=
        /=
        %=
        +=
        -=
        <<=
        >>=
        >>>=
        &=
        ^=
        |=
        &&=
        ||=
*)

and assignmentExpression (ts,a,b) : (token list * Ast.EXPR) = 
    let val _ = trace([">> assignmentExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = conditionalExpression(ts,a,b)
    in case ts1 of
        Assign :: _ => 
            let
                fun isInitStep (b:Ast.INIT_STEP) : bool =
                    case b of 
                       Ast.InitStep _ => true
                     | _ => false

                fun makeSetExpr (Ast.AssignStep (lhs,rhs)) = Ast.SetExpr (Ast.Assign,lhs,rhs)

                val p = patternFromExpr nd1
                val (ts2,nd2) = assignmentExpression(tl ts1,a,b) 
                val (binds,inits) = desugarPattern p NONE (SOME nd2)
                val (inits,assigns) = List.partition isInitStep inits    (* separate init steps and assign steps *)
                val sets = map makeSetExpr assigns
            in case binds of
                [] => (ts2,hd sets)
              | _ => (ts2,Ast.LetExpr {defs=(binds,inits),  
                                         body=Ast.ListExpr sets,
                                         head=NONE})
                        (* introduce a letexpr to narrow the scope of the temps *)
            end
          | ( ModulusAssign :: _ 
          | LogicalAndAssign :: _
          | BitwiseAndAssign :: _
          | DivAssign :: _
          | BitwiseXorAssign :: _
          | LogicalOrAssign :: _
          | BitwiseOrAssign :: _
          | PlusAssign :: _
          | LeftShiftAssign :: _
          | MinusAssign :: _
          | RightShiftAssign :: _
          | UnsignedRightShiftAssign :: _
          | MultAssign :: _ ) =>
            let
                val (ts2,nd2) = compoundAssignmentOperator ts1
                val (ts3,nd3) = assignmentExpression(tl ts1,a,b)                        
            in
                (ts3,Ast.SetExpr(nd2,nd1,nd3))
            end
      | _ =>
            (trace(["<< assignmentExpression with next=",tokenname(hd(ts1))]);
            (ts1,nd1))
    end

(*
    CompoundAssignmentOperator
        *=
        /=
        %=
        +=
        -=
        <<=
        >>=
        >>>=
        &=
        ^=
        |=
        &&=
        ||=
*)

    and compoundAssignmentOperator ts =
        case ts of
            ModulusAssign :: _                 => (tl ts,Ast.AssignRemainder NONE)
          | LogicalAndAssign :: _             => (tl ts,Ast.AssignLogicalAnd)
          | BitwiseAndAssign :: _             => (tl ts,Ast.AssignBitwiseAnd)
          | DivAssign :: _                     => (tl ts,Ast.AssignDivide NONE)
          | BitwiseXorAssign :: _             => (tl ts,Ast.AssignBitwiseXor)
          | LogicalOrAssign :: _             => (tl ts,Ast.AssignLogicalOr)
          | BitwiseOrAssign :: _             => (tl ts,Ast.AssignBitwiseOr)
          | PlusAssign :: _                 => (tl ts,Ast.AssignPlus NONE)
          | LeftShiftAssign :: _             => (tl ts,Ast.AssignLeftShift)
          | MinusAssign :: _                 => (tl ts,Ast.AssignMinus NONE)
          | RightShiftAssign :: _               => (tl ts,Ast.AssignRightShift)
          | UnsignedRightShiftAssign :: _     => (tl ts,Ast.AssignRightShiftUnsigned)
          | MultAssign :: _                   => (tl ts,Ast.AssignTimes NONE)
          | _ => raise ParseError

(*
    ListExpression(b)    
        AssignmentExpression(b)
        ListExpression(b)  ,  AssignmentExpression(b)

    right recursive:

    ListExpression(b)    
        AssignmentExpression(b) ListExpressionPrime(b)
    
    ListExpressionPrime(b)    
        empty
        , AssignmentExpression(b) ListExpressionPrime(b)
*)

and listExpression (ts,b) : (token list * Ast.EXPR) = 
    let
        val _ =    trace([">> listExpression with next=",tokenname(hd ts)])
        fun listExpression' (ts,b) : (token list * Ast.EXPR list) =
            let
                val _ =    trace([">> listExpression' with next=",tokenname(hd ts)])
            in case ts of
                Comma :: _ =>
                    let
                        val (ts1,nd1) = assignmentExpression(tl ts,ALLOWLIST,b)
                        val (ts2,nd2) = listExpression'(ts1,b)
                    in
                        (trace(["<< listExpression' with next=",tokenname(hd(ts2))]);
                        (ts2, nd1 :: nd2))
                    end
              | _ => 
                    (trace(["<< listExpression' with next=",tokenname(hd(ts))]);
                    (ts, []))
            end
        val (ts1,nd1) = assignmentExpression(ts,ALLOWLIST,b)
        val (ts2,nd2) = listExpression'(ts1,b)
    in
        trace(["<< listExpression with next=",tokenname(hd(ts2))]);
        (ts2, Ast.ListExpr (nd1 :: nd2))
    end

(*
    Pattern(a, b, g)    
        SimplePattern(a, b, g)
        ObjectPattern(g)
        ArrayPattern(g)
*)

and pattern (ts,a,b,g) : (token list * PATTERN) =
    let
    in case ts of
        LeftBrace :: _ => objectPattern (ts,g)
      | LeftBracket :: _ => arrayPattern (ts,g)
      | _ => simplePattern (ts,a,b,g)
    end

and patternFromExpr (e) : (PATTERN) =
    let val _ = trace([">> patternFromExpr"])
    in case e of
        Ast.LiteralExpr (Ast.LiteralObject {...}) => objectPatternFromExpr (e)
      | Ast.LiteralExpr (Ast.LiteralArray {...}) => arrayPatternFromExpr (e)
      | _ => simplePatternFromExpr (e)
    end

and patternFromListExpr (Ast.ListExpr (e::[])) : (PATTERN) = patternFromExpr e
  | patternFromListExpr (_)  = (error(["invalid pattern"]); raise ParseError)

(*
    SimplePattern(a, b, noExpr)    
        Identifier
        
    SimplePattern(a, b, allowExpr)    
        PostfixExpression(a, b)
*)

and simplePattern (ts,a,b,NOEXPR) =
    let val _ = trace([">> simplePattern(a,b,NOEXPR) with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = identifier ts
    in
        trace(["<< simplePattern(a,b,NOEXPR) with next=",tokenname(hd(ts1))]);
        (ts1,IdentifierPattern nd1)
    end
  | simplePattern (ts,a,b,ALLOWEXPR) =
    let val _ = trace([">> simplePattern(a,b,ALLOWEXPR) with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = leftHandSideExpression (ts,a,b)
    in
        (trace(["<< simplePattern(a,b,ALLOWEXPR) with next=",tokenname(hd(ts1))]);
        (ts1,SimplePattern nd1))
    end

and simplePatternFromExpr (e) : (PATTERN) =  (* only ever called from ALLOWEXPR contexts *)
    let val _ = trace([">> simplePatternFromExpr"])
    in case e of
        (Ast.ObjectRef _ | Ast.LexicalRef _) =>
            (trace(["<< simplePatternFromExpr"]);
            SimplePattern e)
      | _ => 
            (error(["invalid pattern expression"]);
            raise ParseError)
    end

(*
    ObjectPattern(g)   
        {  DestructuringFieldList(g)  }
*)

and objectPattern (ts,g) =
    let val _ = trace([">> objectPattern with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftBrace :: ts =>
            let
                val (ts1,nd1) = destructuringFieldList (ts,g)
            in case ts1 of
                RightBrace :: _ => (tl ts1,ObjectPattern nd1)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

and objectPatternFromExpr e =
    let val _ = trace([">> objectPatternFromExpr"])
    in case e of
        (Ast.LiteralExpr (Ast.LiteralObject {expr,ty})) =>
            let
                val p = destructuringFieldListFromExpr expr
            in
                trace(["<< objectPatternFromExpr"]);
                ObjectPattern p
            end
      | _ => raise ParseError
    end
    
(*
    DestructuringFieldList(g)
        DestructuringField(g)
        DestructuringFieldList(g)  ,  DestructuringField(g)
*)

and destructuringFieldList (ts,g) =
    let val _ = trace([">> destructuringFieldList with next=",tokenname(hd(ts))])
        fun destructuringFieldList' (ts,g) =
            let
            in case ts of
                Comma :: _ =>
                    let
                        val (ts1,nd1) = destructuringField (tl ts,g)
                        val (ts2,nd2) = destructuringFieldList' (ts1,g)
                    in
                        (ts2,nd1::nd2)
                    end
              | _ => (ts,[])
            end
        val (ts1,nd1) = destructuringField (ts,g)
        val (ts2,nd2) = destructuringFieldList' (ts1,g)
    in
        (ts2,nd1::nd2)
    end

and destructuringFieldListFromExpr e =
    let val _ = trace([">> destructuringFieldList"])
        fun destructuringFieldListFromExpr' e =
            let
            in case e of
                [] =>
                    let
                    in
                        []
                    end
              | _ => 
                let
                    val p1 = destructuringFieldFromExpr (hd e)
                    val p2 = destructuringFieldListFromExpr' (tl e)
                in
                    p1::p2
                end
            end
        val p1 = destructuringFieldFromExpr (hd e)
        val p2 = destructuringFieldListFromExpr' (tl e)
    in
        trace(["<< destructuringFieldList"]);
        p1::p2
    end


(*
    DestructuringField(g)
        NonAttributeQualifiedIdentifier  :  Pattern(NOLIST,ALLOWIN,g)
*)

and destructuringField (ts,g) =
    let
        val (ts1,nd1) = fieldName ts
    in case ts1 of
        Colon :: _ => 
            let
                val (ts2,nd2) = pattern (tl ts1,NOLIST,ALLOWIN,g)
            in
                (ts2,{ident=nd1,pattern=nd2})
            end
      | _ => raise ParseError
    end

and destructuringFieldFromExpr e =
    let val _ = trace([">> destructuringFieldFromExpr"])
        val {kind,name,init} = e
        val p = patternFromExpr init
    in
        trace(["<< destructuringFieldFromExpr"]);
        {ident=name,pattern=p}
    end

(*
    ArrayPattern(g)   
        [  DestructuringElementList(g)  ]
*)

and arrayPattern (ts,g) =
    let
    in case ts of
        LeftBracket :: _ => 
            let
                val (ts1,nd1) = destructuringElementList (tl ts,g)
            in case ts1 of
                RightBracket :: _ => (tl ts1,ArrayPattern nd1)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

and arrayPatternFromExpr (e) =
    let val _ = trace([">> arrayPatternFromExpr"])
    in case e of
        Ast.LiteralExpr (Ast.LiteralArray {exprs,ty}) =>
            let
                val p = destructuringElementListFromExpr exprs
            in
                trace(["<< arrryPatternFromExpr"]);
                ArrayPattern p
            end
      | _ => raise ParseError
    end

(*
    DestructuringElementList(g)   
        empty
        DestructuringElement(g)
        , DestructuringElementList(g)
        DestructuringElement(g) , DestructuringElementList(g)
*)

and destructuringElementList (ts,g) =
    let val _ = trace([">> destructuringElementList with next=",tokenname(hd(ts))]) 
    in case ts of
        RightBracket :: _ => (ts,[])
      | Comma :: _ => 
            let
                val (ts1,nd1) = destructuringElementList (tl ts,g)
            in
                (ts1,SimplePattern(Ast.LiteralExpr(Ast.LiteralUndefined)) :: nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = destructuringElement (ts,g)
            in case ts1 of
                Comma :: _ =>
                    let
                        val (ts2,nd2) = destructuringElementList (tl ts1,g)
                    in
                        (ts2,nd1::nd2)
                    end
              | _ => (ts1,nd1::[])
            end
    end

and destructuringElementListFromExpr e =
    let val _ = trace([">> destructuringFieldList"])
        fun destructuringElementListFromExpr' e =
            let
            in case e of
                [] =>
                    let
                    in
                        []
                    end
              | _ => 
                let
                    val p1 = destructuringElementFromExpr (hd e)
                    val p2 = destructuringElementListFromExpr' (tl e)
                in
                    p1::p2
                end
            end
        val p1 = destructuringElementFromExpr (hd e)
        val p2 = destructuringElementListFromExpr' (tl e)
    in
        trace(["<< destructuringElementList"]);
        p1::p2
    end

(*
    DestructuringElement(g)   
        Pattern(NOLIST,ALLOWIN,g)
*)

and destructuringElement (ts,g) =
    let val _ = trace([">> destructuringElement with next=",tokenname(hd(ts))])         
    in
        pattern (ts,NOLIST,ALLOWIN,g)
    end

and destructuringElementFromExpr e =
    let val _ = trace([">> destructuringElementFromExpr"])
        val p = patternFromExpr e
    in
        trace(["<< destructuringElementFromExpr"]);
        p
    end

(*
    TypedIdentifier(a,b)    
        SimplePattern(a,b,noExpr)
        SimplePattern(a,b,noExpr)  :  TypeExpression
*)

and typedIdentifier (ts,ns) =
    let val _ = trace([">> typedIdentifier with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = simplePattern (ts,NOLIST,NOIN,NOEXPR)
    in case ts1 of
        Colon :: _ => 
            let
                val (ts2,nd2) = nullableTypeExpression (tl ts1)
            in
                (ts2, {p=nd1,t=SOME nd2})
            end
      | _ => 
            let
            in
                (trace(["<< typedIdentifier with next=",tokenname(hd(ts1))]);
                (ts1, {p=nd1,t=NONE}))
            end
        
    end

(*        
    TypedPattern(a,b,g)
        SimplePattern(a,b,g)
        SimplePattern(a,b,g)  :  TypeExpression
        ObjectPattern
        ObjectPattern  :  ObjectType
        ArrayPattern
        ArrayPattern  :  ArrayType
*)

and typedPattern (ts,a,b) =
    let val _ = trace([">> typedPattern with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftBrace :: _ => 
            let
                val (ts1,nd1) = objectPattern (ts,NOEXPR)
            in case ts1 of
                Colon :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in
                        (ts2,{p=nd1,t=SOME nd2})
                    end
              | _ =>
                    (ts1,{p=nd1,t=NONE})
            end
      | LeftBracket :: _ => 
            let
                val (ts1,nd1) = arrayPattern (ts,NOEXPR)
            in case ts1 of
                Colon :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in
                        (ts2,{p=nd1,t=SOME nd2})
                    end
              | _ =>
                    (ts1,{p=nd1,t=NONE})
            end
      | _ =>
            let
                val (ts1,nd1) = simplePattern (ts,a,b,NOEXPR)
            in case ts1 of
                Colon :: _ =>
                    let
                        val (ts2,nd2) = nullableTypeExpression (tl ts1)
                    in
                        (ts2,{p=nd1,t=SOME nd2})
                    end
              | _ =>
                    (trace(["<< typedPattern with next=",tokenname(hd ts1)]);
                    (ts1,{p=nd1,t=NONE}))
            end
    end

(*
    TYPE EXPRESSIONS
*)

(*
    NullableTypeExpression    
        null
        undefined
        TypeExpression
        TypeExpression  ?
        TypeExpression  !
        
    TypeExpression    
        FunctionType
        UnionType
        RecordType
        ArrayType
        TypeIdentifier
*)

and nullableTypeExpression (ts) : (token list * Ast.TYPE_EXPR) =
    let val _ = trace([">> nullableTypeExpression with next=",tokenname(hd ts)])
    in case ts of
        Null :: _ => (tl ts, Ast.SpecialType Ast.Null)
      | Undefined :: _ => (tl ts, Ast.SpecialType Ast.Undefined)
      | _ =>
            let
                val (ts1,nd1) = typeExpression ts
            in case ts1 of
                Not :: _ =>
                    (tl ts1,Ast.NullableType {expr=nd1,nullable=false})
              | QuestionMark :: _ =>
                    (tl ts1,Ast.NullableType {expr=nd1,nullable=true}) 
              | _ =>
                    (ts1,nd1) 
            end
    end

and typeExpression (ts) : (token list * Ast.TYPE_EXPR) =
    let val _ = trace([">> typeExpression with next=",tokenname(hd ts)])
    in case ts of
        Function :: _ => functionType ts
      | LeftParen :: _ => unionType ts
      | LeftBrace :: _ => objectType ts
      | LeftBracket :: _ => arrayType ts
      | _ =>
            let
                val (ts1,nd1) = typeIdentifier ts
            in
                (ts1,needType(nd1,NONE))
            end
    end

(*
    FunctionType    
        function  FunctionSignature
*)

and functionType (ts) : (token list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> functionType with next=",tokenname(hd(ts))])         
    in case ts of
        Function :: _ => 
            let
                val (ts1,nd1) = functionSignatureType (tl ts)
            in
                trace(["<< functionType with next=",tokenname(hd ts1)]);
                (ts1, (Ast.FunctionType (functionTypeFromSignature nd1)))
            end
      | _ => raise ParseError
    end

and functionTypeFromSignature fsig : Ast.FUNC_TYPE = 
    let
        fun paramTypes (params:Ast.BINDING list) : (Ast.TYPE_EXPR list) =
                    case params of
                        [] => []
                      | _ =>
                            let
                                val (Ast.Binding {ident,ty}) = hd params
                                val types = paramTypes (tl params)
                            in case (pattern,ty) of
                                (_,SOME t) => t :: types
                              | (_,NONE) => Ast.SpecialType Ast.Any::types
                            end
    in 
       case fsig of
        Ast.FunctionSignature {typeParams,params,returnType,thisType,hasRest,...} =>
            let
                val (b,i) = params
                val types = paramTypes b
                val minArgs = 0  (* FIXME *)
            in
                {typeParams=typeParams,
                 params=types,
                 result=returnType,
                 thisType=thisType,
                 hasRest=hasRest,
                 minArgs=minArgs}
            end
    end
    
(*
    UnionType    
        (  TypeExpressionList  )
*)

and unionType (ts) : (token list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> unionType with next=",tokenname(hd(ts))])         
    in case ts of
        LeftParen :: _ => 
            let
                val (ts1,nd1) = typeExpressionList (tl ts)
            in case ts1 of
                RightParen :: _ =>
                    (tl ts1, Ast.UnionType nd1)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    ObjectType    
        {  FieldTypeList  }
*)

and objectType (ts) : (token list * Ast.TYPE_EXPR) = 
    let val _ = trace([">> objectType with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftBrace :: ts1 => 
            let
                val (ts2,nd2) = fieldTypeList ts1
            in case ts2 of
                RightBrace :: ts3 => 
                    (trace(["<< objectType with next=",tokenname(hd(ts3))]);
                    (ts3,Ast.ObjectType nd2))
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    FieldTypeList
        empty
        NonemptyFieldTypeList

    NonemptyFieldTypeList
        FieldType
        FieldType  ,  NonemptyFieldTypeList
*)

and fieldTypeList ts =
    let val _ = trace([">> fieldTypeList with next=",tokenname(hd(ts))]) 
        fun nonemptyFieldTypeList (ts) =
            let
                val (ts1,nd1) = fieldType(ts)
            in case ts1 of
                Comma :: _ => 
                    let
                        val (ts2,nd2) = nonemptyFieldTypeList (tl ts1)
                    in
                        (ts2,nd1::nd2)
                    end
              | _ => (ts1,nd1::[])
            end
    in case ts of
        RightBrace :: _ => (ts,[])
      | _ => 
        let
            val (ts1,nd1) = nonemptyFieldTypeList (ts)
        in
            (ts1,nd1)
         end
    end

(*
    FieldType    
        FieldName  :  TypeExpression    
*)

and fieldType (ts) : (token list * Ast.FIELD_TYPE) =
    let val _ = trace([">> fieldType with next=",tokenname(hd(ts))]) 
        val (ts1,Ast.Identifier{ident=nd1,...}) = fieldName ts   (* fixme: non exhaustive *)
    in case ts1 of
        Colon :: _ =>
            let
                val (ts2,nd2) = nullableTypeExpression (tl ts1)
            in
                (ts2,{name=nd1,ty=nd2})
            end
      | _ => raise ParseError
    end

(*
    ArrayType    
        [  ElementTypeList  ]
*)

and arrayType (ts) : (token list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> arrayType with next=",tokenname(hd(ts))]) 
    in case ts of
        LeftBracket :: _ => 
            let
                val (ts1,nd1) = elementTypeList (tl ts)
            in case ts1 of
                RightBracket :: _ => (tl ts1,Ast.ArrayType nd1)
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    ElementTypeList    
        empty
        TypeExpression
        ,  ElementTypeList
        TypeExpression  ,  ElementTypeList
*)

and elementTypeList (ts) : token list * Ast.TYPE_EXPR list =
    let val _ = trace([">> elementTypeList with next=",tokenname(hd(ts))]) 
    in case ts of
        RightBracket :: _ => (ts,[])
      | Comma :: _ => 
            let
                val (ts1,nd1) = elementTypeList (tl ts)
            in
                (ts1,Ast.SpecialType(Ast.Any) :: nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = nullableTypeExpression (ts)
            in case ts1 of
                Comma :: _ =>
                    let
                        val (ts2,nd2) = elementTypeList (tl ts1)
                    in
                        (ts2,nd1::nd2)
                    end
              | _ => (ts1,nd1::[])
            end
    end

(*
    TypeExpressionList    
        TypeExpression
        TypeExpressionList  ,  TypeExpression
*)

and typeExpressionList (ts)
    : (token list * Ast.TYPE_EXPR list) = 
    let
        fun typeExpressionList' (ts) =
            let
            in case ts of
                Comma :: _ =>
                    let
                        val (ts1,nd1) = nullableTypeExpression (tl ts)
                        val (ts2,nd2) = typeExpressionList' ts1
                    in
                        (ts2,nd1::nd2)
                    end
              | _ => (ts,[])
            end
        val (ts1,nd1) = nullableTypeExpression ts
        val (ts2,nd2) = typeExpressionList' ts1
    in
        (ts2,nd1::nd2)
    end

(* STATEMENTS *)

(*

Statementw    
    Block
    BreakStatement Semicolonw
    ContinueStatement Semicolonw
    DefaultXMLNamespaceStatement Semicolonw
    DoStatement Semicolonw
    ExpressionStatement Semicolonw
    ForStatementw
    IfStatementw
    LabeledStatementw
    LetStatementw
    ReturnStatement Semicolon(omega)
    SuperStatement Semicolonw
    SwitchStatement
    ThrowStatement Semicolonw
    TryStatement
    WhileStatementw
    WithStatementw
*)

and semicolon (ts,FULL) : (token list) =
    let val _ = trace([">> semicolon(FULL) with next=", tokenname(hd ts)])
    in case ts of
        SemiColon :: _ => (tl ts)
      | (Eof | RightBrace) :: _ => (ts)   (* ABBREV special cases *)
      | _ => 
            if newline ts then (log(["inserting semicolon"]);(ts))
            else (error(["expecting semicolon before ",tokenname(hd ts)]); raise ParseError)
    end
  | semicolon (ts,_) =
    let val _ = trace([">> semicolon(ABBREV | NOSHORTIF) with next=", tokenname(hd ts)])
    in case ts of
        SemiColon :: _ => (tl ts)
      | _ => 
          (trace(["<< semicolon(ABBREV | NOSHORTIF) with next=", tokenname(hd ts)]);
          (ts))
    end

and statement (ts,w) : (token list * Ast.STMT) =
    let val _ = trace([">> statement with next=", tokenname(hd ts)])
    in case ts of
        If :: _ =>
            let
                val (ts1,nd1) = ifStatement (ts,w)
            in
                (ts1,nd1)
            end
      | Return :: _ =>
            let
                val (ts1,nd1) = returnStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | LeftBrace :: _ =>
            let
                val (ts1,nd1) = blockStatement ts
            in
                (ts1,nd1)
            end
      | Switch :: _ =>
            let
                val (ts1,nd1) = switchStatement ts
            in
                (ts1,nd1)
            end
      | Super :: _ =>
            let
                val (ts1,nd1) = superStatement ts
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | Do :: _ =>
            let
                val (ts1,nd1) = doStatement ts
            in
                (ts1,nd1)
            end
      | While :: _ =>
            let
                val (ts1,nd1) = whileStatement (ts,w)
            in
                (ts1,nd1)
            end
      | For :: _ =>
            let
                val (ts1,nd1) = forStatement (ts,w)
            in
                (ts1,nd1)
            end
      | Let :: LeftParen :: _ =>
            let
                val (ts1,nd1) = letStatement (ts,w)
            in
                (ts1,nd1)
            end
      | With :: _ =>
            let
                val (ts1,nd1) = withStatement (ts,w)
            in
                (ts1,nd1)
            end
      | Identifier _ :: Colon :: _ =>
            let
                val (ts1,nd1) = labeledStatement (ts,w)
            in
                (ts1,nd1)
            end
      | Continue :: _ =>
            let
                val (ts1,nd1) = continueStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | Break :: _ =>
            let
                val (ts1,nd1) = breakStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | Throw :: _ =>
            let
                val (ts1,nd1) = throwStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | Try :: _ =>
            let
                val (ts1,nd1) = tryStatement (ts)
            in
                (ts1,nd1)
            end
      | Default :: Xml :: Namespace :: Assign :: _ =>
            let
                val (ts1,nd1) = defaultXmlNamespaceStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | _ =>
            let
                val (ts1,nd1) = expressionStatement (ts)
                 val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                trace(["<< statement with next=", tokenname(hd ts2)]);
                (ts2,nd2)
            end
    end

(*
    Substatement(w)    
        EmptyStatement
        Statement(w)
*)

and substatement (ts,w) : (token list * Ast.STMT) =
    let val _ = trace([">> substatement with next=", tokenname(hd ts)])
    in case ts of
        SemiColon :: _ =>
            (tl ts, Ast.EmptyStmt)
      | _ => 
            statement(ts,w)
    end

(*    
Semicolonabbrev     
    ;
    VirtualSemicolon
    empty
    
SemicolonnoShortIf    
    ;
    VirtualSemicolon
    empty
    
Semicolonfull    
    ;
    VirtualSemicolon

*)

(*
    EmptyStatement     
        ;
*)

and emptyStatement ts =
    let
    in case ts of
        SemiColon :: ts1 => (ts1,Ast.EmptyStmt)
      | _ => raise ParseError
    end

(*
    BlockStatement     
        Block
*)

and blockStatement (ts) =
    let val _ = trace([">> blockStatement with next=", tokenname(hd ts)])
    in case ts of
        LeftBrace :: _ => 
            let
                val (ts1,nd1) = block (ts,LOCAL)
            in
                trace(["<< blockStatement with next=", tokenname(hd ts)]);
                (ts1,Ast.BlockStmt nd1)
            end
      | _ => raise ParseError
    end

(*
    LabeledStatement(w)
        Identifier  :  Substatement(w)

*)

and labeledStatement (ts,w) =
    let
    in case ts of
        Identifier id :: Colon :: _  =>
            let
                val (ts1,nd1) = substatement (tl (tl ts),w)
            in
                (ts1,Ast.LabeledStmt (id,nd1))
            end
      | _ => raise ParseError
    end

(*
    
ExpressionStatement    
    [lookahead !{ function, { }] ListExpression (allowLet,allowIn)

*)

and expressionStatement (ts) : (token list * Ast.STMT) =
    let val _ = trace([">> expressionStatement with next=", tokenname(hd ts)])
        val (ts1,nd1) = listExpression(ts,ALLOWIN)
    in
        trace(["<< expressionStatement with next=", tokenname(hd ts1)]);
        (ts1,Ast.ExprStmt(nd1))
    end

(*
    SuperStatement    
        super Arguments
*)

and superStatement (ts) : (token list * Ast.STMT) =
    let val _ = trace([">> superStatement with next=", tokenname(hd ts)])
    in case ts of
        Super :: _ =>
            let
                val (ts1,nd1) = arguments (tl ts)
            in
                (ts1,Ast.SuperStmt (Ast.ListExpr nd1))
            end
      | _ => raise ParseError
    end

(*
    SwitchStatement    
        switch ParenListExpression  {  CaseElements  }
        switch  type  (  ListExpression(allowList, allowIn)  :  TypeExpression  ) 
             {  TypeCaseElements  }
        
    CaseElements    
        empty
        CaseLabel
        CaseLabel CaseElementsPrefix
        CaseLabel CaseElementsPrefix Directive(abbrev)
        
    CaseElementsPrefix    
        empty
        CaseElementsPrefix  CaseLabel
        CaseElementsPrefix  Directive(full)
    
    right recursive: 
            
    CaseElementsPrefix    
        empty
        CaseLabel CaseElementsPrefix
        Directive(full) CaseElementsPrefix
                
    CaseLabel    
        case ListExpression(allowIn) :
        default :
*)

and switchStatement (ts) : (token list * Ast.STMT) =
    let val _ = trace([">> switchStatement with next=", tokenname(hd ts)])
    in case ts of
        Switch :: Type :: LeftParen :: _ =>
            let
                val (ts1,nd1) = listExpression (tl (tl (tl ts)),ALLOWIN)
            in case ts1 of
                Colon :: _ => 
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in case ts2 of
                        RightParen :: LeftBrace :: _ =>
                            let
                                val (ts3,nd3) = typeCaseElements (tl (tl ts2))
                            in case ts3 of
                                RightBrace :: _ => (tl ts3,Ast.SwitchTypeStmt{cond=nd1,ty=nd2,cases=nd3})
                              | _ => raise ParseError
                            end
                      | _ => raise ParseError
                    end
              | _ => raise ParseError
            end
      | Switch :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
            in case ts1 of
                (LeftBrace :: _) =>
                    let
                        val (ts2,nd2) = caseElements (tl ts1)
                    in case ts2 of
                        RightBrace :: _ => (tl ts2,Ast.SwitchStmt{cond=nd1,cases=nd2})
                      | _ => raise ParseError
                    end
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

and isDefaultCase (x) =
    case x of 
        NONE => true
      | _ => false

and caseElements (ts) : (token list * Ast.CASE list) =
    let val _ = trace([">> caseElements with next=", tokenname(hd ts)])
    in case ts of
        RightBrace :: _ =>
            (tl ts,[])
      | (Case | Default) :: _ =>
            let
                val (ts1,nd1) = caseLabel (ts,false)
                val (ts2,nd2) = caseElementsPrefix (ts1,isDefaultCase nd1)
            in case nd2 of
                [] =>
                    let
                    in
                        (ts2,{label=nd1,
                              body=Ast.Block {pragmas=[],defns=[],body=[],head=NONE},
                              inits=NONE}::[])
                    end
              | first :: follows =>
                    let
                    in
                        (ts2,{label=nd1,body=(#body first),inits=NONE} :: follows)
                    end
            end
      | _ => raise ParseError
    end

and caseElementsPrefix (ts,has_default) : (token list * Ast.CASE list) =
    let val _ = trace([">> caseElementsPrefix with next=", tokenname(hd ts)])
    in case ts of
        RightBrace :: _ =>
            (ts,[])
      | (Case | Default) :: _ =>
            let
                val (ts1,nd1) = caseLabel (ts,has_default)
                val (ts2,nd2) = caseElementsPrefix (ts1,has_default orelse (isDefaultCase nd1))
            in case nd2 of
                [] =>
                    let
                    in
                        (* append an empty CASE to the start of the list to
                           seed the list of previously parsed directives,
                           which get added when the stack unwinds *)

                        (ts2,{label=NONE,body=Ast.Block{pragmas=[],defns=[],body=[],head=NONE},inits=NONE} ::
                            ({label=nd1,body=Ast.Block{pragmas=[],defns=[],body=[],head=NONE},inits=NONE}::[]))
                    end
              | first :: follows =>
                    let
                    in
                        (ts2,{label=NONE,body=Ast.Block{pragmas=[],defns=[],body=[],head=NONE},inits=NONE} ::
                            ({label=nd1,body=(#body first),inits=NONE} :: follows))
                    end
            end
      | _ => 
            let
                val (ts1,nd1) = directive (ts,LOCAL,FULL)
                val (ts2,nd2) = caseElementsPrefix (ts1,has_default)
            in case nd2 of
                [] =>
                    let
                    in
                        (ts2,{label=NONE,body=Ast.Block nd1,inits=NONE}::[])
                    end
              | first :: follows =>
                    let
                        val {pragmas=p1,defns=d1,body=s1, ...} = nd1
                        val {pragmas=p2,defns=d2,body=s2, ...} = (case #body first of Ast.Block b => b)
                        val body = Ast.Block{pragmas=(p1@p2),defns=(d1@d2),body=(s1@s2),head=NONE}
                    in
                        (ts2,{label=NONE,body=body,inits=NONE}::follows)
                    end
            end
    end

and caseLabel (ts,has_default) : (token list * Ast.EXPR option) =
    let val _ = trace([">> caseLabel with next=", tokenname(hd ts)])
    in case (ts,has_default) of
        (Case :: _,_) =>
            let
                val (ts1,nd1) = listExpression (tl ts,ALLOWIN)
            in case ts1 of
                Colon :: _ => (tl ts1,SOME nd1)
              | _ => raise ParseError
            end
      | (Default :: _,false) =>
            let
            in case tl ts of
                Colon :: _ => (tl (tl ts),NONE)
              | _ => raise ParseError
            end
      | (Default :: _,true) =>
            (error(["redundant default switch case"]); raise ParseError)
      | _ => raise ParseError
    end

(*
    TypeCaseBinding    
        (  TypedIdentifier  VariableInitialisation(allowList, allowIn)  )
        
    TypeCaseElements    
        TypeCaseElement
        TypeCaseElements  TypeCaseElement
        
    TypeCaseElement    
        case  (  TypedPattern(noList, allowIn, noExpr)  )  Block
        default  Block
*)

and typeCaseBinding (ts) : (token list * Ast.BINDINGS) =
    let val _ = trace([">> caseCaseBinding with next=", tokenname(hd ts)])
    in case ts of
        LeftParen :: _ =>
            let
                val (ts1,{p,t}) = typedIdentifier (tl ts,NONE)
            in case ts1 of
                Assign :: _ =>
                    let
                        val (ts2,nd2) = variableInitialisation(ts1,ALLOWLIST,ALLOWIN)
                    in case ts2 of
                        RightParen :: _ =>
                            (tl ts2, desugarPattern p t (SOME nd2))
                      | _ => raise ParseError
                    end
              | _ => 
                    let
                    in case ts1 of
                        RightParen :: _ =>
                            (tl ts1, desugarPattern p t NONE)
                      | _ => raise ParseError
                    end
            end
      | _ => 
            raise ParseError
    end

(*
    TypeCaseElements
        TypeCaseElement
        TypeCaseElements  TypeCaseElement

    TypeCaseElement
        case  (  TypedPattern(noList, noIn, noExpr)  )  Block
        default  Block

    right recursive:

    TypeCaseElements
        TypeCaseElement TypeCaseElements'

    TypeCaseElements'
        empty
        TypeCaseElement TypeCaseElements'
*)

and isDefaultTypeCase (x) =
    case x of 
        NONE => true
      | _ => false

and typeCaseElements (ts) : (token list * Ast.TYPE_CASE list) =
    let val _ = trace([">> typeCaseElements with next=", tokenname(hd ts)])
        fun typeCaseElements' (ts,has_default) : (token list * Ast.TYPE_CASE list) =
            let val _ = trace([">> typeCaseElements' with next=", tokenname(hd ts)])
            in case ts of
                (Case | Default) :: _ => 
                    let
                        val (ts1,nd1) = typeCaseElement (ts,has_default)
                        val (ts2,nd2) = typeCaseElements' (ts1,has_default orelse (isDefaultTypeCase (#ty nd1)))
                    in
                        trace(["<< typeCaseElements' with next=", tokenname(hd ts2)]);
                        (ts2,nd1::nd2)
                    end
              | _ => (ts,[])
            end
        val (ts1,nd1) = typeCaseElement (ts,false)
        val (ts2,nd2) = typeCaseElements' (ts1,isDefaultTypeCase (#ty nd1))
    in
        trace(["<< typeCaseElements with next=", tokenname(hd ts2)]);
        (ts2,nd1::nd2)
    end

and typeCaseElement (ts,has_default) 
    : (token list * Ast.TYPE_CASE) =
    let val _ = trace([">> typeCaseElement with next=", tokenname(hd ts)])
    in case (ts,has_default) of
        (Case :: LeftParen :: _,_) =>
            let
                val (ts1,{p,t}) = typedPattern(tl (tl ts),NOLIST,NOIN)
            in case ts1 of
                RightParen :: _ =>
                    let
                        val (ts2,nd2) = block (tl ts1,LOCAL)
                    in
                        trace(["<< typeCaseElement with next=", tokenname(hd ts2)]);
                        (ts2, {bindings=desugarPattern p t NONE, ty=t, body=nd2,inits=NONE})
                    end
              | _ => raise ParseError
            end
      | (Default :: _,false) =>
            let
                val (ts1,nd1) = block (tl ts,LOCAL)
            in
                trace(["<< typeCaseElement with next=", tokenname(hd ts1)]);
                (ts1, {bindings=([],[]), ty=SOME (Ast.SpecialType Ast.Any), body=nd1, inits=NONE})
            end
      | (Default :: _,true) =>
            (error(["redundant default switch type case"]); raise ParseError)
      | _ => raise ParseError
    end
(*
    
LabeledStatementw    
    Identifier : Substatementw
*)

(*
    IfStatement(abbrev)
        if ParenListExpression Substatement(abbrev)
        if ParenListExpression Substatement(noShortIf) else Substatement(abbrev)
        
    IfStatement(full)    
        if ParenListExpression Substatement(full)
        if ParenListExpression Substatement(noShortIf) else Substatement(full)
        
    IfStatementno(ShortIf)    
        if ParenListExpression Substatement(noShortIf) else Substatementno(ShortIf)
*)

and ifStatement (ts,ABBREV) =
    let val _ = trace([">> ifStatement(ABBREV) with next=", tokenname(hd ts)])
    in case ts of
        If :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1,ABBREV)
            in case ts2 of
                Else :: _ =>
                    let
                        val (ts3,nd3) = substatement(tl ts2,ABBREV)
                    in
                        (ts3,Ast.IfStmt {cnd=nd1,thn=nd2,els=nd3})
                    end
              | _ => 
                    let
                    in
                        (ts2,Ast.IfStmt {cnd=nd1,thn=nd2,els=Ast.EmptyStmt})
                    end
            end
          | _ => raise ParseError
    end
  | ifStatement (ts,FULL) =
    let val _ = trace([">> ifStatement(FULL) with next=", tokenname(hd ts)])
    in case ts of
        If :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1,FULL)
            in case ts2 of
                Else :: _ =>
                    let
                        val (ts3,nd3) = substatement(tl ts2,FULL)
                    in
                        (ts3,Ast.IfStmt {cnd=nd1,thn=nd2,els=nd3})
                    end
              | _ => 
                    let
                    in
                        (ts2,Ast.IfStmt {cnd=nd1,thn=nd2,els=Ast.EmptyStmt})
                    end
            end
          | _ => raise ParseError
    end
  | ifStatement (ts,SHORTIF) =
    let val _ = trace([">> ifStatement(SHORTIF) with next=", tokenname(hd ts)])
    in case ts of
        If :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1,NOSHORTIF)
            in case ts2 of
                Else :: _ =>
                    let
                        val (ts3,nd3) = substatement(tl ts2,SHORTIF)
                    in
                        (ts3,Ast.IfStmt {cnd=nd1,thn=nd2,els=nd3})
                    end
              | _ => 
                    raise ParseError
            end
          | _ => raise ParseError
    end

(*
    DoStatement    
        do Substatement(abbrev) while ParenListExpression
*)

and doStatement (ts) : (token list * Ast.STMT) =
    let val _ = trace([">> doStatement with next=", tokenname(hd ts)])
    in case ts of
        Do :: _ =>
            let
                val (ts1,nd1) = substatement(tl ts, ABBREV)
            in case ts1 of
                While :: _ =>
                    let
                        val (ts2,nd2) = parenListExpression (tl ts1)
                    in
                        (ts2,Ast.DoWhileStmt {body=nd1,cond=nd2,fixtures=NONE,contLabel=NONE})
                    end
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    WhileStatement(w)    
        while ParenListExpression Substatement(w)
*)

and whileStatement (ts,w) : (token list * Ast.STMT) =
    let val _ = trace([">> whileStatement with next=", tokenname(hd ts)])
    in case ts of
        While :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1, w)
            in
                (ts2,Ast.WhileStmt {cond=nd1,fixtures=NONE,body=nd2,contLabel=NONE})
            end
      | _ => raise ParseError
    end

(*
    ForStatement(w)
        for  (  ForInitialiser  ;  OptionalExpression  ;  OptionalExpression  )  Substatement(w)
        for  (  ForInBinding  in  ListExpression(allowIn)  )  Substatement(w)    
        for  each  ( ForInBinding  in  ListExpression(allowIn)  )  Substatement(w)    
            
    ForInitialiser        
        empty    
        ListExpression(noIn)
        VariableDefinition(noIn)
            
    OptionalExpression        
        ListExpression(allowIn)
        empty    

*)

(* note: even though for-each-in and for-in use ForInBinding, we get there by two different
         paths. With for-in we parse for ForInitializer, which is more general and translate
         when we see 'in'. With for-each-in, we parse immediately for ForInBinding. Positive
         and negative cases should produce the same error or ast result
*)

(* todo: code reuse opps here *)

and forStatement (ts,w) : (token list * Ast.STMT) =
    let val _ = trace([">> forStatement with next=", tokenname(hd ts)])
    in case ts of
        For :: LeftParen :: _ =>
            let
                val (ts1,defn,init) = forInitialiser (tl (tl ts))
            in case ts1 of
                SemiColon :: _ =>
                    let
                        val (ts2,nd2) = optionalExpression (tl ts1)
                        val (ts3,nd3) = optionalExpression (ts2)
                    in case ts3 of
                        RightParen :: _ =>
                            let
                                val (ts4,nd4) = substatement (tl ts3,w)
                            in 
                                (ts4,Ast.ForStmt{ 
                                            defn=defn,
                                            init=init,
                                            cond=nd2,
                                            update=nd3,
                                            contLabel=NONE,
                                            fixtures=NONE,
                                            body=nd4})
                            end
                      | _ => raise ParseError
                    end
              | In :: _ =>
                    let
                        val len = case defn of SOME {bindings=(b,i),...} => length b
                        val (b,i) = if (len > 1) 
                                    then (error(["too many bindings on left side of in"]); 
                                          raise ParseError)
                                    else if (len = 0) (* convert inits to pattern *)
                                        then case init of 
                                            Ast.ExprStmt e::[] => 
                                                desugarPattern (patternFromListExpr e) NONE NONE
                                          | _ => LogErr.internalError [""]
                                        else (#bindings (valOf defn))
                        val (ts2,nd2) = listExpression (tl ts1,ALLOWIN)
                    in case ts2 of
                        RightParen :: _ =>
                            let
                                val (ts3,nd3) = substatement (tl ts2,w)
                            in 
                                (ts3,Ast.ForInStmt{ 
                                             defn=defn,
                                             obj=nd2,
                                             contLabel=NONE,
                                             fixtures=NONE,
                                             inits=NONE,
                                             body=nd3 })
                            end
                      | _ => raise ParseError
                    end
              | _ => raise ParseError
            end
(* FIXME

      | For :: Each :: LeftParen :: _ =>
            let
                val (ts1,bindings) = forInBinding (tl (tl (tl ts)))
            in case ts1 of
                In :: _ =>
                    let
                        val (ts2,nd2) = listExpression (tl ts1,ALLOWIN)
                    in case ts2 of
                        RightParen :: _ =>
                            let
                                val (ts3,nd3) = substatement (tl ts2,w)
                            in 
                                (ts3,Ast.ForEachStmt{ 
                                             defns=bindings,
                                             obj=nd2,
                                             contLabel=NONE,
                                             fixtures=NONE,
                                             inits=NONE,
                                             body=nd3 })
                            end
                      | _ => raise ParseError
                    end
              | _ => raise ParseError
            end
*)
      | _ => raise ParseError
    end

(*
    for ( var x = 10; ... ) ...

    
*)

and forInitialiser (ts) 
    : (token list * Ast.VAR_DEFN option * Ast.STMT list) =
    let val _ = trace([">> forInitialiser with next=", tokenname(hd ts)])
    in case ts of
        (Var | Let | Const) :: _ =>
            let
                val (ts1,{defns,body,...}) = variableDefinition (ts,hd (!defaultNamespace),
                                                    false,false,NOIN,LOCAL)
            in case defns of
                (Ast.VariableDefn vd :: []) =>
                    (trace(["<< forInitialiser with next=", tokenname(hd ts1)]);
                    (ts1,SOME vd,body))   
              | _ => raise ParseError
            end
      | SemiColon :: _ =>
            let
            in
                trace(["<< forInitialiser with next=", tokenname(hd ts)]);
                (ts,NONE,[Ast.EmptyStmt])
            end
      | _ => 
            let
                val (ts1,nd1) = listExpression (ts,NOIN)
            in 
                trace ["<< forInitialiser with next=", tokenname(hd ts1)];
                (ts1,NONE,[Ast.ExprStmt nd1])
            end
    end

and optionalExpression (ts) : (token list * Ast.EXPR) =
    let val _ = trace([">> optionalExpression with next=", tokenname(hd ts)])
    in case ts of
        SemiColon :: _ =>
            let
            in
                (tl ts,Ast.ListExpr [])
            end
      | RightParen :: _ =>
            let
            in
                (ts,Ast.ListExpr [])
            end
      | _ => 
            let
                val (ts1,nd1) = listExpression (ts,NOIN)
            in case ts1 of
                SemiColon :: _ =>
                    let
                    in
                        (tl ts1,nd1)
                    end
              | _ => 
                    (ts1,nd1)
            end
    end

(*
    ForInBinding        
        Pattern(allowList, noIn, allowExpr)    
        VariableDefinitionKind VariableBinding(allowList, noIn)            
*)

and forInBinding (ts) 
    : (token list * Ast.BINDINGS) =
    let val _ = trace([">> forInBinding with next=", tokenname(hd ts)])
    in case ts of
        (Var | Let | Const) :: _ =>
            let
                val (ts1,nd1) = variableDefinitionKind ts
                val (ts2,(b,i)) = variableBinding (ts1,ALLOWLIST,NOIN)
            in 
                trace ["<< forInBinding with next=", tokenname(hd ts1)];
                (ts2,(b,i))
            end
      | _ => 
            let
                val (ts1,nd1) = pattern (ts,ALLOWLIST,NOIN,ALLOWEXPR)
                val (b,i) = desugarPattern nd1 NONE NONE
            in 
                trace ["<< forInitialiser with next=", tokenname(hd ts1)];
                (ts1,(b,i))
            end
    end

(*
    LetStatement(w)    
        let  (  LetBindingList  )  Substatement(w)
*)

and letStatement (ts,w) : (token list * Ast.STMT) =
    let val _ = trace([">> letStatement with next=", tokenname(hd ts)])
    in case ts of
        Let :: LeftParen :: _ => 
            let
                val (ts1,nd1) = letBindingList (tl (tl ts))
                val defn = Ast.VariableDefn {kind=Ast.LetVar,
                                             ns=Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Internal "")),
                                             static=false,
                                             prototype=false,
                                             bindings=nd1}
            in case ts1 of
                RightParen :: _ =>
                    let
                        val (ts2,nd2) = substatement(tl ts1, w)
                    in
                        trace(["<< letStatement with next=",tokenname(hd(ts2))]);
                        (ts2,Ast.LetStmt (Ast.Block {pragmas=[],defns=[defn],head=NONE,body=[nd2]}))
                    end
               |    _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    WithStatement(w)
        with  (  ListExpression(allowIn)  )  Substatement(w)
        with  (  ListExpression(allowIn)  :  TypeExpression  )  Substatement(w)
*)

and withStatement (ts,w) : (token list * Ast.STMT) =
    let val _ = trace([">> withStatement with next=", tokenname(hd ts)])
    in case ts of
        With :: LeftParen :: _ => 
            let
                val (ts1,nd1) = listExpression (tl (tl ts),ALLOWIN)
            in case ts1 of
                RightParen :: _ =>
                    let
                        val (ts2,nd2) = substatement(tl ts1, w)
                    in
                        (trace(["<< withStatement with next=",tokenname(hd(ts2))]);
                        (ts2,Ast.WithStmt {obj=nd1,ty=Ast.SpecialType Ast.Any,body=nd2}))
                    end
              | Colon :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in case ts2 of
                        RightParen :: _ =>
                            let
                                val (ts3,nd3) = substatement(tl ts2, w)
                            in
                                (trace(["<< withStatement with next=",tokenname(hd(ts3))]);
                                (ts3,Ast.WithStmt {obj=nd1,ty=nd2,body=nd3}))
                            end
                       |    _ => raise ParseError
                    end
               |    _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    ContinueStatement    
        continue
        continue [no line break] Identifier
*)

and continueStatement ts: (token list * Ast.STMT) =
    let
    in case ts of
        Continue :: (SemiColon | RightBrace) :: _ => 
            (tl ts,Ast.ContinueStmt NONE)
      | Continue :: _ =>
            if newline(tl ts) then 
                (tl ts,Ast.ContinueStmt NONE)
            else
                let
                    val (ts1,nd1) = identifier (tl ts)
                in
                    (ts1,Ast.ContinueStmt (SOME nd1))
                end
      | _ => raise ParseError
    end

(*
    BreakStatement    
        break
        break [no line break] Identifier
*)

and breakStatement ts: (token list * Ast.STMT) =
    let val _ = trace([">> breakStatement with next=", tokenname(hd ts)])
    in case ts of
        Break :: (SemiColon | RightBrace) :: _ => 
            (tl ts,Ast.BreakStmt NONE)
      | Break :: _ =>
            if newline(tl ts) then 
                (tl ts,Ast.BreakStmt NONE)
            else
                let
                    val (ts1,nd1) = identifier (tl ts)
                in
                    trace(["<< breakStatement with next=", tokenname(hd ts)]);
                    (ts1,Ast.BreakStmt (SOME nd1))
                end
      | _ => raise ParseError
    end

(*
    ReturnStatement    
        return
        return [no line break] ListExpressio(nallowIn)
*)

and returnStatement ts =
    let
    in case ts of
        Return :: (SemiColon | RightBrace) :: _ => 
            (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
      | Return :: _ =>
            if newline(tl ts) then 
                (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
            else 
                let
                    val (ts1,nd1) = listExpression(tl ts, ALLOWIN)
                in
                    (ts1,Ast.ReturnStmt nd1)
                end
      | _ => raise ParseError
    end

(*
    ThrowStatement     
        throw  ListExpression(allowIn)
*)

and throwStatement ts =
    let
    in case ts of
        Throw :: _ =>
            let
                val (ts1,nd1) = listExpression(tl ts, ALLOWIN)
            in
                (ts1,Ast.ThrowStmt nd1)
            end
      | _ => raise ParseError
    end

(*
    TryStatement    
        try Block CatchClauses
        try Block CatchClauses finally Block
        try Block finally Block
        
    CatchClauses    
        CatchClause
        CatchClauses CatchClause
        
    CatchClause    
        catch  (  Parameter  )  Block
*)

and tryStatement (ts) : (token list * Ast.STMT) =
    let val _ = trace([">> tryStatement with next=", tokenname(hd ts)])
    in case ts of
        Try :: _ =>
            let
                val (ts1,nd1) = block(tl ts,LOCAL)
            in case ts1 of
                Finally :: _ =>
                    let
                        val (ts2,nd2) = block (tl ts1,LOCAL)
                    in
                        (ts2,Ast.TryStmt {block=nd1,catches=[],finally=SOME nd2})
                    end
              | _ => 
                    let
                        val (ts2,nd2) = catchClauses ts1
                    in case ts2 of
                        Finally :: _ =>
                            let
                                val (ts3,nd3) = block (tl ts2,LOCAL)
                            in
                                (ts3,Ast.TryStmt {block=nd1,catches=nd2,finally=SOME nd3})
                            end
                      | _ =>
                            let
                            in
                                (ts2,Ast.TryStmt {block=nd1,catches=nd2,finally=NONE})
                            end
                    end
            end
      | _ => raise ParseError
    end

and catchClauses (ts) =
    let val _ = trace([">> catchClauses with next=", tokenname(hd ts)])
        val (ts1,nd1) = catchClause ts
    in case ts1 of
        Catch :: _ =>
            let
                val (ts2,nd2) = catchClauses ts1
            in
                (ts2,nd1::nd2)
            end
      | _ =>
            let
            in
                (ts1,nd1::[])
            end
    end

and catchClause (ts) =
    let val _ = trace([">> catchClause with next=", tokenname(hd ts)])
    in case ts of
        Catch :: LeftParen :: _ =>
            let
                val (ts1,{pattern,ty,...}) = parameter (tl (tl ts))
                val bindings = desugarPattern pattern ty NONE
            in case ts1 of
                RightParen :: _ =>
                    let
                        val (ts2,nd2) = block (tl ts1,LOCAL)
                    in
                        (ts2,{bindings=bindings,ty=ty,block=nd2,fixtures=NONE})
                    end
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    DefaultXMLNamespaceStatement    
        default  xml  namespace = NonAssignmentExpression(allowList, allowIn)
*)

and defaultXmlNamespaceStatement (ts) =
    let val _ = trace([">> defaultXmlNamespaceStatement with next=", tokenname(hd ts)])
    in case ts of
        Default :: Xml :: Namespace :: Assign :: _ =>
            let
                val (ts1,nd1) = nonAssignmentExpression ((tl (tl (tl (tl ts)))),ALLOWLIST,ALLOWIN)
            in
                (ts1,Ast.Dxns {expr=nd1})
            end
      | _ => raise ParseError
    end
    
(* DIRECTIVES *)

(*
    Directives(t)    
        empty
        DirectivesPrefix(t) Directive(t,abbrev)
    
    DirectivesPrefix(t)   
        empty
        Pragmas
        DirectivesPrefix(t) Directive(t,full)

    right recursive:

    DirectivesPrefix(t)   
        empty
        Pragmas DirectivePrefix'(t)

    DirectivesPrefix'(t)
           empty
        Directive(t,full) DirectivesPrefix'(t)
*)

and directives (ts,t) : (token list * Ast.DIRECTIVES) =
    let val _ = trace([">> directives with next=", tokenname(hd ts)])
    in case ts of
        (RightBrace | Eof) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE})
      | _ => 
            let
                val (ts1,nd1) = directivesPrefix (ts,t)
(*                val (ts2,nd2) = directive(ts1,t,ABBREV)   

todo: the trailing directive(abbrev) is parsed by directivesPrefix. 
      the semicolon(full) parser checks for the abbrev case and acts
      like semicolon(abbrev) if needed. clarify this in the code.
*)
            in
                trace(["<< directives with next=", tokenname(hd ts1)]);
                (ts1,nd1)
            end
    end

and directivesPrefix (ts,t:tau) : (token list * Ast.DIRECTIVES) =
    let val _ = trace([">> directivesPrefix with next=", tokenname(hd ts)])
        fun directivesPrefix' (ts,t) : (token list * Ast.DIRECTIVES) =
            let val _ = trace([">> directivesPrefix' with next=", tokenname(hd ts)])
            in case ts of
                (RightBrace | Eof) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE})
              | _ => 
                    let
                        val (ts1,{pragmas=p1,defns=d1,body=s1,...}) = directive (ts,t,FULL)
                        val (ts2,{pragmas=p2,defns=d2,body=s2,...}) = directivesPrefix' (ts1,t)
                    in
                        trace(["<< directivesPrefix' with next=", tokenname(hd ts2)]);
                        (ts2,{pragmas=(p1@p2),defns=(d1@d2),body=(s1@s2),head=NONE})
                    end
            end
    in case ts of
        (RightBrace | Eof) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE})
      | (Use | Import) :: _ => 
            let
                val (ts1,nd1) = pragmas ts
                val (ts2,{pragmas=p2,defns=d2,body=s2,...}) = directivesPrefix' (ts1,t)
            in
                trace(["<< directivesPrefix with next=", tokenname(hd ts2)]);
                (ts2, {pragmas=nd1,defns=d2,body=s2,head=NONE})
            end
      | _ => 
            let
                val (ts2,nd2) = directivesPrefix' (ts,t)
            in
                trace(["<< directivesPrefix with next=", tokenname(hd ts2)]);
                (ts2,nd2)
            end
    end

(*
    Directive(t,w)
        EmptyStatement
        Statement(w)
        AnnotatableDirective(t,w)
        Attributes  [no line break]  AnnotatableDirective(t,w)
*)


and directive (ts,t:tau,w:omega) : (token list * Ast.DIRECTIVES) =
    let val _ = trace([">> directive with next=", tokenname(hd ts)])
    in case ts of
        SemiColon :: _ => 
            let
                val (ts1,nd1) = emptyStatement ts
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE})
            end
      | Let :: LeftParen :: _  => (* dispatch let statement before let var *)
            let
                val (ts1,nd1) = statement (ts,w)
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE})
            end
      | (Let | Const | Var | Function | Class | Interface | Namespace | Type) :: _ => 
            let
                val (ts1,nd1) = annotatableDirective (ts,defaultAttrs,t,w)
            in
                (ts1,nd1)
            end
      | (Dynamic | Final | Override | Native | Prototype | Static ) :: _ =>
            let
                val (ts1,nd1) = attributes (ts,defaultAttrs,t)
                val {native,...} = nd1
            in case native of
                true => 
                    let
                        val (ts2,nd2) = functionDeclaration (ts1,nd1)    (* native function f(); *)
                        val (ts3,nd3) = (semicolon(ts2,w),nd2)
                    in
                        (ts3,nd3)
                    end
              | false =>
                    let
                        val (ts2,nd2) = annotatableDirective (ts1,nd1,t,w)
                    in
                        (ts2,nd2)
                    end
            end
      | (Identifier _ | Private | Public | Protected | Internal | Intrinsic) :: 
        (Dynamic | Final | Native | Override | Prototype | Static | 
         Var | Let | Const | Function | Class | Interface | Namespace | Type) :: _ =>
            let
                val (ts1,nd1) = attributes (ts,defaultAttrs,t)
                val {native,...} = nd1
            in case native of
                true => 
                    let
                        val (ts2,nd2) = functionDeclaration (ts1,nd1)
                        val (ts3,nd3) = (semicolon(ts2,w),nd2)
                    in
                        (ts3,nd3)
                    end
              | false =>
                    let
                        val (ts2,nd2) = annotatableDirective (ts1,nd1,t,w)
                    in
                        (ts2,nd2)
                    end
            end
      | _ => 
            let
                val (ts1,nd1) = statement (ts,w)
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE})
            end
    end

(*
    AnnotatableDirective(global, w)    
        VariableDefinition(allowIn)  Semicolon(w)
        FunctionDefinition(global)
        ClassDefinition
        InterfaceDefinition
        NamespaceDefinition  Semicolon(w)
        TypeDefinition  Semicolon(w)

    AnnotatableDirective(interface, w)
        FunctionDeclaration  Semicolon(w)
        
    AnnotatableDirective(t, w)    
        VariableDefinition(allowIn)  Semicolon(w)
        FunctionDefinition(t)
        NamespaceDefinition  Semicolon(w)
        TypeDefinition  Semicolon(w)
*)

and annotatableDirective (ts,attrs,GLOBAL,w) : (token list * Ast.DIRECTIVES)  =
    let val _ = trace([">> annotatableDirective GLOBAL with next=", tokenname(hd ts)])
        val {ns,prototype,static,...} = attrs
    in case ts of
        Let :: Function :: _ =>
            functionDefinition (ts,attrs,GLOBAL)
      | Function :: _ =>
            functionDefinition (ts,attrs,GLOBAL)
      | Class :: _ =>
            classDefinition (ts,attrs)
      | Interface :: _ =>
            interfaceDefinition (ts,attrs)
      | Namespace :: _ =>
            let
                val (ts1,nd1) = namespaceDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | Type :: _ =>
            let
                val (ts1,nd1) = typeDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (Let | Var | Const) :: _ => 
            let
                val (ts1,nd1) = variableDefinition (ts,ns,prototype,static,ALLOWIN,GLOBAL)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | _ => 
            raise ParseError
    end
  | annotatableDirective (ts,attrs,INTERFACE,w) : (token list * Ast.DIRECTIVES)  =
    let val _ = trace([">> annotatableDirective INTERFACE with next=", tokenname(hd ts)])
    in case ts of
        Function :: _ =>
            let
                val (ts1,nd1) = functionDeclaration (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | Type :: _ =>
            let
                val (ts1,nd1) = typeDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | _ => 
            raise ParseError
    end
  | annotatableDirective (ts,attrs,t,w) : (token list * Ast.DIRECTIVES)  =
    let val _ = trace([">> annotatableDirective omega with next=", tokenname(hd ts)])
        val {ns,prototype,static,...} = attrs
    in case ts of
        Let :: Function :: _ =>
            functionDefinition (ts,attrs,t)
      | Function :: _ =>
            functionDefinition (ts,attrs,t)
      | Namespace :: _ =>
            let
                val (ts1,nd1) = namespaceDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | Type :: _ =>
            let
                val (ts1,nd1) = typeDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (Let | Var | Const) :: _ => 
            let
                val (ts1,nd1) = variableDefinition (ts,ns,prototype,static,ALLOWIN,t)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | _ => 
            raise ParseError
    end

(*
    Attributes    
        Attribute
        Attribute  [no line break]  Attributes
        
    Attribute    
        NamespaceAttribute
        dynamic
        final
        native
        override
        prototype
        static
        [  AssignmentExpressionallowList, allowIn  ]

    NamespaceAttribute    
        ReservedNamespace
        PackageIdentifier  .  Identifier
        Identifier
*)

and attributes (ts,attrs,t) =
    let val _ = trace([">> attributes with next=", tokenname(hd ts)])
    in case ts of
        (Dynamic | Final | Native | Override | Prototype | Static | 
         Private | Protected | Public | Internal | Intrinsic | Identifier _) :: _ =>
            let
                val (ts1,nd1) = attribute (ts,attrs,t)
                val (ts2,nd2) = attributes (ts1,nd1,t)
            in
                trace(["<< attributes with next=", tokenname(hd ts)]);
                (ts2,nd2)
            end
      | _ =>
            let
            in
                trace(["<< attributes with next=", tokenname(hd ts)]);
                (ts,attrs)
            end
    end

and attribute (ts,{ns,override,static,final,dynamic,
                                    prototype,native,rest },GLOBAL) =
    let val _ = trace([">> attribute with next=", tokenname(hd ts)])
    in case (ts) of
        Dynamic :: _ =>
            let
            in
                (tl ts, { 
                        ns = ns,
                        override = override,
                           static = static,
                        final = final,
                           dynamic = true,
                        prototype = prototype,
                        native = native,
                        rest = rest})
            end
      | Final :: _ =>
            let
            in
                (tl ts,{ 
                        ns = ns,
                        override = override,
                           static = static,
                        final = true,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = native,
                        rest = rest})
            end
      | Native :: _ =>
            let
            in
                (tl ts,{ 
                        ns = ns,
                        override = override,
                           static = static,
                        final = final,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = true,
                        rest = rest})
            end
      | (Override | Static | Prototype ) :: _ => 
            (error(["invalid attribute in global context"]);raise ParseError)
      | _ => 
            let
                val (ts1,nd1) = namespaceAttribute (ts,GLOBAL)
            in
                (tl ts,{ 
                        ns = nd1,
                        override = override,
                           static = static,
                        final = final,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = native,
                        rest = rest})                
            end
    end
  | attribute (ts,{ns,override,static,final,dynamic,
                                    prototype,native,rest },CLASS) =
    let val _ = trace([">> attribute with next=", tokenname(hd ts)])
    in case ts of
        Final :: _ =>
            let
            in
                (tl ts,{ 
                        ns = ns,
                        override = override,
                           static = static,
                        final = true,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = native,
                        rest = rest})
            end
      | Native :: _ =>
            let
            in
                (tl ts,{ 
                        ns = ns,
                        override = override,
                           static = static,
                        final = final,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = true,
                        rest = rest})
            end
      | Override :: _ =>
            let
            in
                (tl ts,{ 
                        ns = ns,
                        override = true,
                           static = static,
                        final = final,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = native,
                        rest = rest})
            end
      | Prototype :: _ =>
            let
            in
                (tl ts,{ 
                        ns = ns,
                        override = override,
                           static = static,
                        final = final,
                           dynamic = dynamic,
                        prototype = true,
                        native = native,
                        rest = rest})
            end
      | Static :: _ =>
            let
            in
                (tl ts,{ 
                        ns = ns,
                        override = override,
                           static = true,
                        final = final,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = native,
                        rest = rest})
            end
      | Dynamic :: _ => 
            (error(["invalid attribute in class context"]);raise ParseError)
      | _ => 
            let
                val (ts1,nd1) = namespaceAttribute (ts,CLASS)
            in
                (tl ts,{ 
                        ns = nd1,
                        override = override,
                           static = static,
                        final = final,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = native,
                        rest = rest})                
            end
    end
  | attribute (ts,{ns,override,static,final,dynamic,
                                    prototype,native,rest },INTERFACE) =
        let
        in case ts of
            (Dynamic | Final | Native | Override | Prototype | Static | 
             Private | Protected | Public | Internal | Intrinsic | Identifier _) :: _ =>
                (error(["attributes not allowed on a interface methods"]);
                 raise ParseError)
          | _ =>
                (ts,{ 
                        ns = ns,
                        override = override,
                           static = static,
                        final = final,
                           dynamic = dynamic,
                        prototype = prototype,
                        native = native,
                        rest = rest})                
        end
  | attribute (ts,{ns,override,static,final,dynamic,
                                    prototype,native,rest },LOCAL) =
        let
        in case ts of
            (Dynamic | Final | Native | Override | Prototype | Static | 
             Private | Protected | Public | Internal | Intrinsic | Identifier _) :: _ =>
                (error(["attributes not allowed on local definitions"]);
                 raise ParseError)
          | _ =>
            (ts,{ 
                    ns = ns,
                    override = override,
                       static = static,
                    final = final,
                       dynamic = dynamic,
                    prototype = prototype,
                    native = native,
                    rest = rest})                
        end

and namespaceAttribute (ts,GLOBAL) =
    let val _ = trace([">> namespaceAttribute with next=", tokenname(hd ts)])
    in case ts of
        (Internal :: _ | Intrinsic :: _ | Public :: _) => 
            let
                val (ts1,nd1) = reservedNamespace ts
            in
                (ts1,Ast.LiteralExpr (Ast.LiteralNamespace nd1))
            end
      | Identifier s :: _ =>
            let
            in
                (tl ts,Ast.LexicalRef {ident=Ast.Identifier{ident=s,openNamespaces=[]}})
            end
      | _ => raise ParseError
    end
  | namespaceAttribute (ts,CLASS) =
    let val _ = trace([">> namespaceAttribute with next=", tokenname(hd ts)])
    in case ts of
        (Internal :: _ | Intrinsic :: _ | Private :: _ | Protected :: _ | Public :: _) => 
            let
                val (ts1,nd1) = reservedNamespace ts
            in
                (ts1,Ast.LiteralExpr (Ast.LiteralNamespace nd1))
            end
      | Identifier s :: _ =>
            let
            in
                (tl ts,Ast.LexicalRef {ident=Ast.Identifier{ident=s,openNamespaces=[]}})
            end
      | _ => raise ParseError
    end
  | namespaceAttribute (ts,_) =
    let val _ = trace([">> namespaceAttribute with next=", tokenname(hd ts)])
    in case ts of
        _ => raise ParseError
    end
        

(* DEFINITIONS *)    

(*    
    VariableDefinition(b)    
        VariableDefinitionKind  VariableBindingList(allowList, b)
        
    VariableDefinitionKind    
        const
        let
        let const
        var
        
    VariableBindingList(a, b)    
        VariableBinding(a, b)
        VariableBindingList(noList, b)  ,  VariableBinding(a, b)
*)

and variableDefinition (ts,ns:Ast.EXPR,prototype,static,b,t) : (token list * Ast.DIRECTIVES) =
    let val _ = trace([">> variableDefinition with next=", tokenname(hd ts)])
        val (ts1,nd1) = variableDefinitionKind(ts)
        val (ts2,(b,i)) = variableBindingList (ts1,ALLOWLIST,b)

        fun isTempBinding (b:Ast.BINDING) : bool =
            case b of 
               Ast.Binding {ident=Ast.TempIdent _,...} => true
             | _ => false

        fun isTempInit (b:Ast.INIT_STEP) : bool =
            case b of 
               Ast.InitStep (Ast.TempIdent _,_) => true
             | _ => false

        val (tempBinds,propBinds) = List.partition isTempBinding b
        val (tempInits,propInits) = List.partition isTempInit i

        val initStmts = [Ast.InitStmt {kind=nd1,ns=ns,prototype=prototype,static=static,inits=propInits}]
        val tempDefns = case tempBinds of [] => [] 
                          | _ => [Ast.VariableDefn {kind=Ast.LetVar,
                                                   bindings=(tempBinds,tempInits),
                                                   ns=Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Internal "")),
                                                   prototype=false,
                                                   static=false}]

        val stmt = Ast.BlockStmt (Ast.Block {pragmas=[],defns=tempDefns,head=NONE,body=initStmts})

    in
        (ts2,{pragmas=[],
              defns=[Ast.VariableDefn {kind=nd1,
                                       bindings=(propBinds,[]),
                                       ns=ns,
                                       prototype=prototype,
                                       static=static}],
              body=[stmt],
              head=NONE})
    end

and variableDefinitionKind (ts) =
    let
    in case ts of
        Const :: _ => 
            (tl ts,Ast.Const)
      | Var :: _ => 
            (tl ts,Ast.Var)
      | Let :: Const :: _ => 
            (tl (tl ts), Ast.LetConst)
      | Let :: _ => 
            (tl ts, Ast.LetVar)
      | _ => raise ParseError
    end

and variableBindingList (ts,a,b) : (token list * Ast.BINDINGS) = 
    let val _ = trace([">> variableBindingList with next=", tokenname(hd ts)])
        fun variableBindingList' (ts,a,b) : (token list * Ast.BINDINGS) =
            let val _ = trace([">> variableBindingList' with next=", tokenname(hd ts)])
            in case ts of
                Comma :: _ =>
                    let
                        val (ts1,(d1,s1)) = variableBinding(tl ts,a,b)
                           val (ts2,(d2,s2)) = variableBindingList'(ts1,a,b)
                      in
                        trace(["<< variableBindingList' with next=", tokenname(hd ts2)]);
                         (ts2,(d1@d2,s1@s2))
                      end
              | _ => (ts,([],[]))
            end
        val (ts1,(d1,s1)) = variableBinding(ts,a,b)
           val (ts2,(d2,s2)) = variableBindingList'(ts1,a,b)
       in
        trace(["<< variableBindingList with next=", tokenname(hd ts2)]);
        (ts2,(d1@d2,s1@s2))
    end

(*
    VariableBinding(a, b)    
        TypedIdentifier
        TypedPattern(noList, noIn, noExpr) VariableInitialisation(a, b)
        
    VariableInitialisation(a, b)    
        =  AssignmentExpression(a, b)    

    Desugar patterns, types and initialisers into binding and 

*)

and variableBinding (ts,a,beta) : (token list * Ast.BINDINGS) = 
    let val _ = trace([">> variableBinding with next=", tokenname(hd ts)])
        val (ts1,{p,t}) = typedPattern (ts,a,beta)  (* parse the more general syntax *)
    in case (ts1,p,beta) of
            (Assign :: _,_,_) =>
                let
                    val (ts2,nd2) = variableInitialisation (ts1,a,beta)
                    val (b,i) = desugarPattern p t (SOME nd2) 
                in
                    trace(["<< variableBinding with next=", tokenname(hd ts2)]);
                    (ts2, (b,i))
                end
          | (In :: _,_,NOIN) => (* okay, we are in a for-in or for-each-in binding *)
                let
                    val (b,i) = desugarPattern p t NONE
                in
                    trace(["<< variableBinding with next=", tokenname(hd ts1)]);
                    (ts1, (b,i))
                end
          | (_,IdentifierPattern _,_) =>   (* check for the more specific syntax allowed
                                                  when there is no init *)
                let
                    val (b,i) = desugarPattern p t NONE
                in
                    trace(["<< variableBinding with next=", tokenname(hd ts1)]);
                    (ts1, (b,i))
                end
          | (_,_,_) => (error(["destructuring pattern without initialiser"]); raise ParseError)
    end

and variableInitialisation (ts,a,b) : (token list * Ast.EXPR) =
    let val _ = trace([">> variableInitialisation with next=", tokenname(hd ts)])
    in case ts of
        Assign :: _ =>
            let
                val (ts1,nd1) = assignmentExpression (tl ts,a,b)
            in
                (ts1,nd1)
            end
      | _ => raise ParseError
    end

(*
    FunctionDeclaration    
        function  Identifier  FunctionSignature
*)

and functionDeclaration (ts,attrs) =
    let val _ = trace([">> functionDeclaration with next=", tokenname(hd ts)])
        val {ns,final,native,override,prototype,static,...} = attrs
    in case ts of
        Function :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
                val (ts2,nd2) = functionSignature (ts1)
            in
                (ts2,{pragmas=[],
                      defns=[Ast.FunctionDefn {kind=Ast.Var,
                                               ns=ns,
                                               final=final,
                                               native=native,
                                               override=override,
                                               prototype=prototype,
                                               static=static,
                                               func=Ast.Func {name={ident=nd1,kind=Ast.Ordinary},
                                                              fsig=nd2, 
                                                              param=([],[]),
                                                              defaults=[],
                                                              ty=functionTypeFromSignature nd2,
                                                              block=Ast.Block {pragmas=[],
                                                                               defns=[],
                                                                               body=[],
                                                                               head=NONE}}}],
                      body=[],
                      head=NONE})
            end
      | _ => raise ParseError
    end

(*
    FunctionDefinition(class)    
        function  ClassName  ConstructorSignature  FunctionBody
        function  FunctionName  FunctionSignature  FunctionBody
        let  function  FunctionName  FunctionSignature  FunctionBody
        
    FunctionDefinition(t)    
        function  FunctionName  FunctionSignature  FunctionBody
        let  function  FunctionName  FunctionSignature  FunctionBody
        
    FunctionName    
        Identifier
        OperatorName
        to
        call
        construct
        get  PropertyIdentifier
        set  PropertyIdentifier
        call  PropertyIdentifier
        construct  PropertyIdentifier

    OperatorName [one of]    
        +   -   ~   *   /   %   <   >   <=   >=   ==   <<   >>   >>>   &   |   ===   !=   !==
*)

and isCurrentClass ({ident,kind}) = if (ident=(!currentClassName)) andalso (kind=Ast.Ordinary) 
                                    then true 
                                    else false

and functionDefinition (ts,attrs,CLASS) =
    let val _ = trace([">> functionDefinition(CLASS) with next=", tokenname(hd ts)])
        val {ns,final,native,override,prototype,static,...} = attrs
        val (ts1,nd1) = functionKind (ts)
        val (ts2,nd2) = functionName (ts1)
    in case (nd1,isCurrentClass(nd2)) of
        (Ast.Var,true) =>
            let
                val (ts3,nd3) = constructorSignature (ts2)
            in case ts3 of
                LeftBrace :: _ =>
                    let
                        val (ts4,nd4) = functionBody (ts3)
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.ConstructorDefn 
                                            {ns=ns,
                                             native=native,
                                             ctor=Ast.Ctor {settings=[],
                                                            func=Ast.Func {name=nd2,
                                                                           fsig=nd3,
                                                                           param=([],[]),
                                                                           defaults=[],
                                                                           ty=functionTypeFromSignature nd3,
                                                                           block=nd4}}}],
                              body=[],
                              head=NONE})
                    end
              | _ => 
                    let
                        val (ts4,nd4) = listExpression (ts3,ALLOWIN)
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.ConstructorDefn 
                                            {ns=ns,
                                             native=native,
                                             ctor=Ast.Ctor {settings=[],
                                                            func=Ast.Func {name=nd2,
                                                                           fsig=nd3,
                                                                           param=([],[]),
                                                                           defaults=[],
                                                                           ty=functionTypeFromSignature nd3,
                                                                           block=Ast.Block 
                                                                                     {pragmas=[],
                                                                                      defns=[],
                                                                                      body=[Ast.ReturnStmt nd4],
                                                                                      head=NONE}}}}],
                              body=[],
                              head=NONE})
                    end
            end
      | (Ast.LetVar,true) => (error (["class name not allowed in 'let function'"]);raise ParseError)
      | _ =>
            let
                val (ts3,nd3) = functionSignature (ts2)
            in case ts3 of
                LeftBrace :: _ =>
                    let
                        val (ts4,nd4) = functionBody (ts3)
                        val ident = (#ident nd2)
                        val func = Ast.Func {name=nd2,
                                             fsig=nd3,
                                             param=([],[]),
                                             defaults=[],
                                             ty=functionTypeFromSignature nd3,
                                             block=nd4}
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.FunctionDefn {kind=nd1, 
                                                       ns=ns,
                                                       final=final,
                                                       native=native,
                                                       override=override,
                                                       prototype=prototype,
                                                       static=static,
                                                       func=func}],
                              body=[],
                              head=NONE})
                    end
              | _ => 
                    let
                        val (ts4,nd4) = listExpression (ts3,ALLOWIN)
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.FunctionDefn {kind=nd1, 
                                                       ns=ns,
                                                       final=final,
                                                       native=native,
                                                       override=override,
                                                       prototype=prototype,
                                                       static=static,
                                                       func=Ast.Func {name=nd2,
                                                                      fsig=nd3, 
                                                                      param=([],[]),
                                                                      defaults=[],
                                                                      ty=functionTypeFromSignature nd3,
                                                                      block=Ast.Block { pragmas=[],
                                                                                        defns=[],
                                                                                        body=[Ast.ReturnStmt nd4],
                                                                                        head=NONE}}}],
                              body=[],
                              head=NONE })
                    end
            end
    end

  | functionDefinition (ts,attrs,t) =
    let val _ = trace([">> functionDefinition with next=", tokenname(hd ts)])
        val {ns,final,native,override,prototype,static,...} = attrs
        val (ts1,nd1) = functionKind (ts)
        val (ts2,nd2) = functionName (ts1)
        val (ts3,nd3) = functionSignature (ts2)
        val (ts4,nd4) = functionBody (ts3)
        val ident = (#ident nd2)
        val func = Ast.Func {name=nd2,
                             fsig=nd3,
                             param=([],[]),
                             defaults=[],
                             ty=functionTypeFromSignature(nd3),
                             block=nd4}
    in
        (ts4,{pragmas=[],
              defns=[Ast.FunctionDefn {kind=nd1, 
                                       ns=ns,
                                       final=final,
                                       native=native,
                                       override=override,
                                       prototype=prototype,
                                       static=static,
                                       func=func}],
              body=[Ast.InitStmt {kind=nd1,
                                   ns=ns,
                                   prototype=false,
                                   static=false,
                                   inits=[Ast.InitStep (Ast.PropIdent ident,
                                                        Ast.LiteralExpr (Ast.LiteralFunction func))]}],
              head=NONE})
    end

and functionKind (ts) =
    let val _ = trace([">> functionKind with next=", tokenname(hd ts)])
    in case ts of
        Function :: _ => 
            (trace(["<< functionKind with next=", tokenname(hd (tl ts))]);
            (tl ts,Ast.Var))   (* reuse VAR_DEFN_KIND *)
      | Let :: Function :: _ => 
            (tl (tl ts), Ast.LetVar)
      | _ => raise ParseError
    end


and functionName (ts) : (token list * Ast.FUNC_NAME) =
    let val _ = trace([">> functionName with next=", tokenname(hd ts)])
    in case ts of
        (Plus | Minus | BitwiseNot | Mult | Div | Modulus | LessThan |
         GreaterThan | LessThanOrEquals | GreaterThanOrEquals | Equals | LeftShift |
         RightShift | UnsignedRightShift | BitwiseAnd | BitwiseOr | StrictEquals |
         NotEquals | StrictNotEquals) :: _ => 
            let 
                val (ts1,nd1) = operatorName ts
            in
                (ts1,{kind=Ast.Operator,ident=nd1})
            end            
      | To :: _ => 
            let
            in
                (tl ts,{kind=Ast.ToFunc,ident=""})
            end
      | Call :: _ => 
            let
            in case (tl ts) of
                (LeftParen | LeftDotAngle) :: _ =>
                    let
                    in
                        (tl ts,{kind=Ast.Call,ident=""})
                    end
              | _ =>
                    let
                        val (ts1,nd1) = propertyIdentifier (tl ts)
                    in
                        (ts1,{kind=Ast.Call,ident=nd1})
                    end
            end
      | Construct :: _ => 
            let
            in case (tl ts) of
                (LeftParen | LeftDotAngle) :: _ =>
                    let
                    in
                        (tl ts,{kind=Ast.Call,ident=""})
                    end
              | _ =>
                    let
                        val (ts1,nd1) = propertyIdentifier (tl ts)
                    in
                        (ts1,{kind=Ast.Call,ident=nd1})
                    end
            end
      | Get :: _ => 
            let
                val (ts1,nd1) = propertyIdentifier (tl ts)
            in
                (ts1,{kind=Ast.Get,ident=nd1})
            end
      | Set :: _ => 
            let
                val (ts1,nd1) = propertyIdentifier (tl ts)
            in
                (ts1,{kind=Ast.Set,ident=nd1})
            end
      | _ => 
            let
                val (ts1,nd1) = identifier ts
            in
                trace(["<< functionName with next=", tokenname(hd ts1)]);
                (ts1,{kind=Ast.Ordinary,ident=nd1})
            end
    end

(*    
    +   -   ~   *   /   %   <   >   <=   >=   ==   <<   >>   >>>   &   |   ===   !=   !== 
*)

and operatorName (ts) =
    let
    in case ts of
        Plus :: _ => (tl ts,"+")
      | Minus :: _ => (tl ts,"-")
      | BitwiseNot :: _ => (tl ts,"~")
      | Mult :: _ => (tl ts,"*")
      | Div :: _ => (tl ts,"/")
      | Modulus :: _ => (tl ts,"%")
      | LessThan :: _ => (tl ts,"<")
      | GreaterThan :: _ => (tl ts,">")
      | LessThanOrEquals :: _ => (tl ts,"<=")
      | GreaterThanOrEquals :: _ => (tl ts,">=")
      | Equals :: _ => (tl ts,"=")
      | LeftShift :: _ => (tl ts,">>")
      | RightShift :: _ => (tl ts,"<<")
      | UnsignedRightShift :: _ => (tl ts,"<<<")
      | BitwiseAnd :: _ => (tl ts,"&")
      | BitwiseOr :: _ => (tl ts,"|")
      | StrictEquals :: _ => (tl ts,"===")
      | NotEquals :: _ => (tl ts,"!=")
      | NotStrictEquals :: _ => (tl ts,"!==")
      | _ => raise ParseError
    end

(*
    ConstructorSignature    
        TypeParameters  (  Parameters  )
        TypeParameters  (  Parameters  )  ConstructorInitialiser
*)

and constructorSignature (ts) = 
    let val _ = trace([">> constructorSignature with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = typeParameters ts
    in case ts1 of
        LeftParen :: This :: Colon ::  _ =>
               let
                val (ts2,nd2) = typeIdentifier (tl (tl (tl ts1)))
            in case ts2 of
                Comma :: _ =>
                    let
                           val (ts3,nd3) = parameters (tl ts2)  
                       in case ts3 of
                           RightParen :: _ =>
                               let
                                   val (ts4,nd4) = constructorInitialiser (tl ts3)
                               in
                                (log(["<< constructorSignature with next=",tokenname(hd ts4)]);
                                (ts4,Ast.FunctionSignature
                                        { typeParams=nd1,
                                          thisType=SOME (needType(nd2,NONE)),
                                          params=nd3,
                                          returnType=(Ast.SpecialType Ast.VoidType),
                                          settings=SOME nd4,
                                          hasRest=false })) (* do we need this *)
                               end
                         | _ => raise ParseError
                    end
                 | _ => raise ParseError
            end
      | LeftParen :: _ =>
               let
                   val (ts2, nd2) = parameters (tl ts1)
               in case ts2 of
                   RightParen :: _ =>
                       let
                           val (ts3,nd3) = constructorInitialiser (tl ts2)
                       in
                        (log(["<< constructorSignature with next=",tokenname(hd ts3)]);
                        (ts3,Ast.FunctionSignature
                                { typeParams=nd1,
                                  params=nd2,
                                  returnType=(Ast.SpecialType Ast.VoidType),
                                  settings=SOME nd3,
                                  thisType=NONE,
                                  hasRest=false })) (* do we need this *)
                       end
                 | _ => raise ParseError
            end
      | _ => raise ParseError
    end


(*
    ConstructorInitialiser    
        :  InitialiserList
        
    InitaliserList    
        Initialiser
        InitialiserList  ,  Initialiser
        
    Initialiser    
        Pattern(noList, noIn, noExpr)  VariableInitialisation(noList, allowIn)
*)

and constructorInitialiser ts 
    : (token list * Ast.BINDINGS) = 
    let val _ = trace([">> constructorInitialiser with next=",tokenname(hd(ts))]) 
    in case ts of
        Colon :: _ => 
            let
                val (ts1,nd1) = initialiserList (tl ts)
            in
                log(["<< constructorInitialiser with next=",tokenname(hd ts1)]);
                (ts1,nd1)
            end
      | _ => (ts,([],[]))
    end

and initialiserList (ts) 
    : (token list * Ast.BINDINGS) = 
    let val _ = trace([">> initialiserList with next=", tokenname(hd ts)])
        fun initialiserList' (ts)
            : (token list * Ast.BINDINGS) = 
            let val _ = trace([">> initialiserList' with next=", tokenname(hd ts)])
            in case ts of
                Comma :: _ =>
                    let
                        val (ts1,(b1,i1)) = initialiser(tl ts)
                        val (ts2,(b2,i2)) = initialiserList'(ts1)
                    in
                        trace(["<< initialiserList' with next=", tokenname(hd ts2)]);
                        (ts2,(b1@b2,i1@i2))
                    end
              | _ => (ts,([],[]))
            end
        val (ts1,(b1,i1)) = initialiser(ts)
        val (ts2,(b2,i2)) = initialiserList'(ts1)
    in
        trace(["<< initialiserList with next=", tokenname(hd ts2)]);
        (ts2,(b1@b2,i1@i2))
    end

and initialiser (ts) 
    : (token list * Ast.BINDINGS) =
    let val _ = trace([">> initialiser with next=", tokenname(hd ts)])
        val (ts1,nd1) = pattern (ts,NOLIST,NOIN,NOEXPR)
    in case (ts1) of
            Assign :: _ =>
                let
                    val (ts2,nd2) = variableInitialisation (ts1,NOLIST,NOIN)
                in
                    trace(["<< initialiser with next=", tokenname(hd ts2)]);
                    (ts2, desugarPattern nd1 NONE (SOME nd2))
                end
          | _ => (error(["constructor initialiser without assignment"]); raise ParseError)
    end

(*
    FunctionBody    
        Block(function)
*)

and functionBody (ts) =
    let val _ = trace([">> functionBody with next=", tokenname(hd ts)])
    in case ts of
        LeftBrace :: _ =>
            let
                val (ts1,nd1) = block (ts,LOCAL)
            in
                (ts1,nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = listExpression (ts,ALLOWIN)
            in
                (ts1,Ast.Block {pragmas=[],defns=[],body=[Ast.ReturnStmt nd1],head=NONE})
            end
    end
        
(*
    ClassDefinition    
        class  ClassName  ClassInheritance  ClassBlock
        
    ClassName    
        ParameterisedClassName
        ParameterisedClassName  !
        
    ParameterisedClassName    
        Identifier
        Identifier  TypeParameters
        
*)

and classDefinition (ts,attrs) =
    let val _ = trace([">> classDefinition with next=", tokenname(hd ts)])
    in case ts of
        Class :: _ =>
            let
                val {ns,final,dynamic,...} = attrs
                val (ts1,{ident,params,nonnullable}) = className (tl ts)
                val (ts2,{extends,implements}) = classInheritance (ts1)
                val _ = currentClassName := ident;
                val (ts3,nd3) = classBody (ts2)
                val _ = currentClassName := "";

                fun isLet (d:Ast.DEFN) (* borrowed from defn.sml *)
                    : bool =
                    case d of 
                        Ast.VariableDefn {kind,...} => (kind=Ast.LetVar) orelse (kind=Ast.LetConst)
                      | Ast.FunctionDefn fd => false
                      | Ast.ConstructorDefn cd => false
                      | Ast.TypeDefn _ => false
                      | Ast.NamespaceDefn _ => false
                      | _ => LogErr.defnError ["illegal definition type in class"]

                val (Ast.Block {body,defns,...}) = nd3
                val letDefns = List.filter isLet defns
            in
                (ts3,{pragmas=[],
                      body=[Ast.ClassBlock 
                                {ns=ns,
                                 ident=ident,
                                 name=NONE,
                                 extends=NONE,  (* filled in by definer *)
                                 fixtures=NONE,
                                 block=Ast.Block {body=body,
                                            defns=letDefns,
                                            head=NONE,
                                            pragmas=[]}}],
                      defns=[Ast.ClassDefn {ident=ident,
                                            nonnullable=nonnullable,
                                            ns=ns,
                                            final=final,
                                            dynamic=dynamic,
                                            params=params,
                                            extends=extends,
                                            implements=implements,
                                            block=nd3,
                                            classDefns=[],
                                            instanceDefns=[] }],
                     head=NONE})
            end
      | _ => raise ParseError
    end

and className (ts) =
    let val _ = trace([">> className with next=", tokenname(hd ts)])
        val (ts1,{ident,params}) = parameterisedClassName ts
    in case ts1 of
        Not :: _ =>
            let
            in
                (tl ts1,{ident=ident,params=params,nonnullable=true})
            end
      | _ =>
            let
            in
                (ts1,{ident=ident,params=params,nonnullable=false})
            end
    end

and parameterisedClassName (ts) =
    let val _ = trace([">> parameterisedClassName with next=", tokenname(hd ts)])
        val (ts1,nd1) = identifier ts
    in case ts1 of
        LeftDotAngle :: _ =>
            let
                val (ts2,nd2) = typeParameters (ts1)
            in
                (ts2,{ident=nd1,params=nd2})
            end
      | _ =>
            let
            in
                (ts1,{ident=nd1,params=[]})
            end
    end

(*
    ClassInheritance    
        empty
        extends  TypeIdentifier
        implements  TypeIdentifierList
        extends  TypeIdentifier  implements  TypeIdentifierList
        
    TypeIdentifierList    
        TypeIdentifier
        TypeIdentifier  ,  TypeIdentifierList

    ClassBody    
        Block
*)

and classInheritance (ts) = 
    let val _ = trace([">> classInheritance with next=", tokenname(hd ts)])
    in case ts of
        Extends :: _ =>
            let
                val (ts1,nd1) = typeIdentifier (tl ts)
            in case ts1 of
                Implements :: _ =>
                    let
                        val (ts2,nd2) = typeIdentifierList (tl ts1)
                    in
                        (ts2,{extends=SOME nd1,implements=nd2})
                    end
              | _ =>
                    let
                    in
                        (ts1,{extends=SOME nd1,implements=[]})
                    end    
            end
      | Implements :: _ =>
            let
                val (ts1,nd1) = typeIdentifierList (tl ts)
            in
                (ts1,{extends=NONE,implements=nd1})
            end
      | _ => (ts,{extends=NONE,implements=[]})
    end

and typeIdentifierList (ts) = 
    let val _ = trace([">> typeIdentifierList with next=", tokenname(hd ts)])
        fun typeIdentifierList' (ts) =
            let
            in case ts of
                Comma :: _ =>
                    let
                        val (ts1,nd1) = typeIdentifier (tl ts)
                        val (ts2,nd2) = typeIdentifierList' (ts1)
                    in
                        (ts2,nd1::nd2)
                    end
              | _ =>
                    let
                    in
                        (ts,[])
                    end
            end
        val (ts1,nd1) = typeIdentifier (ts)
        val (ts2,nd2) = typeIdentifierList' (ts1)
    in
        trace(["<< typeIdentifierList with next=", tokenname(hd ts2)]);
        (ts2,nd1::nd2)
    end

and classBody (ts) =
    let val _ = trace([">> classBody with next=", tokenname(hd ts)])
        val (ts1,nd1) = block (ts,CLASS)
    in
        (ts1,nd1)
    end
        

(*        
    InterfaceDefinition    
        interface  ClassName  InterfaceInheritance  InterfaceBody
        
    InterfaceInheritance    
        empty
        extends  TypeIdentifierList
        
    InterfaceBody    
        Block(interface)
*)

and interfaceDefinition (ts,attrs) =
    let val _ = trace([">> interfaceDefinition with next=", tokenname(hd ts)])
        val {ns,...} = attrs
    in case ts of
        Interface :: _ =>
            let
                val (ts1,{ident,params,nonnullable}) = className (tl ts)
                val (ts2,{extends}) = interfaceInheritance (ts1)
                val (ts3,nd3) = interfaceBody (ts2)
            in
                 (ts3,{pragmas=[],
                       body=[],
                       defns=[Ast.InterfaceDefn {ident=ident,
                                                 nonnullable=nonnullable,
                                                 ns=ns,
                                                 params=params,
                                                 extends=extends,
                                                 block=nd3}],
                       head=NONE})
            end
      | _ => raise ParseError
    end

and interfaceInheritance (ts) = 
    let val _ = trace([">> interfaceInheritance with next=", tokenname(hd ts)])
    in case ts of
        Extends :: _ =>
            let
                val (ts1,nd1) = typeIdentifierList (tl ts)
            in
                (ts1,{extends=nd1})
            end
      | _ => (ts,{extends=[]})
    end

and interfaceBody (ts) =
    let val _ = trace([">> interfaceBody with next=", tokenname(hd ts)])
        val (ts1,nd1) = block (ts,INTERFACE)
    in
        trace(["<< interfaceBody with next=", tokenname(hd ts)]);
        (ts1,nd1)
    end
        
(*        
    NamespaceDefinition
        namespace  Identifier  NamespaceInitialisation
        
    NamespaceInitialisation    
        empty
        =  StringLiteral
        =  SimpleTypeIdentifier
*)

and namespaceDefinition (ts,attrs) =
    let val _ = trace([">> namespaceDefinition with next=", tokenname(hd ts)])
        val {ns,...} = attrs
    in case ts of
        Namespace :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
                val (ts2,nd2) = namespaceInitialisation ts1
            in
                trace(["<< namespaceDefinition with next=", tokenname(hd ts2)]);
                (ts2,{pragmas=[],
                      body=[],
                      defns=[Ast.NamespaceDefn {ns=ns,
                                                ident=nd1,
                                                init=nd2}],
                      head=NONE})
            end
      | _ => raise ParseError
    end
        
and namespaceInitialisation (ts) : (token list * Ast.EXPR option) =
    let val _ = trace([">> namespaceInitialisation with next=", tokenname(hd ts)])
    in case ts of
         Assign :: StringLiteral s :: _ =>
            let
                val (ts1,nd1) = (tl (tl ts), 
                        Ast.LiteralExpr(Ast.LiteralString(s)))
            in
                trace(["<< namespaceInitialisation StringLiteral with next=", tokenname(hd ts1)]);
                (ts1,SOME nd1)
            end
      | Assign :: _ =>
            let
                val (ts1,nd1) = simpleTypeIdentifier (tl ts)
            in
                trace(["<< namespaceInitialisation simpleTypeIdentifer with next=", tokenname(hd ts1)]);
                (ts1,SOME (Ast.LexicalRef {ident=nd1}))
            end
      | _ => (trace(["<< namespaceInitialisation none with next=", tokenname(hd ts)]);
             (ts,NONE))
    end

(*  
    TypeDefinition    
        type  Identifier  TypeInitialisation
        
    TypeInitialisation    
        =  TypeExpression
*)

and typeDefinition (ts,attrs) =
    let val _ = trace([">> typeDefinition with next=", tokenname(hd ts)])
        val {ns,...} = attrs
    in case ts of
        Type :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
                val (ts2,nd2) = typeInitialisation ts1
            in
                trace(["<< typeDefinition with next=", tokenname(hd ts2)]);
                (ts2,{pragmas=[],
                      body=[],
                      defns=[Ast.TypeDefn {ns=ns,
                                           ident=nd1,
                                           init=nd2}],
                      head=NONE})
            end
      | _ => raise ParseError
    end
        
and typeInitialisation (ts) : (token list * Ast.TYPE_EXPR) =
    let val _ = trace([">> typeInitialisation with next=", tokenname(hd ts)])
    in case ts of
        Assign :: _ =>
            let
                val (ts1,nd1) = nullableTypeExpression (tl ts)
            in
                trace(["<< typeInitialisation with next=", tokenname(hd ts1)]);
                (ts1,nd1)
            end
      | _ => raise ParseError
    end

(* PRAGMAS *)

(*
    Pragmas    
        Pragma
        Pragmas  Pragma
        
    Pragma    
        UsePragma  Semicolon(full)
        ImportPragma  Semicolon(full)
        
*)

and pragmas (ts) : token list * Ast.PRAGMA list =
    let val _ = trace([">> pragmas with next=", tokenname(hd ts)])
        val (ts1,nd1) = pragma(ts)
    in case ts1 of
        (Use | Import) :: _ => 
            let
                val (ts2,nd2) = pragmas (ts1)
            in
                (ts2, nd1 @ nd2)
            end
      | _ =>
            (ts1, nd1)
    end

and pragma ts =
    let val _ = trace([">> pragma with next=", tokenname(hd ts)])
    in case ts of
        Use :: _ => 
            let
                val (ts1,nd1) = usePragma ts
                 val (ts2,nd2) = (semicolon (ts1,FULL),nd1)
            in
                (ts2,nd2)
            end
      | Import :: _ => 
            let
                val (ts1,nd1) = importPragma ts
                 val (ts2,nd2) = (semicolon (ts1,FULL),nd1)
            in
                (ts2,nd2)
            end
      | _ => raise ParseError
    end

(*
    UsePragma    
        use  PragmaItems        
*)

and usePragma ts =
    let val _ = trace([">> usePragma with next=", tokenname(hd ts)])
    in case ts of
        Use :: _ => pragmaItems (tl ts)
      | _ => raise ParseError
    end

(*
    PragmaItems    
        PragmaItem
        PragmaItems  ,  PragmaItem

    right recursive:

    PragmaItems
        PragmaItem PragmaItems'

    PragmaItems'
        empty
        , PragmaItem PragmaItems'
*)
        
and pragmaItems ts =
    let val _ = trace([">> pragmaItems with next=", tokenname(hd ts)])
        fun pragmaItems' ts =
            let
            in case ts of
                Comma :: _ =>
                    let
                        val (ts1,nd1) = pragmaItem (tl ts)
                        val (ts2,nd2) = pragmaItems' ts1
                    in
                        (ts2,nd1::nd2)
                    end
              | _ => (ts,[])
            end
        val (ts1,nd1) = pragmaItem ts
        val (ts2,nd2) = pragmaItems' ts1
    in
        (ts2,nd1::nd2)
    end

(*
    PragmaItem    
        decimal
        double
        int
        Number
        uint
        precision NumberLiteral
        rounding Identifier
        standard
        strict
        default namespace SimpleTypeIdentifier
        namespace SimpleTypeIdentifier
*)

and pragmaItem ts =
    let val _ = trace([">> pragmaItem with next=", tokenname(hd ts)])
    in case ts of
        Decimal :: _ => (tl ts,Ast.UseNumber Ast.Decimal)
      | Double :: _ => (tl ts,Ast.UseNumber Ast.Double)
      | Int :: _ => (tl ts,Ast.UseNumber Ast.Int)
      | Number :: _ => (tl ts,Ast.UseNumber Ast.Number)
      | UInt :: _ => (tl ts,Ast.UseNumber Ast.UInt)
      | Precision :: NumberLiteral n :: _ => 
            let
                val i = Ast.LiteralNumber n
            in
                (tl (tl ts), Ast.UsePrecision i)
            end
      | Rounding :: Identifier s :: _ => 
            let
                val m = Ast.HalfUp
            in
                (tl (tl ts), Ast.UseRounding m)
            end
      | Standard :: _ => (tl ts,Ast.UseStandard)
      | Strict :: _ => (tl ts,Ast.UseStrict)
      | Default :: Namespace :: _ => 
            let
                val (ts1,nd1) = simpleTypeIdentifier (tl (tl ts))
            in
                (ts1, Ast.UseDefaultNamespace nd1)
            end
      | Namespace :: Intrinsic :: _ => 
            let
                val (ts1,nd1) = (tl (tl ts), Ast.Identifier {ident="intrinsic",openNamespaces=[]})
            in
                (ts1, Ast.UseNamespace nd1)
            end
      | Namespace :: _ => 
            let
                val (ts1,nd1) = simpleTypeIdentifier (tl ts)
            in
                (ts1, Ast.UseNamespace nd1)
            end
      | _ =>
            raise ParseError
    end

(*
    ImportPragma    
        import  Identifier  =  ImportName
        import  ImportName
        
    ImportName    
        PackageIdentifier  .  PropertyIdentifier
*)

and importPragma (ts) : token list * Ast.PRAGMA list =
    let val _ = trace([">> importPragma with next=", tokenname(hd ts)])
    in case ts of
        Import :: _ :: Assign :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
                val (ts2,{package,name}) = importName (tl ts1)
            in
                (ts2,[Ast.Import {package=package,name=name,alias=SOME nd1}])
            end
      | Import :: _ =>
            let
                val (ts1,{package,name}) = importName (tl ts)
            in
                (ts1,[Ast.Import {package=package,name=name,alias=NONE}])
            end
      | _ => raise ParseError
    end

and importName (ts) =
    let val _ = trace([">> importName with next=", tokenname(hd ts)])
    in case ts of
        Identifier p :: Dot :: _ =>
            let
                val (ts1,nd1) = (tl ts,p)
                val (ts2,nd2) = importName (tl ts1)
                val dotOrNot = if (#package nd2) = "" then "" else "."
            in
                (ts2,{package=(nd1^dotOrNot^(#package nd2)),name=(#name nd2)})
            end
      | Mult :: _ =>
            let
                val (ts1,nd1) = (tl ts,"*")
            in
                (ts1,{package="",name=nd1})
            end
      | Identifier p :: _ =>
            let
                val (ts1,nd1) = (tl ts,p)
            in
                (ts1,{package="",name=nd1})
            end
      | _ => raise ParseError
    end

(* BLOCKS AND PROGRAMS *)

(*
    Block(t)    
        {  Directives(t)  }
*)

and block (ts,t) : (token list * Ast.BLOCK) =
    let val _ = trace([">> block with next=", tokenname(hd ts)])
    in case ts of
        LeftBrace :: RightBrace :: _ => 
            ((trace(["<< block with next=", tokenname(hd (tl (tl ts)))]);
            (tl (tl ts),Ast.Block {pragmas=[],defns=[],body=[],head=NONE})))
      | LeftBrace :: _ =>
            let
                val (ts1,nd1) = directives (tl ts,t)
            in case ts1 of
                RightBrace :: _ => 
                    (trace(["<< block with next=", tokenname(hd (tl ts1))]);
                    (tl ts1,Ast.Block nd1))
              | _ => raise ParseError
            end
      | _ => raise ParseError
    end

(*
    Program    
        Directives(global)
        Packages  Directives(global)
        
    Packages    
        Packages
        Package Packages
        
    Package    
        PackageAttributes  package  PackageNameOpt  PackageBody
        
    PackageAttributes    
        internal
        empty
        
    PackageNameOpt    
        empty
        PackageName
        
    PackageName [create a lexical PackageIdentifier with the sequence of characters that make a PackageName]    
        Identifier
        PackageName  .  Identifier
        
    PackageBody    
        Blockglobal
*)
    
and program (ts) : (token list * Ast.PROGRAM) =
    let val _ = trace([">> program with next=",tokenname(hd(ts))])
    in case ts of
        ((Internal :: Package :: _) |
         (Package :: _)) =>
            let
                val (ts1,nd1) = packages ts
                val (ts2,nd2) = directives (ts1,GLOBAL)
            in
                (ts2,{block=Ast.Block nd2,fixtures=NONE,packages=nd1})
            end
      | _ =>
            let
                val (ts1,nd1) = directives (ts,GLOBAL)
            in
                (ts1,{block=Ast.Block nd1,fixtures=NONE,packages=[]})
            end
    end

and packages ts =
    let val _ = trace([">> packages with next=",tokenname(hd(ts))])
    in case ts of
        (Internal :: Package :: _ | Package :: _) =>
            let
                val (ts1,nd1) = package ts
                val (ts2,nd2) = packages ts1
            in
                trace(["<< packages with next=",tokenname(hd(ts2))]);
                (ts2,nd1::nd2)
            end
      | _ => (trace(["<< packages with next=",tokenname(hd(ts))]);(ts,[]))
    end

(*
    Package    
        PackageAttributes  package  PackageNameOpt  PackageBody
        
    PackageAttributes    
        internal
        empty
        
    PackageNameOpt    
        empty
        PackageName
        
    PackageName [create a lexical PackageIdentifier with the sequence of characters that make a PackageName]    
        Identifier
        PackageName  .  Identifier
        
    PackageBody    
        Block(global)
*)

and package ts =
    let val _ = trace([">> package with next=",tokenname(hd(ts))])
    in case ts of
        Internal :: Package :: _ => 
            let
                val (ts1,nd1) = packageName (tl (tl ts))
                val (ts2,nd2) = block (ts1,GLOBAL)
            in
                (ts2,{name=nd1, block=nd2})
            end
      | Package :: LeftBrace :: _ =>
            let
                val (ts1,nd1) = block (tl ts,GLOBAL)
            in
                (ts1, {name="", block=nd1})
            end
      | Package :: _ =>
            let
                val (ts1,nd1) = packageName (tl ts)
                val (ts2,nd2) = block (ts1,GLOBAL)
            in
                (ts2, {name=nd1, block=nd2})
            end
      | _ => raise ParseError
    end

and packageName (ts) =
    let val _ = trace([">> packageName with next=", tokenname(hd ts)])
        val (ts1,nd1) = identifier ts
    in case ts1 of
        Dot :: _ =>
            let
                val (ts2,nd2) = packageName (tl ts1)
            in
                (ts2,nd1^"."^nd2)
            end
      | _ =>
            let
            in
                (ts1,nd1)
            end
    end

fun mkReader filename = 
    let
        val stream = TextIO.openIn filename
    in
        fn _ => case TextIO.inputLine stream of
                    SOME line => (log ["read line ", line]; line)
                  | NONE => ""
    end

(*
   scan to first < or /

   the scanner state includes the reader and a token stream.
   when the token stream is empty continue scanning in the current slash context until another key character is encountered.

   switch slash contexts at the normal points in the program.
*)

fun dumpTokens (ts,lst) =
    case ts of
        [] => rev lst
      | _ => dumpTokens(tl ts, tokenname(hd ts) :: "\n  " :: lst)

fun dumpLineBreaks (lbs,lst) =
    case lbs of
        [] => rev lst
      | _ => dumpLineBreaks(tl lbs, Int.toString(hd lbs) :: "\n  " :: lst)

fun lex (reader) : (token list) =
    let 
        val lexer = Lexer.makeLexer reader
        val tokens = Lexer.UserDeclarations.token_list lexer
        val line_breaks = !Lexer.UserDeclarations.line_breaks
    in
        log ("tokens:" :: dumpTokens(tokens,[])); 
        log ("line breaks:" :: dumpLineBreaks(line_breaks,[])); 
        tokens
    end

fun lexFile (filename : string) : (token list) = lex (mkReader filename)
(*
    let 
        val lexer = Lexer.makeLexer (mkReader filename)
        val tokens = Lexer.UserDeclarations.token_list lexer
        val line_breaks = !Lexer.UserDeclarations.line_breaks
    in
        log ("tokens:" :: dumpTokens(tokens,[])); 
        log ("line breaks:" :: dumpLineBreaks(line_breaks,[])); 
        tokens
    end
*)

fun lexLines (lines : string list) : (token list) =
    let val reader = let val r = ref lines
                     in
                         fn _ => (case !r of
                                       (line::lines) => (r := lines; line)
                                     | [] => "")
                     end
    in
        lex reader
    end

fun parse ts =
    let 
    val (residual, result) = (program ts) 
    fun check_residual [Eof] = ()
      | check_residual _ = raise ParseError
    in
    check_residual residual;
    log ["parsed all input, pretty-printing:"];
    Pretty.ppProgram result;
    result
    end

fun logged thunk name =
    (log ["scanning ", name];
     let val ast = thunk ()
     in
         log ["parsed ", name, "\n"];
         ast
     end)
     handle ParseError => (log ["parse error"]; raise ParseError)
         | Lexer.LexError => (log ["lex error"]; raise Lexer.LexError)

fun parseFile filename =
    logged (fn _ => parse (lexFile filename)) filename

(*
    (log ["scanning ", filename];
     let val ast = parse (lexFile filename)
     in
         log ["parsed ", filename, "\n"];
         ast
     end)
     handle ParseError => (log ["parse error"]; raise ParseError)
          | Lexer.LexError => (log ["lex error"]; raise Lexer.LexError)
*)

fun parseLines lines =
    logged (fn _ => parse (lexLines lines)) "<<string>>"

end (* Parser *)
