(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(*
 * The following licensing terms and conditions apply and must be
 * accepted in order to use the Reference Implementation:
 * 
 *    1. This Reference Implementation is made available to all
 * interested persons on the same terms as Ecma makes available its
 * standards and technical reports, as set forth at
 * http://www.ecma-international.org/publications/.
 * 
 *    2. All liability and responsibility for the implementation or other
 * use of this Reference Implementation rests with the implementor, and
 * not with any of the parties who contribute to, or who own or hold any
 * copyright in, this Reference Implementation.
 * 
 *    3. THIS REFERENCE IMPLEMENTATION IS PROVIDED BY THE COPYRIGHT
 * HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * End of Terms and Conditions
 * 
 * Copyright (c) 2007 Adobe Systems Inc., The Mozilla Foundation, Opera
 * Software ASA, and others.
 *)
structure Parser = struct

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[parse] " :: ss) else ()
fun error ss = LogErr.parseError ss

exception ParseError = LogErr.ParseError
exception LexError = LogErr.LexError
exception EofError = LogErr.EofError

open Token

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

type ATTRS = {ns: Ast.EXPR option,
              override: bool,
              static: bool,
              final: bool,
              dynamic: bool,
              prototype: bool,
              native: bool,
              rest: bool}

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

val currentClassName : Ast.IDENT ref = ref Ustring.empty
val currentPackageName : Ast.IDENT ref = ref Ustring.empty

fun newline (ts : (TOKEN * Ast.LOC) list) =
    let
        val (_, {file, span, post_newline}) = hd ts
    in
        post_newline
    end

fun locOf ts = 
    case ts of 
        (_, loc) :: _ => (SOME loc)
      | _ => (NONE)
             
fun setLoc ts = 
    LogErr.setLoc (locOf ts)

val defaultAttrs : ATTRS = 
    {
        ns = NONE,
        override = false,
        static = false,
        final = false,
        dynamic = false,
        prototype = false,
        native = false,
        rest = false }

val defaultRestAttrs : ATTRS = 
    { 
        ns = NONE,
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


(*
    var x : int = 10
    
    Binding {ident="x",ty=int}
    InitStep ("x",10)

    Turn a pattern, type and initialiser into lists of bindings, inits, and statements.
    The outer defn will wrap both with the attributes

    
*)
                            
fun desugarPattern (loc:Ast.LOC option)
                   (pattern:PATTERN)
                   (ty:Ast.TYPE_EXPR)
                   (expr:Ast.EXPR option)
                   (nesting:int)
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
                                (element_types: Ast.TYPE_EXPR)
                                (temp:Ast.EXPR) 
                                (n:int)
            : (Ast.BINDING list * Ast.INIT_STEP list) =
            let
            in case element_ptrns of
                p::plist =>
                    let
                        val str = Ast.LiteralString (Ustring.fromInt n)
                        val ident = Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr str), 
                                                               openNamespaces = [] }
                        val e = SOME (Ast.ObjectRef {base=temp, ident=ident, loc=loc})
                        val t = Ast.ElementTypeRef (element_types,n)
                        val (binds, inits) = desugarPattern loc p t e (nesting+1)
                        val (binds', inits') = desugarArrayPattern plist element_types temp (n+1)
                    in
                        ((binds @ binds'), (inits @ inits'))
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
                                 (field_types:Ast.TYPE_EXPR) 
                                 (temp:Ast.EXPR)
            : (Ast.BINDING list * Ast.INIT_STEP list) =
            let
            in
            case field_ptrns of
                fp::fps =>
                    let
                        val (binds,inits) = desugarFieldPattern fp field_types temp
                        val (binds',inits') = desugarObjectPattern fps field_types temp
                    in
                        ((binds @ binds'), (inits @ inits'))
                    end
              | [] => ([],[])  
            end

        (*
            Use the field name to get the field type and associate that field type 
            with the field's pattern. Deference the given expression with the field
            name to get the value of the field.
        *)

        and desugarFieldPattern (field_pattern: FIELD_PATTERN) 
                                (field_types: Ast.TYPE_EXPR)
                                (temp: Ast.EXPR)
            : (Ast.BINDING list * Ast.INIT_STEP list) =
            let
                val {ident,pattern=p} = field_pattern
            in
                case (field_types,ident) of
                    (ty, Ast.Identifier {ident=id,...}) =>
                        (* if the field pattern is typed, it must have a identifier for
                           its name so we can do the mapping to its field type *)
                        let
                            val t = Ast.FieldTypeRef (ty,id)
                            val e = SOME (Ast.ObjectRef {base=temp, ident=ident, loc=loc})
                        in
                            desugarPattern loc p t e (nesting+1)
                        end
                  | (_,_) =>
                        let
                            val t = Ast.SpecialType Ast.Any
                            val e = SOME (Ast.ObjectRef {base=temp, ident=ident, loc=loc})
                        in
                            desugarPattern loc p t e (nesting+1)
                        end
            end

    in 
        case (pattern,expr) of
            (SimplePattern expr,_) => desugarSimplePattern expr
          | (IdentifierPattern id,_) => desugarIdentifierPattern id
          | (_,_) =>
                let
                    val e = case expr of SOME e => e | _ => (Ast.GetTemp 0)
                    val temp_n = nesting
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

and identifier [] = error ["expecting 'identifier', but ran out of tokens"]
  | identifier ((t,_)::ts) =
    let val ustr = case t of
        Identifier(us) => us
      | (Call
      | Debugger
      | Decimal
      | Double
      | Dynamic
      | Each
      | Final
      | Get
      | Goto
      | Has
      | Include
      | Int
      | Invoke
      | Namespace
      | Native
      | Number
      | Override
      | Precision
      | Prototype
      | Rounding
      | Standard
      | Strict
      | UInt
      | Set
      | Static
      | Type
      | Undefined
      | Xml
      | Yield) => Ustring.fromString (tokenname (t,()))
      | _ => error ["expecting 'identifier' before '",tokenname (t,()),"'"]
    in
        (ts, ustr)
    end

(*
    Qualifier
        ReservedNamespace
        PropertyIdentifier
*)

and qualifier ts =
    let
    in case ts of
        ((Internal, _) :: _ | (Intrinsic, _) :: _ | (Private, _) :: _ | (Protected, _) :: _ | (Public, _) :: _) => 
            let
                val (ts1,nd1) = reservedNamespace ts
            in
                (ts1,Ast.LiteralExpr(Ast.LiteralNamespace nd1))
            end
      | (Mult, _) :: _ =>
            let
            in
                (tl ts,Ast.LexicalRef{ident=Ast.WildcardIdentifier, loc=locOf ts})
            end
      | _ => 
            let
                val (ts1,nd1) = identifier ts
            in
                (ts1,Ast.LexicalRef{ident=Ast.Identifier {ident=nd1, openNamespaces=[]}, loc=locOf ts})
            end
    end

and reservedNamespace ts =
    let val _ = trace([">> reservedNamespace with next=",tokenname(hd(ts))])
    in case ts of
        (Internal, _) :: tr => 
            (tr, Ast.Internal (!currentPackageName))
      | (Intrinsic, _) :: tr => 
            (tr, Ast.Intrinsic)
      | (Private, _) :: tr => 
            (tr, Ast.Private (!currentClassName))
      | (Protected, _) :: tr => 
            (tr, Ast.Protected (!currentClassName))
      | (Public, _) :: tr => 
            (tr, Ast.Public (!currentPackageName))
      | _ => error ["unknown reserved namespace"]
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
        ((Internal, _) :: _ | (Intrinsic, _) :: _ | (Private, _) :: _ | (Protected, _) :: _ | (Public, _) :: _) => 
          let 
              val (ts1, nd1) = reservedNamespace(ts)
          in case ts1 of
                 (DoubleColon, _) :: ts2 => qualifiedIdentifier'(ts2,Ast.LiteralExpr(Ast.LiteralNamespace nd1))
               | _ => error ["qualified namespace without double colon"]
          end
      | (Mult,_) :: _ => 
          let
              val (ts1, nd1) = (tl ts, Ast.WildcardIdentifier)
          in case ts1 of
              (DoubleColon, _) :: _ =>
                  qualifiedIdentifier'(tl ts1,Ast.LexicalRef ({ident=nd1, loc=locOf ts1}))
            | _ =>
                  (trace(["<< simpleQualifiedIdentifier with next=",tokenname(hd(ts1))]);
                   (ts1,nd1))
          end
      | _ => 
          let
              val (ts1, nd1) = identifier(ts)
              val id = Ast.Identifier {ident=nd1, openNamespaces=[]}
          in case ts1 of
              (DoubleColon, _) :: _ =>
                  qualifiedIdentifier'(tl ts1,Ast.LexicalRef ({ident=id, loc=locOf ts}))
            | _ =>
                  (trace(["<< simpleQualifiedIdentifier with next=",tokenname(hd(ts1))]);
                   (ts1,id))
          end
    end

and expressionQualifiedIdentifier (ts) =
    let 
        val (ts1,nd1) = parenListExpression(ts)
    in case ts1 of
        (DoubleColon, _) :: _ => 
            let
                val (ts2,nd2) = qualifiedIdentifier'(tl ts1,nd1) 
(* todo: make qualifier be an EXPR list *) 
            in
                (ts2,nd2)
            end

      | _ => error ["unknown form of expression-qualified identifier"]
    end

and reservedOrOrdinaryIdentifier ts =
    case isreserved(hd ts) of
        true => (tl ts, Ustring.fromString (tokenname(hd ts)))
      | false => 
            case ts of
                (Mult, _) :: _ => (tl ts, Ustring.asterisk)
              | _ => identifier(ts)

and reservedIdentifier [] = error ["no reserved identifier"]
  | reservedIdentifier (t::ts) =
    case isreserved(t) of
        true => (ts, Ustring.fromString (tokenname t))
      | false => error ["non-reserved identifier"]

and qualifiedIdentifier' (ts1, nd1) : ((TOKEN * Ast.LOC) list * Ast.IDENT_EXPR) =
    let val _ = trace([">> qualifiedIdentifier' with next=",tokenname(hd(ts1))]) 
    in case ts1 of
        (LeftBracket, _) :: ts => 
            let
                val (ts2,nd2) = brackets (ts1)
                val (ts3,nd3) = (ts2,Ast.QualifiedExpression({qual=nd1,expr=nd2}))

            in
                (ts3,nd3)
            end
      | tk :: ts =>
            let
                val (ts2,nd2) = reservedOrOrdinaryIdentifier(ts1)
                val qid = Ast.QualifiedIdentifier({qual=nd1, ident=nd2})
                val (ts3,nd3) = (ts2,qid)
            in
                (ts3,nd3)
            end
      | _ => error ["empty token stream for qualified identifier"]
    end

(*
    NonAttributeQualifiedIdentifier    
        SimpleQualifiedIdentifier
        ExpressionQualifiedIdentifier
*)

and nonAttributeQualifiedIdentifier ts =
    let val _ = trace([">> nonAttributeQualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftParen, _) :: _ => expressionQualifiedIdentifier(ts)
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
        (At, _) :: (LeftBracket, _) :: _ => 
            let
                val (ts1,nd1) = brackets(tl ts)
            in
                (ts1,Ast.AttributeIdentifier (Ast.ExpressionIdentifier { expr = nd1, 
                                                                         openNamespaces = [] }))
            end
      | (At, _) :: _ => 
            let
                val (ts1,nd1) = nonAttributeQualifiedIdentifier(tl ts)
            in
                (ts1,Ast.AttributeIdentifier nd1)
            end
      | _ => 
            error ["unknown form of attribute identifier"]
    end

(*

    QualifiedIdentifier    
        AttributeIdentifier
        NonAttributeQualifiedIdentifier

*)

and qualifiedIdentifier ts =
    let val _ = trace([">> qualifiedIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        (At, _) :: _ => attributeIdentifier(ts)
      | _ => nonAttributeQualifiedIdentifier(ts)
    end

(*
    TypeIdentifier    
        SimpleTypeIdentifier
        SimpleTypeIdentifier  .<  TypeExpressionList  >
*)

and propertyIdentifier ts =
    let val _ = trace([">> propertyIdentifier with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = nonAttributeQualifiedIdentifier ts
    in case ts1 of
        (LeftDotAngle, _) :: _ => 
            let
                val (ts2,nd2) = typeExpressionList (tl ts1)
            in case ts2 of
                (GreaterThan, _) :: _ =>
                    (trace(["<< propertyIdentifier with next=",tokenname(hd(tl ts2))]); 
                    (tl ts2,Ast.TypeIdentifier {ident=nd1,typeArgs=nd2}))
              | _ => error ["unknown final token of parametric type expression"]
            end
      | _ =>
            (trace(["<< propertyIdentifier with next=",tokenname(hd(ts1))]); 
            (ts1, nd1))
    end

and primaryIdentifier ts =
    let val _ = trace([">> primaryIdentifier with next=",tokenname(hd(ts))]) 
    in case ts of
        (Identifier _, _) :: (Dot, _) :: _ => 
            let 
                val (ts1,nd1) = path ts
            in case ts1 of
                (Dot, _) :: _ =>
                    let
                       val (ts2,nd2) = propertyIdentifier (tl ts1)
                    in
                       (ts2,Ast.UnresolvedPath (nd1,nd2)) 
                    end
              | _ => LogErr.internalError ["primaryIdentifier"]
            end
      | _ => propertyIdentifier(ts)
    end

and path (ts) : (TOKEN * Ast.LOC) list * Ast.IDENT list =
    let val _ = trace([">> path with next=", tokenname(hd ts)])
        val (ts1,nd1) = identifier ts
    in case ts1 of
           (Dot, _) :: (Identifier _, _) :: (Dot, _) :: (Identifier _, _) :: _ =>
           let
               val (ts2,nd2) = path (tl ts1)
           in
               (ts2,nd1::nd2)
           end
         | _ =>
           let
           in
               (ts1,nd1::[])
           end
    end

(*
    ParenExpression    
        (  AssignmentExpressionallowLet, allowIn  )
*)

and parenExpression ts =
    let val _ = trace([">> parenExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftParen, _) :: ts1 => 
            let
                val (ts2,nd2:Ast.EXPR) = assignmentExpression (ts1,ALLOWLIST,ALLOWIN)
            in case ts2 of
                (RightParen, _) :: ts3 => (ts3,nd2)
              | _ => error ["unknown final token of paren expression"]
            end
      | _ => error ["unknown initial token of paren expression"]
    end

(*
    ParenListExpression    
        (  ListExpression(ALLOWIN)  )
*)

and parenListExpression (ts) : ((TOKEN * Ast.LOC) list * Ast.EXPR) =
    let val _ = trace([">> parenListExpression with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftParen, _) :: _ => 
            let
                val (ts1,nd1) = listExpression (tl ts,ALLOWIN)
                val nd1 = case nd1 of 
                              Ast.ListExpr [x] => x
                            | x => x
            in case ts1 of
                (RightParen, _) :: _ => 
                    (trace(["<< parenListExpression with next=",tokenname(hd(ts1))]);
                    (tl ts1,nd1))
              | _ => error ["unknown final token of paren list expression"]
            end
      | _ => error ["unknown initial token of paren list expression"]
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
        (Function, _) :: ts1 => 
            let
            in case ts1 of
                ((LeftDotAngle | LeftParen),_) :: _ => 
                    let
                        val (ts3,nd3) = functionSignature ts1
                    in case (ts3,a) of
                        ((LeftBrace, _) :: _,_) => 
                            let
                                val (ts4,nd4) = block (ts3,LOCAL)
                            in
                                (ts4,Ast.LiteralExpr 
                                         (Ast.LiteralFunction 
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=Ustring.empty},
                                                         fsig=nd3,
                                                         block=nd4,
                                                         isNative=false,
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
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=Ustring.empty},
                                                         fsig=nd3,
                                                         block=Ast.Block {pragmas=[],
                                                                          defns=[],
                                                                          body=[Ast.ReturnStmt nd4],
                                                                          head=NONE,
                                                                          loc=locOf ts3},
                                                         isNative=false,
                                                         param=([],[]),
                                                         defaults=[],
                                                         ty=functionTypeFromSignature nd3})))
                                
                            end
                      | _ => error ["unknown body form in anonymous function expression"]
                    end
              | _ => 
                    let
                        val (ts2,nd2) = identifier ts1
                        val (ts3,nd3) = functionSignature ts2
                    in case (ts3,a) of
                        ((LeftBrace, _) :: _,_) => 
                            let
                                val (ts4,nd4) = block (ts3,LOCAL)
                            in
                                (ts4,Ast.LiteralExpr 
                                         (Ast.LiteralFunction 
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=nd2},
                                                         fsig=nd3,
                                                         block=nd4,
                                                         isNative=false,
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
                                                               head=NONE,
                                                               loc=locOf ts3},
                                                    isNative=false,
                                                    param=([],[]),
                                                    defaults=[],
                                                    ty=functionTypeFromSignature nd3})))
                            end
                      | _ => error ["unknown body form in named function expression"]
                    end
            end
      | _ => error ["unknown form of function expression"]
    end

(*
    FunctionSignature    
        TypeParameters  (  Parameters  )  ResultType
        TypeParameters  (  this  :  TypeIdentifier  ,  Parameters  )  ResultType
*)

and needType (nd:Ast.IDENT_EXPR,nullable:bool option) = 
    case nd of
        Ast.Identifier {ident,...} =>
            if (ident=Ustring.asterisk)
                then Ast.SpecialType Ast.Any
                else if( ident=Ustring.Object_ )  (* FIXME: check for *the* object name *)
                    then Ast.TypeName nd
                    else Ast.TypeName nd
      | _ => Ast.TypeName nd

and functionSignature (ts) : ((TOKEN * Ast.LOC) list * Ast.FUNC_SIG) =
    let val _ = trace([">> functionSignature with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = typeParameters ts
    in case ts1 of
        (LeftParen, _) :: (This, _) :: (Colon, _) ::  _ =>
            let
                val (ts2,nd2) = primaryIdentifier (tl (tl (tl ts1)))
                val temp = Ast.Binding {ident=Ast.ParamIdent 0, ty=Ast.SpecialType Ast.Any}
                                    (* FIXME: what is the type of this? *)
            in case ts2 of
                (Comma, _) :: _ =>
                    let
                           val (ts3,((b,i),e,t),hasRest) = nonemptyParameters (tl ts2) 0 false
                       in case ts3 of
                           (RightParen, _) :: _ =>
                               let
                                   val (ts4,nd4) = resultType (tl ts3)
                                   val thisType = SOME (needType (nd2,SOME false))
                               in
                                trace(["<< functionSignature with next=",tokenname(hd ts4)]);
                                (ts4,Ast.FunctionSignature
                                     {typeParams=nd1,
                                      thisType=thisType,
                                      params=(b,i),
                                      paramTypes=t,
                                      defaults=e,
                                      returnType=nd4,
                                      ctorInits=NONE,
                                      hasRest=hasRest })
                               end
                         | _ => error ["unknown token in functionSignature"]
                    end
                 | (RightParen, _) :: _ =>
                   let
                       val (ts3,nd3) = resultType (tl ts2)
                   in
                       trace ["<< functionSignature with next=",tokenname(hd ts3)];
                       (ts3,Ast.FunctionSignature
                                { typeParams=nd1,
                                  thisType=SOME (needType (nd2,SOME false)),
                                  params=([],[]),
                                  paramTypes=[],
                                  defaults=[],
                                  returnType=nd3,
                                  ctorInits=NONE,
                                  hasRest=false})
                   end
                 | _ => error ["unknown final token of this-qualified function signature"]
            end
      | (LeftParen, _) :: _ =>
               let
                   val (ts2,((b,i),e,t),hasRest) = parameters (tl ts1)
               in case ts2 of
                   (RightParen, _) :: _ =>
                       let
                           val (ts3,nd3) = resultType (tl ts2)
                       in
                        trace ["<< functionSignature with next=",tokenname(hd ts3)];
                        (ts3,Ast.FunctionSignature
                                 {typeParams=nd1,
                                  params=(b,i),
                                  paramTypes=t,
                                  defaults=e,
                                  returnType=nd3,
                                  ctorInits=NONE,
                                  thisType=NONE,  (* todo *)
                                  hasRest=hasRest })
                       end
                 | _ => error ["unknown final token of function signature"]
            end
      | _ => error ["unknown initial token of function signature"]
    end

and functionSignatureType (ts) =
    let val _ = trace([">> functionSignatureType with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = typeParameters ts
    in case ts1 of
        (LeftParen, _) :: (This, _) :: (Colon, _) ::  _ =>
            let
                val (ts2,nd2) = primaryIdentifier (tl (tl (tl ts1)))
            in case ts2 of
                (Comma, _) :: _ =>
                    let
                        val (ts3,(d,t)) = nonemptyParametersType (tl ts2)  
                    in case ts3 of
                        (RightParen, _) :: _ =>
                           let
                               val (ts4,nd4) = resultType (tl ts3)
                           in
                               trace ["<< functionSignature with next=",tokenname(hd ts4)];
                               (ts4,Ast.FunctionSignature
                                        { typeParams=nd1,
                                          thisType=SOME (needType (nd2,SOME false)),
                                          params=([],[]),
                                          paramTypes=t,
                                          defaults=d,
                                          returnType=nd4,
                                          ctorInits=NONE,
                                          hasRest=false }) (* do we need this *)
                               end
                      | _ => error ["unknown token in functionSignatureType"]
                    end
              | (RightParen, _) :: _ =>
                    let
                        val (ts3,nd3) = resultType (tl ts2)
                    in
                        trace ["<< functionSignature with next=",tokenname(hd ts3)];
                        (ts3,Ast.FunctionSignature
                                 { typeParams=nd1,
                                   thisType=SOME (needType (nd2,SOME false)),
                                   params=([],[]),
                                   paramTypes=[],
                                   defaults=[],
                                   returnType=nd3,
                                   ctorInits=NONE,
                                   hasRest=false }) (* do we need this *)
                          end
              | _ => error ["unknown token in functionSignatureType"]
            end
      | (LeftParen, _) :: _ =>
            let
                val (ts2, (d,t)) = parametersType (tl ts1)
            in case ts2 of
                   (RightParen, _) :: _ =>
                       let
                           val (ts3,nd3) = resultType (tl ts2)
                       in
                        trace ["<< functionSignature with next=",tokenname(hd ts3)];
                        (ts3,Ast.FunctionSignature
                                 { typeParams=nd1,
                                   params=([],[]),
                                   paramTypes=t,
                                   defaults=d,
                                   returnType=nd3,
                                   ctorInits=NONE,
                                   thisType=NONE,  (* todo *)
                                   hasRest=false }) (* do we need this *)
                       end
                 | _ => error ["unknown token in functionSignatureType"]
            end
      | _ => error ["unknown token in functionSignatureType"]
    end

(*
    TypeParameters    
        empty
        .<  TypeParameterList  >  
*)

and typeParameters ts =
    let val _ = trace([">> typeParameters with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftDotAngle, _) :: _ => 
            let
                val (ts1,nd1) = typeParameterList (tl ts)
            in case ts1 of
                (GreaterThan, _) :: _ => 
                    let
                    in
                        trace(["<< typeParameters with next=",tokenname(hd(tl ts1))]);
                        (tl ts1,nd1)
                    end
              | _ => error ["unknown token in typeParameters"]
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

and typeParameterList (ts) : (TOKEN * Ast.LOC) list * Ustring.STRING list =
    let val _ = trace([">> typeParameterList with next=",tokenname(hd(ts))]) 
        fun typeParameterList' (ts) =
            let
            in case ts of
                (Comma, _) :: _ =>
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

and nonemptyParameters (ts) (n) (initRequired)
    : ((TOKEN * Ast.LOC) list * (Ast.BINDINGS * Ast.EXPR list * Ast.TYPE_EXPR list) * bool) = 
    let
    in case ts of
        (TripleDot, _) :: _ => 
            let
                val (ts1,nd1) = restParameter ts n
            in case ts1 of
                (RightParen, _) :: _ => (ts1,nd1,true)
              | _ => error ["unknown token in nonemptyParameters"]
            end
      | _ => 
            let
                val (ts1,((b1,i1),e1,t1)) = parameterInit ts n initRequired
            in case ts1 of
                (RightParen, _) :: _ => (ts1,((b1,i1),e1,t1),false)
              | (Comma, _) :: _ =>
                    let                        
                        val (ts2,((b2,i2),e2,t2),hasRest) = nonemptyParameters (tl ts1) (n+1) (not (length e1 = 0))
                    in
                        (ts2,((b1@b2,i1@i2),e1@e2,t1@t2),hasRest)
                    end
              | _ => error ["unknown token in nonemptyParameters"]
            end
    end

and nonemptyParametersType (ts)
    : ((TOKEN * Ast.LOC) list * (Ast.EXPR list * Ast.TYPE_EXPR list)) = 
    let
    in case ts of
        (TripleDot, _) :: _ => 
            let
                val (ts1,nd1) = restParameterType ts
            in case ts1 of
                (RightParen, _) :: _ => (ts1,nd1)
              | _ => error ["unknown token in nonemptyParametersType"]
            end
      | _ => 
            let
                val (ts1,(d1,t1)) = parameterInitType ts
            in case ts1 of
                (RightParen, _) :: _ => (ts1,(d1,t1))
              | (Comma, _) :: ts2 =>
                    let
                        val (ts3,(d3,t3)) = nonemptyParametersType ts2
                    in
                        (ts3,(d1@d3,t1@t3))
                    end
              | _ => error ["unknown token in nonemptyParametersType"]
            end
    end

and parameters (ts) 
    : ((TOKEN * Ast.LOC) list * (Ast.BINDINGS * Ast.EXPR list * Ast.TYPE_EXPR list) * bool) =
    let val _ = trace([">> parameters with next=",tokenname(hd(ts))]) 
    in case ts of 
        (RightParen, _) :: ts1 => (ts,(([],[]),[],[]),false)
      | _ => nonemptyParameters ts 0 false
    end

and parametersType (ts) 
    : ((TOKEN * Ast.LOC) list * (Ast.EXPR list * Ast.TYPE_EXPR list))=
    let val _ = trace([">> parameters with next=",tokenname(hd(ts))]) 
    in case ts of 
        (RightParen, _) :: ts1 => (ts,([],[]))
      | _ => nonemptyParametersType ts
    end

(*
    ParameterInit
        Parameter
        Parameter  =  NonAssignmentExpression(ALLOWIN)
*)

and parameterInit (ts) (n) (initRequired)
    : ((TOKEN * Ast.LOC) list * (Ast.BINDINGS * Ast.EXPR list * Ast.TYPE_EXPR list)) =
    let val _ = trace([">> parameterInit with next=",tokenname(hd(ts))]) 
        val (ts1,(temp,nd1)) = parameter ts n
    in case (ts1,initRequired) of
        ((Assign, _) :: _,_) => 
            let
                val {pattern,ty,...} = nd1
                val (ts2,nd2) = nonAssignmentExpression (tl ts1,NOLIST,ALLOWIN)
                val (b,i) = desugarPattern (locOf ts) pattern ty (SOME (Ast.GetParam n)) (0)
            in 
                trace(["<< parameterInit with next=",tokenname(hd(ts))]);
                (ts2, ((temp::b,i),[nd2],[ty]))
            end
      | (_,false) => 
            let
                val {pattern,ty,...} = nd1
                val (b,i) = desugarPattern (locOf ts) pattern ty (SOME (Ast.GetParam n)) (0)
            in
                trace(["<< parameterInit with next=",tokenname(hd(ts))]);
                (ts1, ((temp::b,i),[],[ty]))
            end
      | _ => error ["default expression required"]
    end

and parameterInitType (ts) 
    : ((TOKEN * Ast.LOC) list * (Ast.EXPR list * Ast.TYPE_EXPR list)) = 
    let val _ = trace([">> parameterInitType with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = parameterType ts
    in case ts1 of
        (Assign,_) :: _ =>
            let
                val (ts2,init) = (tl ts1,Ast.LiteralExpr Ast.LiteralUndefined)
            in
                trace(["<< parameterInitType with next=",tokenname(hd(ts))]);
                (ts2, ([init],[nd1]))
            end
      | _ =>
            let
            in
                trace(["<< parameterInitType with next=",tokenname(hd(ts))]);
                (ts1, ([],[nd1]))
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

and parameter (ts) (n) 
    : ((TOKEN * Ast.LOC) list * (Ast.BINDING * {pattern:PATTERN, ty:Ast.TYPE_EXPR})) =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = parameterKind (ts)
        val (ts2,(p,t)) = typedPattern (ts1,NOLIST,ALLOWIN)
        val temp = Ast.Binding {ident=Ast.ParamIdent n,ty=t}
    in
        trace(["<< parameter with next=",tokenname(hd(ts2))]);
        (ts2,(temp,{pattern=p,ty=t}))
    end

and parameterType (ts) =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))])
        val (ts2,t) = nullableTypeExpression ts
    in
        trace(["<< parameter with next=",tokenname(hd(ts2))]);
        (ts2,t)
    end

and parameterKind (ts) 
    : ((TOKEN * Ast.LOC) list * Ast.VAR_DEFN_TAG)  = 
    let val _ = trace([">> parameterKind with next=",tokenname(hd(ts))]) 
    in case ts of
        (Const, _) :: ts1 => (ts1,Ast.Const)
      | ts1 => (ts1,Ast.Var)
    end

(*
    RestParameter
        ...
        ...  ParameterKind TypedIdentifier
        ...  ParameterKind TypedPattern
*)

and restParameter (ts) (n): ((TOKEN * Ast.LOC) list * (Ast.BINDINGS * Ast.EXPR list * Ast.TYPE_EXPR list)) =
    let val _ = trace([">> restParameter with next=",tokenname(hd(ts))])
    in case ts of
        (TripleDot, _) :: _ =>
            let
            in case tl ts of
                (RightParen, _) :: _ => 
                    (tl ts, (([Ast.Binding{ident=Ast.PropIdent Ustring.empty,ty=Ast.SpecialType Ast.Any}],[]),[],[Ast.ArrayType [Ast.SpecialType Ast.Any]]))
              | _ =>
                    let
                        val (ts1,(temp,{pattern,ty,...})) = parameter (tl ts) n
                        val (b,i) = desugarPattern (locOf ts) pattern ty (SOME (Ast.GetParam n)) (0)
                    in
                        (ts1, ((temp::b,i),[Ast.LiteralExpr (Ast.LiteralArray {exprs=[],ty=NONE})],[Ast.ArrayType [Ast.SpecialType Ast.Any]]))
                    end
            end
      | _ => error ["unknown token in restParameter"]
    end

and restParameterType (ts) : ((TOKEN * Ast.LOC) list * (Ast.EXPR list * Ast.TYPE_EXPR list)) =
    let val _ = trace([">> restParameter with next=",tokenname(hd(ts))])
    in case ts of
        (TripleDot, _) :: _ =>
            let
            in case tl ts of
                (RightParen, _) :: _ => 
                    (tl ts,([],[Ast.ArrayType []])) 
              | _ =>
                    let
                        val (ts1:(TOKEN * Ast.LOC) list,ty) = parameterType (tl ts)
                    in
                        (ts1,([],[ty]))
                    end
            end
      | _ => error ["unknown token in restParameterType"]
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
        (Colon, _) :: (Void, _) :: ts1 => (ts1,Ast.SpecialType(Ast.VoidType))
      | (Colon, _) :: _ => 
            let
                val (ts1,nd1) = nullableTypeExpression (tl ts)
            in
                trace ["<< resultType with next=",tokenname(hd ts1)];
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
        (LeftBrace, _) :: _ => 
            let
                val (ts1,nd1) = fieldList (tl ts)
            in case ts1 of
                (RightBrace, _) :: (Colon, _) :: _ => 
                    let
                        val (ts2,nd2) = typeExpression (tl (tl ts1))
                    in
                        (ts2,Ast.LiteralObject {expr=nd1,ty=SOME nd2})
                    end
              | (RightBrace, _) :: _ => 
                    (tl ts1,Ast.LiteralObject {expr=nd1,ty=NONE})
              | _ => error ["unknown token in objectLiteral"]
            end
      | _ => error ["unknown token in objectLiteral"]
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
                (Comma, _) :: ts2 => 
                    let
                        val (ts3,nd3) = nonemptyFieldList (ts2)
                    in
                        (ts3,nd1::nd3)
                    end
              | _ => (ts1,nd1::[])
            end
    in case ts of
        (RightBrace, _) :: _ => 
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
        ((Get | Set),_) :: (Colon, _) :: _ =>  (* special case for fields with name 'get' or 'set' *)
            let
                val (ts1,nd1) = fieldKind ts
                val (ts2,nd2) = fieldName ts1
            in case ts2 of
                (Colon, _) :: _ =>
                    let
                        val (ts3,nd3) = assignmentExpression (tl ts2,NOLIST,ALLOWIN)
                    in
                        (ts3,{kind=nd1,name=nd2,init=nd3})
                    end
              | _ => error ["unknown token in literalField"]
            end
      | (Get, _) :: _ =>
            let
                val (ts1,nd1) = fieldName (tl ts)
                val (ts2,{fsig,block}) = functionCommon (ts1)
            in
                (ts2,{kind=Ast.Var,
                      name=nd1,
                      init=Ast.LiteralExpr 
                               (Ast.LiteralFunction 
                                    (Ast.Func {name={kind=Ast.Get, ident=Ustring.empty},
                                               fsig=fsig,
                                               block=block,
                                               isNative=false,
                                               param=([],[]),
                                               defaults=[],
                                               ty=functionTypeFromSignature fsig}))})
            end
      | (Set, _) :: _ =>
            let
                val (ts1,nd1) = fieldName (tl ts)
                val (ts2,{fsig,block}) = functionCommon (ts1)
            in
                (ts2,{kind=Ast.Var,
                      name=nd1,
                      init=Ast.LiteralExpr 
                               (Ast.LiteralFunction 
                                    (Ast.Func {name={kind=Ast.Get,ident=Ustring.empty},
                                               fsig=fsig,
                                               block=block,
                                               isNative=false,
                                               param=([],[]),
                                               defaults=[],
                                               ty=functionTypeFromSignature fsig}))})
            end
      | _ => 
            let
                val (ts1,nd1) = fieldKind ts
                val (ts2,nd2) = fieldName ts1
            in case ts2 of
                (Colon, _) :: _ =>
                    let
                        val (ts3,nd3) = assignmentExpression (tl ts2,NOLIST,ALLOWIN)
                    in
                        (ts3,{kind=nd1,name=nd2,init=nd3})
                    end
              | _ => error ["unknown token in literalField"]
            end
    end

and fieldKind (ts) : ((TOKEN * Ast.LOC) list * Ast.VAR_DEFN_TAG)  = 
    let val _ = trace([">> fieldKind with next=",tokenname(hd(ts))]) 
    in case ts of
        ((Const | Get | Set ), _) :: (Colon, _) :: _ => (ts,Ast.Var)
      | (Const, _) :: _ => (tl ts,Ast.Const)
      | _ => (ts,Ast.Var)
    end

and fieldName (ts) : (TOKEN * Ast.LOC) list * Ast.IDENT_EXPR =
    let val _ = trace([">> fieldName with next=",tokenname(hd(ts))]) 
    in case ts of
        (StringLiteral s, _) :: ts1 => (ts1,Ast.Identifier {ident=s,openNamespaces=[]})

      | (DecimalLiteral n, _) :: ts1 => 
        (ts1, Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr(Ast.LiteralContextualDecimal n)),
                                         openNamespaces = []})
      | (DecimalIntegerLiteral n, _) :: ts1 => 
        (ts1, Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr(Ast.LiteralContextualDecimalInteger n)),
                                         openNamespaces = [] })
      | (HexIntegerLiteral n, _) :: ts1 => 
        (ts1, Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr(Ast.LiteralContextualHexInteger n)),
                                         openNamespaces = []})
      | (OctIntegerLiteral n, _) :: ts1 => 
        (ts1, Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr(Ast.LiteralContextualOctInteger n)),
                                         openNamespaces = []})
      | _ => 
            let
                val (ts1,nd1) = reservedOrOrdinaryIdentifier (ts)
            in
                (ts1,Ast.Identifier {ident=nd1,openNamespaces=[]})  (* todo: allow qualified identifier *)
            end
    end

and functionCommon ts =
    let val _ = trace([">> functionCommon with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = functionSignature ts
    in case ts1 of
        (LeftBrace, _) :: _ => 
            let
                val (ts2,nd2) = block (ts1,LOCAL)
            in
                (ts2,{fsig=nd1,block=nd2})
            end
      | _ => (error(["expecting {"]); error ["unknown token in functionCommon"])
    end

(*
    ArrayLiteral    
        [  ElementList  ]
        [  ElementList  ]  :  ArrayType
*)

and arrayLiteral (ts) =
    let val _ = trace([">> arrayLiteral with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftBracket, _) :: _ => 
            let
                val (ts1,nd1) = elementList (tl ts)
            in case ts1 of
                (RightBracket, _) :: (Colon, _) :: _ => 
                    let
                        val (ts2,nd2) = typeExpression (tl (tl ts1))
                    in
                        (ts2,Ast.LiteralArray {exprs=nd1,ty=SOME nd2})
                    end
              | (RightBracket, _) :: _ => 
                    (tl ts1,Ast.LiteralArray {exprs=nd1,ty=NONE})
              | _ => error ["unknown token in arrayLiteral"]
            end
      | _ => error ["unknown token in arrayLiteral"]
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
        (RightBracket, _) :: _ => (ts,[])
      | (Comma, _) :: _ => 
            let
                val (ts1,nd1) = elementList (tl ts)
            in
                (ts1,Ast.LiteralExpr(Ast.LiteralUndefined) :: nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = assignmentExpression (ts,NOLIST,ALLOWIN)
            in case ts1 of
                (Comma, _) :: _ =>
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
        (Null, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNull))
      | (True, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean true))
      | (False, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean false))

      | (DecimalLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralContextualDecimal n))
      | (DecimalIntegerLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralContextualDecimalInteger n))
      | (HexIntegerLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralContextualHexInteger n))
      | (OctIntegerLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralContextualOctInteger n))

      | (ExplicitDecimalLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralDecimal n))
      | (ExplicitDoubleLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralDouble n))
      | (ExplicitIntLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralInt n))
      | (ExplicitUIntLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralUInt n))

      | (StringLiteral s,_) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralString s))
      | (This, _) :: ts1 => (ts1, Ast.ThisExpr)
      | (LeftParen, _) :: _ => 
            parenListExpression ts
      | (LeftBracket, _) :: _ => 
            let
                val (ts1,nd1) = arrayLiteral ts
            in
                (ts1,Ast.LiteralExpr nd1)
            end
      | (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = objectLiteral ts
            in
                (ts1,Ast.LiteralExpr nd1)
            end
      | (Function, _) :: _ => functionExpression (ts,a,b)
      | (LexBreakDiv thunks, _) :: _ => 
        (case (#lex_regexp thunks)() of
             (RegexpLiteral str, _) :: rest =>
             (rest, Ast.LiteralExpr(Ast.LiteralRegExp {str=str}))
           | _ => error ["non-regexp-literal token after '/' lexbreak"])

(* todo:
      | (XmlMarkup | LessThan ) :: _ => xmlInitializer ts
*)

      | (At, _) :: _ => 
            let
                val (ts1,nd1) = attributeIdentifier ts
            in
                (ts1,Ast.LexicalRef {ident=nd1, loc=locOf ts})
            end
      | _ => 
            let
                val (ts1,nd1) = primaryIdentifier ts
            in
                (ts1,Ast.LexicalRef {ident=nd1, loc=locOf ts})
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
        (Super, _) :: _ =>
            let
            in case tl ts of
                (LeftParen, _) :: _ => 
                    let
                           val (ts1,nd1) = parenExpression(tl (tl ts))
                    in
                        (ts1,Ast.SuperExpr(SOME(nd1)))
                    end
                | _ => 
                    (tl ts,Ast.SuperExpr(NONE))
            end
      | _ => error ["unknown token in superExpression"]
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
        (New, _) :: _ =>
            let
                val (ts1,nd1) = memberExpression(tl ts,a,b)
            in case ts1 of
                (LeftParen, _) :: _ =>
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
      | (Super, _) :: _ =>
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
        ((LeftBracket, _) :: _ | (Dot, _) :: _) =>
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
        ((LeftBracket, _) :: _ | (Dot, _) :: _) =>
            let
                val (ts1,nd1) = propertyOperator(ts,nd)
                val (ts2,nd2) = callExpressionPrime(ts1,nd1,a,b)
            in
                trace(["<< callExpressionPrime with next=",tokenname(hd(ts2))]);
                (ts2,nd2)
            end
      | (LeftParen, _) :: _ => 
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
        (New, _) :: (New, _) :: _ =>
            let
                val (ts1,nd1) = newExpression(tl ts,a,b)  (* eat only the first new *)
            in
                trace(["<< newExpression new new with next=",tokenname(hd(ts1))]);
                (ts1,Ast.NewExpr({obj=nd1,actuals=[]}))
            end
      | (New, _) :: _ =>
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
                (LeftParen, _) :: _ =>
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

and arguments (ts) : ((TOKEN * Ast.LOC) list * Ast.EXPR list)  =
    let val _ = trace([">> arguments with next=",tokenname(hd(ts))])
    in case ts of
        (LeftParen, _) :: (RightParen, _) :: _ => 
            let
            in
                trace(["<< arguments with next=",tokenname(hd(tl (tl ts)))]);
                (tl (tl ts),[]) 
            end
      | (LeftParen, _) :: _ => 
            let
                val (ts1,nd1) = argumentList(tl ts)
            in case ts1 of
                (RightParen, _) :: _ => 
                    let
                    in
                        trace(["<< arguments with next=",tokenname(hd(tl ts1))]);
                        (tl ts1,nd1)
                    end
              | _ => error ["unknown token in arguments"]
            end
      | _ => error ["unknown token in arguments"]
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

and argumentList (ts) : ((TOKEN * Ast.LOC) list * Ast.EXPR list)  =
    let val _ = trace([">> argumentList with next=",tokenname(hd(ts))])
        fun argumentList' (ts) : ((TOKEN * Ast.LOC) list * Ast.EXPR list) =
            let val _ = trace([">> argumentList' with next=",tokenname(hd(ts))])
            in case ts of
                (Comma, _) :: _ => 
                    let
                        val (ts1,nd1) = assignmentExpression(tl ts,NOLIST,ALLOWIN)
                        val (ts2,nd2) = argumentList'(ts1)
                    in
                        (ts2,nd1::nd2)
                    end
              | (RightParen, _) :: _ => 
                    (ts,[])
              | _ => 
                (trace ["*syntax error*: expect '",tokenname (RightParen,0), "' before '",tokenname(hd ts),"'"];
                 error ["unknown token in argumentList"])
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
        (Dot, _) :: (LeftParen, _) :: _ =>
                    let
                        val (ts1,nd1) = parenListExpression(tl ts)
                    in case ts1 of
                        (DoubleColon, _) :: (LeftBracket, _) :: _ => 
                               let
                                val (ts2,nd2) = brackets(tl ts1)
                               in
                                   (ts2,Ast.ObjectRef {base=nd,ident=Ast.QualifiedExpression {
                                            qual=nd1, expr=nd2}, loc=locOf ts})
                            end
                      | (DoubleColon, _) :: _ => 
                            let
                                val (ts2,nd2) = reservedOrOrdinaryIdentifier(tl ts1)
                            in
                                (ts2,Ast.ObjectRef({base=nd,
                                                    ident=Ast.QualifiedIdentifier 
                                                              {qual=nd1, ident=nd2}, 
                                                    loc=locOf ts}))
                            end
                      | _ => error ["unknown token in propertyOperator"] (* e4x filter expr *)
                    end
      | (Dot, _) :: _ => 
                    let
                     in case (isreserved(hd (tl ts)),tl ts) of
                        ((true,((Intrinsic | Private | Public | Protected | Internal),_) ::_) |
                         (false,_)) => 
                            let
                                val (ts1,nd1) = propertyIdentifier (tl ts)
                            in
                                (ts1,Ast.ObjectRef {base=nd,ident=nd1,loc=locOf ts})
                            end
                      | (true,_) =>
                            let
                                val (ts1,nd1) = reservedIdentifier (tl ts)
                            in
                                (ts1,Ast.ObjectRef 
                                         { base=nd,
                                           ident=Ast.Identifier 
                                                     { ident=nd1,
                                                       openNamespaces=[] },
                                           loc=locOf ts})
                            end 
                    end
      | (LeftBracket, _) :: _ => 
            let
                val (ts1,nd1) = brackets(ts)
            in
                (ts1,Ast.ObjectRef 
                         { base=nd,
                           ident=Ast.ExpressionIdentifier {expr = nd1, openNamespaces = []},
                           loc=locOf ts})
            end
      | _ => error ["unknown token in propertyOperator"]
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

and brackets (ts) : ((TOKEN * Ast.LOC) list * Ast.EXPR) =
    let val _ = trace([">> brackets with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftBracket, _) :: ts' =>
            let
                val (ts1,nd1) = listExpression (ts',ALLOWIN)
            in case ts1 of
                (Colon, _) :: ts'' => 
                    let
                        val (ts2,nd2) = listExpression (ts'',ALLOWIN)
                    in case ts2 of
                        (RightBracket, _) :: ts'' => (ts'',Ast.SliceExpr (nd1,nd2,Ast.ListExpr [])) 
                      | _ => error ["unknown token in brackets"]
                    end
              | (RightBracket, _) :: ts'' => (ts'',nd1) 
              | _ => error ["unknown token in brackets"]
            end
      | _ => error ["unknown token in brackets"]
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
        (New, _) :: (New, _) :: _ =>
            let
                val (ts1,nd1) = newExpression(ts,a,b)
            in
                (ts1,nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = memberExpression(ts,a,b)
            in case ts1 of
                (LeftParen, _) :: _ =>
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
        (PlusPlus, _) :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostIncrement NONE,nd1))
      | (MinusMinus, _) :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostDecrement NONE,nd1))
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
        (Delete, _) :: ts1 => 
            let 
                val (ts2,nd2) = postfixExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.Delete,nd2)) 
            end
      | (Void, _) :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.Void,nd2)) 
            end
      | (TypeOf, _) :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.Typeof,nd2)) 
            end
      | (PlusPlus, _) :: ts1 => 
            let 
                val (ts2,nd2) = postfixExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.PreIncrement NONE,nd2)) 
            end
      | (MinusMinus, _) :: ts1 => 
            let 
                val (ts2,nd2) = postfixExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.PreDecrement NONE,nd2)) 
            end
      | (Plus, _) :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.UnaryPlus NONE,nd2)) 
            end
      | (Minus, _) :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.UnaryMinus NONE,nd2)) 
            end
      | (BitwiseNot, _) :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.BitwiseNot,nd2)) 
            end
      | (Not, _) :: ts1 => 
            let 
                val (ts2,nd2) = unaryExpression (ts1,a,b) 
            in 
                (ts2,Ast.UnaryExpr(Ast.LogicalNot,nd2)) 
            end
      | (Type, _) :: _ =>
            (case (hd (tl ts)) of
                ((Null | Undefined | Identifier _ | LeftParen | LeftBrace | LeftBracket | Function
                 | Private | Protected | Public | Internal | Intrinsic | Mult),_) =>
                   if newline (tl ts)
                   then 
                       (tl ts,Ast.LexicalRef {ident=Ast.Identifier {ident=Ustring.type_,openNamespaces=[[]]},loc=NONE})
                   else
                       let 
                           val (ts1,nd1) = nullableTypeExpression (tl ts)
                       in 
                           (ts1,Ast.TypeExpr(nd1)) 
                       end
              | _ =>
                    let
                    in
                        (tl ts,Ast.LexicalRef {ident=Ast.Identifier {ident=Ustring.type_,openNamespaces=[[]]},loc=NONE})
                    end)
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
                (Mult, _) :: ts2 => 
                    let 
                        val (ts3,nd3) = unaryExpression (ts2,a,b) 
                    in 
                        multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Times NONE,nd1,nd3),a,b) 
                    end

              | (LexBreakDiv x,_) :: _ =>
                    let    
                        val _ = trace ["hd ts1 = ", tokenname(hd ts1)]
                    in case (#lex_initial x)() of
                        (Div, _) :: ts2 => 
                            let 
                                val (ts3,nd3) = unaryExpression (ts2,a,b) 
                            in 
                                multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Divide NONE,nd1,nd3),a,b) 
                            end
                      | (DivAssign, divAssignLoc) :: ts2 => 
                            (trace(["<< multiplicative"]);((DivAssign, divAssignLoc) :: ts2, nd1))
                      | _ => error ["missing token in multiplicativeExpression"]
                    end

              | (Modulus, _) :: ts2 => 
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
                (Plus, _) :: ts2 => 
                    let 
                        val (ts3,nd3) = multiplicativeExpression (ts2,a,b) 
                    in 
                        additiveExpression' (ts3,Ast.BinaryExpr(Ast.Plus NONE,nd1,nd3),a,b) 
                    end
              | (Minus, _) :: ts2 => 
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
                (LeftShift, _) :: ts2 => 
                    let 
                        val (ts3,nd3) = additiveExpression (ts2,a,b) 
                    in 
                        shiftExpression' (ts3,Ast.BinaryExpr(Ast.LeftShift,nd1,nd3),a,b) 
                    end
              | (RightShift, _) :: ts2 => 
                    let 
                        val (ts3,nd3) = additiveExpression (ts2,a,b)
                    in 
                        shiftExpression' (ts3,Ast.BinaryExpr(Ast.RightShift,nd1,nd3),a,b) 
                    end
              | (UnsignedRightShift, _) :: ts2 => 
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
                (((LexBreakLessThan x),_) :: _,_) =>
                    let    
                    in case (#lex_initial x)() of
                        (LessThan, _) :: ts2 => 
                            let 
                                val (ts3,nd3) = shiftExpression (ts2,a,b) 
                            in 
                                relationalExpression' (ts3,Ast.BinaryExpr(Ast.Less NONE,nd1,nd3),a,ALLOWIN) 
                            end
                      | _ => error ["unknown token in relationalExpression"]
                    end

              | ((LessThan, _) :: ts2,_) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.Less NONE,nd1,nd3),a,ALLOWIN) 
                    end
              | ((GreaterThan, _) :: ts2,_) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.Greater NONE,nd1,nd3),a,ALLOWIN) 
                    end
              | ((LessThanOrEquals, _) :: ts2, _) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.LessOrEqual NONE,nd1,nd3),a,ALLOWIN) 
                    end
              | ((GreaterThanOrEquals, _) :: ts2, _) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.GreaterOrEqual NONE,nd1,nd3),a,ALLOWIN) 
                    end
              | ((In, _) :: ts2, ALLOWIN) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.In,nd1,nd3),a,ALLOWIN) 
                    end
              | ((InstanceOf, _) :: ts2, _) => 
                    let 
                        val (ts3,nd3) = shiftExpression (ts2,a,b) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.InstanceOf,nd1,nd3),a,ALLOWIN) 
                    end
              | ((Cast, _) :: ts2, _) => 
                    let 
                        val (ts3,nd3) = typeExpression (ts2) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Cast,nd1,nd3),a,ALLOWIN) 
                    end
              | ((To, _) :: ts2, _) => 
                    let 
                        val (ts3,nd3) = typeExpression (ts2) 
                    in 
                        relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.To,nd1,nd3),a,ALLOWIN) 
                    end
              | ((Is, _) :: ts2, _) => 
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
                (Equals, _) :: ts2 => 
                    let 
                        val (ts3,nd3) = relationalExpression (ts2,a,b) 
                    in 
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.Equals NONE,nd1,nd3)) 
                    end
              | (NotEquals, _) :: ts2 => 
                    let 
                        val (ts3,nd3) = relationalExpression (ts2,a,b) 
                    in 
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.NotEquals NONE,nd1,nd3)) 
                    end
              | (StrictEquals, _) :: ts2 => 
                    let 
                        val (ts3,nd3) = relationalExpression (ts2,a,b) 
                    in 
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictEquals NONE,nd1,nd3)) 
                    end
              | (StrictNotEquals, _) :: ts2 => 
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
                (BitwiseAnd, _) :: ts2 => 
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
                (BitwiseXor, _) :: ts2 => 
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
                (BitwiseOr, _) :: ts2 => 
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
                (LogicalAnd, _) :: ts2 => 
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
                (LogicalOr, _) :: ts2 => 
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
        (Let, _) :: _ => letExpression(ts,b)
      | (Yield, _) :: _ => yieldExpression(ts,b)
      | _ => 
            let
                val (ts2,nd2) = logicalOrExpression(ts,ALLOWLIST,b)
            in case ts2 of
                (QuestionMark, _) :: ts3 => 
                    let
                        val (ts4,nd4) = assignmentExpression(ts3,ALLOWLIST,b)
                    in case ts4 of
                        (Colon, _) :: ts5 =>
                            let
                                val (ts6,nd6) = assignmentExpression(ts5,ALLOWLIST,b)
                            in
                                (ts6, Ast.TernaryExpr (nd2, nd4, nd6))
                            end
                      | _ => error ["unknown token in conditionalExpression"]                            
                    end
              | _ => 
                    (trace(["<< conditionalExpression ALLOWLIST with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end
 
  | conditionalExpression (ts,NOLIST,b) =
    let val _ = trace([">> conditionalExpression NOLIST with next=",tokenname(hd(ts))])
    in case ts of
        (Yield, _) :: _ => simpleYieldExpression ts
      | _ => 
            let
                val (ts2,nd2) = logicalOrExpression(ts,NOLIST,b)
            in case ts2 of
                (QuestionMark, _) :: ts3 => 
                    let
                        val (ts4,nd4) = assignmentExpression(ts3,NOLIST,b)
                    in case ts4 of
                        (Colon, _) :: ts5 =>
                            let
                                val (ts6,nd6) = assignmentExpression(ts5,NOLIST,b)
                            in
                                (ts6, Ast.TernaryExpr (nd2, nd4, nd6))
                            end
                      | _ => error ["unknown token in conditionalExpression"]                            
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
        (Let, _) :: _ => letExpression(ts,b)
      | (Yield, _) :: _ => yieldExpression(ts,b)
      | _ => 
            let
                val (ts2,nd2) = logicalOrExpression(ts,ALLOWLIST,b)
            in case ts2 of
                (QuestionMark, _) :: ts3 => 
                    let
                        val (ts4,nd4) = nonAssignmentExpression(ts3,ALLOWLIST,b)
                    in case ts4 of
                        (Colon, _) :: ts5 =>
                            let
                                val (ts6,nd6) = nonAssignmentExpression(ts5,ALLOWLIST,b)
                            in
                                (ts6,nd6)
                            end
                      | _ => error ["unknown token in nonAssignmentExpression"]                            
                    end
              | _ => 
                    (trace(["<< nonAssignmentExpression ALLOWLIST with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end
 
  | nonAssignmentExpression (ts,NOLIST,b) =
    let val _ = trace([">> nonAssignmentExpression NOLIST with next=",tokenname(hd(ts))])
    in case ts of
        (Yield, _) :: _ => simpleYieldExpression ts
      | _ => 
            let
                val (ts2,nd2) = logicalOrExpression(ts,NOLIST,b)
            in case ts2 of
                (QuestionMark, _) :: ts3 => 
                    let
                        val (ts4,nd4) = nonAssignmentExpression(ts3,NOLIST,b)
                    in case ts4 of
                        (Colon, _) :: ts5 =>
                            let
                                val (ts6,nd6) = nonAssignmentExpression(ts5,NOLIST,b)
                            in
                                (ts6,nd6)
                            end
                      | _ => error ["unknown token in nonAssignmentExpression"]                            
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
        (Let, _) :: (LeftParen, _) :: ts1 => 
            let
                val (ts2,nd2) = letBindingList (ts1)
            in 
            case ts2 of
                (RightParen, _) :: ts3 =>
                    let
                        val (ts4,nd4) = listExpression(ts3,b)
                    in
                        (trace(["<< letExpression with next=",tokenname(hd(ts4))]);
                        (ts4,Ast.LetExpr{defs=nd2,body=nd4,head=NONE}))
                    end
               |    _ => error ["unknown token in letExpression"]
            end
      | _ => error ["unknown token in letExpression"]
    end

(*
    LetBindingList    
        empty
        NonemptyLetBindingList

    NonemptyLetBindingList    
        VariableBindingallowIn
        VariableBindingallowIn  ,  NonemptyLetBindingList
*)

and letBindingList (ts) : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) =
    let val _ = trace([">> letBindingList with next=",tokenname(hd(ts))]) 
        fun nonemptyLetBindingList (ts) : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) = 
            let
                val (ts1,(b1,i1)) = variableBinding (ts,NOLIST,ALLOWIN)
            in case ts1 of
                (RightParen, _) :: _ => (ts1,(b1,i1))
              | (Comma, _) :: _ =>
                    let
                        val (ts2,(b2,i2)) = nonemptyLetBindingList (tl ts1)
                    in
                        (trace(["<< nonemptyLetBindingList with next=",tokenname(hd ts2)]);
                        (ts2,(b1@b2,i1@i2)))
                    end
              | _ => error ["unknown token in letBindingList"]
            end
    in case ts of 
        (RightParen, _) :: _ => 
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
        (Yield, _) :: ts1 => 
            let
            in case ts1 of
                ((SemiColon, _) :: _ | (RightBrace, _) :: _ | (RightParen, _) :: _) => (ts1,Ast.YieldExpr NONE)
              | _ => 
                    let
                        val (ts2,nd2) = listExpression(ts1,b)
                    in
                        (ts2,Ast.YieldExpr(SOME nd2))
                    end
            end
      | _ => error ["unknown token in yieldExpression"]
    end

(*
    SimpleYieldExpression    
        yield
*)

and simpleYieldExpression ts =
    case ts of
        (Yield, _) :: ts1 => (ts1,Ast.YieldExpr NONE)
      | _ => error ["unknown token in simpleYieldExpression"]

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

and assignmentExpression (ts,a,b) : ((TOKEN * Ast.LOC) list * Ast.EXPR) = 
    let val _ = trace([">> assignmentExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = conditionalExpression(ts,a,b)
    in case ts1 of
        (Assign, _) :: _ => 
            let
                fun isInitStep (b:Ast.INIT_STEP) : bool =
                    case b of 
                       Ast.InitStep _ => true
                     | _ => false

                fun makeSetExpr (Ast.AssignStep (lhs,rhs)) = Ast.SetExpr (Ast.Assign,lhs,rhs)
                  | makeSetExpr (Ast.InitStep _) = LogErr.internalError ["makeSetExpr: shouldn't get here"]

                val p = patternFromExpr nd1
                val (ts2,nd2) = assignmentExpression(tl ts1,a,b) 
                val (binds,inits) = desugarPattern (locOf ts) p (Ast.SpecialType Ast.Any) (SOME nd2) 0  (* type is meaningless *)
                val (inits,assigns) = List.partition isInitStep inits    (* separate init steps and assign steps *)
                val sets = map makeSetExpr assigns
            in case binds of
                [] => (ts2,hd sets)
              | _ => (ts2,Ast.LetExpr {defs=(binds,inits),  
                                         body=Ast.ListExpr sets,
                                         head=NONE})
                        (* introduce a letexpr to narrow the scope of the temps *)
            end
          | ( (ModulusAssign, _) :: _ 
          | (LogicalAndAssign, _) :: _
          | (BitwiseAndAssign, _) :: _
          | (DivAssign, _) :: _
          | (BitwiseXorAssign, _) :: _
          | (LogicalOrAssign, _) :: _
          | (BitwiseOrAssign, _) :: _
          | (PlusAssign, _) :: _
          | (LeftShiftAssign, _) :: _
          | (MinusAssign, _) :: _
          | (RightShiftAssign, _) :: _
          | (UnsignedRightShiftAssign, _) :: _
          | (MultAssign, _) :: _ ) =>
            let
                val (ts2,nd2) = compoundAssignmentOperator ts1
                val (ts3,nd3) = assignmentExpression(tl ts1,a,b)                        
            in
                (ts3,Ast.SetExpr(nd2,nd1,nd3))
            end
          | (LexBreakDiv thunks, _) :: _ =>
            let
                val tok_list = (#lex_initial thunks)()
            in
                case tok_list of
                    (DivAssign, _) :: _ =>
						let
							val (ts2,nd2) = compoundAssignmentOperator tok_list
							val (ts3,nd3) = assignmentExpression(tl tok_list,a,b)                        
						in
							(ts3,Ast.SetExpr(nd2,nd1,nd3))
						end
                  | _ => error ["non-div-assign token after '/' lexbreak"]
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
            (ModulusAssign, _) :: _                 => (tl ts,Ast.AssignRemainder NONE)
          | (LogicalAndAssign, _) :: _             => (tl ts,Ast.AssignLogicalAnd)
          | (BitwiseAndAssign, _) :: _             => (tl ts,Ast.AssignBitwiseAnd)
          | (DivAssign, _) :: _                     => (tl ts,Ast.AssignDivide NONE)
          | (BitwiseXorAssign, _) :: _             => (tl ts,Ast.AssignBitwiseXor)
          | (LogicalOrAssign, _) :: _             => (tl ts,Ast.AssignLogicalOr)
          | (BitwiseOrAssign, _) :: _             => (tl ts,Ast.AssignBitwiseOr)
          | (PlusAssign, _) :: _                 => (tl ts,Ast.AssignPlus NONE)
          | (LeftShiftAssign, _) :: _             => (tl ts,Ast.AssignLeftShift)
          | (MinusAssign, _) :: _                 => (tl ts,Ast.AssignMinus NONE)
          | (RightShiftAssign, _) :: _               => (tl ts,Ast.AssignRightShift)
          | (UnsignedRightShiftAssign, _) :: _     => (tl ts,Ast.AssignRightShiftUnsigned)
          | (MultAssign, _) :: _                   => (tl ts,Ast.AssignTimes NONE)
          | _ => error ["unknown token in assignmentExpression"]

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

and listExpression (ts,b) : ((TOKEN * Ast.LOC) list * Ast.EXPR) = 
    let
        val _ =    trace([">> listExpression with next=",tokenname(hd ts)])
        fun listExpression' (ts,b) : ((TOKEN * Ast.LOC) list * Ast.EXPR list) =
            let
                val _ =    trace([">> listExpression' with next=",tokenname(hd ts)])
            in case ts of
                (Comma, _) :: _ =>
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

and pattern (ts,a,b,g) : ((TOKEN * Ast.LOC) list * PATTERN) =
    let
    in case ts of
        (LeftBrace, _) :: _ => objectPattern (ts,g)
      | (LeftBracket, _) :: _ => arrayPattern (ts,g)
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
  | patternFromListExpr (_)  = (error(["invalid pattern"]); error ["unknown token in patternFromListExpr"])

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
            error ["unknown token in simplePatternFromExpr"])
    end

(*
    ObjectPattern(g)   
        {  DestructuringFieldList(g)  }
*)

and objectPattern (ts,g) =
    let val _ = trace([">> objectPattern with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftBrace, _) :: ts =>
            let
                val (ts1,nd1) = destructuringFieldList (ts,g)
            in case ts1 of
                (RightBrace, _) :: _ => (tl ts1,ObjectPattern nd1)
              | _ => error ["unknown token in objectPattern"]
            end
      | _ => error ["unknown token in objectPattern"]
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
      | _ => error ["unknown token in objectPatternFromExpr"]
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
                (Comma, _) :: _ =>
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
        (Colon, _) :: _ => 
            let
                val (ts2,nd2) = pattern (tl ts1,NOLIST,ALLOWIN,g)
            in
                (ts2,{ident=nd1,pattern=nd2})
            end
      | _ => error ["unknown token in destructuringField"]
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
        (LeftBracket, _) :: _ => 
            let
                val (ts1,nd1) = destructuringElementList (tl ts,g)
            in case ts1 of
                (RightBracket, _) :: _ => (tl ts1,ArrayPattern nd1)
              | _ => error ["unknown token in arrayPattern"]
            end
      | _ => error ["unknown token in arrayPattern"]
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
      | _ => error ["unknown token in arrayPatternFromExpr"]
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
        (RightBracket, _) :: _ => (ts,[])
      | (Comma, _) :: _ => 
            let
                val (ts1,nd1) = destructuringElementList (tl ts,g)
            in
                (ts1,SimplePattern(Ast.LiteralExpr(Ast.LiteralUndefined)) :: nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = destructuringElement (ts,g)
            in case ts1 of
                (Comma, _) :: _ =>
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

and typedIdentifier (ts,ns) 
    : (TOKEN * Ast.LOC) list * (PATTERN * Ast.TYPE_EXPR) =
    let val _ = trace([">> typedIdentifier with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = simplePattern (ts,NOLIST,NOIN,NOEXPR)
    in case ts1 of
        (Colon, _) :: _ => 
            let
                val (ts2,nd2) = nullableTypeExpression (tl ts1)
            in
                (ts2,(nd1,nd2))
            end
      | _ => 
            let
            in
                (trace(["<< typedIdentifier with next=",tokenname(hd(ts1))]);
                (ts1,(nd1,Ast.SpecialType Ast.Any)))
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

and typedPattern (ts,a,b) 
    : (TOKEN * Ast.LOC) list * (PATTERN * Ast.TYPE_EXPR) =
    let val _ = trace([">> typedPattern with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftBrace, _) :: _ => 
            let
                val (ts1,nd1) = objectPattern (ts,NOEXPR)
            in case ts1 of
                (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in
                        (ts2,(nd1,nd2))
                    end
              | _ =>
                    (ts1,(nd1,Ast.ObjectType []))
            end
      | (LeftBracket, _) :: _ => 
            let
                val (ts1,nd1) = arrayPattern (ts,NOEXPR)
            in case ts1 of
                (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in
                        (ts2,(nd1,nd2))
                    end
              | _ =>
                    (ts1,(nd1,Ast.ArrayType []))
            end
      | _ =>
            let
                val (ts1,nd1) = simplePattern (ts,a,b,NOEXPR)
            in case ts1 of
                (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = nullableTypeExpression (tl ts1)
                    in
                        (ts2,(nd1,nd2))
                    end
              | _ =>
                    (trace(["<< typedPattern with next=",tokenname(hd ts1)]);
                    (ts1,(nd1,Ast.SpecialType Ast.Any)))
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

and nullableTypeExpression (ts) : ((TOKEN * Ast.LOC) list * Ast.TYPE_EXPR) =
    let val _ = trace([">> nullableTypeExpression with next=",tokenname(hd ts)])
    in case ts of
        (Null, _) :: _ => (tl ts, Ast.SpecialType Ast.Null)
      | (Undefined, _) :: _ => (tl ts, Ast.SpecialType Ast.Undefined)
      | _ =>
            let
                val (ts1,nd1) = typeExpression ts
            in case ts1 of
                (Not, _) :: _ =>
                    (tl ts1,Ast.NullableType {expr=nd1,nullable=false})
              | (QuestionMark, _) :: _ =>
                    (tl ts1,Ast.NullableType {expr=nd1,nullable=true}) 
              | _ =>
                    (ts1,nd1) 
            end
    end

and typeExpression (ts) : ((TOKEN * Ast.LOC) list * Ast.TYPE_EXPR) =
    let val _ = trace([">> typeExpression with next=",tokenname(hd ts)])
    in case ts of
        (Null, _) :: _ => (tl ts, Ast.SpecialType Ast.Null)
      | (Undefined, _) :: _ => (tl ts, Ast.SpecialType Ast.Undefined)
      | (Function, _) :: _ => functionType ts
      | (LeftParen, _) :: _ => unionType ts
      | (LeftBrace, _) :: _ => objectType ts
      | (LeftBracket, _) :: _ => arrayType ts
      | _ =>
            let
                val (ts1,nd1) = primaryIdentifier ts
            in
                (ts1,needType(nd1,NONE))
            end
    end

(*
    FunctionType    
        function  FunctionSignature
*)

and functionType (ts) : ((TOKEN * Ast.LOC) list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> functionType with next=",tokenname(hd(ts))])         
    in case ts of
        (Function, _) :: _ => 
            let
                val (ts1,nd1) = functionSignatureType (tl ts)
            in
                trace(["<< functionType with next=",tokenname(hd ts1)]);
                (ts1, (Ast.FunctionType (functionTypeFromSignature nd1)))
            end
      | _ => error ["unknown token in functionType"]
    end


and functionTypeFromSignature fsig : Ast.FUNC_TYPE = 
    let
(*
        fun paramTypes (params:Ast.BINDING list) : (Ast.TYPE_EXPR list) =
                    case params of
                        [] => []
                      | _ =>
                            let
                                val (Ast.Binding {ident,ty}) = hd params
                                val types = paramTypes (tl params)
                            in case (ident,ty) of
                                (Ast.PropIdent _,t) => t :: types
                              | _ => types   (* ignore temps from desugaring *)
                            end
*)
    in 
       case fsig of
        Ast.FunctionSignature {typeParams,params,paramTypes,returnType,thisType,hasRest,defaults,...} =>
            let
                val (b,i) = params
            in
                {typeParams=typeParams,
                 params=paramTypes,
                 result=returnType,
                 thisType=thisType,
                 hasRest=hasRest,
                 minArgs=(length paramTypes)-(length defaults)}
            end
    end
  
(*
    UnionType    
        (  TypeExpressionList  )
*)

and unionType (ts) : ((TOKEN * Ast.LOC) list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> unionType with next=",tokenname(hd(ts))])         
    in case ts of
        (LeftParen, _) :: _ => 
            let
                val (ts1,nd1) = typeExpressionList (tl ts)
            in case ts1 of
                (RightParen, _) :: _ =>
                    (tl ts1, Ast.UnionType nd1)
              | _ => error ["unknown token in unionType"]
            end
      | _ => error ["unknown token in unionType"]
    end

(*
    ObjectType    
        {  FieldTypeList  }
*)

and objectType (ts) : ((TOKEN * Ast.LOC) list * Ast.TYPE_EXPR) = 
    let val _ = trace([">> objectType with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftBrace, _) :: ts1 => 
            let
                val (ts2,nd2) = fieldTypeList ts1
            in case ts2 of
                (RightBrace, _) :: ts3 => 
                    (trace(["<< objectType with next=",tokenname(hd(ts3))]);
                    (ts3,Ast.ObjectType nd2))
              | _ => error ["unknown token in objectType"]
            end
      | _ => error ["unknown token in objectType"]
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
                (Comma, _) :: _ => 
                    let
                        val (ts2,nd2) = nonemptyFieldTypeList (tl ts1)
                    in
                        (ts2,nd1::nd2)
                    end
              | _ => (ts1,nd1::[])
            end
    in case ts of
        (RightBrace, _) :: _ => (ts,[])
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

and fieldType (ts) : ((TOKEN * Ast.LOC) list * Ast.FIELD_TYPE) =
    let val _ = trace([">> fieldType with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = fieldName ts
        val ident = case nd1 of
                        Ast.Identifier{ident,...} => ident
                      | _ => LogErr.internalError ["fieldType: unexpected field name"]
    in case ts1 of
        (Colon, _) :: _ =>
            let
                val (ts2,nd2) = nullableTypeExpression (tl ts1)
            in
                (ts2,{name=ident,ty=nd2})
            end
      | _ => error ["unknown token in fieldType"]
    end

(*
    ArrayType    
        [  ElementTypeList  ]
*)

and arrayType (ts) : ((TOKEN * Ast.LOC) list * Ast.TYPE_EXPR)  =
    let val _ = trace([">> arrayType with next=",tokenname(hd(ts))]) 
    in case ts of
        (LeftBracket, _) :: _ => 
            let
                val (ts1,nd1) = elementTypeList (tl ts)
            in case ts1 of
                (RightBracket, _) :: _ => (tl ts1,Ast.ArrayType nd1)
              | _ => error ["unknown token in arrayType"]
            end
      | _ => error ["unknown token in arrayType"]
    end

(*
    ElementTypeList    
        empty
        TypeExpression
        ,  ElementTypeList
        TypeExpression  ,  ElementTypeList
*)

and elementTypeList (ts) : (TOKEN * Ast.LOC) list * Ast.TYPE_EXPR list =
    let val _ = trace([">> elementTypeList with next=",tokenname(hd(ts))]) 
    in case ts of
        (RightBracket, _) :: _ => (ts,[])
      | (Comma, _) :: _ => 
            let
                val (ts1,nd1) = elementTypeList (tl ts)
            in
                (ts1,Ast.SpecialType(Ast.Any) :: nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = nullableTypeExpression (ts)
            in case ts1 of
                (Comma, _) :: _ =>
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
    : ((TOKEN * Ast.LOC) list * Ast.TYPE_EXPR list) = 
    let
        fun typeExpressionList' (ts) =
            let
            in case ts of
                (Comma, _) :: _ =>
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
    SwitchStatement
    ThrowStatement Semicolonw
    TryStatement
    WhileStatementw
    WithStatementw
*)

and semicolon (ts,FULL) : ((TOKEN * Ast.LOC) list) =
    let val _ = trace([">> semicolon(FULL) with next=", tokenname(hd ts)])
    in case ts of
        (SemiColon, _) :: _ => (tl ts)
      | ((Eof | RightBrace),_) :: _ => (ts)   (* ABBREV special cases *)
      | _ => 
            if newline ts then (trace ["inserting semicolon"]; ts)
            else (error(["expecting semicolon before ",tokenname(hd ts)]); error ["unknown token in semicolon"])
    end
  | semicolon (ts,_) =
    let val _ = trace([">> semicolon(ABBREV | NOSHORTIF) with next=", tokenname(hd ts)])
    in case ts of
        (SemiColon, _) :: _ => (tl ts)
      | _ => 
          (trace(["<< semicolon(ABBREV | NOSHORTIF) with next=", tokenname(hd ts)]);
          (ts))
    end

and statement (ts,t,w) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let 
        val _ = setLoc ts
        val _ = trace([">> statement with next=", tokenname(hd ts)])
                
    in case ts of
        (If, _) :: _ =>
            let
                val (ts1,nd1) = ifStatement (ts,w)
            in
                (ts1,nd1)
            end
      | (Return, _) :: _ =>
            let
                val (ts1,nd1) = returnStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = blockStatement (ts,t)
            in
                (ts1,nd1)
            end
      | (Switch, _) :: _ =>
            let
                val (ts1,nd1) = switchStatement ts
            in
                (ts1,nd1)
            end
      | (Do, _) :: _ =>
            let
                val (ts1,nd1) = doStatement ts
            in
                (ts1,nd1)
            end
      | (While, _) :: _ =>
            let
                val (ts1,nd1) = whileStatement (ts,w)
            in
                (ts1,nd1)
            end
      | (For, _) :: _ =>
            let
                val (ts1,nd1) = forStatement (ts,w)
            in
                (ts1,nd1)
            end
      | (Let, _) :: (LeftParen, _) :: _ =>
            let
                val (ts1,nd1) = letStatement (ts,w)
            in
                (ts1,nd1)
            end
      | (With, _) :: _ =>
            let
                val (ts1,nd1) = withStatement (ts,w)
            in
                (ts1,nd1)
            end
      | (Identifier _, _) :: (Colon, _) :: _ =>
            let
                val (ts1,nd1) = labeledStatement (ts,w)
            in
                (ts1,nd1)
            end
      | (Continue, _) :: _ =>
            let
                val (ts1,nd1) = continueStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (Break, _) :: _ =>
            let
                val (ts1,nd1) = breakStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (Throw, _) :: _ =>
            let
                val (ts1,nd1) = throwStatement (ts)
                val (ts2,nd2) = (semicolon (ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (Try, _) :: _ =>
            let
                val (ts1,nd1) = tryStatement (ts)
            in
                (ts1,nd1)
            end
      | (Default, _) :: (Xml, _) :: (Namespace, _) :: (Assign, _) :: _ =>
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

and substatement (ts,w) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> substatement with next=", tokenname(hd ts)])
    in case ts of
        (SemiColon, _) :: _ =>
            (tl ts, Ast.EmptyStmt)
      | _ => 
            statement(ts,LOCAL,w)
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
        (SemiColon, _) :: ts1 => (ts1,Ast.EmptyStmt)
      | _ => error ["unknown token in emptyStatement"]
    end

(*
    BlockStatement     
        Block
*)

and blockStatement (ts,t:tau) =
    let val _ = trace([">> blockStatement with next=", tokenname(hd ts)])
    in case ts of
        (LeftBrace, _) :: _ => 
            let
                val (ts1,nd1) = block (ts,t)
            in
                trace(["<< blockStatement with next=", tokenname(hd ts)]);
                (ts1,Ast.BlockStmt nd1)
            end
      | _ => error ["unknown token in blockStatement"]
    end

(*
    LabeledStatement(w)
        Identifier  :  Substatement(w)

*)

and labeledStatement (ts,w) =
    let
    in case ts of
        (Identifier id,_) :: (Colon, _) :: _  =>
            let
                val (ts1,nd1) = substatement (tl (tl ts),w)
            in
                (ts1,Ast.LabeledStmt (id,nd1))
            end
      | _ => error ["unknown token in labeledStatement"]
    end

(*
    
ExpressionStatement    
    [lookahead !{ function, { }] ListExpression (allowLet,allowIn)

*)

and expressionStatement (ts) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> expressionStatement with next=", tokenname(hd ts)])
        val (ts1,nd1) = listExpression(ts,ALLOWIN)
    in
        trace(["<< expressionStatement with next=", tokenname(hd ts1)]);
        (ts1,Ast.ExprStmt(nd1))
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

and switchStatement (ts) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> switchStatement with next=", tokenname(hd ts)])
    in case ts of
        (Switch, _) :: (Type, _) :: (LeftParen, _) :: _ =>
            let
                val (ts1,nd1) = listExpression (tl (tl (tl ts)),ALLOWIN)
            in case ts1 of
                (Colon, _) :: _ => 
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in case ts2 of
                        (RightParen, _) :: (LeftBrace, _) :: _ =>
                            let
                                val (ts3,nd3) = typeCaseElements (tl (tl ts2))
                            in case ts3 of
                                (RightBrace, _) :: _ => (tl ts3,Ast.SwitchTypeStmt{cond=nd1,ty=nd2,cases=nd3})
                              | _ => error ["unknown token in switchStatement"]
                            end
                      | _ => error ["unknown token in switchStatement"]
                    end
              | _ => error ["unknown token in switchStatement"]
            end
      | (Switch, _) :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
            in case ts1 of
                ((LeftBrace, _) :: _) =>
                    let
                        val (ts2,nd2) = caseElements (tl ts1)
                    in case ts2 of
                        (RightBrace, _) :: _ => (tl ts2,Ast.SwitchStmt{mode=NONE,cond=nd1,cases=nd2,labels=[]})
                      | _ => error ["unknown token in switchStatement"]
                    end
              | _ => error ["unknown token in switchStatement"]
            end
      | _ => error ["unknown token in switchStatement"]
    end

and isDefaultCase (x) =
    case x of 
        NONE => true
      | _ => false

and caseElements (ts) : ((TOKEN * Ast.LOC) list * Ast.CASE list) =
    let val _ = trace([">> caseElements with next=", tokenname(hd ts)])
    in case ts of
        (RightBrace, _) :: _ =>
            (tl ts,[])
      | ((Case | Default), _) :: _ =>
            let
                val (ts1,nd1) = caseLabel (ts,false)
                val (ts2,nd2) = caseElementsPrefix (ts1,isDefaultCase nd1)
            in case nd2 of
                [] =>
                    let
                    in
                        (ts2,{label=nd1,
                              body=Ast.Block {pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts2},
                              inits=NONE}::[])
                    end
              | first :: follows =>
                    let
                    in
                        (ts2,{label=nd1,body=(#body first),inits=NONE} :: follows)
                    end
            end
      | _ => error ["unknown token in caseElements"]
    end

and caseElementsPrefix (ts,has_default) : ((TOKEN * Ast.LOC) list * Ast.CASE list) =
    let val _ = trace([">> caseElementsPrefix with next=", tokenname(hd ts)])
    in case ts of
        (RightBrace, _) :: _ =>
            (ts,[])
      | ((Case | Default), _) :: _ =>
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

                        (ts2,{label=NONE,body=Ast.Block{pragmas=[],defns=[],body=[],
                                                        head=NONE,loc=locOf ts2},inits=NONE} ::
                            ({label=nd1,body=Ast.Block{pragmas=[],defns=[],body=[],
                                                       head=NONE,loc=locOf ts2},inits=NONE}::[]))
                    end
              | first :: follows =>
                    let
                    in
                        (ts2,{label=NONE,body=Ast.Block{pragmas=[],defns=[],body=[],
                                                        head=NONE,loc=locOf ts2},inits=NONE} ::
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
                        val body = Ast.Block{pragmas=(p1@p2),defns=(d1@d2),body=(s1@s2),head=NONE,loc=locOf ts2}
                    in
                        (ts2,{label=NONE,body=body,inits=NONE}::follows)
                    end
            end
    end

and caseLabel (ts,has_default) : ((TOKEN * Ast.LOC) list * Ast.EXPR option) =
    let val _ = trace([">> caseLabel with next=", tokenname(hd ts)])
    in case (ts,has_default) of
        ((Case, _) :: _,_) =>
            let
                val (ts1,nd1) = listExpression (tl ts,ALLOWIN)
            in case ts1 of
                (Colon, _) :: _ => (tl ts1,SOME nd1)
              | _ => error ["unknown token in caseLabel"]
            end
      | ((Default, _) :: _,false) =>
            let
            in case tl ts of
                (Colon, _) :: _ => (tl (tl ts),NONE)
              | _ => error ["unknown token in caseLabel"]
            end
      | ((Default, _) :: _,true) =>
            (error(["redundant default switch case"]); error ["unknown token in caseLabel"])
      | _ => error ["unknown token in caseLabel"]
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

and typeCaseBinding (ts) : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) =
    let val _ = trace([">> caseCaseBinding with next=", tokenname(hd ts)])
    in case ts of
        (LeftParen, _) :: _ =>
            let
                val (ts1,(p,t)) = typedIdentifier (tl ts,NONE)
            in case ts1 of
                (Assign, _) :: _ =>
                    let
                        val (ts2,nd2) = variableInitialisation(ts1,ALLOWLIST,ALLOWIN)
                    in case ts2 of
                        (RightParen, _) :: _ =>
                            (tl ts2, desugarPattern (locOf ts) p t (SOME nd2) 0)
                      | _ => error ["unknown token in typeCaseBinding"]
                    end
              | _ => 
                    let
                    in case ts1 of
                        (RightParen, _) :: _ =>
                            (tl ts1, desugarPattern (locOf ts) p t NONE 0)
                      | _ => error ["unknown token in typeCaseBinding"]
                    end
            end
      | _ => 
            error ["unknown token in typeCaseBinding"]
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
        Ast.SpecialType Ast.Any => true
      | _ => false

and typeCaseElements (ts) : ((TOKEN * Ast.LOC) list * Ast.CATCH_CLAUSE list) =
    let val _ = trace([">> typeCaseElements with next=", tokenname(hd ts)])
        fun typeCaseElements' (ts,has_default) : ((TOKEN * Ast.LOC) list * Ast.CATCH_CLAUSE list) =
            let val _ = trace([">> typeCaseElements' with next=", tokenname(hd ts)])
            in case ts of
                ((Case | Default), _) :: _ => 
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
    : ((TOKEN * Ast.LOC) list * Ast.CATCH_CLAUSE) =
    let val _ = trace([">> typeCaseElement with next=", tokenname(hd ts)])
    in case (ts,has_default) of
           ((Case, _) :: (LeftParen, _) :: _,_) =>
           let
               val (ts1,(temp,{pattern,ty})) = parameter (tl (tl ts)) 0
               val (b,i) = desugarPattern (locOf ts) pattern ty (SOME (Ast.GetParam 0)) 0
           in 
               case ts1 of
                   (RightParen, _) :: _ =>
                   let
                       val (ts2,nd2) = block (tl ts1,LOCAL)
                   in
                       (ts2,{bindings=((temp::b),i),ty=ty,block=nd2,fixtures=NONE,inits=NONE})
                   end                    
                 | _ => error ["unknown token in typeCaseElement"]
           end
         | ((Default, _) :: _,false) =>
           let
               val (ts1,nd1) = block (tl ts,LOCAL)
           in
               trace(["<< typeCaseElement with next=", tokenname(hd ts1)]);
               (ts1,{bindings=([],[]),ty=Ast.SpecialType Ast.Any,block=nd1,fixtures=NONE,inits=NONE})
           end
         | ((Default, _) :: _,true) =>
           (error(["redundant default switch type case"]); error ["unknown token in typeCaseElement"])
         | _ => error ["unknown token in typeCaseElement"]
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
        (If, _) :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1,ABBREV)
            in case ts2 of
                (Else, _) :: _ =>
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
          | _ => error ["unknown token in ifStatement"]
    end
  | ifStatement (ts,FULL) =
    let val _ = trace([">> ifStatement(FULL) with next=", tokenname(hd ts)])
    in case ts of
        (If, _) :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1,FULL)
            in case ts2 of
                (Else, _) :: _ =>
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
          | _ => error ["unknown token in ifStatement"]
    end
  | ifStatement (ts,SHORTIF) =
    let val _ = trace([">> ifStatement(SHORTIF) with next=", tokenname(hd ts)])
    in case ts of
        (If, _) :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1,NOSHORTIF)
            in case ts2 of
                (Else, _) :: _ =>
                    let
                        val (ts3,nd3) = substatement(tl ts2,SHORTIF)
                    in
                        (ts3,Ast.IfStmt {cnd=nd1,thn=nd2,els=nd3})
                    end
              | _ => 
                    error ["unknown token in ifStatement"]
            end
          | _ => error ["unknown token in ifStatement"]
    end

(*
    DoStatement    
        do Substatement(abbrev) while ParenListExpression
*)

and doStatement (ts) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> doStatement with next=", tokenname(hd ts)])
    in case ts of
        (Do, _) :: _ =>
            let
                val (ts1,nd1) = substatement(tl ts, ABBREV)
            in case ts1 of
                (While, _) :: _ =>
                    let
                        val (ts2,nd2) = parenListExpression (tl ts1)
                    in
                        (ts2,Ast.DoWhileStmt {body=nd1,cond=nd2,fixtures=NONE,labels=[]})
                    end
              | _ => error ["unknown token in doStatement"]
            end
      | _ => error ["unknown token in doStatement"]
    end

(*
    WhileStatement(w)    
        while ParenListExpression Substatement(w)
*)

and whileStatement (ts,w) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> whileStatement with next=", tokenname(hd ts)])
    in case ts of
        (While, _) :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1, w)
            in
                (ts2,Ast.WhileStmt {cond=nd1,fixtures=NONE,body=nd2,labels=[]})
            end
      | _ => error ["unknown token in whileStatement"]
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

(* note: even though for-each-in and for-in use ForInBinding, we can get there by two different
         paths. With for-in we parse for ForInitializer, which is more general and translate
         when we see 'in'. With for-each-in, we could parse immediately for ForInBinding but
         with desugaring it seems simpler to use the same parsing code. Positive and negative 
         cases should produce the same error or ast result
*)

(* todo: code reuse opps here *)

and forStatement (ts,w) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> forStatement with next=", tokenname(hd ts)])
        val (ts,isEach) = case ts of (For, _) :: (Each, _) :: _ => (tl (tl (tl ts)),true) | _ => (tl (tl ts),false)
        val (ts1,defn,inits) = forInitialiser ts
    in case ts1 of
        (SemiColon, _) :: _ =>
            let
                val (ts2,nd2) = optionalExpression (tl ts1)
                val (ts3,nd3) = optionalExpression (ts2)
            in case ts3 of
                (RightParen, _) :: _ =>
                    let
                        val (ts4,nd4) = substatement (tl ts3,w)
                    in
                        (ts4,Ast.ForStmt{ 
                                    defn=defn,
                                    init=inits,
                                    cond=nd2,
                                    update=nd3,
                                    labels=[],
                                    fixtures=NONE,
                                    body=nd4})
                    end
              | _ => error ["unknown token in forStatement"]
            end
      | (In, _) :: _ =>
            let
                fun desugarForInInit (init:Ast.EXPR) : Ast.EXPR =
                    (* ISSUE: there might be an opportunity to share 
                              code here with assignmentExpression *)
                    let
                        fun isInitStep (b:Ast.INIT_STEP) : bool =
                            case b of 
                               Ast.InitStep _ => true
                             | _ => false
                
                        fun makeSetExpr (Ast.AssignStep (lhs,rhs)) 
                                = Ast.SetExpr (Ast.Assign,lhs,rhs)
                          | makeSetExpr (Ast.InitStep _) 
                                = LogErr.internalError ["makeSetExpr: shouldn't get here"]
                
                        val p = patternFromListExpr init
                        val (binds,inits) = desugarPattern (locOf ts) p 
                                                           (Ast.SpecialType Ast.Any) 
                                                           (SOME (Ast.GetParam 0)) 0  
                                                                          (* type is meaningless *)
                        val (inits,assigns) = List.partition isInitStep inits
                                                         (* separate init steps and assign steps *)
                        val sets = map makeSetExpr assigns
                        val paramBind = Ast.Binding {ident=Ast.ParamIdent 0,
                                                     ty=Ast.SpecialType Ast.Any}
                    in
                        Ast.LetExpr {defs=(paramBind::binds,inits),
                                     body=Ast.ListExpr sets,
                                     head=NONE}
                    end

                val len = case defn of SOME {bindings=(b,i),...} => length b | NONE => 0
                val next = 
                        if len = 0 (* convert inits to pattern *)
                            then case inits of 
                                Ast.ExprStmt e::[] => 
                                Ast.ExprStmt (desugarForInInit e)
                              | _ => LogErr.internalError ["invalid forIn inits 1"]
                            else case inits of
                                Ast.InitStmt i :: [] => 
                                Ast.InitStmt i (* already desugared *)
                              | _ => LogErr.internalError ["invalid forIn inits 2"]
                val (ts2,nd2) = listExpression (tl ts1,ALLOWIN)
            in case ts2 of
                (RightParen, _) :: _ =>
                    let
                        val (ts3,nd3) = substatement (tl ts2,w)
                    in
                            (ts3,Ast.ForInStmt{ 
                                     isEach=isEach,
                                     defn=defn,
                                     obj=nd2,
                                     labels=[],
                                     fixtures=NONE,
                                     next=next,
                                     body=nd3 })
                    end
              | _ => error ["unknown token in forStatement"]
            end
      | _ => error ["unknown token in forStatement"]
    end

(*
    for ( var x = 10; ... ) ...

    
*)

and forInitialiser (ts) 
    : ((TOKEN * Ast.LOC) list * Ast.VAR_DEFN option * Ast.STMT list) =
    let val _ = trace([">> forInitialiser with next=", tokenname(hd ts)])
    in case ts of
        ((Var | Let | Const), _) :: _ =>
            let
                val (ts1,{defns,body,...}) = variableDefinition (ts,NONE,
                                                    false,false,NOIN,LOCAL)
            in case (defns) of
                (Ast.VariableDefn vd :: []) =>
                    (trace(["<< forInitialiser with next=", tokenname(hd ts1)]);
                    (ts1,SOME vd,body))
              | _ => error ["unknown token in forInitialiser"]
            end
      | (SemiColon, _) :: _ =>
            let
            in
                trace(["<< forInitialiser with next=", tokenname(hd ts)]);
                (ts,NONE,[Ast.ExprStmt (Ast.ListExpr [])])
            end
      | _ =>
            let
                val (ts1,nd1) = listExpression (ts,NOIN)
            in
                trace ["<< forInitialiser with next=", tokenname(hd ts1)];
                (ts1,NONE,[Ast.ExprStmt nd1])           
            end
    end

and optionalExpression (ts) : ((TOKEN * Ast.LOC) list * Ast.EXPR) =
    let val _ = trace([">> optionalExpression with next=", tokenname(hd ts)])
    in case ts of
        (SemiColon, _) :: _ =>
            let
            in
                (tl ts,Ast.ListExpr [])
            end
      | (RightParen, _) :: _ =>
            let
            in
                (ts,Ast.ListExpr [])
            end
      | _ => 
            let
                val (ts1,nd1) = listExpression (ts,NOIN)
            in case ts1 of
                (SemiColon, _) :: _ =>
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
    : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) =
    let val _ = trace([">> forInBinding with next=", tokenname(hd ts)])
    in case ts of
        ((Var | Let | Const), _) :: _ =>
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
                val (b,i) = desugarPattern (locOf ts) nd1 (Ast.SpecialType Ast.Any) (SOME (Ast.GetTemp 0)) 0
            in 
                trace ["<< forInitialiser with next=", tokenname(hd ts1)];
                (ts1,(b,i))
            end
    end

(*
    LetStatement(w)    
        let  (  LetBindingList  )  Substatement(w)
*)

and letStatement (ts,w) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> letStatement with next=", tokenname(hd ts)])
    in case ts of
        (Let, _) :: (LeftParen, _) :: _ => 
            let
                val (ts1,nd1) = letBindingList (tl (tl ts))
                val defn = Ast.VariableDefn {kind=Ast.LetVar,
                                             ns=SOME (Ast.LiteralExpr (Ast.LiteralNamespace Name.noNS)),
                                             static=false,
                                             prototype=false,
                                             bindings=nd1}
            in case ts1 of
                (RightParen, _) :: _ =>
                    let
                        val (ts2,nd2) = substatement(tl ts1, w)
                    in
                        trace(["<< letStatement with next=",tokenname(hd(ts2))]);
                        (ts2,Ast.LetStmt (Ast.Block {pragmas=[],defns=[defn],head=NONE,body=[nd2],loc=locOf ts1}))
                    end
               |    _ => error ["unknown token in letStatement"]
            end
      | _ => error ["unknown token in letStatement"]
    end

(*
    WithStatement(w)
        with  (  ListExpression(allowIn)  )  Substatement(w)
        with  (  ListExpression(allowIn)  :  TypeExpression  )  Substatement(w)
*)

and withStatement (ts,w) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> withStatement with next=", tokenname(hd ts)])
    in case ts of
        (With, _) :: (LeftParen, _) :: _ => 
            let
                val (ts1,nd1) = listExpression (tl (tl ts),ALLOWIN)
            in case ts1 of
                (RightParen, _) :: _ =>
                    let
                        val (ts2,nd2) = substatement(tl ts1, w)
                    in
                        (trace(["<< withStatement with next=",tokenname(hd(ts2))]);
                        (ts2,Ast.WithStmt {obj=nd1,ty=Ast.SpecialType Ast.Any,body=nd2}))
                    end
              | (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in case ts2 of
                        (RightParen, _) :: _ =>
                            let
                                val (ts3,nd3) = substatement(tl ts2, w)
                            in
                                (trace(["<< withStatement with next=",tokenname(hd(ts3))]);
                                (ts3,Ast.WithStmt {obj=nd1,ty=nd2,body=nd3}))
                            end
                       |    _ => error ["unknown token in withStatement"]
                    end
               |    _ => error ["unknown token in withStatement"]
            end
      | _ => error ["unknown token in withStatement"]
    end

(*
    ContinueStatement    
        continue
        continue [no line break] Identifier
*)

and continueStatement ts: ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let
    in case ts of
        (Continue, _) :: ((SemiColon | RightBrace), _) :: _ => 
            (tl ts,Ast.ContinueStmt NONE)
      | (Continue, _) :: _ =>
            if newline(tl ts) then 
                (tl ts,Ast.ContinueStmt NONE)
            else
                let
                    val (ts1,nd1) = identifier (tl ts)
                in
                    (ts1,Ast.ContinueStmt (SOME nd1))
                end
      | _ => error ["unknown token in continueStatement"]
    end

(*
    BreakStatement    
        break
        break [no line break] Identifier
*)

and breakStatement ts: ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> breakStatement with next=", tokenname(hd ts)])
    in case ts of
        (Break, _) :: ((SemiColon | RightBrace), _) :: _ => 
            (tl ts,Ast.BreakStmt NONE)
      | (Break, _) :: _ =>
            if newline(tl ts) then 
                (tl ts,Ast.BreakStmt NONE)
            else
                let
                    val (ts1,nd1) = identifier (tl ts)
                in
                    trace(["<< breakStatement with next=", tokenname(hd ts)]);
                    (ts1,Ast.BreakStmt (SOME nd1))
                end
      | _ => error ["unknown token in breakStatement"]
    end

(*
    ReturnStatement    
        return
        return [no line break] ListExpressio(nallowIn)
*)

and returnStatement ts =
    let
    in case ts of
        (Return, _) :: ((SemiColon | RightBrace), _) :: _ => 
            (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
      | (Return, _) :: _ =>
            if newline(tl ts) then 
                (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
            else 
                let
                    val (ts1,nd1) = listExpression(tl ts, ALLOWIN)
                in
                    (ts1,Ast.ReturnStmt nd1)
                end
      | _ => error ["unknown token in returnStatement"]
    end

(*
    ThrowStatement     
        throw  ListExpression(allowIn)
*)

and throwStatement ts =
    let
    in case ts of
        (Throw, _) :: _ =>
            let
                val (ts1,nd1) = listExpression(tl ts, ALLOWIN)
            in
                (ts1,Ast.ThrowStmt nd1)
            end
      | _ => error ["unknown token in throwStatement"]
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

and tryStatement (ts) : ((TOKEN * Ast.LOC) list * Ast.STMT) =
    let val _ = trace([">> tryStatement with next=", tokenname(hd ts)])
    in case ts of
        (Try, _) :: _ =>
            let
                val (ts1,nd1) = block(tl ts,LOCAL)
            in case ts1 of
                (Finally, _) :: _ =>
                    let
                        val (ts2,nd2) = block (tl ts1,LOCAL)
                    in
                        (ts2,Ast.TryStmt {block=nd1,catches=[],finally=SOME nd2})
                    end
              | _ => 
                    let
                        val (ts2,nd2) = catchClauses ts1
                    in case ts2 of
                        (Finally, _) :: _ =>
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
      | _ => error ["unknown token in tryStatement"]
    end

and catchClauses (ts)
    : (TOKEN * Ast.LOC) list * {bindings:Ast.BINDINGS, ty:Ast.TYPE_EXPR, fixtures:Ast.FIXTURES option, inits:Ast.INITS option, block:Ast.BLOCK} list=
    let val _ = trace([">> catchClauses with next=", tokenname(hd ts)])
        val (ts1,nd1) = catchClause ts
    in case ts1 of
        (Catch, _) :: _ =>
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

and catchClause (ts) 
    : (TOKEN * Ast.LOC) list * {bindings:Ast.BINDINGS, ty:Ast.TYPE_EXPR, fixtures:Ast.FIXTURES option, inits:Ast.INITS option, block:Ast.BLOCK}=
    let val _ = trace([">> catchClause with next=", tokenname(hd ts)])
    in case ts of
        (Catch, _) :: (LeftParen, _) :: _ =>
            let
                val (ts1,(temp,{pattern,ty})) = parameter (tl (tl ts)) 0
                val (b,i) = desugarPattern (locOf ts) pattern ty (SOME (Ast.GetParam 0)) 0
            in case ts1 of
                (RightParen, _) :: _ =>
                    let
                        val (ts2,nd2) = block (tl ts1,LOCAL)
                    in
                        (ts2,{bindings=((temp::b),i),ty=ty,block=nd2,fixtures=NONE,inits=NONE})
                    end
              | _ => error ["unknown token in catchClause"]
            end
      | _ => error ["unknown token in catchClause"]
    end

(*
    DefaultXMLNamespaceStatement    
        default  xml  namespace = NonAssignmentExpression(allowList, allowIn)
*)

and defaultXmlNamespaceStatement (ts) =
    let val _ = trace([">> defaultXmlNamespaceStatement with next=", tokenname(hd ts)])
    in case ts of
        (Default, _) :: (Xml, _) :: (Namespace, _) :: (Assign, _) :: _ =>
            let
                val (ts1,nd1) = nonAssignmentExpression ((tl (tl (tl (tl ts)))),ALLOWLIST,ALLOWIN)
            in
                (ts1,Ast.DXNStmt {expr=nd1})
            end
      | _ => error ["unknown token in defaultXmlNamespaceStatement"]
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

and directives (ts,t) : ((TOKEN * Ast.LOC) list * Ast.DIRECTIVES) =
    let 
        val _ = setLoc ts
        val _ = trace([">> directives with next=", tokenname(hd ts)])
    in case ts of
        ((RightBrace | Eof), _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
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

and directivesPrefix (ts,t:tau) : ((TOKEN * Ast.LOC) list * Ast.DIRECTIVES) =
    let 
        val _ = setLoc ts
        val _ = trace([">> directivesPrefix with next=", tokenname(hd ts)])
        fun directivesPrefix' (ts,t) : ((TOKEN * Ast.LOC) list * Ast.DIRECTIVES) =
            let val _ = trace([">> directivesPrefix' with next=", tokenname(hd ts)])
            in case ts of
                ((RightBrace | Eof), _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
              | _ => 
                    let
                        val (ts1,{pragmas=p1,defns=d1,body=s1,...}) = directive (ts,t,FULL)
                        val (ts2,{pragmas=p2,defns=d2,body=s2,...}) = directivesPrefix' (ts1,t)
                    in
                        trace(["<< directivesPrefix' with next=", tokenname(hd ts2)]);
                        (ts2,{pragmas=(p1@p2),defns=(d1@d2),body=(s1@s2),head=NONE,loc=locOf ts})
                    end
            end
    in case ts of
        ((RightBrace | Eof), _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
      | ((Use | Import), _) :: _ => 
            let
                val (ts1,nd1) = pragmas ts
                val (ts2,{pragmas=p2,defns=d2,body=s2,...}) = directivesPrefix' (ts1,t)
            in
                trace(["<< directivesPrefix with next=", tokenname(hd ts2)]);
                (ts2, {pragmas=nd1,defns=d2,body=s2,head=NONE,loc=locOf ts})
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


and directive (ts,t:tau,w:omega) : ((TOKEN * Ast.LOC) list * Ast.DIRECTIVES) =
    let 
        val _ = setLoc ts
        val _ = trace([">> directive with next=", tokenname(hd ts)])
    in case ts of
        (SemiColon, _) :: _ => 
            let
                val (ts1,nd1) = emptyStatement ts
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE,loc=locOf ts})
            end
      | (Let, _) :: (LeftParen, _) :: _  => (* dispatch let statement before let var *)
            let
                val (ts1,nd1) = statement (ts,LOCAL,w)
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE,loc=locOf ts})
            end
      | (Prototype, _) :: ((Dot | Assign), _) :: _  => (* dispatch non attr prototype ref *)
            let
                val (ts1,nd1) = statement (ts,LOCAL,w)
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE,loc=locOf ts})
            end
      | ((Let | Const | Var | Function | Class | Interface | Namespace | Type), _) :: _ => 
            let
                val (ts1,nd1) = annotatableDirective (ts,defaultAttrs,t,w)
            in
                (ts1,nd1)
            end
      | ((Dynamic | Final | Override | Native | Prototype | Static ), _) :: _ =>
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
      | ((Identifier _ | Private | Public | Protected | Internal | Intrinsic), _) :: 
           ((Dynamic | Final | Native | Override | Prototype | Static | 
             Var | Let | Const | Function | Class | Interface | Namespace | Type), _) :: _ =>
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
                val (ts1,nd1) = statement (ts,t,w)
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE,loc=locOf ts})
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

and annotatableDirective (ts, attrs:ATTRS, GLOBAL, w) : ((TOKEN * Ast.LOC) list * Ast.DIRECTIVES)  =
    let val _ = trace([">> annotatableDirective GLOBAL with next=", tokenname(hd ts)])
        val {ns,prototype,static,...} = attrs
    in case ts of
        (Let, _) :: (Function, _) :: _ =>
            functionDefinition (ts,attrs,GLOBAL)
      | (Const, _) :: (Function, _) :: _ =>
            functionDefinition (ts,attrs,GLOBAL)
      | (Function, _) :: _ =>
            functionDefinition (ts,attrs,GLOBAL)
      | (Class, _) :: _ =>
            classDefinition (ts,attrs)
      | (Interface, _) :: _ =>
            interfaceDefinition (ts,attrs)
      | (Namespace, _) :: _ =>
            let
                val (ts1,nd1) = namespaceDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (Type, _) :: _ =>
            let
                val (ts1,nd1) = typeDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | ((Let | Var | Const), _) :: _ => 
            let
                val (ts1,nd1) = variableDefinition (ts,ns,prototype,static,ALLOWIN,GLOBAL)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | _ => 
            error ["unknown token in annotatableDirective"]
    end
  | annotatableDirective (ts,attrs,INTERFACE,w) : ((TOKEN * Ast.LOC) list * Ast.DIRECTIVES)  =
    let val _ = trace([">> annotatableDirective INTERFACE with next=", tokenname(hd ts)])
    in case ts of
        (Function, _) :: _ =>
            let
                val (ts1,nd1) = functionDeclaration (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (Type, _) :: _ =>
            let
                val (ts1,nd1) = typeDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | _ => 
            error ["unknown token in annotatableDirective"]
    end
  | annotatableDirective (ts,attrs,t,w) : ((TOKEN * Ast.LOC) list * Ast.DIRECTIVES)  =
    let val _ = trace([">> annotatableDirective omega with next=", tokenname(hd ts)])
        val {ns,prototype,static,...} = attrs
    in case ts of
        (Let, _) :: (Function, _) :: _ =>
            functionDefinition (ts,attrs,t)
      | (Function, _) :: _ =>
            functionDefinition (ts,attrs,t)
      | (Namespace, _) :: _ =>
            let
                val (ts1,nd1) = namespaceDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | (Type, _) :: _ =>
            let
                val (ts1,nd1) = typeDefinition (ts,attrs)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | ((Let | Var | Const), _) :: _ => 
            let
                val (ts1,nd1) = variableDefinition (ts,ns,prototype,static,ALLOWIN,t)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
      | _ => 
            error ["unknown token in annotatableDirective"]
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

and attributes (ts, attrs:ATTRS, t) 
    : ((TOKEN * Ast.LOC) list * ATTRS) =
    let 
        val _ = setLoc ts
        val _ = trace([">> attributes with next=", tokenname(hd ts)])
    in case ts of
        ((Dynamic | Final | Native | Override | Prototype | Static | 
          Private | Protected | Public | Internal | Intrinsic | Identifier _), _) :: _ =>
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

and attribute (ts, attrs:ATTRS, GLOBAL) 
    : ((TOKEN * Ast.LOC) list * ATTRS) =
    let val _ = trace([">> attribute with next=", tokenname(hd ts)])
        val {ns,override,static,final,dynamic,prototype,native,rest} = attrs
    in case (ts) of
        (Dynamic, _) :: _ =>
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
      | (Final, _) :: _ =>
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
      | (Native, _) :: _ =>
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
      | ((Override | Static | Prototype ), _) :: _ => 
        error(["invalid attribute in global context"])
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
        (Final, _) :: _ =>
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
      | (Native, _) :: _ =>
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
      | (Override, _) :: _ =>
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
      | (Prototype, _) :: _ =>
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
      | (Static, _) :: _ =>
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
      | (Dynamic, _) :: _ => 
            (error(["invalid attribute in class context"]);error ["unknown token in attribute"])
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
               ((Dynamic | Final | Native | Override | Prototype | Static | 
                 Private | Protected | Public | Internal | Intrinsic | Identifier _), _) :: _ =>
               error ["attributes not allowed on a interface methods"]
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
               ((Dynamic | Final | Native | Override | Prototype | Static | 
                 Private | Protected | Public | Internal | Intrinsic | Identifier _), _) :: _ =>
                (error(["attributes not allowed on local definitions"]);
                 error ["unknown token in attribute"])
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

and namespaceAttribute (ts,GLOBAL) 
    : ((TOKEN * Ast.LOC) list * Ast.EXPR option) =
    let val _ = trace([">> namespaceAttribute with next=", tokenname(hd ts)])
    in case ts of
        ((Internal, _) :: _ | (Intrinsic, _) :: _ | (Public, _) :: _) => 
            let
                val (ts1,nd1) = reservedNamespace ts
            in
                (ts1, SOME (Ast.LiteralExpr (Ast.LiteralNamespace nd1)))
            end
      | (Identifier s, _) :: _ =>
            let
            in
                (tl ts, SOME (Ast.LexicalRef {ident=Ast.Identifier {ident=s, openNamespaces=[]},
                                              loc=locOf ts}))
            end
      | _ => error ["unknown token in namespaceAttribute"]
    end
  | namespaceAttribute (ts,CLASS) =
    let val _ = trace([">> namespaceAttribute with next=", tokenname(hd ts)])
    in case ts of
        ((Internal, _) :: _ | (Intrinsic, _) :: _ | (Private, _) :: _ | (Protected, _) :: _ | (Public, _) :: _) => 
            let
                val (ts1,nd1) = reservedNamespace ts
            in
                (ts1, SOME (Ast.LiteralExpr (Ast.LiteralNamespace nd1)))
            end
      | (Identifier s, _) :: _ =>
            let
            in
                (tl ts, SOME (Ast.LexicalRef {ident=Ast.Identifier{ident=s,openNamespaces=[]},
                                              loc=locOf ts}))
            end
      | _ => error ["unknown token in namespaceAttribute"]
    end
  | namespaceAttribute (ts,_) =
    let val _ = trace([">> namespaceAttribute with next=", tokenname(hd ts)])
    in case ts of
        _ => error ["unknown token in namespaceAttribute"]
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

and variableDefinition (ts,ns:Ast.EXPR option,prototype,static,b,t) : ((TOKEN * Ast.LOC) list * Ast.DIRECTIVES) =
    let val _ = trace([">> variableDefinition with next=", tokenname(hd ts)])
        val (ts1,nd1) = variableDefinitionKind(ts)
        val (ts2,(b,i)) = variableBindingList (ts1,ALLOWLIST,b)

        fun isTempBinding (b:Ast.BINDING) : bool =
            case b of 
               Ast.Binding {ident=(Ast.TempIdent _|Ast.ParamIdent _),...} => true
             | _ => false

        fun isTempInit (b:Ast.INIT_STEP) : bool =
            case b of 
               Ast.InitStep ((Ast.TempIdent _|Ast.ParamIdent _),_) => true
             | _ => false

        val (tempBinds,propBinds) = List.partition isTempBinding b
        val (tempInits,propInits) = List.partition isTempInit i

        val initStmts = [Ast.InitStmt {kind=nd1,ns=ns,prototype=prototype,static=static,temps=(tempBinds,tempInits),inits=(propInits)}]

    in
        (ts2,{pragmas=[],
              defns=[Ast.VariableDefn {kind=nd1,
                                       bindings=(propBinds,[]),
                                       ns=ns,
                                       prototype=prototype,
                                       static=static}],
              body=initStmts,
              head=NONE,
              loc=locOf ts})
    end

and variableDefinitionKind (ts) =
    let
    in case ts of
        (Const, _) :: _ => 
            (tl ts,Ast.Const)
      | (Var, _) :: _ => 
            (tl ts,Ast.Var)
      | (Let, _) :: (Const, _) :: _ => 
            (tl (tl ts), Ast.LetConst)
      | (Let, _) :: _ => 
            (tl ts, Ast.LetVar)
      | _ => error ["unknown token in variableDefinitionKind"]
    end

and variableBindingList (ts,a,b) : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) = 
    let val _ = trace([">> variableBindingList with next=", tokenname(hd ts)])
        fun variableBindingList' (ts,a,b) : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) =
            let val _ = trace([">> variableBindingList' with next=", tokenname(hd ts)])
            in case ts of
                (Comma, _) :: _ =>
                    let
                        val (ts1,(d1,s1)) = variableBinding(tl ts,a,b)
                        val (ts2,(d2,s2)) = variableBindingList'(ts1,a,b)
                    in case (ts2,b) of
                        ((In,_)::_, NOIN) => error ["too many for-in bindings"]
                      | _ =>
                            (trace(["<< variableBindingList' with next=", tokenname(hd ts2)]);
                            (ts2,(d1@d2,s1@s2)))
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

    Desugar patterns, types and initialisers into binding and init steps

*)

and variableBinding (ts,a,beta) : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) = 
    let val _ = trace([">> variableBinding with next=", tokenname(hd ts)])
        val (ts1,(p,t)) = typedPattern (ts,a,beta)  (* parse the more general syntax *)
    in case (ts1,p,beta) of
            ((Assign, _) :: _,_,_) =>
                let
                    val (ts2,nd2) = variableInitialisation (ts1,a,beta)
                in case (ts2,beta) of
                    ((In, _) :: _, NOIN) =>
                        let
                           val temp = Ast.Binding {ident=Ast.ParamIdent 0,ty=t}
                           val init = Ast.InitStep (Ast.ParamIdent 0,nd2)   (* ISSUE: should this value be desugared for side effects *)
                           val (b,i) = desugarPattern (locOf ts) p t (SOME (Ast.GetParam 0)) 1
                        in
                           trace(["<< variableBinding with next=", tokenname(hd ts2)]);
                           (ts2, (temp::b,init::i))
                        end
                  | _ =>
                        let
                           val (b,i) = desugarPattern (locOf ts) p t (SOME nd2) 0
                        in
                           trace(["<< variableBinding with next=", tokenname(hd ts2)]);
                           (ts2, (b,i))
                        end
                end
          | ((In, _) :: _,_,NOIN) => (* okay, we are in a for-in or for-each-in binding *)
                let
                    val temp = Ast.Binding {ident=Ast.ParamIdent 0,ty=t}
                    val init = Ast.InitStep (Ast.ParamIdent 0, Ast.LiteralExpr Ast.LiteralUndefined)  (* for symmetry with above for-in case *)
                    val (b,i) = desugarPattern (locOf ts) p t (SOME (Ast.GetParam 0)) 1
                in
                    trace(["<< variableBinding IN with next=", tokenname(hd ts1)]);
                    (ts1, (temp::b,init::i))
                end
          | (_,IdentifierPattern _,_) =>   (* check for the more specific syntax allowed
                                                  when there is no init *)
                let
                    val (b,i) = desugarPattern (locOf ts) p t NONE 0
                in
                    trace(["<< variableBinding with next=", tokenname(hd ts1)]);
                    (ts1, (b,i))
                end
          | (_,_,_) => (error(["destructuring pattern without initialiser"]); error ["unknown token in variableBinding"])
    end

and variableInitialisation (ts,a,b) : ((TOKEN * Ast.LOC) list * Ast.EXPR) =
    let val _ = trace([">> variableInitialisation with next=", tokenname(hd ts)])
    in case ts of
        (Assign, _) :: _ =>
            let
                val (ts1,nd1) = assignmentExpression (tl ts,a,b)
            in
                (ts1,nd1)
            end
      | _ => error ["unknown token in variableInitialisation"]
    end

(*
    FunctionDeclaration    
        function  Identifier  FunctionSignature
*)

and functionDeclaration (ts,attrs) =
    let val _ = trace([">> functionDeclaration with next=", tokenname(hd ts)])
        val {ns,final,native,override,prototype,static,...} = attrs
    in case ts of
        (Function, _) :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
                val (ts2,nd2) = functionSignature (ts1)
            in
                (ts2,{pragmas=[],
                      defns=[Ast.FunctionDefn {kind=Ast.Var,
                                               ns=ns,
                                               final=final,
                                               override=override,
                                               prototype=prototype,
                                               static=static,
                                               func=Ast.Func {name={ident=nd1,kind=Ast.Ordinary},
                                                              fsig=nd2, 
                                                              param=([],[]),
                                                              defaults=[],
                                                              ty=functionTypeFromSignature nd2,
                                                              isNative=native,
                                                              block=Ast.Block {pragmas=[],
                                                                               defns=[],
                                                                               body=[],
                                                                               head=NONE,
                                                                               loc=locOf ts}}}],
                      body=[],
                      head=NONE,
                      loc=locOf ts})
            end
      | _ => error ["execting function declaration"]
    end

(*
    FunctionDefinition(class)    
        function  ClassName  ConstructorSignature  FunctionBody
        function  OperatorName  FunctionSignature  FunctionBody
        function  FunctionName  FunctionSignature  FunctionBody
        
    FunctionDefinition(t)    
        function  FunctionName  FunctionSignature  FunctionBody
        let  function  FunctionName  FunctionSignature  FunctionBody
        const  function  FunctionName  FunctionSignature  FunctionBody
        
    FunctionName    
        Identifier
		get  Identifier
		set  Identifier
		get  *
		set  *
		call  *
		has  *

    OperatorName [one of]    
        +   -   ~   *   /   %   <   >   <=   >=   ==   <<   >>   >>>   &   |   ===   !=   !==
*)

and isCurrentClass ({ident,kind}) = if (ident=(!currentClassName)) andalso (kind=Ast.Ordinary) 
                                    then true 
                                    else false

and functionDefinition (ts,attrs:ATTRS,CLASS) =
    let val _ = trace([">> functionDefinition(CLASS) with next=", tokenname(hd ts)])
        val {ns,final,override,prototype,static,...} = attrs
        val (ts1,nd1) = functionKind (ts)
        val (ts2,nd2) = functionName (ts1)
    in case (nd1,isCurrentClass(nd2),prototype) of
        (Ast.Var,true,false) =>
            let
                val (ts3,nd3) = constructorSignature (ts2)
            in case ts3 of
                (LeftBrace, _) :: _ =>
                    let
                        val (ts4,nd4) = functionBody (ts3)
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.ConstructorDefn (Ast.Ctor {settings=([],[]), superArgs=[],
                                                            func=Ast.Func {name=nd2,
                                                                           fsig=nd3,
                                                                           param=([],[]),
                                                                           defaults=[],
                                                                           ty=functionTypeFromSignature nd3,
                                                                           isNative=false,
                                                                           block=nd4}})],
                              body=[],
                              head=NONE,
                              loc=locOf ts})
                    end
              | _ => 
                    let
                        val (ts4,nd4) = listExpression (ts3,ALLOWIN)
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.ConstructorDefn (Ast.Ctor
                                             {settings=([],[]),
                                              superArgs=[],
                                              func=Ast.Func {name=nd2,
                                                             fsig=nd3,
                                                             param=([],[]),
                                                             defaults=[],
                                                             ty=functionTypeFromSignature nd3,
                                                             isNative=false,
                                                             block=Ast.Block 
                                                                       {pragmas=[],
                                                                        defns=[],
                                                                        body=[Ast.ReturnStmt nd4],
                                                                        head=NONE,
                                                                        loc=locOf ts3}}})],
                              body=[],
                              head=NONE,
                              loc=locOf ts})
                    end
            end

      | (Ast.LetVar,true,_) => 
        error ["class name not allowed in 'let function'"]
        
      | (_,false,false) =>  (* static or instance method *)
            let
                val (ts3,nd3) = functionSignature (ts2)
            in case ts3 of
                (LeftBrace, _) :: _ =>
                    let
                        val (ts4,nd4) = functionBody (ts3)
                        val ident = (#ident nd2)
                        val func = Ast.Func {name=nd2,
                                             fsig=nd3,
                                             param=([],[]),
                                             defaults=[],
                                             ty=functionTypeFromSignature nd3,
                                             isNative=false,
                                             block=nd4}
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.FunctionDefn {kind=if (nd1=Ast.Var) then Ast.Const else nd1, 
                                          (* in a class all functions are read only *)
                                                       ns=ns,
                                                       final=final,
                                                       override=override,
                                                       prototype=prototype,
                                                       static=static,
                                                       func=func}],
                              body=[],
                              head=NONE,
                              loc=locOf ts})
                    end
              | _ => 
                    let
                        val (ts4,nd4) = listExpression (ts3,ALLOWIN)
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.FunctionDefn {kind=if (nd1=Ast.Var) then Ast.Const else nd1,
                                                       ns=ns,
                                                       final=final,
                                                       override=override,
                                                       prototype=prototype,
                                                       static=static,
                                                       func=Ast.Func {name=nd2,
                                                                      fsig=nd3, 
                                                                      param=([],[]),
                                                                      defaults=[],
                                                                      ty=functionTypeFromSignature nd3,
                                                                      isNative=false,
                                                                      block=Ast.Block { pragmas=[],
                                                                                        defns=[],
                                                                                        body=[Ast.ReturnStmt nd4],
                                                                                        head=NONE,
                                                                                        loc=locOf ts3}}}],
                              body=[],
                              head=NONE,
                              loc=locOf ts})
                    end
            end
      | (_,false,true) =>  (* prototype function *)
            let
                val (ts3,nd3) = functionSignature (ts2)
            in case ts3 of
                (LeftBrace, _) :: _ =>
                    let
                        val (ts4,nd4) = functionBody (ts3)
                        val ident = (#ident nd2)
                        val func = Ast.Func {name=nd2,
                                             fsig=nd3,
                                             param=([],[]),
                                             defaults=[],
                                             ty=functionTypeFromSignature nd3,
                                             isNative=false,
                                             block=nd4}
                        val initSteps = [Ast.InitStep (Ast.PropIdent ident, Ast.LiteralExpr (Ast.LiteralFunction func))]
                        val initStmts = [Ast.InitStmt {kind=nd1,
                                                       ns=ns,
                                                       prototype=prototype,
                                                       static=static,
                                                       temps=([],[]),
                                                       inits=initSteps}]

                    in
                        (ts4,{pragmas=[],
                              defns=[],
                              body=initStmts,
                              head=NONE,
                              loc=locOf ts})
                    end
              | _ => 
                    let
                        val (ts4,nd4) = listExpression (ts3,ALLOWIN)
                        val ident = (#ident nd2)
                        val func = Ast.Func {name=nd2,
                                             fsig=nd3,
                                             param=([],[]),
                                             defaults=[],
                                             ty=functionTypeFromSignature nd3,
                                             isNative=false,
                                             block=Ast.Block {pragmas=[],
                                                              defns=[],
                                                              body=[Ast.ReturnStmt nd4],
                                                              head=NONE,
                                                              loc=locOf ts3}}
                        val initSteps = [Ast.InitStep (Ast.PropIdent ident, 
                                                       Ast.LiteralExpr (Ast.LiteralFunction func))]
                        val initStmts = [Ast.InitStmt {kind=nd1,
                                                       ns=ns,
                                                       prototype=prototype,
                                                       static=static,
                                                       temps=([],[]),
                                                       inits=initSteps}]
                    in
                        (ts4,{pragmas=[],
                              defns=[],
                              body=initStmts,
                              head=NONE,
                              loc=locOf ts})
                    end
            end
      | _ => LogErr.unimplError ["functionDefinition"]
    end

  | functionDefinition (ts,attrs,t) =
    let val _ = trace([">> functionDefinition with next=", tokenname(hd ts)])
        val {ns,final,override,prototype,static,...} = attrs
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
                             isNative=false,
                             block=nd4}
    in
        (ts4,{pragmas=[],
              defns=[Ast.FunctionDefn {kind=nd1, 
                                       ns=ns,
                                       final=final,
                                       override=override,
                                       prototype=prototype,
                                       static=static,
                                       func=func}],
              body=[],
(*
              body=[Ast.InitStmt {kind=nd1,
                                   ns=ns,
                                   prototype=false,
                                   static=false,
                                   temps=([],[]),
                                   inits=[Ast.InitStep (Ast.PropIdent ident,
                                                        Ast.LiteralExpr (Ast.LiteralFunction func))]}],
*)
              head=NONE,
              loc=locOf ts})
    end

and functionKind (ts) =
    let val _ = trace([">> functionKind with next=", tokenname(hd ts)])
    in case ts of
        (Function, _) :: _ => 
            (trace(["<< functionKind with next=", tokenname(hd (tl ts))]);
            (tl ts,Ast.Var))   (* reuse VAR_DEFN_KIND *)
      | (Let, _) :: (Function, _) :: _ => 
            (tl (tl ts), Ast.LetVar)
      | (Const, _) :: (Function, _) :: _ => 
            (tl (tl ts), Ast.Const)
      | _ => error ["unknown token in functionKind"]
    end


and functionName (ts) : ((TOKEN * Ast.LOC) list * Ast.FUNC_NAME) =
    let val _ = trace([">> functionName with next=", tokenname(hd ts)])
    in case ts of
        ((Plus | Minus | BitwiseNot | Mult | Div | Modulus | LessThan |
          GreaterThan | LessThanOrEquals | GreaterThanOrEquals | Equals | LeftShift |
          RightShift | UnsignedRightShift | BitwiseAnd | BitwiseOr | StrictEquals |
          NotEquals | StrictNotEquals), _) :: _ => 
            let 
                val (ts1,nd1) = operatorName ts
            in
                (ts1,{kind=Ast.Operator,ident=nd1})
            end
      | (Get, _) :: ((LeftParen | LeftDotAngle), _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.get_})
      | (Get, _) :: (Mult, _) :: _ => (tl ts,{kind=Ast.Get,ident=Ustring.asterisk})
      | (Get, _) :: _ => 
            let
                val (ts1,nd1) = identifier (tl ts)
            in
                (ts1,{kind=Ast.Get,ident=nd1})
            end

      | (Set, _) :: ((LeftParen | LeftDotAngle), _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.set_})
      | (Set, _) :: (Mult, _) :: _ => (tl ts,{kind=Ast.Set,ident=Ustring.asterisk})
      | (Set, _) :: _ => 
            let
                val (ts1,nd1) = identifier (tl ts)
            in
                (ts1,{kind=Ast.Set,ident=nd1})
            end

      | (Call, _) :: ((LeftParen | LeftDotAngle), _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.call_})
      | (Call, _) :: (Mult, _) :: _ => (tl ts,{kind=Ast.Call,ident=Ustring.asterisk})

      | (Has, _) :: ((LeftParen | LeftDotAngle), _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.has_})
      | (Has, _) :: (Mult, _) :: _ => (tl ts,{kind=Ast.Has,ident=Ustring.asterisk})

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

and operatorName [] = error ["missing token in operatorName"]
  | operatorName ((t,_)::ts) =
    let val opStr = case t of
       (Plus
      | Minus
      | BitwiseNot
      | Mult
      | Div
      | Modulus
      | LessThan
      | GreaterThan
      | LessThanOrEquals
      | GreaterThanOrEquals
      | Equals
      | LeftShift
      | RightShift
      | UnsignedRightShift
      | BitwiseAnd
      | BitwiseOr
      | StrictEquals
      | NotEquals
      | StrictNotEquals) => tokenname (t,())
      | _ => error ["unknown token in operatorName"]
    in
        (ts, Ustring.fromString opStr)
    end

(*
    ConstructorSignature    
        TypeParameters  (  Parameters  )
        TypeParameters  (  Parameters  )  : ConstructorInitialiser
*)

and constructorSignature (ts) = 
    let val _ = trace([">> constructorSignature with next=",tokenname(hd(ts))]) 
        val (ts1,nd1) = typeParameters ts
    in case ts1 of
        (LeftParen, _) :: _ =>
               let
                   val (ts2,((b,i),e,t),hasRest) = parameters (tl ts1)
               in case ts2 of
                   (RightParen, _) :: (Colon, _) :: _ =>
                       let
                           val (ts3,nd3) = constructorInitialiser (tl (tl ts2))
                       in
                           trace ["<< constructorSignature with next=",tokenname(hd ts3)];
                           (ts3,Ast.FunctionSignature
                                    { typeParams=nd1,
                                      params=(b,i),
                                      paramTypes=t,
                                      defaults=e,
                                      returnType=(Ast.SpecialType Ast.VoidType),
                                      ctorInits=SOME nd3,
                                      thisType=NONE,
                                      hasRest=hasRest })
                       end
                 | (RightParen, _) :: _ =>
                       let
                       in
                           trace ["<< constructorSignature with next=",tokenname(hd ts2)];
                           (tl ts2,Ast.FunctionSignature
                                    { typeParams=nd1,
                                      params=(b,i),
                                      paramTypes=t,
                                      defaults=e,
                                      returnType=(Ast.SpecialType Ast.VoidType),
                                      ctorInits=SOME (([],[]),[]),
                                      thisType=NONE,
                                      hasRest=hasRest })
                       end
                 | _ => error ["unknown token in constructorSignature"]
            end
      | _ => error ["unknown token in constructorSignature"]
    end


(*
    ConstructorInitialiser    
        InitialiserList
        InitialiserList SuperInitialiser
        SuperInitialiser
        
    InitaliserList    
        Initialiser
        InitialiserList  ,  Initialiser
        
    Initialiser    
        Pattern(noList, noIn, noExpr)  VariableInitialisation(noList, allowIn)

    SuperInitialiser
        super Arguments
*)

and constructorInitialiser ts 
    : ((TOKEN * Ast.LOC) list * (Ast.BINDINGS * Ast.EXPR list)) =
    let val _ = trace([">> constructorInitialiser with next=",tokenname(hd(ts))]) 
    in case ts of
        (Super, _) :: _ => 
            let
                val (ts1,nd1) = arguments (tl ts)
            in
                trace ["<< constructorInitialiser with next=",tokenname(hd ts1)];
                (ts1,(([],[]),nd1))
            end
      | _ => 
            let
                val (ts1,nd1) = initialiserList ts
            in case ts1 of
                (Super, _) :: _ => 
                    let
                        val (ts2,nd2) = arguments (tl ts1)
                    in
                        trace ["<< constructorInitialiser with next=",tokenname(hd ts1)];
                        (ts2,(nd1,nd2))
                    end
              | _ =>
                    let
                    in
                        trace ["<< constructorInitialiser with next=",tokenname(hd ts1)];
                        (ts1,(nd1,[]))
                    end
            end
    end

and initialiserList (ts) 
    : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) = 
    let val _ = trace([">> initialiserList with next=", tokenname(hd ts)])
        fun initialiserList' (ts)
            : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) = 
            let val _ = trace([">> initialiserList' with next=", tokenname(hd ts)])
            in case ts of
                (Comma, _) :: (Super, _) :: _ =>
                    (tl ts,([],[]))
              | (Comma, _) :: _ =>
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
    : ((TOKEN * Ast.LOC) list * Ast.BINDINGS) =
    let val _ = trace([">> initialiser with next=", tokenname(hd ts)])
        val (ts1,nd1) = pattern (ts,NOLIST,NOIN,ALLOWEXPR)
    in case (ts1) of
            (Assign, _) :: _ =>
                let
                    val (ts2,nd2) = variableInitialisation (ts1,NOLIST,NOIN)
                in
                    trace(["<< initialiser with next=", tokenname(hd ts2)]);
                    (ts2, desugarPattern (locOf ts) nd1 (Ast.SpecialType Ast.Any) (SOME nd2) 0) (* type meaningless *)
                end
          | _ => (error(["constructor initialiser without assignment"]); error ["unknown token in initialiser"])
    end

and superInitialiser (ts) 
    : ((TOKEN * Ast.LOC) list * Ast.EXPR list) =
    let val _ = trace([">> superInitialiser with next=", tokenname(hd ts)])
    in case ts of
        (Super, _) :: _ =>
            let
                val (ts1,nd1) = arguments (tl ts)
            in
                (ts1,nd1)
            end
      | _ => error ["unknown token in superInitialiser"]
    end

(*
    FunctionBody    
        Block(function)
*)

and functionBody (ts) =
    let val _ = trace([">> functionBody with next=", tokenname(hd ts)])
    in case ts of
        (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = block (ts,LOCAL)
            in
                (ts1,nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = listExpression (ts,ALLOWIN)
            in
                (ts1,Ast.Block {pragmas=[],
                                defns=[],
                                body=[Ast.ReturnStmt nd1],
                                head=NONE,
                                loc=locOf ts})
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

and classDefinition (ts,attrs:ATTRS) =
    let val _ = trace([">> classDefinition with next=", tokenname(hd ts)])
    in case ts of
        (Class, _) :: _ =>
            let
                val {ns,final,dynamic,...} = attrs
                val (ts1,{ident,params,nonnullable}) = className (tl ts)
                val (ts2,{extends,implements}) = classInheritance (ts1)
                val _ = currentClassName := ident;
                val (ts3,nd3) = classBody (ts2)
                val _ = currentClassName := Ustring.empty;

                fun isLet (d:Ast.DEFN) (* borrowed from defn.sml *)
                    : bool =
                    case d of 
                        Ast.VariableDefn {kind,...} => (kind=Ast.LetVar) orelse (kind=Ast.LetConst)
                      | Ast.FunctionDefn fd => false
                      | Ast.ConstructorDefn cd => false
                      | Ast.TypeDefn _ => false
                      | Ast.NamespaceDefn _ => false
                      | _ => LogErr.defnError ["illegal definition type in class"]

                fun isProto (d:Ast.DEFN) 
                    : bool = 
                    case d of 
                        Ast.VariableDefn vd => (#prototype vd)
                      | Ast.FunctionDefn fd => (#prototype fd)
                      | Ast.ConstructorDefn _ => false
                      | Ast.TypeDefn _ => false
                      | Ast.NamespaceDefn _ => false
                      | _ => LogErr.defnError ["illegal definition type in class"]
        
                fun isStatic (d:Ast.DEFN)
                    : bool = 
                    case d of 
                        Ast.VariableDefn vd => (#static vd)
                      | Ast.FunctionDefn fd => (#static fd)
                      | Ast.ConstructorDefn _ => false
                      | Ast.TypeDefn _ => true
                      | Ast.NamespaceDefn _ => true
                      | _ => LogErr.defnError ["illegal definition type in class"]

                fun isCtor (d:Ast.DEFN) : bool = 
                    case d of 
                        Ast.ConstructorDefn _ => true
                      | _ => false
                         
                fun isInstanceInit (s:Ast.STMT)
                    : bool =
                    let
                    in case s of
                        Ast.InitStmt {kind, static, prototype,...} =>
                            not ((kind = Ast.LetVar) orelse 
                                 (kind = Ast.LetConst) orelse
                                 prototype orelse 
                                 static)
                      | _ => false                    
                    end

                val (Ast.Block {pragmas,body,defns,...}) = nd3
                val (letDefns,defns) = List.partition isLet defns
                val (protoDefns,defns) = List.partition isProto defns
                val (ctorDefns,defns) = List.partition isCtor defns
                val (classDefns,instanceDefns) = List.partition isStatic defns

                val ctorDefn = case ctorDefns of [Ast.ConstructorDefn cd] => SOME cd 
                                               | [] => NONE 
                                               | _ => LogErr.internalError ["more than one ctor"]

                val (instanceStmts,body) = List.partition isInstanceInit body

                val classDefn = Ast.ClassDefn {ident=ident,
                                            nonnullable=nonnullable,
                                            ns=ns,
                                            final=final,
                                            dynamic=dynamic,
                                            params=params,
                                            extends=extends,
                                            implements=implements,
                                            classDefns=classDefns,
                                            instanceDefns=instanceDefns,
                                            instanceStmts=instanceStmts,
                                            ctorDefn=ctorDefn }

            in
                (ts3,{pragmas=[],
                      body=[Ast.ClassBlock 
                                {ns=ns,
                                 ident=ident,
                                 name=NONE,
                                 block=Ast.Block {body=body,
                                                  defns=letDefns,
                                                  head=NONE,
                                                  pragmas=pragmas,
                                                  loc=locOf ts2}}],
                      defns=[classDefn],
                      head=NONE,
                      loc=locOf ts})
            end
      | _ => error ["unknown token in classDefinition"]
    end

and className (ts) =
    let val _ = trace([">> className with next=", tokenname(hd ts)])
        val (ts1,{ident,params}) = parameterisedClassName ts
    in case ts1 of
        (Not, _) :: _ =>
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
        (LeftDotAngle, _) :: _ =>
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
        (Extends, _) :: _ =>
            let
                val (ts1,nd1) = primaryIdentifier (tl ts)
            in case ts1 of
                (Implements, _) :: _ =>
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
      | (Implements, _) :: _ =>
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
                (Comma, _) :: _ =>
                    let
                        val (ts1,nd1) = primaryIdentifier (tl ts)
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
        val (ts1,nd1) = primaryIdentifier (ts)
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

and interfaceDefinition (ts,attrs:ATTRS) =
    let val _ = trace([">> interfaceDefinition with next=", tokenname(hd ts)])
        val {ns,...} = attrs
    in case ts of
        (Interface, _) :: _ =>
            let
                val (ts1,{ident,params,nonnullable}) = className (tl ts)
                val (ts2,{extends}) = interfaceInheritance (ts1)
                val (ts3,nd3) = interfaceBody (ts2)
                val (Ast.Block {pragmas,body,defns,...}) = nd3
            in
                 (ts3,{pragmas=[],
                       body=[],
                       defns=[Ast.InterfaceDefn {ident=ident,
                                                 nonnullable=nonnullable,
                                                 ns=ns,
                                                 params=params,
                                                 extends=extends,
                                                 instanceDefns=defns}],
                       head=NONE,
                       loc=locOf ts})
            end
      | _ => error ["unknown token in interfaceDefinition"]
    end

and interfaceInheritance (ts) = 
    let val _ = trace([">> interfaceInheritance with next=", tokenname(hd ts)])
    in case ts of
        (Extends, _) :: _ =>
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

and namespaceDefinition (ts,attrs:ATTRS) =
    let val _ = trace([">> namespaceDefinition with next=", tokenname(hd ts)])
        val {ns,...} = attrs
    in case ts of
        (Namespace, _) :: _ =>
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
                      head=NONE,
                      loc=locOf ts})
            end
      | _ => error ["unknown token in namespaceDefinition"]
    end
        
and namespaceInitialisation (ts) : ((TOKEN * Ast.LOC) list * Ast.EXPR option) =
    let val _ = trace([">> namespaceInitialisation with next=", tokenname(hd ts)])
    in case ts of
         (Assign, _) :: (StringLiteral s, _) :: _ =>
            let
                val (ts1,nd1) = (tl (tl ts), 
                                 Ast.LiteralExpr(Ast.LiteralString(s)))
            in
                trace(["<< namespaceInitialisation StringLiteral with next=", tokenname(hd ts1)]);
                (ts1,SOME nd1)
            end
      | (Assign, _) :: _ =>
            let
                val (ts1,nd1) = primaryIdentifier (tl ts)
            in
                trace(["<< namespaceInitialisation simpleTypeIdentifer with next=", tokenname(hd ts1)]);
                (ts1,SOME (Ast.LexicalRef {ident=nd1, loc=locOf ts}))
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

and typeDefinition (ts,attrs:ATTRS) =
    let val _ = trace([">> typeDefinition with next=", tokenname(hd ts)])
        val {ns,...} = attrs
    in case ts of
        (Type, _) :: _ =>
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
                      head=NONE,
                      loc=locOf ts})
            end
      | _ => error ["unknown token in typeDefinition"]
    end
        
and typeInitialisation (ts) : ((TOKEN * Ast.LOC) list * Ast.TYPE_EXPR) =
    let val _ = trace([">> typeInitialisation with next=", tokenname(hd ts)])
    in case ts of
        (Assign, _) :: _ =>
            let
                val (ts1,nd1) = nullableTypeExpression (tl ts)
            in
                trace(["<< typeInitialisation with next=", tokenname(hd ts1)]);
                (ts1,nd1)
            end
      | _ => error ["unknown token in typeInitialisation"]
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

and pragmas (ts) : (TOKEN * Ast.LOC) list * Ast.PRAGMA list =
    let val _ = trace([">> pragmas with next=", tokenname(hd ts)])
        val (ts1,nd1) = pragma(ts)
    in case ts1 of
        ((Use | Import), _) :: _ => 
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
        (Use, _) :: _ => 
            let
                val (ts1,nd1) = usePragma ts
                 val (ts2,nd2) = (semicolon (ts1,FULL),nd1)
            in
                (ts2,nd2)
            end
      | (Import, _) :: _ => 
            let
                val (ts1,nd1) = importPragma ts
                 val (ts2,nd2) = (semicolon (ts1,FULL),nd1)
            in
                (ts2,nd2)
            end
      | _ => error ["unknown token in pragma"]
    end

(*
    UsePragma    
        use  PragmaItems        
*)

and usePragma ts =
    let val _ = trace([">> usePragma with next=", tokenname(hd ts)])
    in case ts of
        (Use, _) :: _ => pragmaItems (tl ts)
      | _ => error ["unknown token in usePragma"]
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
                (Comma, _) :: _ =>
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
        (Decimal, _) :: _ => (tl ts,Ast.UseNumber Ast.Decimal)
      | (Double, _) :: _ => (tl ts,Ast.UseNumber Ast.Double)
      | (Int, _) :: _ => (tl ts,Ast.UseNumber Ast.Int)
      | (Number, _) :: _ => (tl ts,Ast.UseNumber Ast.Number)
      | (UInt, _) :: _ => (tl ts,Ast.UseNumber Ast.UInt)
      | (Precision, _) :: (DecimalIntegerLiteral n, _) :: _ => 
            let
                val i = case (Int.fromString n) of 
                            SOME i => i
                          | NONE => error ["non-integral number literal in 'use precision' pragma"]
            in
                (tl (tl ts), Ast.UsePrecision i)
            end
      | (Rounding, _) :: (Identifier s, _) :: _ => 
            let
                val m = Decimal.HalfUp
            in
                (tl (tl ts), Ast.UseRounding m)
            end
      | (Standard, _) :: _ => (tl ts,Ast.UseStandard)
      | (Strict, _) :: _ => (tl ts,Ast.UseStrict)
      | (Default, _) :: (Namespace, _) :: ((Public | Internal | Intrinsic | Protected | Private), _) :: _ => 
            let
                val (ts1,nd1) = reservedNamespace (tl (tl ts))
            in
                (ts1, Ast.UseDefaultNamespace (Ast.LiteralExpr (Ast.LiteralNamespace nd1)))
            end
      | (Default, _) :: (Namespace, _) :: _ => 
            let
                val (ts1,nd1) = primaryIdentifier (tl (tl ts))
            in
                (ts1, Ast.UseDefaultNamespace (Ast.LexicalRef {ident=nd1, loc=locOf ts}))
            end
      | (Namespace, _) :: (Intrinsic, _) :: _ => 
            let
                val (ts1,nd1) = (tl (tl ts), Ast.LiteralExpr (Ast.LiteralNamespace Ast.Intrinsic))
            in
                (ts1, Ast.UseNamespace nd1)
            end
      | (Namespace, _) :: _ => 
            let
                val (ts1,nd1) = primaryIdentifier (tl ts)
            in
                (ts1, Ast.UseNamespace (Ast.LexicalRef { ident = nd1, loc = locOf ts}))
            end
      | _ =>
            LogErr.parseError ["invalid pragma"]
    end

(*
    ImportPragma    
        import  Identifier  =  ImportName
        import  ImportName
        
    ImportName    
        PackageIdentifier  .  PropertyIdentifier
*)

and importPragma (ts) : (TOKEN * Ast.LOC) list * Ast.PRAGMA list =
    let val _ = trace([">> importPragma with next=", tokenname(hd ts)])
    in case ts of
        (Import, _) :: _ :: (Assign, _) :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
                val (ts2,(p,i)) = importName (tl ts1)
            in
                (ts2,[Ast.Import {package=p,name=i,alias=SOME nd1}])
            end
      | (Import, _) :: _ =>
            let
                val (ts1,(p,i)) = importName (tl ts)
            in
                (ts1,[Ast.Import {package=p,name=i,alias=NONE}])
            end
      | _ => error ["unknown token in importPragma"]
    end

and importName (ts) : ((TOKEN * Ast.LOC) list * (Ast.IDENT list * Ast.IDENT)) =
    let val _ = trace([">> importName with next=", tokenname(hd ts)])
    in case ts of
        (Identifier p, _) :: (Dot, _) :: _ =>
            let
                val (ts1,nd1) = (tl ts,p)
                val (ts2,(p,i)) = importName (tl ts1)
            in
                (ts2,(nd1::p,i))
            end
      | (Mult, _) :: _ =>
            let
                val (ts1,nd1) = (tl ts, Ustring.asterisk)
            in
                (ts1,([],nd1))
            end
      | (Identifier p, _) :: _ =>
            let
                val (ts1,nd1) = (tl ts,p)
            in
                (ts1,([],nd1))
            end
      | _ => error ["unknown token in importName"]
    end

(* BLOCKS AND PROGRAMS *)

(*
    Block(t)    
        {  Directives(t)  }
*)

and block (ts,t) : ((TOKEN * Ast.LOC) list * Ast.BLOCK) =
    let 
        val _ = setLoc ts
        val _ = trace([">> block with next=", tokenname(hd ts)])
    in case ts of
        (LeftBrace, _) :: (RightBrace, _) :: _ => 
            ((trace(["<< block with next=", tokenname(hd (tl (tl ts)))]);
            (tl (tl ts),Ast.Block {pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})))
      | (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = directives (tl ts,t)
            in case ts1 of
                (RightBrace, _) :: _ => 
                    (trace(["<< block with next=", tokenname(hd (tl ts1))]);
                    (tl ts1,Ast.Block nd1))
              | (Eof, _) :: _ => raise EofError
              | _ => error ["unknown token in block"]
            end
      | _ => error ["unknown token in block"]
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
    
and program (ts) : ((TOKEN * Ast.LOC) list * Ast.PROGRAM) =
    let val _ = trace([">> program with next=",tokenname(hd(ts))])
    in case ts of
        (((Internal, _) :: (Package, _) :: _) |
         ((Package, _) :: _)) =>
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

and packages ts 
    : (TOKEN * Ast.LOC) list * Ast.PACKAGE list =
    let val _ = trace([">> packages with next=",tokenname(hd(ts))])
    in case ts of
        ((Internal, _) :: (Package, _) :: _ | (Package, _) :: _) =>
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
        
    PackageName
        Identifier
        PackageName  .  Identifier
        
    PackageBody    
        Block(global)
*)

and package ts : ((TOKEN * Ast.LOC) list * Ast.PACKAGE) =
    let val _ = trace([">> package with next=",tokenname(hd(ts))])
    in case ts of
        (Internal, _) :: (Package, _) :: _ => 
            let
                val (ts1,nd1) = packageName (tl (tl ts))
                val (ts2,nd2) = block (ts1,GLOBAL)
            in
                (ts2, {name=nd1, block=nd2})
            end
      | (Package, _) :: (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = block (tl ts,GLOBAL)
            in
                (ts1, {name=[], block=nd1})
            end
      | (Package, _) :: _ =>
            let
                val (ts1,nd1) = packageName (tl ts)
                val (ts2,nd2) = block (ts1,GLOBAL)
            in
                (ts2, {name=nd1, block=nd2})
            end
      | _ => error ["unknown token in package"]
    end

and packageName (ts) : (TOKEN * Ast.LOC) list * Ast.IDENT list =
    let val _ = trace([">> packageName with next=", tokenname(hd ts)])
        val (ts1,nd1) = identifier ts
    in case ts1 of
        (Dot, _) :: (Identifier _, _) :: _ =>
            let
                val (ts2,nd2) = packageName (tl ts1)
            in
                (ts2,nd1::nd2)
            end
      | _ =>
            let
            in
                (ts1,nd1::[])
            end
    end

fun mkReader filename = 
    let
        val stream = TextIO.openIn filename
    in
        fn _ => case TextIO.inputLine stream of
                    SOME line => (trace ["read line ", line]; Ustring.fromSource line)
                  | NONE => (TextIO.closeIn stream; Ustring.emptySource)
    end

(*
   scan to first < or /

   the scanner state includes the reader and a token stream.
   when the token stream is empty continue scanning in the
   current slash context until another key character is encountered.

   switch slash contexts at the normal points in the program.
*)

fun lexFile (filename : string) : ((TOKEN * Ast.LOC) list) = 
    Lexer.lex (filename, mkReader filename)

fun lexLines (lines : Ustring.SOURCE list) : ((TOKEN * Ast.LOC) list) =
    let
        val r = ref lines
        fun reader _ = 
            case !r of
                (line::lines) => (r := lines; line)
              | [] => Ustring.emptySource
    in
        Lexer.lex ("<no filename>", reader)
    end

fun parse ts =
    let 
        val (residual, result) = (program ts) 
        fun check_residual [(Eof, _)] = ()
          | check_residual _ = error ["residual tokens after parse"]
    in
        check_residual residual;
        trace ["parsing complete:"];
        (if (!doTrace)
         then Pretty.ppProgram result
         else ());
        result
    end

fun logged thunk name =
    (trace ["scanning ", name];
     let val ast = thunk ()
     in
         trace ["parsed ", name, "\n"];
         ast
     end)

fun parseFile filename =
    logged (fn _ => parse (lexFile filename)) filename

fun parseLines lines =
    logged (fn _ => parse (lexLines lines)) "<<string>>"

end (* Parser *)
