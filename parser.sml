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
 *    2. All liability and responsibility for any use of this Reference
 * Implementation rests with the user, and not with any of the parties
 * who contribute to, or who own or hold any copyright in, this Reference
 * Implementation.
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

datatype ALPHA =
    AllowList
  | NoList

datatype BETA =
    AllowIn
  | NoIn

datatype GAMMA =
    AllowExpr
  | NoExpr

datatype OMEGA =
    Abbrev
  | NoShortIf
  | Full

datatype TAU =
    GlobalScope
  | ClassScope
  | InterfaceScope
  | LocalScope

type ATTRS = {ns: Ast.EXPR option,
              override: bool,
              static: bool,
              final: bool,
              dynamic: bool,
              prototype: bool,
              native: bool,
              rest: bool}

type TOKENS = (TOKEN * Ast.LOC) list

fun makeTy (typeExpr:Ast.TYPE_EXPR) 
    : Ast.TY = 
    Ast.Ty { expr = typeExpr,
             ribId = NONE }

fun unwrapTy (ty:Ast.TY) 
    : Ast.TYPE_EXPR = 
    let
        
        val Ast.Ty { expr, ... } = ty
    in 
        expr
    end
    
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
(* val currentPackageName : Ast.IDENT ref = ref Ustring.empty *)

fun newline (ts : TOKENS) =
    let
        val (_, {file, span, post_newline}) = hd ts
    in
        post_newline
    end

fun locOf (ts:TOKENS) =
    case ts of
        (_, loc) :: _ => (SOME loc)
      | _ => (NONE)

fun setLoc (ts:TOKENS) =
    LogErr.setLoc (locOf ts)

fun unionLoc (SOME { file=file0, span=(lower, _), post_newline=postNewline } : Ast.LOC option)
             (SOME { file=file1, span=(_, upper), post_newline=_           } : Ast.LOC option) =
    if file0 = file1
    then SOME { file=file0, span=(lower, upper), post_newline=postNewline }
    else error ["Cannot union locations from different files"]
  | unionLoc _ _ =
    NONE

fun unionLocExcludingLast(SOME { file=file0, span=(lower, _), post_newline=postNewline    } : Ast.LOC option)
                         (SOME { file=file1, span=({line=line,col=col}, _), post_newline=_} : Ast.LOC option) =
    if file0 = file1
    then SOME { file=file0, span=(lower, {line=line, col=col-1}), post_newline=postNewline }
    else error ["Cannot union locations from different files"]
  | unionLocExcludingLast _ _ =
    NONE


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
(*
                | NONE => ([bind],[Ast.InitStep (ident,Ast.LiteralExpr Ast.LiteralUndefined)])
*)
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
    let fun tn () = Ustring.fromString (tokenname (t,()))
        val ustr = case t of
        Identifier(us) => us
      | Call => tn ()
      | Cast => tn ()
      | Const => tn ()
      | Decimal => tn ()
      | Double => tn ()
      | Dynamic => tn ()
      | Each => tn ()
      | Eval => tn ()
      | Final => tn ()
      | Get => tn ()
      | Has => tn ()
      | Implements => tn ()
      | Import => tn ()
      | Int => tn ()
      | Interface => tn ()
      | Internal => tn ()
      | Intrinsic => tn ()
      | Is => tn ()
      | Let => tn ()
      | Namespace => tn ()
      | Native => tn ()
      | Number => tn ()
      | Override => tn ()
      | Package => tn ()
      | Precision => tn ()
      | Private => tn ()
      | Protected => tn ()
      | Prototype => tn ()
      | Public => tn ()
      | Rounding => tn ()
      | Set => tn ()
      | Standard => tn ()
      | Static => tn ()
      | Strict => tn ()
      | To => tn ()
      | Type => tn ()
      | UInt => tn ()
      | Undefined => tn ()
      | Use => tn ()
      | Xml => tn ()
      | Yield => tn ()
      | _ => error ["expecting 'identifier' before '",tokenname (t,()),"'"]
    in
        (ts, ustr)
    end

(*
    Qualifier
        ReservedNamespace
        PropertyIdentifier
*)

and qualifier (ts:TOKENS) =
    let
        fun rn () =
            let
                val (ts1,nd1) = reservedNamespace ts
            in
                (ts1,Ast.LiteralExpr(Ast.LiteralNamespace nd1))
            end
    in case ts of
        (Internal,  _) :: _ => rn ()
      | (Intrinsic, _) :: _ => rn ()
      | (Private,   _) :: _ => rn ()
      | (Protected, _) :: _ => rn ()
      | (Public,    _) :: _ => rn ()
      | (Mult,      _) :: _ =>
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

and reservedNamespace (ts:TOKENS) =
    let val _ = trace([">> reservedNamespace with next=",tokenname(hd(ts))])
    in case ts of
        (Internal, _) :: tr =>
            (tr, Ast.Internal Ustring.empty)  (* the definer computes the package name *)
      | (Intrinsic, _) :: tr =>
            (tr, Ast.Intrinsic)
      | (Private, _) :: tr =>
            (tr, Ast.Private Ustring.empty)
      | (Protected, _) :: tr =>
            (tr, Ast.Protected Ustring.empty)
      | (Public, _) :: tr =>
            (tr, Ast.Public Ustring.empty)
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

and simpleQualifiedIdentifier (ts:TOKENS) =
    let val _ = trace([">> simpleQualifiedIdentifier with next=",tokenname(hd(ts))])
        fun rn () =
            let
                val (ts1, nd1) = reservedNamespace(ts)
            in case ts1 of
                   (DoubleColon, _) :: ts2 => qualifiedIdentifier'(ts2,Ast.LiteralExpr(Ast.LiteralNamespace nd1))
                 | _ => error ["qualified namespace without double colon"]
            end
    in case ts of
        (Internal,  _) :: _ => rn ()
      | (Intrinsic, _) :: _ => rn ()
      | (Private,   _) :: _ => rn ()
      | (Protected, _) :: _ => rn ()
      | (Public,    _) :: _ => rn ()
      | (Mult,      _) :: _ =>
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

and reservedOrOrdinaryIdentifier (ts:TOKENS) =
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

and nonAttributeQualifiedIdentifier (ts:TOKENS) =
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

and attributeIdentifier (ts:TOKENS) =
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

and qualifiedIdentifier (ts:TOKENS) =
    let val _ = trace([">> qualifiedIdentifier with next=",tokenname(hd(ts))])
    in case ts of
        (At, _) :: _ => attributeIdentifier(ts)
      | _ => nonAttributeQualifiedIdentifier(ts)
    end

and propertyIdentifier (ts:TOKENS) = 
    nonAttributeQualifiedIdentifier ts
    
and primaryIdentifier (ts:TOKENS) =
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

and parenExpression (ts:TOKENS) =
    let val _ = trace([">> parenExpression with next=",tokenname(hd(ts))])
    in case ts of
        (LeftParen, _) :: ts1 =>
            let
                val (ts2,nd2:Ast.EXPR) = assignmentExpression (ts1,AllowList,AllowIn)
            in case ts2 of
                (RightParen, _) :: ts3 => (ts3,nd2)
              | _ => error ["unknown final token of paren expression"]
            end
      | _ => error ["unknown initial token of paren expression"]
    end

(*
    ParenListExpression
        (  ListExpression(AllowIn)  )
*)

and parenListExpression (ts:TOKENS) : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> parenListExpression with next=",tokenname(hd(ts))])
    in case ts of
        (LeftParen, _) :: _ =>
            let
                val (ts1,nd1,_) = listExpression (tl ts,AllowIn)
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

and functionExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> functionExpression with next=",tokenname(hd(ts))])
    in case ts of
        (Function, funcStartLoc) :: ts1 =>
            let
                fun anonymousFunctionSignature () =
                    let
                        val (ts3,nd3) = functionSignature ts1
                    in case (ts3,a) of
                        ((LeftBrace, _) :: _,_) =>
                            let
                                val (ts4,nd4) = block (ts3,LocalScope)
                                val Ast.Block {loc=blockLoc, ...} = nd4
                            in
                                (ts4,Ast.LiteralExpr
                                         (Ast.LiteralFunction
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=Ustring.empty},
                                                         fsig=nd3,
                                                         block=SOME nd4,
                                                         native=false,
                                                         defaults=[],
                                                         param=Ast.Head ([],[]),
                                                         ty=functionTypeFromSignature nd3,
                                                         loc=unionLoc (SOME funcStartLoc) blockLoc})))
                            end
                      | (_,AllowList) =>
                            let
                                val (ts4,nd4,listLoc) = listExpression (ts3,b)
                            in
                                (ts4,Ast.LiteralExpr
                                         (Ast.LiteralFunction
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=Ustring.empty},
                                                         fsig=nd3,
                                                         block=SOME (Ast.Block {pragmas=[],
                                                                                defns=[],
                                                                                body=[Ast.ReturnStmt nd4],
                                                                                head=NONE,
                                                                                loc=locOf ts3}),
                                                         native=false,
                                                         param=Ast.Head ([],[]),
                                                         defaults=[],
                                                         ty=functionTypeFromSignature nd3,
                                                         loc=unionLoc (SOME funcStartLoc) listLoc})))

                            end
                      | _ => error ["unknown body form in anonymous function expression"]
                    end
            in case ts1 of
                (LeftDotAngle, _) :: _ => anonymousFunctionSignature ()
              | (LeftParen,    _) :: _ => anonymousFunctionSignature ()
              | _ =>
                    let
                        val (ts2,nd2) = identifier ts1
                        val (ts3,nd3) = functionSignature ts2
                    in case (ts3,a) of
                        ((LeftBrace, _) :: _,_) =>
                            let
                                val (ts4,nd4) = block (ts3,LocalScope)
                                val Ast.Block {loc=blockLoc, ...} = nd4
                            in
                                (ts4,Ast.LiteralExpr
                                         (Ast.LiteralFunction
                                              (Ast.Func {name={kind=Ast.Ordinary,ident=nd2},
                                                         fsig=nd3,
                                                         block=SOME nd4,
                                                         native=false,
                                                         param=Ast.Head ([],[]),
                                                         defaults=[],
                                                         ty=functionTypeFromSignature nd3,
                                                         loc=unionLoc (SOME funcStartLoc) blockLoc})))
                            end
                      | (_,AllowList) =>
                            let
                                val (ts4,nd4,listLoc) = listExpression (ts3,b)
                            in
                                (ts4,Ast.LiteralExpr
                                         (Ast.LiteralFunction
                                              (Ast.Func
                                                   {name={kind=Ast.Ordinary,ident=nd2},
                                                    fsig=nd3,
                                                    block=SOME (Ast.Block
                                                                    {pragmas=[],
                                                                     defns=[],
                                                                     body=[Ast.ReturnStmt nd4],
                                                                     head=NONE,
                                                                     loc=locOf ts3}),
                                                    native=false,
                                                    param=Ast.Head ([],[]),
                                                    defaults=[],
                                                    ty=functionTypeFromSignature nd3,
                                                    loc=unionLoc (SOME funcStartLoc) listLoc})))
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
                if( ident=Ustring.Object_ )  (* FIXME: check for *the* object name *)
                then Ast.TypeName nd
                else Ast.TypeName nd
(* Don't convert to Ast.Any so we can distinguish from un-anno'd defs
   for handling compatibility cases, such as writable functions
        Ast.WildcardIdentifier =>
                Ast.SpecialType Ast.Any
*)
      | _ => Ast.TypeName nd

and functionSignature (ts) : ((TOKEN * Ast.LOC) list * Ast.FUNC_SIG) =
    let val _ = trace([">> functionSignature with next=",tokenname(hd(ts))])
        val (ts1,nd1) = typeParameters ts
    in case ts1 of
        (LeftParen, _) :: (This, _) :: (Colon, _) ::  _ =>
            let
                val (ts2,nd2) = typeExpression (tl (tl (tl ts1)))
                val temp = Ast.Binding {ident=Ast.ParamIdent 0, ty=Ast.SpecialType Ast.Any}
            in case ts2 of
                (Comma, _) :: _ =>
                    let
                           val (ts3,((b,i),e,t),hasRest) = nonemptyParameters (tl ts2) 0 false
                       in case ts3 of
                           (RightParen, _) :: _ =>
                               let
                                   val (ts4,nd4) = resultType (tl ts3)
                               in
                                trace(["<< functionSignature with next=",tokenname(hd ts4)]);
                                (ts4,Ast.FunctionSignature
                                     {typeParams=nd1,
                                      thisType=SOME (unwrapTy nd2),
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
                                  thisType=SOME (unwrapTy nd2),
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

and typeParameters (ts:TOKENS)
    : (TOKENS * (Ustring.STRING list)) =
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

and typeParameterList (ts:TOKENS)
    : (TOKENS * (Ustring.STRING list)) =
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
        NonemptyParameters(AllowList)

    NonemptyParameters
        ParameterInit
        ParameterInit  ,  NonemptyParameters
        RestParameter
*)

and nonemptyParameters (ts) (n) (initRequired)
    : (TOKENS * (Ast.BINDINGS * Ast.EXPR list * Ast.TYPE_EXPR list) * bool) =
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
    : (TOKENS * (Ast.EXPR list * Ast.TYPE_EXPR list)) =
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
    : (TOKENS * (Ast.BINDINGS * Ast.EXPR list * Ast.TYPE_EXPR list) * bool) =
    let val _ = trace([">> parameters with next=",tokenname(hd(ts))])
    in case ts of
        (RightParen, _) :: ts1 => (ts,(([],[]),[],[]),false)
      | _ => nonemptyParameters ts 0 false
    end

and parametersType (ts)
    : (TOKENS * (Ast.EXPR list * Ast.TYPE_EXPR list))=
    let val _ = trace([">> parameters with next=",tokenname(hd(ts))])
    in case ts of
        (RightParen, _) :: ts1 => (ts,([],[]))
      | _ => nonemptyParametersType ts
    end

(*
    ParameterInit
        Parameter
        Parameter  =  NonAssignmentExpression(AllowIn)
*)

and parameterInit (ts) (n) (initRequired)
    : (TOKENS * (Ast.BINDINGS * Ast.EXPR list * Ast.TYPE_EXPR list)) =
    let val _ = trace([">> parameterInit with next=",tokenname(hd(ts))])
        val (ts1,(temp,nd1)) = parameter ts n
    in case (ts1,initRequired) of
        ((Assign, _) :: _,_) =>
            let
                val {pattern,ty,...} = nd1
                val (ts2,nd2) = nonAssignmentExpression (tl ts1,NoList,AllowIn)
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
    : (TOKENS * (Ast.EXPR list * Ast.TYPE_EXPR list)) =
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
        ParameterKind TypedIdentifier(AllowIn)
        ParameterKind TypedPattern

    ParameterKind
        empty
        const
*)

and parameter (ts) (n)
    : (TOKENS * (Ast.BINDING * {pattern:PATTERN, ty:Ast.TYPE_EXPR})) =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))])
        val (ts1,nd1) = parameterKind (ts)
        val (ts2,(p,t)) = typedPattern (ts1,NoList,AllowIn)
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
        (ts2, (unwrapTy t))
    end

and parameterKind (ts)
    : (TOKENS * Ast.VAR_DEFN_TAG)  =
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

and restParameter (ts) (n): (TOKENS * (Ast.BINDINGS * Ast.EXPR list * Ast.TYPE_EXPR list)) =
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

and restParameterType (ts) : (TOKENS * (Ast.EXPR list * Ast.TYPE_EXPR list)) =
    let val _ = trace([">> restParameter with next=",tokenname(hd(ts))])
    in case ts of
        (TripleDot, _) :: _ =>
            let
            in case tl ts of
                (RightParen, _) :: _ =>
                    (tl ts,([],[Ast.ArrayType []]))
              | _ =>
                    let
                        val (ts1:TOKENS,ty) = parameterType (tl ts)
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

and resultType (ts:TOKENS)
    : (TOKENS * Ast.TYPE_EXPR) =
    let val _ = trace([">> resultType with next=",tokenname(hd(ts))])
    in case ts of
        (Colon, _) :: (Void, _) :: ts1 => (ts1,Ast.SpecialType(Ast.VoidType))
      | (Colon, _) :: _ =>
            let
                val (ts1,nd1) = nullableTypeExpression (tl ts)
            in
                trace ["<< resultType with next=",tokenname(hd ts1)];
                (ts1,unwrapTy nd1)
            end
      | ts1 => (ts1,Ast.SpecialType(Ast.Any))
    end

(*
    ObjectLiteral
        {  FieldList  }
        {  FieldList  }  :  TypeExpression
*)

and objectLiteral (ts:TOKENS)
    : (TOKENS * Ast.LITERAL) =
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
              | _ => error ["unknown token in objectLiteral ",tokenname (hd ts1)]
            end
      | _ => error ["unknown token in objectLiteral ",tokenname (hd ts)]
    end

(*
    FieldList
        empty
        LiteralField
        LiteralField  ,  FieldList
*)

and fieldList (ts:TOKENS)
    : (TOKENS * (Ast.FIELD list)) =
    let val _ = trace([">> fieldList with next=",tokenname(hd(ts))])
    in case ts of
        (RightBrace, _) :: _ =>
            (trace(["<< fieldList with next=",tokenname(hd(ts))]);
            (ts,[]))
      | _ =>
        let
            val (ts1,nd1) = literalField(ts)
        in case ts1 of
            (Comma, _) :: _ =>
                let
                    val (ts2,nd2) = fieldList (tl ts1)
                in
                    trace(["<< fieldList with next=",tokenname(hd(ts2))]);
                    (ts2,nd1::nd2)
                end
          | _ =>
            (trace(["<< fieldList with next=",tokenname(hd(ts1))]);
            (ts1,nd1::[]))
        end
    end

(*
    LiteralField
        FieldKind  FieldName  :  AssignmentExpression(NoList, AllowIn)
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

and literalField (ts:TOKENS)
  : (TOKENS * Ast.FIELD)=
    let val _ = trace([">> literalField with next=",tokenname(hd(ts))])
        fun getterSetter () =
            let
                val (ts1,nd1) = fieldKind ts
                val (ts2,nd2) = fieldName ts1
            in case ts2 of
                (Colon, _) :: _ =>
                    let
                        val (ts3,nd3) = assignmentExpression (tl ts2,NoList,AllowIn)
                    in
                        (ts3,{kind=nd1,name=nd2,init=nd3})
                    end
              | _ => error ["unknown token in literalField"]
            end
    in case ts of
        (* special case for fields with name 'get' or 'set' *)
        (Get, _) :: (Colon,_) :: _ => getterSetter ()
      | (Set, _) :: (Colon,_) :: _ => getterSetter ()
      | (Get, funcStartLoc) :: _ =>
            let
                val (ts1,nd1) = fieldName (tl ts)
                val (ts2,{fsig,block}) = functionCommon (ts1)
                val Ast.Block {loc=blockLoc, ...} = block
            in
                (ts2,{kind=Ast.Var,
                      name=nd1,
                      init=Ast.LiteralExpr
                               (Ast.LiteralFunction
                                    (Ast.Func {name={kind=Ast.Get, ident=Ustring.empty},
                                               fsig=fsig,
                                               block=SOME block,
                                               native=false,
                                               param=Ast.Head ([],[]),
                                               defaults=[],
                                               ty=functionTypeFromSignature fsig,
                                               loc=unionLoc (SOME funcStartLoc) blockLoc}))})
            end
      | (Set, funcStartLoc) :: _ =>
            let
                val (ts1,nd1) = fieldName (tl ts)
                val (ts2,{fsig,block}) = functionCommon (ts1)
                val Ast.Block {loc=blockLoc, ...} = block
            in
                (ts2,{kind=Ast.Var,
                      name=nd1,
                      init=Ast.LiteralExpr
                               (Ast.LiteralFunction
                                    (Ast.Func {name={kind=Ast.Get,ident=Ustring.empty},
                                               fsig=fsig,
                                               block=SOME block,
                                               native=false,
                                               param=Ast.Head ([],[]),
                                               defaults=[],
                                               ty=functionTypeFromSignature fsig,
                                               loc=unionLoc (SOME funcStartLoc) blockLoc}))})
            end
      | _ =>
            let
                val (ts1,nd1) = fieldKind ts
                val (ts2,nd2) = fieldName ts1
            in case ts2 of
                (Colon, _) :: _ =>
                    let
                        val (ts3,nd3) = assignmentExpression (tl ts2,NoList,AllowIn)
                    in
                        (ts3,{kind=nd1,name=nd2,init=nd3})
                    end
              | _ => error ["unknown token in literalField"]
            end
    end

and fieldKind (ts:TOKENS)
    : (TOKENS * Ast.VAR_DEFN_TAG)  =
    let val _ = trace([">> fieldKind with next=",tokenname(hd(ts))])
    in case ts of
        (Const, _) :: (Colon, _) :: _ => (ts,Ast.Var)
      | (Get,   _) :: (Colon, _) :: _ => (ts,Ast.Var)
      | (Set,   _) :: (Colon, _) :: _ => (ts,Ast.Var)
      | (Const, _) :: _ => (tl ts,Ast.Const)
      | _ => (ts,Ast.Var)
    end

and fieldName (ts:TOKENS)
    : (TOKENS * Ast.IDENT_EXPR) =
    let val _ = trace([">> fieldName with next=",tokenname(hd(ts))])
    in case ts of
        (StringLiteral s, _) :: ts1 => (ts1,Ast.Identifier {ident=s,openNamespaces=[]})

      | (DecimalLiteral n, _) :: ts1 => 
        (ts1, Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr(Ast.LiteralDecimal n)),
                                         openNamespaces = []})

      | (DoubleLiteral n, _) :: ts1 => 
        (ts1, Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr(Ast.LiteralDouble n)),
                                         openNamespaces = []})

      | (IntLiteral n, _) :: ts1 => 
        (ts1, Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr(Ast.LiteralInt n)),
                                         openNamespaces = []})

      | (UIntLiteral n, _) :: ts1 => 
        (ts1, Ast.ExpressionIdentifier { expr = (Ast.LiteralExpr(Ast.LiteralUInt n)),
                                         openNamespaces = []})

      | _ =>
            let
                val (ts1,nd1) = reservedOrOrdinaryIdentifier (ts)
            in
                (ts1,Ast.Identifier {ident=nd1,openNamespaces=[]})  (* todo: allow qualified identifier *)
            end
    end

and functionCommon (ts:TOKENS)
  : (TOKENS * {fsig: Ast.FUNC_SIG, block: Ast.BLOCK}) =
    let val _ = trace([">> functionCommon with next=",tokenname(hd(ts))])
        val (ts1,nd1) = functionSignature ts
    in case ts1 of
        (LeftBrace, _) :: _ =>
            let
                val (ts2,nd2) = block (ts1,LocalScope)
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

and arrayLiteral (ts:TOKENS)
    : (TOKENS * Ast.LITERAL) =
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
              | _ => error ["unknown token in arrayLiteral ",tokenname (hd ts)]
            end
      | _ => error ["unknown token in arrayLiteral ",tokenname (hd ts)]
    end

(*
    ElementList
        empty
        LiteralElement
        ,  ElementList
        LiteralElement  ,  ElementList

    LiteralElement
        AssignmentExpression(NoList, AllowIn)
*)

and elementList (ts:TOKENS)
    : (TOKENS * Ast.EXPR list) =
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
                val (ts1,nd1) = assignmentExpression (ts,NoList,AllowIn)
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

and primaryExpression (ts:TOKENS, a:ALPHA, b:BETA)
  : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> primaryExpression with next=",tokenname(hd ts)])
    in case ts of
        (Null, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralNull))
      | (True, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean true))
      | (False, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralBoolean false))

      | (DecimalLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralDecimal n))
      | (DoubleLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralDouble n))
      | (IntLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralInt n))
      | (UIntLiteral n, _) :: ts1 => (ts1, Ast.LiteralExpr(Ast.LiteralUInt n))

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

and superExpression (ts:TOKENS)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> superExpression with next=",tokenname(hd(ts))])
    in case ts of
        (Super, _) :: _ =>
            let
            in case tl ts of
                (LeftParen, _) :: _ =>
                    let
                           val (ts1,nd1) = parenExpression(tl ts)
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

and memberExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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

and memberExpressionPrime (ts:TOKENS, nd:Ast.EXPR, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> memberExpressionPrime with next=",tokenname(hd(ts))])
        fun withPropertyOperator () =
            let
                val (ts1,nd1) = propertyOperator(ts,nd)
                val (ts2,nd2) = memberExpressionPrime(ts1,nd1,a,b)
            in
                trace(["<< memberExpressionPrime with next=",tokenname(hd(ts2))]);
                (ts2,nd2)
            end
    in case ts of
        (LeftBracket, _) :: _ => withPropertyOperator ()
      | (Dot, _) :: _ => withPropertyOperator ()
      | (LeftDotAngle, _) :: _ => withPropertyOperator ()
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

and callExpressionPrime (ts:TOKENS, nd:Ast.EXPR, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> callExpressionPrime with next=",tokenname(hd(ts))])
        fun withPropertyOperator () =
            let
                val (ts1,nd1) = propertyOperator(ts,nd)
                val (ts2,nd2) = callExpressionPrime(ts1,nd1,a,b)
            in
                trace(["<< callExpressionPrime with next=",tokenname(hd(ts2))]);
                (ts2,nd2)
            end
    in case ts of
        (LeftBracket, _) :: _ => withPropertyOperator ()
      | (Dot, _) :: _ => withPropertyOperator ()
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

and newExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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
        (  ArgumentList(AllowList)  )
*)

and arguments (ts:TOKENS)
    : (TOKENS * Ast.EXPR list) =
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
        AssignmentExpression(NoList, AllowIn)
        ArgumentList  ,  AssignmentExpression(NoList, AllowIn)

    refactored:

    ArgumentList
        AssignmentExpression(NoList,AllowIn) ArgumentListPrime

    ArgumentListPrime
        empty
        , AssignmentExpression(NoList,AllowIn) ArgumentListPrime
*)

and argumentList (ts:TOKENS)
    : (TOKENS * Ast.EXPR list) =
    let val _ = trace([">> argumentList with next=",tokenname(hd(ts))])
        fun argumentList' (ts) : (TOKENS * Ast.EXPR list) =
            let val _ = trace([">> argumentList' with next=",tokenname(hd(ts))])
            in case ts of
                (Comma, _) :: _ =>
                    let
                        val (ts1,nd1) = assignmentExpression(tl ts,NoList,AllowIn)
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
        val (ts1,nd1) = assignmentExpression(ts,NoList,AllowIn)
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

and propertyOperator (ts:TOKENS, nd:Ast.EXPR)
    : (TOKENS * Ast.EXPR) =
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
                        fun notReserved () =
                            let
                                val (ts1,nd1) = propertyIdentifier (tl ts)
                            in
                                (ts1,Ast.ObjectRef {base=nd,ident=nd1,loc=locOf ts})
                            end
                     in case (isreserved(hd (tl ts)),tl ts) of
                        (false,_)               => notReserved ()
                      | (true,(Intrinsic,_)::_) => notReserved ()
                      | (true,(Private,_)::_)   => notReserved ()
                      | (true,(Public,_)::_)    => notReserved ()
                      | (true,(Protected,_)::_) => notReserved ()
                      | (true,(Internal,_)::_)  => notReserved ()
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
        bracketOrSlice ts nd

      | (LeftDotAngle, _) :: _ =>
        let
            val (ts1, nd1) = typeExpressionList (tl ts)
        in 
            case ts1 of
                (* FIXME: what about >> and >>> *)
                (GreaterThan, _) :: _ => (tl ts1, Ast.ApplyTypeExpr {expr=nd, actuals=nd1})
              | _ => error ["unknown final token of parametric type expression"]
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
*)

and brackets (ts:TOKENS)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> brackets with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBracket, _) :: ts' =>
            let
                val (ts1,nd1,_) = listExpression (ts',AllowIn)
            in 
                case ts1 of
                    (RightBracket, _) :: ts'' => (ts'',nd1)
                  | _ => error ["unknown token in brackets"]
            end
      | _ => error ["unknown token in brackets"]
    end

and bracketOrSlice (ts:TOKENS) (base:Ast.EXPR)
    : (TOKENS * Ast.EXPR) = 
    let val _ = trace([">> bracketOrSlice with next=",tokenname(hd(ts))])

        fun asBracket e = 
            Ast.ObjectRef
                { base=base,
                  ident=Ast.ExpressionIdentifier {expr = e, 
                                                  openNamespaces = []}, 
                  loc=locOf ts}

        fun asSlice a b c = 
            Ast.CallExpr 
            { func = Ast.ObjectRef { base = base,
                                     ident = Ast.QualifiedIdentifier 
                                                 { ident = Ustring.slice_,
                                                   qual = Ast.LiteralExpr 
                                                              (Ast.LiteralNamespace 
                                                                   (Ast.Intrinsic)) },
                                     loc = locOf ts },
              actuals = [ a, b, c ] }

        val none = Ast.ListExpr []

        fun slice2 ts nd1 nd2 = 
            case ts of 
                (RightBracket, _) :: ts' => (ts', asSlice nd1 nd2 none)
              | _ => 
                let
                    val (ts3, nd3, _) = listExpression (ts, AllowIn)
                in
                    case ts3 of 
                        (RightBracket, _) :: ts' => (ts', asSlice nd1 nd2 nd3)
                      | _ => error ["unknown token in slice"]                                                    
                end
                
        fun slice1 ts nd1 = 
            case ts of 
                (RightBracket, _) :: ts' => (ts', asSlice nd1 none none)
              | (Colon, _) :: ts' => slice2 ts' nd1 none
              | _ => 
                let
                    val (ts2,nd2,_) = listExpression (ts, AllowIn)
                in
                    case ts2 of 
                        (RightBracket, _) :: ts' => (ts', asSlice nd1 nd2 none)
                      | (Colon, _) :: ts' => slice2 ts' nd1 nd2
                      | _ => error ["unknown token in slice"]
                end
    in
        case ts of
            (LeftBracket, _) :: (Colon, _) :: ts' => slice1 ts' none 
          | (LeftBracket, _) :: (DoubleColon, x) :: ts' => slice1 ((Colon, x) :: ts') none 
          | (LeftBracket, _) :: ts' => 
            let
                val (ts1,nd1,_) = listExpression (ts',AllowIn)
            in 
                case ts1 of
                    (Colon, _) :: ts'' => slice1 ts'' nd1
                  | (DoubleColon, x) :: ts'' => slice1 ((Colon, x) :: ts'') nd1
                  | (RightBracket, _) :: ts'' => (ts'', asBracket nd1)
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

and leftHandSideExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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

and postfixExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> postfixExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = leftHandSideExpression(ts,a,b)
    in case ts1 of
        (PlusPlus, _) :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostIncrement,nd1))
      | (MinusMinus, _) :: ts2 => (ts2,Ast.UnaryExpr(Ast.PostDecrement,nd1))
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

and unaryExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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
                (ts2,Ast.UnaryExpr(Ast.PreIncrement,nd2))
            end
      | (MinusMinus, _) :: ts1 =>
            let
                val (ts2,nd2) = postfixExpression (ts1,a,b)
            in
                (ts2,Ast.UnaryExpr(Ast.PreDecrement,nd2))
            end
      | (Plus, _) :: ts1 =>
            let
                val (ts2,nd2) = unaryExpression (ts1,a,b)
            in
                (ts2,Ast.UnaryExpr(Ast.UnaryPlus,nd2))
            end
      | (Minus, _) :: ts1 =>
            let
                val (ts2,nd2) = unaryExpression (ts1,a,b)
            in
                (ts2,Ast.UnaryExpr(Ast.UnaryMinus,nd2))
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
            let
                fun specialTypeExpr () =
                    if newline (tl ts)
                    then
                        (tl ts,Ast.LexicalRef {ident=Ast.Identifier {ident=Ustring.type_,openNamespaces=[[]]},loc=NONE})
                    else
                        let
                            val (ts1,nd1) = nullableTypeExpression (tl ts)
                        in
                            (ts1,Ast.TypeExpr nd1)
                        end
            in
                (case (hd (tl ts)) of
                    (Null,         _) => specialTypeExpr ()
                  | (Undefined,    _) => specialTypeExpr ()
                  | (Identifier _, _) => specialTypeExpr ()
                  | (LeftParen,    _) => specialTypeExpr ()
                  | (LeftBrace,    _) => specialTypeExpr ()
                  | (LeftBracket,  _) => specialTypeExpr ()
                  | (Function,     _) => specialTypeExpr ()
                  | (Private,      _) => specialTypeExpr ()
                  | (Protected,    _) => specialTypeExpr ()
                  | (Public,       _) => specialTypeExpr ()
                  | (Internal,     _) => specialTypeExpr ()
                  | (Intrinsic,    _) => specialTypeExpr ()
                  | (Mult,         _) => specialTypeExpr ()
                  | _ =>
                        let
                        in
                            (tl ts,Ast.LexicalRef {ident=Ast.Identifier {ident=Ustring.type_,openNamespaces=[[]]},loc=NONE})
                        end)
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

and multiplicativeExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> multiplicativeExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = unaryExpression (ts,a,b)
        fun multiplicativeExpression' (ts1, nd1,a,b) =
            case ts1 of
                (Mult, _) :: ts2 =>
                    let
                        val (ts3,nd3) = unaryExpression (ts2,a,b)
                    in
                        multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Times,nd1,nd3),a,b)
                    end

              | (LexBreakDiv x,_) :: _ =>
                    let
                        val _ = trace ["hd ts1 = ", tokenname(hd ts1)]
                    in case (#lex_initial x)() of
                        (Div, _) :: ts2 =>
                            let
                                val (ts3,nd3) = unaryExpression (ts2,a,b)
                            in
                                multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Divide,nd1,nd3),a,b)
                            end
                      | (DivAssign, divAssignLoc) :: ts2 =>
                            (trace(["<< multiplicative"]);((DivAssign, divAssignLoc) :: ts2, nd1))
                      | _ => error ["missing token in multiplicativeExpression"]
                    end

              | (Modulus, _) :: ts2 =>
                    let
                        val (ts3,nd3) = unaryExpression (ts2,a,b)
                    in
                        multiplicativeExpression' (ts3,Ast.BinaryExpr(Ast.Remainder,nd1,nd3),a,b)
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

and additiveExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> additiveExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = multiplicativeExpression (ts,a,b)
        fun additiveExpression' (ts1, nd1,a,b) =
            case ts1 of
                (Plus, _) :: ts2 =>
                    let
                        val (ts3,nd3) = multiplicativeExpression (ts2,a,b)
                    in
                        additiveExpression' (ts3,Ast.BinaryExpr(Ast.Plus,nd1,nd3),a,b)
                    end
              | (Minus, _) :: ts2 =>
                    let
                        val (ts3,nd3) = multiplicativeExpression (ts2,a,b)
                    in
                        additiveExpression' (ts3,Ast.BinaryExpr(Ast.Minus,nd1,nd3),a,b)
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

and shiftExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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
    RelationalExpression(AllowIn)
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

    RelationalExpression(NoIn)
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

and relationalExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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
                                relationalExpression' (ts3,Ast.BinaryExpr(Ast.Less,nd1,nd3),a,AllowIn)
                            end
                      | _ => error ["unknown token in relationalExpression"]
                    end

              | ((LessThan, _) :: ts2,_) =>
                    let
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.Less,nd1,nd3),a,AllowIn)
                    end
              | ((GreaterThan, _) :: ts2,_) =>
                    let
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.Greater,nd1,nd3),a,AllowIn)
                    end
              | ((LessThanOrEquals, _) :: ts2, _) =>
                    let
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.LessOrEqual,nd1,nd3),a,AllowIn)
                    end
              | ((GreaterThanOrEquals, _) :: ts2, _) =>
                    let
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.GreaterOrEqual,nd1,nd3),a,AllowIn)
                    end
              | ((In, _) :: ts2, AllowIn) =>
                    let
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.In,nd1,nd3),a,AllowIn)
                    end
              | ((InstanceOf, _) :: ts2, _) =>
                    let
                        val (ts3,nd3) = shiftExpression (ts2,a,b)
                    in
                        relationalExpression' (ts3,Ast.BinaryExpr(Ast.InstanceOf,nd1,nd3),a,AllowIn)
                    end
              | ((Cast, _) :: ts2, _) =>
                    let
                        val (ts3,nd3) = typeExpression (ts2)
                    in
                        relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Cast,nd1,nd3),a,AllowIn)
                    end
              | ((To, _) :: ts2, _) =>
                    let
                        val (ts3,nd3) = typeExpression (ts2)
                    in
                        relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.To,nd1,nd3),a,AllowIn)
                    end
              | ((Is, _) :: ts2, _) =>
                    let
                        val (ts3,nd3) = typeExpression (ts2)
                    in
                        relationalExpression' (ts3,Ast.BinaryTypeExpr(Ast.Is,nd1,nd3),a,AllowIn)
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

and equalityExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> equalityExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = relationalExpression (ts,a,b)
        fun equalityExpression' (ts1,nd1) =
            case ts1 of
                (Equals, _) :: ts2 =>
                    let
                        val (ts3,nd3) = relationalExpression (ts2,a,b)
                    in
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.Equals,nd1,nd3))
                    end
              | (NotEquals, _) :: ts2 =>
                    let
                        val (ts3,nd3) = relationalExpression (ts2,a,b)
                    in
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.NotEquals,nd1,nd3))
                    end
              | (StrictEquals, _) :: ts2 =>
                    let
                        val (ts3,nd3) = relationalExpression (ts2,a,b)
                    in
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictEquals,nd1,nd3))
                    end
              | (StrictNotEquals, _) :: ts2 =>
                    let
                        val (ts3,nd3) = relationalExpression (ts2,a,b)
                    in
                        equalityExpression' (ts3,Ast.BinaryExpr(Ast.StrictNotEquals,nd1,nd3))
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

and bitwiseAndExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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

and bitwiseXorExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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

and bitwiseOrExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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

and logicalAndExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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

and logicalOrExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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
    ConditionalExpression(AllowList, b)
        LetExpression(b)
        YieldExpression(b)
        LogicalOrExpression(b)
        LogicalOrExpression(b)  ?  AssignmentExpression(AllowList,b)
                                   :  AssignmentExpression(AllowList,b)

    ConditionalExpression(NoList, b)
        SimpleYieldExpression
        LogicalOrExpression(b)
        LogicalOrExpression(b)  ?  AssignmentExpression(AllowList,b)
                                   :  AssignmentExpression(NoList,b)

*)

and conditionalExpression (ts:TOKENS, AllowList, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> conditionalExpression AllowList with next=",tokenname(hd(ts))])
    in case ts of
        (Let, _) :: _ => letExpression(ts,b)
      | (Yield, _) :: _ => yieldExpression(ts,b)
      | _ =>
            let
                val (ts2,nd2) = logicalOrExpression(ts,AllowList,b)
            in case ts2 of
                (QuestionMark, _) :: ts3 =>
                    let
                        val (ts4,nd4) = assignmentExpression(ts3,AllowList,b)
                    in case ts4 of
                        (Colon, _) :: ts5 =>
                            let
                                val (ts6,nd6) = assignmentExpression(ts5,AllowList,b)
                            in
                                (ts6, Ast.TernaryExpr (nd2, nd4, nd6))
                            end
                      | _ => error ["unknown token in conditionalExpression"]
                    end
              | _ =>
                    (trace(["<< conditionalExpression AllowList with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end

  | conditionalExpression (ts:TOKENS, NoList, b : BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> conditionalExpression NoList with next=",tokenname(hd(ts))])
    in case ts of
        (Yield, _) :: _ => simpleYieldExpression ts
      | _ =>
            let
                val (ts2,nd2) = logicalOrExpression(ts,NoList,b)
            in case ts2 of
                (QuestionMark, _) :: ts3 =>
                    let
                        val (ts4,nd4) = assignmentExpression(ts3,NoList,b)
                    in case ts4 of
                        (Colon, _) :: ts5 =>
                            let
                                val (ts6,nd6) = assignmentExpression(ts5,NoList,b)
                            in
                                (ts6, Ast.TernaryExpr (nd2, nd4, nd6))
                            end
                      | _ => error ["unknown token in conditionalExpression"]
                    end
              | _ =>
                    (trace(["<< conditionalExpression NoList with next=",tokenname(hd(ts2))]);
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

and nonAssignmentExpression (ts:TOKENS, AllowList, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> nonAssignmentExpression AllowList with next=",tokenname(hd(ts))])
    in case ts of
        (Let, _) :: _ => letExpression(ts,b)
      | (Yield, _) :: _ => yieldExpression(ts,b)
      | _ =>
            let
                val (ts2,nd2) = logicalOrExpression(ts,AllowList,b)
            in case ts2 of
                (QuestionMark, _) :: ts3 =>
                    let
                        val (ts4,nd4) = nonAssignmentExpression(ts3,AllowList,b)
                    in case ts4 of
                        (Colon, _) :: ts5 =>
                            let
                                val (ts6,nd6) = nonAssignmentExpression(ts5,AllowList,b)
                            in
                                (ts6,nd6)
                            end
                      | _ => error ["unknown token in nonAssignmentExpression"]
                    end
              | _ =>
                    (trace(["<< nonAssignmentExpression AllowList with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end

  | nonAssignmentExpression (ts:TOKENS, NoList, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> nonAssignmentExpression NoList with next=",tokenname(hd(ts))])
    in case ts of
        (Yield, _) :: _ => simpleYieldExpression ts
      | _ =>
            let
                val (ts2,nd2) = logicalOrExpression(ts,NoList,b)
            in case ts2 of
                (QuestionMark, _) :: ts3 =>
                    let
                        val (ts4,nd4) = nonAssignmentExpression(ts3,NoList,b)
                    in case ts4 of
                        (Colon, _) :: ts5 =>
                            let
                                val (ts6,nd6) = nonAssignmentExpression(ts5,NoList,b)
                            in
                                (ts6,nd6)
                            end
                      | _ => error ["unknown token in nonAssignmentExpression"]
                    end
              | _ =>
                    (trace(["<< nonAssignmentExpression NoList with next=",tokenname(hd(ts2))]);
                    (ts2,nd2))
            end
        end


(*
    LetExpression(b)
        let  (  LetBindingList  )  ListExpression(b)
*)

and letExpression (ts:TOKENS, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> letExpression with next=",tokenname(hd(ts))])
    in case ts of
        (Let, _) :: (LeftParen, _) :: ts1 =>
            let
                val (ts2,nd2) = letBindingList (ts1)
            in
            case ts2 of
                (RightParen, _) :: ts3 =>
                    let
                        val (ts4,nd4,_) = listExpression(ts3,b)
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

and letBindingList (ts:TOKENS)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> letBindingList with next=",tokenname(hd(ts))])
        fun nonemptyLetBindingList (ts) : (TOKENS * Ast.BINDINGS) =
            let
                val (ts1,(b1,i1)) = variableBinding (ts,NoList,AllowIn)
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

and yieldExpression (ts:TOKENS, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> yieldExpression with next=",tokenname(hd(ts))])
    in case ts of
        (Yield, _) :: ts1 =>
            let
            in case ts1 of
                (SemiColon,  _) :: _ => (ts1,Ast.YieldExpr NONE)
              | (RightBrace, _) :: _ => (ts1,Ast.YieldExpr NONE)
              | (RightParen, _) :: _ => (ts1,Ast.YieldExpr NONE)
              | _ =>
                    let
                        val (ts2,nd2,_) = listExpression(ts1,b)
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

and simpleYieldExpression (ts:TOKENS)
    : (TOKENS * Ast.EXPR) =
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

and assignmentExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
    let val _ = trace([">> assignmentExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = conditionalExpression(ts,a,b)
        fun compoundAssignment () =
            let
                val (ts2,nd2) = compoundAssignmentOperator ts1
                val (ts3,nd3) = assignmentExpression(tl ts1,a,b)
            in
                (ts3,Ast.SetExpr(nd2,nd1,nd3))
            end
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
                val (binds,inits) = desugarPattern (locOf ts) p (Ast.SpecialType Ast.Any) (SOME nd2) 0  
                                                                         (* type is meaningless *)
                val (inits,assigns) = List.partition isInitStep inits    (* separate init steps and assign steps *)
                val sets = map makeSetExpr assigns
            in case binds of
                [] => (ts2,hd sets)
              | _ => (ts2,Ast.LetExpr {defs=(binds,inits),
                                         body=Ast.ListExpr sets,
                                         head=NONE})
                        (* introduce a letexpr to narrow the scope of the temps *)
            end
          | (ModulusAssign, _) :: _ => compoundAssignment ()
          | (LogicalAndAssign, _) :: _ => compoundAssignment ()
          | (BitwiseAndAssign, _) :: _ => compoundAssignment ()
          | (DivAssign, _) :: _ => compoundAssignment ()
          | (BitwiseXorAssign, _) :: _ => compoundAssignment ()
          | (LogicalOrAssign, _) :: _ => compoundAssignment ()
          | (BitwiseOrAssign, _) :: _ => compoundAssignment ()
          | (PlusAssign, _) :: _ => compoundAssignment ()
          | (LeftShiftAssign, _) :: _ => compoundAssignment ()
          | (MinusAssign, _) :: _ => compoundAssignment ()
          | (RightShiftAssign, _) :: _ => compoundAssignment ()
          | (UnsignedRightShiftAssign, _) :: _ => compoundAssignment ()
          | (MultAssign, _) :: _ => compoundAssignment ()
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
            (ModulusAssign, _) :: _                 => (tl ts,Ast.AssignRemainder)
          | (LogicalAndAssign, _) :: _             => (tl ts,Ast.AssignLogicalAnd)
          | (BitwiseAndAssign, _) :: _             => (tl ts,Ast.AssignBitwiseAnd)
          | (DivAssign, _) :: _                     => (tl ts,Ast.AssignDivide)
          | (BitwiseXorAssign, _) :: _             => (tl ts,Ast.AssignBitwiseXor)
          | (LogicalOrAssign, _) :: _             => (tl ts,Ast.AssignLogicalOr)
          | (BitwiseOrAssign, _) :: _             => (tl ts,Ast.AssignBitwiseOr)
          | (PlusAssign, _) :: _                 => (tl ts,Ast.AssignPlus)
          | (LeftShiftAssign, _) :: _             => (tl ts,Ast.AssignLeftShift)
          | (MinusAssign, _) :: _                 => (tl ts,Ast.AssignMinus)
          | (RightShiftAssign, _) :: _               => (tl ts,Ast.AssignRightShift)
          | (UnsignedRightShiftAssign, _) :: _     => (tl ts,Ast.AssignRightShiftUnsigned)
          | (MultAssign, _) :: _                   => (tl ts,Ast.AssignTimes)
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

and listExpression (ts:TOKENS, b:BETA)
    : (TOKENS * Ast.EXPR * (Ast.LOC option)) =
    let
        val _ =    trace([">> listExpression with next=",tokenname(hd ts)])
        fun listExpression' (ts,b) : (TOKENS * Ast.EXPR list) =
            let
                val _ =    trace([">> listExpression' with next=",tokenname(hd ts)])
            in case ts of
                (Comma, _) :: _ =>
                    let
                        val (ts1,nd1) = assignmentExpression(tl ts,AllowList,b)
                        val (ts2,nd2) = listExpression'(ts1,b)
                    in
                        (trace(["<< listExpression' with next=",tokenname(hd(ts2))]);
                        (ts2, nd1 :: nd2))
                    end
              | _ =>
                    (trace(["<< listExpression' with next=",tokenname(hd(ts))]);
                    (ts, []))
            end
        val (ts1,nd1) = assignmentExpression(ts,AllowList,b)
        val (ts2,nd2) = listExpression'(ts1,b)
        val startLoc  = #2 (hd ts )
        val endLoc    = #2 (hd ts2)
        val listLoc   = unionLocExcludingLast (SOME startLoc) (SOME endLoc)
    in
        trace(["<< listExpression with next=",tokenname(hd(ts2))]);
        (ts2, Ast.ListExpr (nd1 :: nd2), listLoc)
    end

(*
    Pattern(a, b, g)
        SimplePattern(a, b, g)
        ObjectPattern(g)
        ArrayPattern(g)
*)

and pattern (ts:TOKENS, a:ALPHA, b:BETA, g:GAMMA)
    : (TOKENS * PATTERN) =
    let
    in case ts of
        (LeftBrace, _) :: _ => objectPattern (ts,g)
      | (LeftBracket, _) :: _ => arrayPattern (ts,g)
      | _ => simplePattern (ts,a,b,g)
    end

and patternFromExpr (e:Ast.EXPR)
    : PATTERN =
    let val _ = trace([">> patternFromExpr"])
    in case e of
        Ast.LiteralExpr (Ast.LiteralObject {...}) => objectPatternFromExpr (e)
      | Ast.LiteralExpr (Ast.LiteralArray {...}) => arrayPatternFromExpr (e)
      | _ => simplePatternFromExpr (e)
    end

and patternFromListExpr (Ast.ListExpr (e::[])) : PATTERN = patternFromExpr e
  | patternFromListExpr (_)  = (error(["invalid pattern"]); error ["unknown token in patternFromListExpr"])

(*
    SimplePattern(a, b, noExpr)
        Identifier

    SimplePattern(a, b, allowExpr)
        PostfixExpression(a, b)
*)

and simplePattern (ts:TOKENS, a:ALPHA, b:BETA, NoExpr) =
    let val _ = trace([">> simplePattern(a,b,NOEXPR) with next=",tokenname(hd(ts))])
        val (ts1,nd1) = identifier ts
    in
        trace(["<< simplePattern(a,b,NOEXPR) with next=",tokenname(hd(ts1))]);
        (ts1,IdentifierPattern nd1)
    end
  | simplePattern (ts:TOKENS, a:ALPHA, b:BETA, AllowExpr) =
    let val _ = trace([">> simplePattern(a,b,AllowExpr) with next=",tokenname(hd(ts))])
        val (ts1,nd1) = leftHandSideExpression (ts,a,b)
    in
        (trace(["<< simplePattern(a,b,AllowExpr) with next=",tokenname(hd(ts1))]);
        (ts1,SimplePattern nd1))
    end

and simplePatternFromExpr (e:Ast.EXPR)
    : PATTERN =  (* only ever called from AllowExpr contexts *)
    let val _ = trace([">> simplePatternFromExpr"])
    in case e of
        Ast.ObjectRef _ => (trace(["<< simplePatternFromExpr"]); SimplePattern e)
      | Ast.LexicalRef _ => (trace(["<< simplePatternFromExpr"]); SimplePattern e)
      | _ =>
            (error(["invalid pattern expression"]);
            error ["unknown token in simplePatternFromExpr"])
    end

(*
    ObjectPattern(g)
        {  DestructuringFieldList(g)  }
*)

and objectPattern (ts:TOKENS, g:GAMMA)
    : (TOKENS * PATTERN) =
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

and objectPatternFromExpr (e:Ast.EXPR)
    : PATTERN =
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

and destructuringFieldList (ts:TOKENS, g:GAMMA)
    : (TOKENS * (FIELD_PATTERN list))=
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

and destructuringFieldListFromExpr (e:Ast.FIELD list)
    : (FIELD_PATTERN list) =
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
        NonAttributeQualifiedIdentifier  :  Pattern(NoList,AllowIn,g)
*)

and destructuringField (ts:TOKENS, g:GAMMA)
    : (TOKENS * FIELD_PATTERN) =
    let
        val (ts1,nd1) = fieldName ts
    in case ts1 of
        (Colon, _) :: _ =>
            let
                val (ts2,nd2) = pattern (tl ts1,NoList,AllowIn,g)
            in
                (ts2,{ident=nd1,pattern=nd2})
            end
      | _ => error ["unknown token in destructuringField"]
    end

and destructuringFieldFromExpr (e:Ast.FIELD)
    : FIELD_PATTERN =
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

and arrayPattern (ts:TOKENS, g:GAMMA) :
  (TOKENS * PATTERN) =
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

and arrayPatternFromExpr (e:Ast.EXPR)
    : PATTERN =
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

and destructuringElementList (ts:TOKENS, g:GAMMA)
    : (TOKENS * (PATTERN list)) =
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

and destructuringElementListFromExpr (e:Ast.EXPR list)
    : (PATTERN list) =
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
        Pattern(NoList,AllowIn,g)
*)

and destructuringElement (ts:TOKENS, g:GAMMA)
    : (TOKENS * PATTERN) =
    let val _ = trace([">> destructuringElement with next=",tokenname(hd(ts))])
    in
        pattern (ts,NoList,AllowIn,g)
    end

and destructuringElementFromExpr (e:Ast.EXPR)
    : PATTERN =
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

and typedIdentifier (ts:TOKENS)
    : TOKENS * (PATTERN * Ast.TYPE_EXPR) =
    let val _ = trace([">> typedIdentifier with next=",tokenname(hd(ts))])
        val (ts1,nd1) = simplePattern (ts,NoList,NoIn,NoExpr)
    in case ts1 of
        (Colon, _) :: _ =>
            let
                val (ts2,nd2) = nullableTypeExpression (tl ts1)
            in
                (ts2,(nd1,unwrapTy nd2))
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

and typedPattern (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * (PATTERN * Ast.TYPE_EXPR)) =
    let val _ = trace([">> typedPattern with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = objectPattern (ts,NoExpr)
            in case ts1 of
                (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in
                        (ts2,(nd1,unwrapTy nd2))
                    end
              | _ =>
                    (ts1,(nd1,Ast.SpecialType Ast.Any))  (* FIXME: this could be {*:*} to be more specific *)
            end
      | (LeftBracket, _) :: _ =>
            let
                val (ts1,nd1) = arrayPattern (ts,NoExpr)
            in case ts1 of
                (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in
                        (ts2,(nd1,unwrapTy nd2))
                    end
              | _ =>
                    (ts1,(nd1,Ast.ArrayType []))
            end
      | _ =>
            let
                val (ts1,nd1) = simplePattern (ts,a,b,NoExpr)
            in case ts1 of
                (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = nullableTypeExpression (tl ts1)
                    in
                        (ts2,(nd1,unwrapTy nd2))
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

and nullableTypeExpression (ts:TOKENS)
    : (TOKENS * Ast.TY) =
    let val _ = trace([">> nullableTypeExpression with next=",tokenname(hd ts)])
        val (ts1,nd1) = typeExpression ts
    in case ts1 of
        (Not, _) :: _ =>
            (tl ts1,makeTy (Ast.NullableType {expr=unwrapTy nd1,nullable=false}))
      | (QuestionMark, _) :: _ =>
            (tl ts1,makeTy (Ast.NullableType {expr=unwrapTy nd1,nullable=true}))
      | _ =>
            (ts1,nd1)
    end

and typeExpression (ts:TOKENS)
    : (TOKENS * Ast.TY) =
    let val _ = trace([">> typeExpression with next=",tokenname(hd ts)])
    in case ts of
        (Function, _) :: _ => functionType ts
      | (LeftParen, _) :: _ => unionType ts
      | (LeftBrace, _) :: _ => objectType ts
      | (LeftBracket, _) :: _ => arrayType ts
      | (Null, _) :: _ => (tl ts, makeTy (Ast.SpecialType Ast.Null))
      | (Undefined, _) :: _ => (tl ts, makeTy (Ast.SpecialType Ast.Undefined))
      | _ =>
            let
                val (ts1,nd1) = primaryIdentifier ts                                
            in
                case ts1 of 
                    (LeftDotAngle, _) :: _ => 
                    let
                        val (ts2,nd2) = typeExpressionList (tl ts1)
                    in case ts2 of
                           (GreaterThan, _) :: _ =>   (* FIXME: what about >> and >>> *)
                           (tl ts2, makeTy (Ast.AppType { base = needType(nd1,NONE),
                                                          args = (map AstQuery.typeExprOf nd2) }))
                         | _ => error ["unknown final token of AppType type expression"]
                    end
                  | _ => (ts1,makeTy (needType(nd1,NONE)))
            end
    end

(*
    FunctionType
        function  FunctionSignature
*)

and functionType (ts:TOKENS)
    : (TOKENS * Ast.TY) =
    let val _ = trace([">> functionType with next=",tokenname(hd(ts))])
    in case ts of
        (Function, _) :: _ =>
            let
                val (ts1,nd1) = functionSignatureType (tl ts)
            in
                trace(["<< functionType with next=",tokenname(hd ts1)]);
                (ts1, (functionTypeFromSignature nd1))
            end
      | _ => error ["unknown token in functionType"]
    end


and functionTypeFromSignature (fsig:Ast.FUNC_SIG)
    : Ast.TY =
    let
        val Ast.FunctionSignature {typeParams,
                                   params,
                                   paramTypes,
                                   returnType,
                                   thisType,
                                   hasRest,
                                   defaults,...} = fsig
        val (b,i) = params
        val typeExpr = Ast.FunctionType {params=paramTypes,
                                         result=returnType,
                                         thisType=thisType,
                                         hasRest=hasRest,
                                         minArgs=(length paramTypes)-(length defaults)}
    in
        case typeParams of 
            [] => makeTy typeExpr
          | _ => makeTy (Ast.LamType { params = typeParams,
                                       body = typeExpr })
    end

(*
    UnionType
        (  TypeExpressionList  )
*)

and unionType (ts:TOKENS)
    : (TOKENS * Ast.TY)  =
    let val _ = trace([">> unionType with next=",tokenname(hd(ts))])
    in case ts of
        (LeftParen, _) :: _ =>
            let
                val (ts1,nd1) = typeExpressionList (tl ts)
            in case ts1 of
                (RightParen, _) :: _ =>
                    (tl ts1, makeTy (Ast.UnionType (map unwrapTy nd1)))
              | _ => error ["unknown token in unionType"]
            end
      | _ => error ["unknown token in unionType"]
    end

(*
    ObjectType
        {  FieldTypeList  }
*)

and objectType (ts:TOKENS)
    : (TOKENS * Ast.TY) =
    let val _ = trace([">> objectType with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBrace, _) :: ts1 =>
            let
                val (ts2,nd2) = fieldTypeList ts1
            in case ts2 of
                (RightBrace, _) :: ts3 =>
                    (trace(["<< objectType with next=",tokenname(hd(ts3))]);
                    (ts3, makeTy (Ast.ObjectType nd2)))
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

and fieldTypeList (ts:TOKENS)
    : (TOKENS * (Ast.FIELD_TYPE list)) =
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

and fieldType (ts:TOKENS)
    : (TOKENS * Ast.FIELD_TYPE) =
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
                (ts2,{name=ident,ty=unwrapTy nd2})
            end
      | _ => error ["unknown token in fieldType"]
    end

(*
    ArrayType
        [  ElementTypeList  ]
*)

and arrayType (ts:TOKENS)
    : (TOKENS * Ast.TY) =
    let val _ = trace([">> arrayType with next=",tokenname(hd(ts))])
    in case ts of
        (LeftBracket, _) :: _ =>
            let
                val (ts1,nd1) = elementTypeList (tl ts)
            in case ts1 of
                (RightBracket, _) :: _ => (tl ts1, makeTy (Ast.ArrayType nd1))
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

and elementTypeList (ts:TOKENS)
    : (TOKENS * Ast.TYPE_EXPR list) =
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
                        (ts2,(unwrapTy nd1)::nd2)
                    end
              | _ => (ts1,(unwrapTy nd1)::[])
            end
    end

(*
    TypeExpressionList
        TypeExpression
        TypeExpressionList  ,  TypeExpression
*)

and typeExpressionList (ts:TOKENS)
    : (TOKENS * Ast.TY list) =
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

and semicolon (ts,Full)
    : (TOKENS) =
    let val _ = trace([">> semicolon(Full) with next=", tokenname(hd ts)])
    in case ts of
        (SemiColon, _) :: _ => (tl ts)
      (* Abbrev special cases *)
      | (Eof,_) :: _ => (ts)
      | (RightBrace,_) :: _ => (ts)
      | _ =>
            if newline ts then (trace ["inserting semicolon"]; ts)
            else (error(["expecting semicolon before ",tokenname(hd ts)]); error ["unknown token in semicolon"])
    end
  | semicolon (ts,_) =
    let val _ = trace([">> semicolon(Abbrev | NoShortIf) with next=", tokenname(hd ts)])
    in case ts of
        (SemiColon, _) :: _ => (tl ts)
      | _ =>
          (trace(["<< semicolon(Abbrev | NoShortIf) with next=", tokenname(hd ts)]);
          (ts))
    end

and statement (ts:TOKENS, t:TAU, w:OMEGA)
    : (TOKENS * Ast.STMT) =
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

and substatement (ts:TOKENS, w:OMEGA)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> substatement with next=", tokenname(hd ts)])
    in case ts of
        (SemiColon, _) :: _ =>
            (tl ts, Ast.EmptyStmt)
      | _ =>
            statement(ts,LocalScope,w)
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

and emptyStatement (ts:TOKENS) :
  (TOKENS * Ast.STMT) =
    let
    in case ts of
        (SemiColon, _) :: ts1 => (ts1,Ast.EmptyStmt)
      | _ => error ["unknown token in emptyStatement"]
    end

(*
    BlockStatement
        Block
*)

and blockStatement (ts:TOKENS, t:TAU)
    : (TOKENS * Ast.STMT) =
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

and labeledStatement (ts:TOKENS, w:OMEGA)
    : (TOKENS * Ast.STMT) =
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

and expressionStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> expressionStatement with next=", tokenname(hd ts)])
        val (ts1,nd1,_) = listExpression(ts,AllowIn)
    in
        trace(["<< expressionStatement with next=", tokenname(hd ts1)]);
        (ts1,Ast.ExprStmt(nd1))
    end

(*
    WithStatement(w)
        with  TypedExpression  Substatement(w)
*)

and withStatement (ts:TOKENS, w:OMEGA)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> withStatement with next=", tokenname(hd ts)])
    in case ts of
        (With, _) :: (LeftParen, _) :: _ =>
            let
                val (ts1,(e1,t1)) = typedExpression (tl ts)
                val (ts2,nd2) = substatement (ts1,w)
            in
                (ts2,Ast.WithStmt {obj=e1,ty=makeTy t1,body=nd2})
            end
      | _ => error ["unknown token in withStatement"]
    end

(*
    TypedExpression
        ParenListExpression
        ParenListExpression  :  NullableTypeExpression
*)

and typedExpression (ts:TOKENS)
    : (TOKENS * (Ast.EXPR*Ast.TYPE_EXPR)) =
    let val _ = trace([">> parenListExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = parenListExpression (ts)
    in case ts1 of
        (Colon, _) :: _ =>
            let
                val (ts2,nd2) = nullableTypeExpression (tl ts1)
            in
                (ts2,(nd1,unwrapTy nd2))
            end
      | _ => (ts1,(nd1,Ast.SpecialType Ast.Any))
    end


(*
    SwitchStatement
        switch  ParenListExpression  {  CaseElements  }
        switch  type  TypedExpression  {  TypeCaseElements  }

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

and switchStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> switchStatement with next=", tokenname(hd ts)])
    in case ts of
        (Switch, _) :: (Type, _) :: _ =>
            let
                val (ts1,(e1,t1)) = typedExpression (tl (tl ts))
            in case ts1 of
                (LeftBrace, _) :: _ =>
                    let
                        val (ts2,nd2) = typeCaseElements (tl ts1)
                    in case ts2 of
                        (RightBrace, _) :: _ => (tl ts2,Ast.SwitchTypeStmt{cond=e1,ty=makeTy t1,cases=nd2})
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
                        (RightBrace, _) :: _ => (tl ts2,Ast.SwitchStmt{cond=nd1,cases=nd2,labels=[]})
                      | _ => error ["unknown token in switchStatement"]
                    end
              | _ => error ["unknown token in switchStatement"]
            end
      | _ => error ["unknown token in switchStatement"]
    end

and isDefaultCase (x:Ast.EXPR option) =
    case x of
        NONE => true
      | _ => false

and caseElements (ts:TOKENS)
    : (TOKENS * Ast.CASE list) =
    let val _ = trace([">> caseElements with next=", tokenname(hd ts)])
        fun nextCase () =
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
    in case ts of
        (RightBrace, _) :: _ =>
            (tl ts,[])
      | (Case, _) :: _ => nextCase ()
      | (Default, _) :: _ => nextCase ()
      | _ => error ["unknown token in caseElements"]
    end

and caseElementsPrefix (ts:TOKENS, has_default:bool)
    : (TOKENS * Ast.CASE list) =
    let val _ = trace([">> caseElementsPrefix with next=", tokenname(hd ts)])
        fun caseOrDefault () =
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
    in case ts of
        (RightBrace, _) :: _ =>
            (ts,[])
      | (Case, _) :: _ => caseOrDefault ()
      | (Default, _) :: _ => caseOrDefault ()
      | _ =>
            let
                val (ts1,nd1) = directive (ts,LocalScope,Full)
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

and caseLabel (ts:TOKENS, has_default:bool)
    : (TOKENS * Ast.EXPR option) =
    let val _ = trace([">> caseLabel with next=", tokenname(hd ts)])
    in case (ts,has_default) of
        ((Case, _) :: _,_) =>
            let
                val (ts1,nd1,_) = listExpression (tl ts,AllowIn)
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

and typeCaseBinding (ts:TOKENS)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> caseCaseBinding with next=", tokenname(hd ts)])
    in case ts of
        (LeftParen, _) :: _ =>
            let
                val (ts1,(p,t)) = typedIdentifier (tl ts)
            in case ts1 of
                (Assign, _) :: _ =>
                    let
                        val (ts2,nd2) = variableInitialisation(ts1,AllowList,AllowIn)
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

and isDefaultTypeCase (x:Ast.TYPE_EXPR) 
    : bool =
    case x of
        Ast.SpecialType Ast.Any => true
      | _ => false

and typeCaseElements (ts:TOKENS)
    : (TOKENS * Ast.CATCH_CLAUSE list) =
    let val _ = trace([">> typeCaseElements with next=", tokenname(hd ts)])
        fun typeCaseElements' (ts,has_default) : (TOKENS * Ast.CATCH_CLAUSE list) =
            let val _ = trace([">> typeCaseElements' with next=", tokenname(hd ts)])
                fun nextCase () =
                    let
                        val (ts1,nd1) = typeCaseElement (ts,has_default)
                        val (ts2,nd2) = typeCaseElements' (ts1,has_default orelse 
                                                               (isDefaultTypeCase (unwrapTy (#ty nd1))))
                    in
                        trace(["<< typeCaseElements' with next=", tokenname(hd ts2)]);
                        (ts2,nd1::nd2)
                    end
            in case ts of
                (Case, _) :: _ => nextCase ()
              | (Default, _) :: _ => nextCase ()
              | _ => (ts,[])
            end
        val (ts1,nd1) = typeCaseElement (ts,false)
        val (ts2,nd2) = typeCaseElements' (ts1,isDefaultTypeCase (unwrapTy (#ty nd1)))
    in
        trace(["<< typeCaseElements with next=", tokenname(hd ts2)]);
        (ts2,nd1::nd2)
    end

and typeCaseElement (ts:TOKENS, has_default:bool)
    : (TOKENS * Ast.CATCH_CLAUSE) =
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
                       val (ts2,nd2) = block (tl ts1,LocalScope)
                   in
                       (ts2,{bindings=((temp::b),i),
                             ty=(makeTy ty),
                             block=nd2,
                             rib=NONE,
                             inits=NONE})
                   end
                 | _ => error ["unknown token in typeCaseElement"]
           end
         | ((Default, _) :: _,false) =>
           let
               val (ts1,nd1) = block (tl ts,LocalScope)
           in
               trace(["<< typeCaseElement with next=", tokenname(hd ts1)]);
               (ts1,{bindings=([],[]),
                     ty=(makeTy (Ast.SpecialType Ast.Any)),
                     block=nd1,
                     rib=NONE,
                     inits=NONE})
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

and ifStatement (ts:TOKENS, Abbrev) :
    (TOKENS * Ast.STMT) =
    let val _ = trace([">> ifStatement(Abbrev) with next=", tokenname(hd ts)])
    in case ts of
        (If, _) :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1,Abbrev)
            in case ts2 of
                (Else, _) :: _ =>
                    let
                        val (ts3,nd3) = substatement(tl ts2,Abbrev)
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
  | ifStatement (ts,Full) =
    let val _ = trace([">> ifStatement(Full) with next=", tokenname(hd ts)])
    in case ts of
        (If, _) :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1,Full)
            in case ts2 of
                (Else, _) :: _ =>
                    let
                        val (ts3,nd3) = substatement(tl ts2,Full)
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
                val (ts2,nd2) = substatement(ts1,NoShortIf)
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

and doStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> doStatement with next=", tokenname(hd ts)])
    in case ts of
        (Do, _) :: _ =>
            let
                val (ts1,nd1) = substatement(tl ts, Abbrev)
            in case ts1 of
                (While, _) :: _ =>
                    let
                        val (ts2,nd2) = parenListExpression (tl ts1)
                    in
                        (ts2,Ast.DoWhileStmt {body=nd1,cond=nd2,rib=NONE,labels=[]})
                    end
              | _ => error ["unknown token in doStatement"]
            end
      | _ => error ["unknown token in doStatement"]
    end

(*
    WhileStatement(w)
        while ParenListExpression Substatement(w)
*)

and whileStatement (ts:TOKENS, w:OMEGA)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> whileStatement with next=", tokenname(hd ts)])
    in case ts of
        (While, _) :: _ =>
            let
                val (ts1,nd1) = parenListExpression (tl ts)
                val (ts2,nd2) = substatement(ts1, w)
            in
                (ts2,Ast.WhileStmt {cond=nd1,rib=NONE,body=nd2,labels=[]})
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

and forStatement (ts:TOKENS, w:OMEGA)
    : (TOKENS * Ast.STMT) =
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
                                    rib=NONE,
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
                val (ts2,nd2,_) = listExpression (tl ts1,AllowIn)
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
                                     rib=NONE,
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

and forInitialiser (ts:TOKENS)
    : (TOKENS * Ast.VAR_DEFN option * Ast.STMT list) =
    let val _ = trace([">> forInitialiser with next=", tokenname(hd ts)])
        fun declaration () =
            let
                val (ts1,{defns,body,...}) = variableDefinition (ts,NONE,
                                                    false,false,NoIn,LocalScope)
            in case (defns) of
                (Ast.VariableDefn vd :: []) =>
                    (trace(["<< forInitialiser with next=", tokenname(hd ts1)]);
                    (ts1,SOME vd,body))
              | _ => error ["unknown token in forInitialiser"]
            end
    in case ts of
        (Var,   _) :: _ => declaration ()
      | (Let,   _) :: _ => declaration ()
      | (Const, _) :: _ => declaration ()
      | (SemiColon, _) :: _ =>
            let
            in
                trace(["<< forInitialiser with next=", tokenname(hd ts)]);
                (ts,NONE,[Ast.ExprStmt (Ast.ListExpr [])])
            end
      | _ =>
            let
                val (ts1,nd1,_) = listExpression (ts,NoIn)
            in
                trace ["<< forInitialiser with next=", tokenname(hd ts1)];
                (ts1,NONE,[Ast.ExprStmt nd1])
            end
    end

and optionalExpression (ts:TOKENS)
    : (TOKENS * Ast.EXPR) =
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
                val (ts1,nd1,_) = listExpression (ts,NoIn)
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

and forInBinding (ts:TOKENS)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> forInBinding with next=", tokenname(hd ts)])
        fun declaration () =
            let
                val (ts1,nd1) = variableDefinitionKind ts
                val (ts2,(b,i)) = variableBinding (ts1,AllowList,NoIn)
            in
                trace ["<< forInBinding with next=", tokenname(hd ts1)];
                (ts2,(b,i))
            end
    in case ts of
        (Var,   _) :: _ => declaration ()
      | (Let,   _) :: _ => declaration ()
      | (Const, _) :: _ => declaration ()
      | _ =>
            let
                val (ts1,nd1) = pattern (ts,AllowList,NoIn,AllowExpr)
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

and letStatement (ts:TOKENS, w:OMEGA)
    : (TOKENS * Ast.STMT) =
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
                (RightParen, _) :: (LeftBrace,_) :: _ =>
                    let
                        val (ts2,nd2) = block (tl ts1, LocalScope)
                    in
                        trace(["<< letStatement with next=",tokenname(hd(ts2))]);
                        (ts2,Ast.LetStmt (Ast.Block {pragmas=[],defns=[defn],head=NONE,body=[Ast.BlockStmt nd2],loc=locOf ts1}) )
                    end
              | (RightParen, _) :: _ => (* oops, actually a LetExpr in statement position *)
                    let
                        val (ts2,nd2,_) = listExpression(tl ts1, AllowIn)
                    in
                        (trace(["<< letExpression with next=",tokenname(hd(ts2))]);
                        (ts2,Ast.ExprStmt (Ast.LetExpr {defs=nd1,body=nd2,head=NONE})))
                    end
               |    _ => error ["unknown token in letStatement"]
            end
      | _ => error ["unknown token in letStatement"]
    end

(*
    ContinueStatement
        continue
        continue [no line break] Identifier
*)

and continueStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let
    in case ts of
        (Continue, _) :: (SemiColon, _) :: _ => (tl ts,Ast.ContinueStmt NONE)
      | (Continue, _) :: (RightBrace, _) :: _ => (tl ts,Ast.ContinueStmt NONE)
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

and breakStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> breakStatement with next=", tokenname(hd ts)])
    in case ts of
        (Break, _) :: (SemiColon, _) :: _ => (tl ts,Ast.BreakStmt NONE)
      | (Break, _) :: (RightBrace, _) :: _ => (tl ts,Ast.BreakStmt NONE)
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

and returnStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let
    in case ts of
        (Return, _) :: (SemiColon, _) :: _ => (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
      | (Return, _) :: (RightBrace, _) :: _ => (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
      | (Return, _) :: _ =>
            if newline(tl ts) then
                (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
            else
                let
                    val (ts1,nd1,_) = listExpression(tl ts, AllowIn)
                in
                    (ts1,Ast.ReturnStmt nd1)
                end
      | _ => error ["unknown token in returnStatement"]
    end

(*
    ThrowStatement
        throw  ListExpression(allowIn)
*)

and throwStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let
    in case ts of
        (Throw, _) :: _ =>
            let
                val (ts1,nd1,_) = listExpression(tl ts, AllowIn)
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

and tryStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> tryStatement with next=", tokenname(hd ts)])
    in case ts of
        (Try, _) :: _ =>
            let
                val (ts1,nd1) = block(tl ts,LocalScope)
            in case ts1 of
                (Finally, _) :: _ =>
                    let
                        val (ts2,nd2) = block (tl ts1,LocalScope)
                    in
                        (ts2,Ast.TryStmt {block=nd1,catches=[],finally=SOME nd2})
                    end
              | _ =>
                    let
                        val (ts2,nd2) = catchClauses ts1
                    in case ts2 of
                        (Finally, _) :: _ =>
                            let
                                val (ts3,nd3) = block (tl ts2,LocalScope)
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

and catchClauses (ts:TOKENS)
    : (TOKENS * {bindings:Ast.BINDINGS,
                 ty:Ast.TY,
                 rib:Ast.RIB option,
                 inits:Ast.INITS option,
                 block:Ast.BLOCK} list) =
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

and catchClause (ts:TOKENS)
    : (TOKENS * {bindings:Ast.BINDINGS,
                 ty:Ast.TY,
                 rib:Ast.RIB option,
                 inits:Ast.INITS option,
                 block:Ast.BLOCK}) =
    let val _ = trace([">> catchClause with next=", tokenname(hd ts)])
    in case ts of
        (Catch, _) :: (LeftParen, _) :: _ =>
            let
                val (ts1,(temp,{pattern,ty})) = parameter (tl (tl ts)) 0
                val (b,i) = desugarPattern (locOf ts) pattern ty (SOME (Ast.GetParam 0)) 0
            in case ts1 of
                (RightParen, _) :: _ =>
                    let
                        val (ts2,nd2) = block (tl ts1,LocalScope)
                    in
                        (ts2,{bindings=((temp::b),i),
                              ty=(makeTy ty),
                              block=nd2,
                              rib=NONE,
                              inits=NONE})
                    end
              | _ => error ["unknown token in catchClause"]
            end
      | _ => error ["unknown token in catchClause"]
    end

(*
    DefaultXMLNamespaceStatement
        default  xml  namespace = NonAssignmentExpression(allowList, allowIn)
*)

and defaultXmlNamespaceStatement (ts:TOKENS)
    : (TOKENS * Ast.STMT) =
    let val _ = trace([">> defaultXmlNamespaceStatement with next=", tokenname(hd ts)])
    in case ts of
        (Default, _) :: (Xml, _) :: (Namespace, _) :: (Assign, _) :: _ =>
            let
                val (ts1,nd1) = nonAssignmentExpression ((tl (tl (tl (tl ts)))),AllowList,AllowIn)
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

and directives (ts:TOKENS, t:TAU)
    : (TOKENS * Ast.DIRECTIVES) =
    let
        val _ = setLoc ts
        val _ = trace([">> directives with next=", tokenname(hd ts)])
    in case ts of
        (RightBrace, _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
      | (Eof, _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
      | _ =>
            let
                val (ts1,nd1) = directivesPrefix (ts,t)
(*                val (ts2,nd2) = directive(ts1,t,Abbrev)

todo: the trailing directive(abbrev) is parsed by directivesPrefix.
      the semicolon(full) parser checks for the abbrev case and acts
      like semicolon(abbrev) if needed. clarify this in the code.
*)
            in
                trace(["<< directives with next=", tokenname(hd ts1)]);
                (ts1,nd1)
            end
    end

and directivesPrefix (ts:TOKENS, t:TAU)
    : (TOKENS * Ast.DIRECTIVES) =
    let
        val _ = setLoc ts
        val _ = trace([">> directivesPrefix with next=", tokenname(hd ts)])
        fun directivesPrefix' (ts,t) : (TOKENS * Ast.DIRECTIVES) =
            let val _ = trace([">> directivesPrefix' with next=", tokenname(hd ts)])
            in case ts of
                (RightBrace, _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
              | (Eof, _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
              | _ =>
                    let
                        val (ts1,{pragmas=p1,defns=d1,body=s1,...}) = directive (ts,t,Full)
                        val (ts2,{pragmas=p2,defns=d2,body=s2,...}) = directivesPrefix' (ts1,t)
                    in
                        trace(["<< directivesPrefix' with next=", tokenname(hd ts2)]);
                        (ts2,{pragmas=(p1@p2),defns=(d1@d2),body=(s1@s2),head=NONE,loc=locOf ts})
                    end
            end
        fun nextDirective () =
            let
                val (ts1,nd1) = pragmas ts
                val (ts2,{pragmas=p2,defns=d2,body=s2,...}) = directivesPrefix' (ts1,t)
            in
                trace(["<< directivesPrefix with next=", tokenname(hd ts2)]);
                (ts2, {pragmas=nd1,defns=d2,body=s2,head=NONE,loc=locOf ts})
            end
    in case ts of
        (RightBrace, _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
      | (Eof, _) :: _ => (ts,{pragmas=[],defns=[],body=[],head=NONE,loc=locOf ts})
      | (Use, _) :: _ => nextDirective ()
      | (Import, _) :: _ => nextDirective ()
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


and directive (ts:TOKENS, t:TAU, w:OMEGA)
    : (TOKENS * Ast.DIRECTIVES) =
    let
        val _ = setLoc ts
        val _ = trace([">> directive with next=", tokenname(hd ts)])
        fun nonAttributePrototype () =
            let
                val (ts1,nd1) = statement (ts,LocalScope,w)
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE,loc=locOf ts})
            end
        fun declaration () =
            let
                val (ts1,nd1) = annotatableDirective (ts,defaultAttrs,t,w)
            in
                (ts1,nd1)
            end
        fun withAttribute () =
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
        fun default () =
            let
                val (ts1,nd1) = statement (ts,t,w)
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE,loc=locOf ts})
            end
        fun withName () =
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
        fun maybeWithName next =
            (case next of
                 Dynamic => withName ()
               | Final => withName ()
               | Native => withName ()
               | Override => withName ()
               | Prototype => withName ()
               | Static => withName ()
               | Var => withName ()
               | Let => withName ()
               | Const => withName ()
               | Function => withName ()
               | Class => withName ()
               | Interface => withName ()
               | Namespace => withName ()
               | Type => withName ()
               | _ => default ())
    in case ts of
        (SemiColon, _) :: _ =>
            let
                val (ts1,nd1) = emptyStatement ts
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE,loc=locOf ts})
            end
      | (Let, _) :: (LeftParen, _) :: _  => (* dispatch let statement before let var *)
            let
                val (ts1,nd1) = statement (ts,LocalScope,w)
            in
                (ts1,{pragmas=[],defns=[],body=[nd1],head=NONE,loc=locOf ts})
            end
      (* dispatch non attr prototype ref *)
      | (Prototype, _) :: (Dot, _) :: _ => nonAttributePrototype ()
      | (Prototype, _) :: (Assign, _) :: _ => nonAttributePrototype ()
      | (Let,       _) :: _ => declaration ()
      | (Const,     _) :: _ => declaration ()
      | (Var,       _) :: _ => declaration ()
      | (Function,  _) :: _ => declaration ()
      | (Class,     _) :: _ => declaration ()
      | (Interface, _) :: _ => declaration ()
      | (Namespace, _) :: _ => declaration ()
      | (Type,      _) :: _ => declaration ()
      | (Dynamic,   _) :: _ => withAttribute ()
      | (Final,     _) :: _ => withAttribute ()
      | (Override,  _) :: _ => withAttribute ()
      | (Native,    _) :: _ => withAttribute ()
      | (Prototype, _) :: _ => withAttribute ()
      | (Static,    _) :: _ => withAttribute ()
      | (Identifier _, _) :: (next, _) :: _ => maybeWithName next
      | (Private,      _) :: (next, _) :: _ => maybeWithName next
      | (Public,       _) :: (next, _) :: _ => maybeWithName next
      | (Protected,    _) :: (next, _) :: _ => maybeWithName next
      | (Internal,     _) :: (next, _) :: _ => maybeWithName next
      | (Intrinsic,    _) :: (next, _) :: _ => maybeWithName next
      | _ => default ()
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

and annotatableDirective (ts:TOKENS, attrs:ATTRS, GlobalScope, w:OMEGA)
    : (TOKENS * Ast.DIRECTIVES) =
    let val _ = trace([">> annotatableDirective Global with next=", tokenname(hd ts)])
        val {ns,prototype,static,...} = attrs
        fun declaration () =
            let
                val (ts1,nd1) = variableDefinition (ts,ns,prototype,static,AllowIn,GlobalScope)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
    in case ts of
        (Let, _) :: (Function, _) :: _ =>
            functionDefinition (ts,attrs,GlobalScope)
      | (Const, _) :: (Function, _) :: _ =>
            functionDefinition (ts,attrs,GlobalScope)
      | (Function, _) :: _ =>
            functionDefinition (ts,attrs,GlobalScope)
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
      | (Let,   _) :: _ => declaration ()
      | (Var,   _) :: _ => declaration ()
      | (Cinst, _) :: _ => declaration ()
      | _ =>
            error ["unknown token in annotatableDirective Global"]
    end
  | annotatableDirective (ts,attrs,InterfaceScope,w) : (TOKENS * Ast.DIRECTIVES)  =
    let val _ = trace([">> annotatableDirective InterfaceScope with next=", tokenname(hd ts)])
        val attrs : ATTRS =
            { ns = SOME (Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Public Ustring.empty)))
            , override = false
            , static = false
            , final = false
            , dynamic = false
            , prototype = false
            , native = false
            , rest = false }
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
            error ["unknown token in annotatableDirective Interface"]
    end
  | annotatableDirective (ts,attrs,t,w) : (TOKENS * Ast.DIRECTIVES)  =
    let val _ = trace([">> annotatableDirective omega with next=", tokenname(hd ts)])
        val {ns,prototype,static,...} = attrs
        fun declaration () =
            let
                val (ts1,nd1) = variableDefinition (ts,ns,prototype,static,AllowIn,t)
                val (ts2,nd2) = (semicolon(ts1,w),nd1)
            in
                (ts2,nd2)
            end
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
      | (Let,   _) :: _ => declaration ()
      | (Var,   _) :: _ => declaration ()
      | (Const, _) :: _ => declaration ()
      | _ =>
            error ["unknown token in annotatableDirective other"]
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

and attributes (ts:TOKENS, attrs:ATTRS, t:TAU)
    : (TOKENS * ATTRS) =
    let
        val _ = setLoc ts
        val _ = trace([">> attributes with next=", tokenname(hd ts)])
        fun next () =
            let
                val (ts1,nd1) = attribute (ts,attrs,t)
                val (ts2,nd2) = attributes (ts1,nd1,t)
            in
                trace(["<< attributes with next=", tokenname(hd ts)]);
                (ts2,nd2)
            end
    in case ts of
        (Dynamic,      _) :: _ => next ()
      | (Final,        _) :: _ => next ()
      | (Native,       _) :: _ => next ()
      | (Override,     _) :: _ => next ()
      | (Prototype,    _) :: _ => next ()
      | (Static,       _) :: _ => next ()
      | (Private,      _) :: _ => next ()
      | (Protected,    _) :: _ => next ()
      | (Public,       _) :: _ => next ()
      | (Internal,     _) :: _ => next ()
      | (Intrinsic,    _) :: _ => next ()
      | (Identifier _, _) :: _ => next ()
      | _ =>
            let
            in
                trace(["<< attributes with next=", tokenname(hd ts)]);
                (ts,attrs)
            end
    end

and attribute (ts:TOKENS, attrs:ATTRS, GlobalScope)
    : (TOKENS * ATTRS) =
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
      | (Override,  _) :: _ => error(["invalid attribute in global context"])
      | (Static,    _) :: _ => error(["invalid attribute in global context"])
      | (Prototype, _) :: _ => error(["invalid attribute in global context"])
      | _ =>
            let
                val (ts1,nd1) = namespaceAttribute (ts,GlobalScope)
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
                                    prototype,native,rest },ClassScope) =
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
                val (ts1,nd1) = namespaceAttribute (ts,ClassScope)
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
                                    prototype,native,rest },InterfaceScope) =
        let
        in case ts of
            (Dynamic,      _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Final,        _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Native,       _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Override,     _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Prototype,    _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Static,       _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Private,      _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Protected,    _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Public,       _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Internal,     _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Intrinsic,    _) :: _ => error ["attributes not allowed on a interface methods"]
          | (Identifier _, _) :: _ => error ["attributes not allowed on a interface methods"]
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
                                    prototype,native,rest },LocalScope) =
        let
        in case ts of
            (Dynamic,      _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Final,        _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Native,       _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Override,     _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Prototype,    _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Static,       _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Private,      _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Protected,    _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Public,       _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Internal,     _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Intrinsic,    _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
          | (Identifier _, _) :: _ => (error(["attributes not allowed on local definitions"]); error ["unknown token in attribute"])
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

and namespaceAttribute (ts:TOKENS, GlobalScope)
    : (TOKENS * Ast.EXPR option) =
    let val _ = trace([">> namespaceAttribute with next=", tokenname(hd ts)])
        fun withReservedNamespace () =
            let
                val (ts1,nd1) = reservedNamespace ts
            in
                (ts1, SOME (Ast.LiteralExpr (Ast.LiteralNamespace nd1)))
            end
    in case ts of
        (Internal,  _) :: _ => withReservedNamespace ()
      | (Intrinsic, _) :: _ => withReservedNamespace ()
      | (Public,    _) :: _ => withReservedNamespace ()
      | (Identifier s, _) :: _ =>
            let
            in
                (tl ts, SOME (Ast.LexicalRef {ident=Ast.Identifier {ident=s, openNamespaces=[]},
                                              loc=locOf ts}))
            end
      | _ => error ["unknown token in namespaceAttribute"]
    end
  | namespaceAttribute (ts,ClassScope) =
    let val _ = trace([">> namespaceAttribute with next=", tokenname(hd ts)])
        fun withReservedNamespace () =
            let
                val (ts1,nd1) = reservedNamespace ts
            in
                (ts1, SOME (Ast.LiteralExpr (Ast.LiteralNamespace nd1)))
            end
    in case ts of
        (Internal,  _) :: _ => withReservedNamespace ()
      | (Intrinsic, _) :: _ => withReservedNamespace ()
      | (Private,   _) :: _ => withReservedNamespace ()
      | (Protected, _) :: _ => withReservedNamespace ()
      | (Public,    _) :: _ => withReservedNamespace ()
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

and variableDefinition (ts:TOKENS, ns:Ast.EXPR option, prototype:bool, static:bool, b:BETA, t:TAU)
    : (TOKENS * Ast.DIRECTIVES) =
    let val _ = trace([">> variableDefinition with next=", tokenname(hd ts)])
        val (ts1,nd1) = variableDefinitionKind(ts)
        val (ts2,(b,i)) = variableBindingList (ts1,AllowList,b)

        fun isTempBinding (b:Ast.BINDING) : bool =
            case b of
               Ast.Binding {ident=(Ast.TempIdent _),...} => true
             | Ast.Binding {ident=(Ast.ParamIdent _),...} => true
             | _ => false

        fun isTempInit (b:Ast.INIT_STEP) : bool =
            case b of
               Ast.InitStep ((Ast.TempIdent _),_) => true
             | Ast.InitStep ((Ast.ParamIdent _),_) => true
             | _ => false

        val (tempBinds,propBinds) = List.partition isTempBinding b
        val (tempInits,propInits) = List.partition isTempInit i

        val initStmts = [Ast.InitStmt {kind=nd1,
                                       ns=ns,
                                       prototype=prototype,
                                       static=static,
                                       temps=(tempBinds,tempInits),
                                       inits=(propInits)}]

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

and variableDefinitionKind (ts:TOKENS)
    : (TOKENS * Ast.VAR_DEFN_TAG) =
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

and variableBindingList (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> variableBindingList with next=", tokenname(hd ts)])
        fun variableBindingList' (ts,a,b) : (TOKENS * Ast.BINDINGS) =
            let val _ = trace([">> variableBindingList' with next=", tokenname(hd ts)])
            in case ts of
                (Comma, _) :: _ =>
                    let
                        val (ts1,(d1,s1)) = variableBinding(tl ts,a,b)
                        val (ts2,(d2,s2)) = variableBindingList'(ts1,a,b)
                    in case (ts2,b) of
                        ((In,_)::_, NoIn) => error ["too many for-in bindings"]
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

and variableBinding (ts:TOKENS, a:ALPHA, beta:BETA)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> variableBinding with next=", tokenname(hd ts)])
        val (ts1,(p,t)) = typedPattern (ts,a,beta)  (* parse the more general syntax *)
    in case (ts1,p,beta) of
            ((Assign, _) :: _,_,_) =>
                let
                    val (ts2,nd2) = variableInitialisation (ts1,a,beta)
                in case (ts2,beta) of
                    ((In, _) :: _, NoIn) =>
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
          | ((In, _) :: _,_,NoIn) => (* okay, we are in a for-in or for-each-in binding *)
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

and variableInitialisation (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPR) =
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

and functionDeclaration (ts:TOKENS, attrs:ATTRS)
    : (TOKENS * Ast.DIRECTIVES) =

    let val _ = trace([">> functionDeclaration with next=", tokenname(hd ts)])
        val {ns,final,native,override,prototype,static,...} = attrs
    in case ts of
        (Function, funcStartLoc) :: _ =>
            let
                val (ts1,nd1) = functionName (tl ts)
                val (ts2,nd2) = functionSignature (ts1)
            in
                (ts2,{pragmas=[],
                      defns=[Ast.FunctionDefn {kind=Ast.Const,
                                               ns=ns,
                                               final=final,
                                               override=override,
                                               prototype=prototype,
                                               static=static,
                                               func=Ast.Func {name=nd1,
                                                              fsig=nd2,
                                                              param=Ast.Head ([],[]),
                                                              defaults=[],
                                                              ty=functionTypeFromSignature nd2,
                                                              native=native,
                                                              block=NONE,
                                                              loc=unionLoc (SOME funcStartLoc) (locOf ts)}}],
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

and functionDefinition (ts:TOKENS, attrs:ATTRS, ClassScope)
    : (TOKENS * Ast.DIRECTIVES) =
    let val _ = trace([">> functionDefinition(ClassScope) with next=", tokenname(hd ts)])
        val {ns,final,override,prototype,static,...} = attrs
        val (ts1,nd1) = functionKind (ts)
        val (ts2,nd2) = functionName (ts1)
    in case (nd1,isCurrentClass(nd2),prototype) of
        (Ast.Const,true,false) =>
            let
                val (ts3,nd3) = constructorSignature (ts2)
            in case ts3 of
                (LeftBrace, _) :: _ =>
                    let
                        val (ts4,nd4) = functionBody (ts3)
                        val Ast.Block {loc=blockLoc, ...} = nd4
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.ConstructorDefn (Ast.Ctor {settings=Ast.Head ([],[]), superArgs=[],
                                                            func=Ast.Func {name=nd2,
                                                                           fsig=nd3,
                                                                           param=Ast.Head ([],[]),
                                                                           defaults=[],
                                                                           ty=functionTypeFromSignature nd3,
                                                                           native=false,
                                                                           loc=unionLoc (locOf ts) blockLoc,
                                                                           block=SOME nd4}})],
                              body=[],
                              head=NONE,
                              loc=locOf ts})
                    end
              | _ =>
                    let
                        val (ts4,nd4,listLoc) = listExpression (ts3,AllowIn)
                        val (ts4,nd4) = (semicolon (ts4,Full),nd4)
                    in
                        (ts4,{pragmas=[],
                              defns=[Ast.ConstructorDefn (Ast.Ctor
                                             {settings=Ast.Head ([],[]),
                                              superArgs=[],
                                              func=Ast.Func {name=nd2,
                                                             fsig=nd3,
                                                             param=Ast.Head ([],[]),
                                                             defaults=[],
                                                             ty=functionTypeFromSignature nd3,
                                                             native=false,
                                                             loc=unionLoc (locOf ts) listLoc,
                                                             block=SOME (Ast.Block
                                                                             {pragmas=[],
                                                                              defns=[],
                                                                              body=[Ast.ReturnStmt nd4],
                                                                              head=NONE,
                                                                              loc=listLoc})}})],
                              body=[],
                              head=NONE,
                              loc=locOf ts})
                    end
            end

      | (Ast.LetConst,true,_) =>
        error ["class name not allowed in 'let function'"]

      | (_,false,false) =>  (* static or instance method *)
            let
                val (ts3,nd3) = functionSignature (ts2)
            in case ts3 of
                (LeftBrace, _) :: _ =>
                    let
                        val (ts4,nd4) = functionBody (ts3)
                        val Ast.Block {loc=blockLoc, ...} = nd4
                        val ident = (#ident nd2)
                        val func = Ast.Func {name=nd2,
                                             fsig=nd3,
                                             param=Ast.Head ([],[]),
                                             defaults=[],
                                             ty=functionTypeFromSignature nd3,
                                             native=false,
                                             loc=unionLoc (locOf ts) blockLoc,
                                             block=SOME nd4}
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
                        val (ts4,nd4,listLoc) = listExpression (ts3,AllowIn)
                        val (ts4,nd4) = (semicolon (ts4,Full),nd4)
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
                                                                      param=Ast.Head ([],[]),
                                                                      defaults=[],
                                                                      ty=functionTypeFromSignature nd3,
                                                                      native=false,
                                                                      loc=unionLoc (locOf ts) listLoc,
                                                                      block=SOME (Ast.Block { pragmas=[],
                                                                                              defns=[],
                                                                                              body=[Ast.ReturnStmt nd4],
                                                                                              head=NONE,
                                                                                              loc=listLoc})}}],
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
                        val Ast.Block {loc=blockLoc, ...} = nd4
                        val ident = (#ident nd2)
                        val func = Ast.Func {name=nd2,
                                             fsig=nd3,
                                             param=Ast.Head ([],[]),
                                             defaults=[],
                                             ty=functionTypeFromSignature nd3,
                                             loc=unionLoc (locOf ts) blockLoc,
                                             native=false,
                                             block=SOME nd4}
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
                        val (ts4,nd4,listLoc) = listExpression (ts3,AllowIn)
                        val (ts4,nd4) = (semicolon (ts4,Full),nd4)
                        val ident = (#ident nd2)
                        val func = Ast.Func {name=nd2,
                                             fsig=nd3,
                                             param=Ast.Head ([],[]),
                                             defaults=[],
                                             ty=functionTypeFromSignature nd3,
                                             loc=unionLoc (locOf ts) listLoc,
                                             native=false,
                                             block=SOME (Ast.Block {pragmas=[],
                                                                    defns=[],
                                                                    body=[Ast.ReturnStmt nd4],
                                                                    head=NONE,
                                                                    loc=listLoc})}
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
        val Ast.Block {loc=blockLoc, ...} = nd4
        val ident = (#ident nd2)
        val ty = functionTypeFromSignature(nd3)

        val func = Ast.Func {name=nd2,
                             fsig=nd3,
                             param=Ast.Head ([],[]),
                             defaults=[],
                             ty=ty,
                             loc=unionLoc (locOf ts) blockLoc,
                             native=false,
                             block=SOME nd4}

        fun hasNonStar (ts) : bool =
            case ts of
                [] => false
              | Ast.SpecialType AstAny :: _ => hasNonStar (tl ts)
              | _ => true

        val hasNonStarAnno = (not (Type.isGroundTy ty))
                             orelse hasNonStar (AstQuery.paramTypesOfFuncTy ty) 
                             orelse hasNonStar [(AstQuery.resultTypeOfFuncTy ty)]
    in
        (ts4,{pragmas=[],
              defns=[Ast.FunctionDefn {kind=if hasNonStarAnno 
                                            then nd1 
                                            else Ast.Var, (* dynamic function are writable *)
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

and functionKind (ts:TOKENS)
    : (TOKENS * Ast.VAR_DEFN_TAG) =
    let val _ = trace([">> functionKind with next=", tokenname(hd ts)])
    in case ts of
        (Function, _) :: _ =>
            (trace(["<< functionKind with next=", tokenname(hd (tl ts))]);
            (tl ts,Ast.Const))   (* reuse VAR_DEFN_KIND *)
      | (Let, _) :: (Function, _) :: _ =>
            (tl (tl ts), Ast.LetConst)
      | (Const, _) :: (Function, _) :: _ =>
            (tl (tl ts), Ast.Const)
      | _ => error ["unknown token in functionKind"]
    end


and functionName (ts:TOKENS)
    : (TOKENS * Ast.FUNC_NAME) =
    let val _ = trace([">> functionName with next=", tokenname(hd ts)])
        fun withOperatorName () =
            let
                val (ts1,nd1) = operatorName ts
            in
                (ts1,{kind=Ast.Operator,ident=nd1})
            end
    in case ts of
        (Plus,                _) :: _ => withOperatorName()
      | (Minus,               _) :: _ => withOperatorName()
      | (BitwiseNot,          _) :: _ => withOperatorName()
      | (Mult,                _) :: _ => withOperatorName()
      | (Div,                 _) :: _ => withOperatorName()
      | (Modulus,             _) :: _ => withOperatorName()
      | (LessThan,            _) :: _ => withOperatorName()
      | (GreaterThan,         _) :: _ => withOperatorName()
      | (LessThanOrEquals,    _) :: _ => withOperatorName()
      | (GreaterThanOrEquals, _) :: _ => withOperatorName()
      | (Equals,              _) :: _ => withOperatorName()
      | (LeftShift,           _) :: _ => withOperatorName()
      | (RightShift,          _) :: _ => withOperatorName()
      | (UnsignedRightShift,  _) :: _ => withOperatorName()
      | (BitwiseAnd,          _) :: _ => withOperatorName()
      | (BitwiseOr,           _) :: _ => withOperatorName()
      | (StrictEquals,        _) :: _ => withOperatorName()
      | (NotEquals,           _) :: _ => withOperatorName()
      | (StrictNotEquals,     _) :: _ => withOperatorName()
      | (Get, _) :: (LeftParen,    _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.get_})
      | (Get, _) :: (LeftDotAngle, _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.get_})
      | (Get, _) :: (Mult, _) :: _ => (tl ts,{kind=Ast.Get,ident=Ustring.asterisk})
      | (Get, _) :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
            in
                (ts1,{kind=Ast.Get,ident=nd1})
            end

      | (Set, _) :: (LeftParen,    _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.set_})
      | (Set, _) :: (LeftDotAngle, _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.set_})
      | (Set, _) :: (Mult, _) :: _ => (tl ts,{kind=Ast.Set,ident=Ustring.asterisk})
      | (Set, _) :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
            in
                (ts1,{kind=Ast.Set,ident=nd1})
            end

      | (Call, _) :: (LeftParen,    _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.call_})
      | (Call, _) :: (LeftDotAngle, _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.call_})
      | (Call, _) :: (Mult, _) :: _ => (tl ts,{kind=Ast.Call,ident=Ustring.asterisk})

      | (Has, _) :: (LeftParen,    _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.has_})
      | (Has, _) :: (LeftDotAngle, _) :: _ => (tl ts,{kind=Ast.Ordinary, ident=Ustring.has_})
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
        Plus => tokenname (t,())
      | Minus => tokenname (t,())
      | BitwiseNot => tokenname (t,())
      | Mult => tokenname (t,())
      | Div => tokenname (t,())
      | Modulus => tokenname (t,())
      | LessThan => tokenname (t,())
      | GreaterThan => tokenname (t,())
      | LessThanOrEquals => tokenname (t,())
      | GreaterThanOrEquals => tokenname (t,())
      | Equals => tokenname (t,())
      | LeftShift => tokenname (t,())
      | RightShift => tokenname (t,())
      | UnsignedRightShift => tokenname (t,())
      | BitwiseAnd => tokenname (t,())
      | BitwiseOr => tokenname (t,())
      | StrictEquals => tokenname (t,())
      | NotEquals => tokenname (t,())
      | StrictNotEquals => tokenname (t,())
      | _ => error ["unknown token in operatorName"]
    in
        (ts, Ustring.fromString opStr)
    end

(*
    ConstructorSignature
        TypeParameters  (  Parameters  )
        TypeParameters  (  Parameters  )  : ConstructorInitialiser
*)

and constructorSignature (ts:TOKENS)
    : (TOKENS * Ast.FUNC_SIG) =
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

and constructorInitialiser (ts:TOKENS)
    : (TOKENS * (Ast.BINDINGS * Ast.EXPR list)) =
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

and initialiserList (ts:TOKENS)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> initialiserList with next=", tokenname(hd ts)])
        fun initialiserList' (ts)
            : (TOKENS * Ast.BINDINGS) =
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

and initialiser (ts:TOKENS)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> initialiser with next=", tokenname(hd ts)])
        val (ts1,nd1) = pattern (ts,NoList,NoIn,AllowExpr)
    in case (ts1) of
            (Assign, _) :: _ =>
                let
                    val (ts2,nd2) = variableInitialisation (ts1,NoList,NoIn)
                in
                    trace(["<< initialiser with next=", tokenname(hd ts2)]);
                    (ts2, desugarPattern (locOf ts) nd1 (Ast.SpecialType Ast.Any) (SOME nd2) 0) (* type meaningless *)
                end
          | _ => (error(["constructor initialiser without assignment"]); error ["unknown token in initialiser"])
    end

and superInitialiser (ts:TOKENS)
    : (TOKENS * Ast.EXPR list) =
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

and functionBody (ts:TOKENS)
    : (TOKENS * Ast.BLOCK) =
    let val _ = trace([">> functionBody with next=", tokenname(hd ts)])
    in case ts of
        (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = block (ts,LocalScope)
            in
                (ts1,nd1)
            end
      | _ =>
            let
                val (ts1,nd1,listLoc) = listExpression (ts,AllowIn)
                val (ts1,nd1) = (semicolon (ts1,Full),nd1)
            in
                (ts1,Ast.Block {pragmas=[],
                                defns=[],
                                body=[Ast.ReturnStmt nd1],
                                head=NONE,
                                loc=listLoc})
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

and classDefinition (ts:TOKENS, attrs:ATTRS)
    : (TOKENS * Ast.DIRECTIVES) =
    let val _ = trace([">> classDefinition with next=", tokenname(hd ts)])
    in case ts of
        (Class, _) :: _ =>
            let
                val {ns,final,dynamic,...} = attrs
                val (ts1,{ident,params,nonnullable}) = className (tl ts)
                val (ts2,{extends,implements}) = classInheritance (ts1)
                val _ = currentClassName := ident
                val (ts3,nd3) = classBody (ts2)
                val _ = currentClassName := Ustring.empty

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

and className (ts:TOKENS)
    : (TOKENS * {ident:Ast.IDENT,
                 params:(Ustring.STRING list),
                 nonnullable:bool }) =
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

and parameterisedClassName (ts:TOKENS)
    : (TOKENS * {ident:Ast.IDENT,
                 params:(Ustring.STRING list) }) =
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

and classInheritance (ts:TOKENS)
    : (TOKENS * {extends:Ast.TYPE_EXPR option,
                 implements:Ast.TYPE_EXPR list}) =
    let val _ = trace([">> classInheritance with next=", tokenname(hd ts)])
    in case ts of
        (Extends, _) :: _ =>
            let
                val (ts1,nd1) = typeExpression (tl ts)
            in case ts1 of
                (Implements, _) :: _ =>
                    let
                        val (ts2,nd2) = typeExpressionList (tl ts1)
                    in
                        (ts2,{extends=SOME (unwrapTy nd1),implements=(map unwrapTy nd2)})
                    end
              | _ =>
                    let
                    in
                        (ts1,{extends=SOME (unwrapTy nd1),implements=[]})
                    end
            end
      | (Implements, _) :: _ =>
            let
                val (ts1,nd1) = typeExpressionList (tl ts)
            in
                (ts1,{extends=NONE,implements=(map unwrapTy nd1)})
            end
      | _ => (ts,{extends=NONE,implements=[]})
    end

and typeIdentifierList (ts:TOKENS)
    : (TOKENS * Ast.IDENT_EXPR list) =
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

and classBody (ts:TOKENS)
    : (TOKENS * Ast.BLOCK) =
    let val _ = trace([">> classBody with next=", tokenname(hd ts)])
        val (ts1,nd1) = block (ts,ClassScope)
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

and interfaceDefinition (ts:TOKENS, attrs:ATTRS)
    : (TOKENS * Ast.DIRECTIVES) =
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

and interfaceInheritance (ts:TOKENS)
    : (TOKENS * {extends:Ast.TYPE_EXPR list}) =
    let val _ = trace([">> interfaceInheritance with next=", tokenname(hd ts)])
    in case ts of
        (Extends, _) :: _ =>
            let
                val (ts1,nd1) = typeExpressionList (tl ts)
            in
                (ts1,{extends=(map unwrapTy nd1)})
            end
      | _ => (ts,{extends=[]})
    end

and interfaceBody (ts:TOKENS)
    : (TOKENS * Ast.BLOCK)  =
    let val _ = trace([">> interfaceBody with next=", tokenname(hd ts)])
        val (ts1,nd1) = block (ts,InterfaceScope)
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

and namespaceDefinition (ts:TOKENS, attrs:ATTRS)
    : (TOKENS * Ast.DIRECTIVES) =
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

and namespaceInitialisation (ts:TOKENS)
    : (TOKENS * Ast.EXPR option) =
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

and typeDefinition (ts:TOKENS, attrs:ATTRS)
    : (TOKENS * Ast.DIRECTIVES) =
    let val _ = trace([">> typeDefinition with next=", tokenname(hd ts)])
        val {ns,...} = attrs
    in case ts of
        (Type, _) :: _ =>
            let
                val (ts1,nd1) = identifier (tl ts)
                val (ts2,nd2) = typeParameters ts1                                    
                val (ts3,nd3) = typeInitialisation ts2
                val t = case nd2 of 
                            [] => nd3
                          | _ => Ast.LamType { params = nd2,
                                               body = nd3 }
            in
                trace(["<< typeDefinition with next=", tokenname(hd ts3)]);
                (ts3,{pragmas=[],
                      body=[],
                      defns=[Ast.TypeDefn {ns=ns,
                                           ident=nd1,
                                           init=t}],
                      head=NONE,
                      loc=locOf ts})
            end
      | _ => error ["unknown token in typeDefinition"]
    end

and typeInitialisation (ts:TOKENS)
    : (TOKENS * Ast.TYPE_EXPR) =
    let val _ = trace([">> typeInitialisation with next=", tokenname(hd ts)])
    in case ts of
        (Assign, _) :: _ =>
            let
                val (ts1,nd1) = nullableTypeExpression (tl ts)
            in
                trace(["<< typeInitialisation with next=", tokenname(hd ts1)]);
                (ts1,unwrapTy nd1)
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

and pragmas (ts:TOKENS)
    : (TOKENS * Ast.PRAGMA list) =
    let val _ = trace([">> pragmas with next=", tokenname(hd ts)])
        val (ts1,nd1) = pragma(ts)
        fun next () =
            let
                val (ts2,nd2) = pragmas (ts1)
            in
                (ts2, nd1 @ nd2)
            end
    in case ts1 of
        (Use,    _) :: _ => next ()
      | (Import, _) :: _ => next ()
      | _ =>
            (ts1, nd1)
    end

and pragma (ts:TOKENS)
    : (TOKENS * Ast.PRAGMA list) =
    let val _ = trace([">> pragma with next=", tokenname(hd ts)])
    in case ts of
        (Use, _) :: _ =>
            let
                val (ts1,nd1) = usePragma ts
                 val (ts2,nd2) = (semicolon (ts1,Full),nd1)
            in
                (ts2,nd2)
            end
      | (Import, _) :: _ =>
            let
                val (ts1,nd1) = importPragma ts
                 val (ts2,nd2) = (semicolon (ts1,Full),nd1)
            in
                (ts2,nd2)
            end
      | _ => error ["unknown token in pragma"]
    end

(*
    UsePragma
        use  PragmaItems
*)

and usePragma (ts:TOKENS)
    : (TOKENS * Ast.PRAGMA list)  =
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

and pragmaItems (ts:TOKENS)
    : (TOKENS * Ast.PRAGMA list) =
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
        standard
        strict
        default namespace SimpleTypeIdentifier
        namespace SimpleTypeIdentifier
*)

and pragmaItem (ts:TOKENS)
    : (TOKENS * Ast.PRAGMA) =
    let val _ = trace([">> pragmaItem with next=", tokenname(hd ts)])
        fun defaultNamespaceReserved () =
            let
                val (ts1,nd1) = reservedNamespace (tl (tl ts))
            in
                (ts1, Ast.UseDefaultNamespace (Ast.LiteralExpr (Ast.LiteralNamespace nd1)))
            end
    in case ts of
        (Decimal, _) :: _ => 
            let
                val (ts1,nd1) = primaryExpression ((tl ts), NoList, NoIn)
	    in
                (ts1, Ast.UseDecimalContext nd1)
            end
      | (Standard, _) :: _ => (tl ts,Ast.UseStandard)
      | (Strict, _) :: _ => (tl ts,Ast.UseStrict)
      | (Default, _) :: (Namespace, _) :: (Public,    _) :: _ => defaultNamespaceReserved ()
      | (Default, _) :: (Namespace, _) :: (Internal,  _) :: _ => defaultNamespaceReserved ()
      | (Default, _) :: (Namespace, _) :: (Intrinsic, _) :: _ => defaultNamespaceReserved ()
      | (Default, _) :: (Namespace, _) :: (Protected, _) :: _ => defaultNamespaceReserved ()
      | (Default, _) :: (Namespace, _) :: (Private,   _) :: _ => defaultNamespaceReserved ()
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
        import  ImportName

    ImportName
        PackageIdentifier  .  PropertyIdentifier
*)

and importPragma (ts:TOKENS)
    : (TOKENS * Ast.PRAGMA list) =
    let val _ = trace([">> importPragma with next=", tokenname(hd ts)])
    in case ts of
	   (Import, _) :: _ =>
           let
               val (ts1,(p,i)) = importName (tl ts)
           in
               (ts1,[Ast.Import {package=p,name=i}])
           end
      | _ => error ["unknown token in importPragma"]
    end

and importName (ts:TOKENS)
    : (TOKENS * (Ast.IDENT list * Ast.IDENT)) =
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

and block (ts:TOKENS, t:TAU)
    : (TOKENS * Ast.BLOCK) =
    let
        val _ = setLoc ts
        val _ = trace([">> block with next=", tokenname(hd ts)])
    in case ts of
        (LeftBrace, locL) :: (RightBrace, locR) :: _ =>
            ((trace(["<< block with next=", tokenname(hd (tl (tl ts)))]);
            (tl (tl ts),Ast.Block {pragmas=[],defns=[],body=[],head=NONE,loc=unionLoc (SOME locL) (SOME locR)})))
      | (LeftBrace, locL) :: _ =>
            let
                val (ts1,{pragmas=pragmas,
                          defns=defns,
                          head=head,
                          body=body,
                          loc=_}) = directives (tl ts,t)
            in case ts1 of
                (RightBrace, locR) :: _ =>
                    (trace(["<< block with next=", tokenname(hd (tl ts1))]);
                    (tl ts1,Ast.Block{pragmas=pragmas,
                                      defns=defns,
                                      head=head,
                                      body=body,
                                      loc=unionLoc (SOME locL) (SOME locR)}))
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

(* 
and program (ts:TOKENS)
    : (TOKENS * Ast.PROGRAM) =
    let val _ = trace([">> program with next=",tokenname(hd(ts))])
        fun withPackages () =
            let
                val (ts1,nd1) = packages ts
                val (ts2,nd2) = directives (ts1,GlobalScope)
            in
                (ts2,{block=Ast.Block nd2,fixtures=NONE,packages=nd1})
            end

    in case ts of
        (Internal, _) :: (Package, _) :: _ => withPackages ()
      | (Package, _) :: _ => withPackages ()
      | _ =>
            let
                val (ts1,nd1) = directives (ts,GlobalScope)
            in
                (ts1,{block=Ast.Block nd1,fixtures=NONE,packages=[]})
            end
    end

and packages (ts:TOKENS)
    : (TOKENS * Ast.PACKAGE list) =
    let val _ = trace([">> packages with next=",tokenname(hd(ts))])
        fun next () =
            let
                val (ts1,nd1) = package ts
                val (ts2,nd2) = packages ts1
            in
                trace(["<< packages with next=",tokenname(hd(ts2))]);
                (ts2,nd1::nd2)
            end
    in case ts of
        (Internal, _) :: (Package, _) :: _ => next ()
      | (Package, _) :: _ => next ()
      | _ => (trace(["<< packages with next=",tokenname(hd(ts))]);(ts,[]))
    end
*)

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
(*
and package (ts:TOKENS)
    : (TOKENS * Ast.PACKAGE) =
    let val _ = trace([">> package with next=",tokenname(hd(ts))])
    in case ts of
        (Internal, _) :: (Package, _) :: _ =>
            let
                val (ts1,nd1) = packageName (tl (tl ts))
                val (ts2,nd2) = block (ts1,GlobalScope)
            in
                (ts2, {name=nd1, block=nd2})
            end
      | (Package, _) :: (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = block (tl ts,GlobalScope)
            in
                (ts1, {name=[], block=nd1})
            end
      | (Package, _) :: _ =>
            let
                val (ts1,nd1) = packageName (tl ts)
                val (ts2,nd2) = block (ts1,GlobalScope)
            in
                (ts2, {name=nd1, block=nd2})
            end
      | _ => error ["unknown token in package"]
    end
*)

and subFragments (ts:TOKENS) 
    : (TOKENS * Ast.FRAGMENT list) = 
    let
        fun fragments (accum:Ast.FRAGMENT list) 
                      (ts0:TOKENS) 
            : (TOKENS * Ast.FRAGMENT list) = 
            case ts0 of 
                (RightBrace, _) :: rest => (rest, List.rev accum)
              | (Eof, _) :: _ => (ts0, List.rev accum)
              | _ => 
                let
                    val (ts1, frag) = fragment ts0
                in                    
                    fragments (frag::accum) ts1 
                end
    in
        case ts of 
            (LeftBrace, _) :: rest => fragments [] rest
          | _ => error ["expecting left brace in subFragments"]
    end
            

and fragment (ts:TOKENS)
    : (TOKENS * Ast.FRAGMENT) =            
    case ts of
        (Internal, _) :: (Package, _) :: rest =>
        let
            val (ts1,nd1) = packageName rest
            val (ts2,nd2) = subFragments (tl ts1)
        in
            (ts2, Ast.Package {name=nd1, fragments=nd2})
        end
      | (Package, _) :: (LeftBrace, _) :: rest =>
        let
            val (ts1,nd1) = subFragments (tl ts)
        in
            (ts1, Ast.Package {name=[], fragments=nd1})
        end
      | (Package, _) :: rest =>
        let
            val (ts1,nd1) = packageName rest
            val (ts2,nd2) = subFragments ts1
        in
            (ts2, Ast.Package {name=nd1, fragments=nd2})
        end

      | (RightBrace, _) :: rest => 
        error ["unexpected right brace opening fragment"]

      (* FIXME: add code here to parse units *)


      | _ => 
        let
            val (ts1, nd1) = directives (ts, GlobalScope)
        in
            (ts1, Ast.Anon (Ast.Block nd1))
        end
        
and packageName (ts:TOKENS)
    : TOKENS * Ast.IDENT list =
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


fun mkReader (filename:string)
    : (unit -> Ustring.SOURCE) =
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

fun lexFile (filename : string)
    : TOKENS =
    Lexer.lex (filename, mkReader filename)

fun lexLines (lines : Ustring.SOURCE list)
    : TOKENS =
    let
        val r = ref lines
        fun reader _ =
            case !r of
                (line::lines) => (r := lines; line)
              | [] => Ustring.emptySource
    in
        Lexer.lex ("<no filename>", reader)
    end

fun parseFrags [(Eof, _)] = ([], [])
  | parseFrags ts = 
    let
        val (residual, frag) = fragment ts
    in
        trace ["parsed fragment:"];
        (if (!doTrace)
         then Pretty.ppFragment frag
         else ());
        let 
            val (residual, frags) = parseFrags residual
        in
            case residual of 
                [] => (residual, frag :: frags)
              | tok::_ => error ["residual token seen after parsing: ", tokenname tok]
        end
    end

fun parse ts = 
    let
        val (_, frags) = parseFrags ts
        val frag = case frags of 
                       [x] => x
                     | other => Ast.Unit { name = NONE, 
                                           fragments = other }
    in
        frag
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
