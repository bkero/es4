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

(*
   The parser is modeled after the design by Peter Sestoft describe here:
       www.itu.dk/courses/FDP/E2002/parsernotes.pdf
*)

structure Parser = struct

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[parse] " :: ss) else ()
fun error ss = LogErr.parseError ss

val astNonceCounter = ref 0
fun nextAstNonce _ = ( astNonceCounter := (!astNonceCounter) + 1;
                       !astNonceCounter)

exception ParseError = LogErr.ParseError
exception LexError = LogErr.LexError
exception EofError = LogErr.EofError

open Token

datatype ALPHA =
    AllowColon
  | NoColon

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

type ATTRS = {ns: Ast.NAMESPACE_EXPRESSION option,
              override: bool,
              static: bool,
              final: bool,
              dynamic: bool,
              prototype: bool,
              native: bool,
              rest: bool}

type TOKENS = (TOKEN * Ast.LOC) list

fun isGeneratorFunction (block:Ast.BLOCK) =
    let
        open CheckAst
        infix withVisitExpr withVisitFunc

        fun visitExpr (expr, _) =
            case expr of
                Ast.YieldExpr _ => Stop true
              | _ => Cont false

        val visitor = default withVisitExpr visitExpr
                              withVisitFunc skip
    in
        foldStmt (visitor, Ast.BlockStmt block, false)
    end
    
(*

    PATTERNS to BINDINGS to FIXTURES
    EXPRS to INITS
*)

datatype PATTERN =
         ObjectPattern of FIELD_PATTERN list
       | ArrayPattern of PATTERN list
       | SimplePattern of Ast.EXPRESSION
       | IdentifierPattern of Ast.IDENTIFIER

withtype FIELD_PATTERN =
         { name: Ast.NAME_EXPRESSION,
           pattern : PATTERN }

type PATTERN_BINDING_PART =
     { kind:Ast.VAR_DEFN_TAG,
       ty:Ast.TYPE,
       prototype:bool,
       static:bool }

val currentClassName : Ast.IDENTIFIER ref = ref Ustring.empty
(* val currentPackageName : Ast.IDENTIFIER ref = ref Ustring.empty *)

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
fun eat (ts: TOKENS, tk0: TOKEN)
    : TOKENS =
    let
        val (tk1,loc) : TOKEN*Ast.LOC = hd ts
    in 
        if tk0 = tk1
            then tl ts
            else error ["found " ^ tokenname (tk0,loc) ^ " expected token " ^ tokenname (hd ts)]
     end
*)

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
                   (ty:Ast.TYPE)
                   (expr:Ast.EXPRESSION option)
                   (nesting:int)
    : (Ast.BINDING list * Ast.INIT_STEP list) =
    let
        fun desugarIdentifierPattern (id:Ast.IDENTIFIER)
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
                | NONE => ([bind],[Ast.InitStep (ident, Ast.LiteralUndefined)])
*)
            end

        fun desugarSimplePattern (patternExpr:Ast.EXPRESSION)
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
                                (element_types: Ast.TYPE)
                                (temp:Ast.EXPRESSION)
                                (n:int)
            : (Ast.BINDING list * Ast.INIT_STEP list) =
            let
            in case element_ptrns of
                p::plist =>
                    let
                        val num = Ast.LiteralDouble (Real64.fromInt n)
                        val e = SOME (Ast.ObjectIndexReference {object=temp, index=num, loc=loc})
                        val t = Ast.TypeIndexReferenceType (element_types,n)
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
                                 (field_types:Ast.TYPE)
                                 (temp:Ast.EXPRESSION)
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
                                (field_types: Ast.TYPE)
                                (temp: Ast.EXPRESSION)
            : (Ast.BINDING list * Ast.INIT_STEP list) =
            let
                val {name, pattern=p} = field_pattern
                val t = Ast.TypeNameReferenceType (field_types, name)
                val e = SOME (Ast.ObjectNameReference {object=temp, name=name, loc=loc})
            in
                desugarPattern loc p t e (nesting+1)
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
      | Const => tn ()
      | Decimal => tn ()
      | Double => tn ()
      | Dynamic => tn ()
      | Each => tn ()
      | Eval => tn ()
      | Final => tn ()
      | Generator => tn ()
      | Generic => tn ()
      | Get => tn ()
      | Has => tn ()
      | Implements => tn ()
      | Interface => tn ()
      | Let => tn ()
      | Like => tn ()
      | Namespace => tn ()
      | Native => tn ()
      | Number => tn ()
      | Override => tn ()
      | Prototype => tn ()
      | Set => tn ()
      | Standard => tn ()
      | Static => tn ()
      | Strict => tn ()
      | Type => tn ()
      | Undefined => tn ()
      | Use => tn ()
      | Xml => tn ()
      | Yield => tn ()
      | _ => error ["expecting 'identifier' before '",tokenname (t,()),"'"]
    in
        (ts, ustr)
    end


(* 

 In the ast: 

     and NAME_EXPRESSION = 
         QualifiedName of { namespace: NAMESPACE_EXPRESSION,
							identifier: IDENTIFIER }
       | UnqualifiedName of { identifier: IDENTIFIER, 
                              openNamespaces: OPEN_NAMESPACES }

     and NAMESPACE_EXPRESSION =
         Namespace of NAMESPACE
       | NamespaceName of NAME_EXPRESSION

 but there is no way to denote an Ast.Namespace in the input token
 stream -- we wire them in ourselves during AST synthesis and definition
 -- so for the sake of *parsing* the grammar looks like this:

    NameExpression''        
        NameExpression'' :: Identifier
        Identifier

    NameExpression'
        StringLiteral :: NameExpression''
        NameExpression

    NameExpression
        StringLiteral   (if in label context)
        DoubleLiteral   (if in label context)
        NameExpression'

  (with an additional wrinkle associated with some label contexts that permit decimal
   and string-literal plain identifiers -- w/o qualification -- as 

 *)

and nameExpression'' (qual:Ast.NAMESPACE_EXPRESSION option) 
                     (ts0: TOKENS)
    : (TOKENS * Ast.NAME_EXPRESSION) = 
    let
        val (ts1, nd1) = identifier ts0
        val ne = case qual of 
                     NONE => Ast.UnqualifiedName { identifier = nd1, 
                                                   openNamespaces = [] }
                   | SOME ns => Ast.QualifiedName { namespace = ns,
                                                    identifier = nd1 }
    in
        case ts1 of 
            (DoubleColon, _) :: _ => 
            nameExpression'' (SOME (Ast.NamespaceName ne)) (tl ts1)
          | _ => (ts1, ne)
    end

and nameExpression' (ts0: TOKENS) 
    : (TOKENS * Ast.NAME_EXPRESSION) =
        case ts0 of 
            (StringLiteral s, _) :: (DoubleColon, _) :: ts => 
            nameExpression'' (SOME (Ast.Namespace (Ast.TransparentNamespace s))) ts
          | _ => nameExpression'' NONE ts0
                 

and nameExpression (ts0: TOKENS)
                   (literalsAsIdents:bool)
    : (TOKENS * Ast.NAME_EXPRESSION) =
      if literalsAsIdents
      then           
          case ts0 of 
              (StringLiteral s, _) :: ts => 
              (ts, Ast.QualifiedName { identifier = s, 
                                       namespace = Ast.Namespace Name.publicNS })
            | (DoubleLiteral d, _) :: ts => 
              (ts, Ast.QualifiedName { identifier = Mach.NumberToString d,
                                       namespace = Ast.Namespace Name.publicNS })
            | _ => nameExpression' ts0
      else
          nameExpression' ts0

(*
    ParenExpression
        (  AssignmentExpression(allowColon,allowIn) )
*)

and parenExpression (ts0: TOKENS)
    : TOKENS * Ast.EXPRESSION =
    let val _ = trace ([">> parenExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (LeftParen, _) :: _ =>
            let
                val (ts1, nd1) = assignmentExpression (tl ts0, AllowColon, AllowIn)
            in case ts1 of
                (RightParen, _) :: _ => (tl ts1, nd1)
              | _ => error ["unknown final token of paren expression"]
            end
      | _ => error ["unknown initial token of paren expression"]
    end

(*
    ParenListExpression
        (  ListExpression(allowIn)  )
*)

and parenListExpression (ts0: TOKENS) 
    : TOKENS * Ast.EXPRESSION =
    let val _ = trace ([">> parenListExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (LeftParen, _) :: _ =>
            let
                val (ts1, nd1, _) = listExpression (tl ts0, AllowColon, AllowIn)
                val nd1 = case nd1 of
                              Ast.ListExpr [x] => x
                            | x => x
            in case ts1 of
                (RightParen, _) :: _ =>
                    (trace (["<< parenListExpression with next=", tokenname (hd (ts1))]);
                    (tl ts1, nd1))
              | _ => error ["unknown final token of paren list expression"]
            end
      | _ => error ["unknown initial token of paren list expression"]
    end

(*
    FunctionExpression(alpha, beta)
        function  Identifier  FunctionSignature  FunctionExpressionBody(alpha, beta)
        function  FunctionSignature  FunctionExpressionBody(alpha, beta)
*)

and functionExpression (ts0: TOKENS, alpha: ALPHA, beta: BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace([">> functionExpression with next=",tokenname(hd(ts0))])
    in case ts0 of
        (Function, funcStartLoc) :: _ =>
            let
                fun anonymousFunctionExpression () =
                    let
                        val (ts1,nd1) = functionSignature (tl ts0)
                        val (ts2,nd2) = functionExpressionBody (ts1, alpha, beta)
                    in
                        (ts2, (Ast.LiteralFunction
                                (Ast.Func {name={kind=Ast.Ordinary,ident=Ustring.empty},
                                           fsig=nd1,
                                           block=SOME nd2,
                                           native=false,
                                           generator=isGeneratorFunction nd2,
                                           defaults=[],
                                           param=Ast.Head ([],[]),
                                           ty=functionTypeFromSignature nd1,
                                           loc=SOME funcStartLoc})))
                    end
               
            in case tl ts0 of
                (LeftDotAngle, _) :: _ => anonymousFunctionExpression ()
              | (LeftParen,    _) :: _ => anonymousFunctionExpression ()
              | _ =>
                    let
                        val (ts1,nd1) = identifier (tl ts0)
                        val (ts2,nd2) = functionSignature ts1
                        val (ts3,nd3) = functionExpressionBody (ts2,alpha,beta)

                        val ident = nd1
                        val ty = functionTypeFromSignature nd2
                        val expr = (Ast.LiteralFunction
                                         (Ast.Func {name={kind=Ast.Ordinary,ident=nd1},
                                                    fsig=nd2,
                                                    block=SOME nd3,
                                                    native=false,
                                                    generator=isGeneratorFunction nd3,
                                                    param=Ast.Head ([],[]),
                                                    defaults=[],
                                                    ty=ty,
                                                    loc=SOME funcStartLoc}))
                        val bid = Ast.PropIdent ident
                        val bindings = [Ast.Binding { ident = bid, ty = ty }]
                        val inits = [Ast.InitStep (bid, expr)]
                        val name = Ast.QualifiedName { identifier = ident, 
                                                       namespace = Ast.Namespace Name.publicNS }
                        val res = Ast.LexicalReference { name = name,
                                                         loc = SOME funcStartLoc }
                    in
                        (* 
                         * ES3 section 13 compatibility.
                         * 
                         * expression: function f () { ... } 
                         * desugaring: (let (f = function() { ... }) f)
                         *)
                        (ts3,
                         Ast.LetExpr 
                             { defs = (bindings, inits),
                               body = res,
                               head = NONE })
                    end
            end
      | _ => error ["unknown form of function expression"]
    end

(*
    FunctionExpressionBody(alpha, beta)  
        Block(local)
        AssignmentExpression(alpha, beta)
*)

and functionExpressionBody (ts0: TOKENS, alpha: ALPHA, beta: BETA) =
    let
    in case ts0 of
        (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = block (ts0, LocalScope)
                val Ast.Block {loc=blockLoc, ...} = nd1
            in
                (ts1,nd1)
            end
      | _ =>
            let
                val startLoc  = #2 (hd ts0)
                val (ts1,nd1) = assignmentExpression (ts0, alpha, beta)
            in
                (ts1, Ast.Block { pragmas=[],
                                  defns=[],
                                  body=[Ast.ReturnStmt nd1],
                                  head=NONE,
                                  loc=SOME startLoc})
            end
    end

(*
    ObjectLiteral
        {  FieldList  }
        {  FieldList  }  :  TypeExpression
*)

and objectLiteral (ts0: TOKENS, alpha: ALPHA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace([">> objectLiteral with next=",tokenname(hd(ts0))])
    in case ts0 of
        (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = fieldList (tl ts0)
            in 
                case (ts1, alpha) of
                    ((RightBrace, _) :: (Colon, _) :: _, AllowColon) =>
                    let
                        val (ts2,nd2) = typeExpression (tl (tl ts1))
                    in
                        (ts2,Ast.LiteralObject {expr=nd1,ty=SOME nd2})
                    end
                  | ((RightBrace, _) :: _, _) =>
                    (tl ts1,Ast.LiteralObject {expr=nd1,ty=NONE})
                  | _ => error ["unknown token in objectLiteral ",tokenname (hd ts1)]
            end
      | _ => error ["unknown token in objectLiteral ",tokenname (hd ts0)]
    end

(*
    FieldList
        empty
        LiteralField
        LiteralField  ,  FieldList
*)

and fieldList (ts0: TOKENS)
    : (TOKENS * (Ast.FIELD list)) =
    let val _ = trace([">> fieldList with next=",tokenname(hd(ts0))])
    in case ts0 of
        (RightBrace, _) :: _ =>
            (trace(["<< fieldList with next=",tokenname(hd(ts0))]);
            (ts0,[]))
      | _ =>
        let
            val (ts1,nd1) = literalField(ts0)
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
        FieldKind  NameExpression  :  AssignmentExpression(AllowColon, AllowIn)
        FieldKind  LiteralString  :  AssignmentExpression(AllowColon, AllowIn)
        get  Identifier  FunctionCommon
        set  Identifier  FunctionCommon

    FieldKind
        empty
        const
*)

and literalField (ts:TOKENS)
  : (TOKENS * Ast.FIELD)=
    let val _ = trace([">> literalField with next=",tokenname(hd(ts))])
        fun getterSetter () =
            let
                val (ts1,nd1) = fieldKind ts
                val (ts2,nd2) = nameExpression ts1 false
            in case ts2 of
                (Colon, _) :: _ =>
                    let
                        val (ts3, nd3) = assignmentExpression (tl ts2, AllowColon, AllowIn)
                    in
                        (ts3, {kind=nd1, name=nd2, init=nd3})
                    end
              | _ => error ["unknown token in literalField"]
            end
    in case ts of
        (* special case for fields with name 'get' or 'set' *)
        (Get, _) :: (Colon,_) :: _ => getterSetter ()
      | (Set, _) :: (Colon,_) :: _ => getterSetter ()
      | (Get, funcStartLoc) :: _ =>
            let
                val (ts1,nd1) = nameExpression (tl ts) false
                val (ts2,{fsig,block}) = functionCommon (ts1)
                val Ast.Block {loc=blockLoc, ...} = block
            in
                (ts2,{kind=Ast.Var,
                      name=nd1,
                      init= Ast.LiteralFunction
                                    (Ast.Func {name={kind=Ast.Get, ident=Ustring.empty},
                                               fsig=fsig,
                                               block=SOME block,
                                               native=false,
                                               generator=isGeneratorFunction block,
                                               param=Ast.Head ([],[]),
                                               defaults=[],
                                               ty=functionTypeFromSignature fsig,
                                               loc=unionLoc (SOME funcStartLoc) blockLoc})})
            end
      | (Set, funcStartLoc) :: _ =>
            let
                val (ts1,nd1) = nameExpression (tl ts) false
                val (ts2,{fsig,block}) = functionCommon (ts1)
                val Ast.Block {loc=blockLoc, ...} = block
            in
                (ts2,{kind=Ast.Var,
                      name=nd1,
                      init=Ast.LiteralFunction
                                    (Ast.Func {name={kind=Ast.Set,ident=Ustring.empty},
                                               fsig=fsig,
                                               block=SOME block,
                                               native=false,
                                               generator=isGeneratorFunction block,
                                               param=Ast.Head ([],[]),
                                               defaults=[],
                                               ty=functionTypeFromSignature fsig,
                                               loc=unionLoc (SOME funcStartLoc) blockLoc})})
            end
            
      | _ =>
        let
            val (ts1,nd1) = fieldKind ts
            val (ts2,nd2) = nameExpression ts1 true
        in case ts2 of
               (Colon, _) :: _ =>
               let
                   val (ts3,nd3) = assignmentExpression (tl ts2, AllowColon, AllowIn)
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
    ArrayLiteral(noColon)
        [  Elements  ]
    
    ArrayLiteral(allowColon)
        [  Elements  ]
        [  Elements  ]  :  TypeExpression

*)

and arrayLiteral (ts0: TOKENS, alpha: ALPHA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> arrayLiteral with next=", tokenname (hd (ts0))])
    in case ts0 of
        (LeftBracket, _) :: _ =>
            let
                val (ts1,nd1) = elements (tl ts0)
            in case (ts1, alpha) of
                ((RightBracket, _) :: (Colon, _) :: _, AllowColon)  =>
                    let
                        val (ts2,nd2) = typeExpression (tl (tl ts1))
                    in
                        (ts2, Ast.LiteralArray {exprs=nd1, ty=SOME nd2})  (* FIXME: more specific type for exprs *)
                    end
              | ((RightBracket, _) :: _, _) =>
                    (tl ts1, Ast.LiteralArray {exprs=nd1, ty=NONE})
              | _ => error ["unknown token in arrayLiteral [1] ", tokenname (hd ts0)]
            end
      | _ => error ["unknown token in arrayLiteral [2]", tokenname (hd ts0)]
    end

(*
    SpreadExpression
        ... AssignmentExpression(AllowColon, AllowIn)
*)
and spreadExpression (ts0: TOKENS)
    : (TOKENS * Ast.EXPRESSION list) =
    let val _ = trace [">> spreadExpression with next=", tokenname (hd ts0)]
    in case ts0 of
           (TripleDot, _) :: ts1 =>
           let
               val (ts2, nd1) = assignmentExpression (ts1, AllowColon, AllowIn)
           in
               (ts2, [Ast.UnaryExpr (Ast.Spread, nd1)])
           end
         | _ =>
           error ["unknown token in spreadExpression", tokenname (hd ts0)]
    end

(*
    Elements
        empty
        LiteralElement
        SpreadExpression
        ,  ElementList
        LiteralElement  ,  ElementList
        LiteralElement  ElementComprehension
*)

and elements (ts0: TOKENS)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> elements with next=", tokenname (hd (ts0))])
    in case ts0 of
        (RightBracket, _) :: _ => (ts0, Ast.ListExpr [])
      | (Comma, _) :: _ =>
            let
                val (ts1, nd1) = elementList (tl ts0)
            in
                (ts1, Ast.ListExpr (Ast.LiteralUndefined :: nd1))
            end
      | (TripleDot, _) :: _ =>
            let
                val (ts1, nd1) = spreadExpression ts0
            in
                (ts1, Ast.ListExpr nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = assignmentExpression (ts0, AllowColon, AllowIn)
            in case ts1 of
                (Comma, _) :: _ =>
                    let
                        val (ts2,nd2) = elementList (tl ts1)
                    in
                        trace (["<< elements with next=", tokenname (hd (ts2))]);
                        (ts2, Ast.ListExpr (nd1::nd2))
                    end
              | (For, _) :: _ =>
                    let
                        val (ts2, nd2) = elementComprehension (ts1, nd1)
                    in
                        trace (["<< elements with next=", tokenname (hd (ts2))]);
                        (ts2, nd2)
                    end
              | _ => 
                    let
                    in
                        trace (["<< elements with next=", tokenname (hd (ts1))]);
                        (ts1, Ast.ListExpr (nd1::[]))
                    end
            end
    end

(*
    ElementList
        empty
        SpreadExpression
        ,  ElementList
        LiteralElement
        LiteralElement  ,  ElementList

    LiteralElement
        AssignmentExpression(AllowColon, AllowIn)

*)

and elementList (ts0: TOKENS)
    : (TOKENS * Ast.EXPRESSION list) =
    let
        val _ = trace ([">> elementList with next=", tokenname(hd ts0)])
    in case ts0 of
        (RightBracket, _) :: _ => (ts0, [])
      | (Comma, _) :: _ =>
            let
                val (ts1, nd1) = elementList (tl ts0)
            in
                (ts1, Ast.LiteralUndefined :: nd1)
            end
      | (TripleDot, _) :: _ =>
            spreadExpression ts0
      | _ =>
            let
                val (ts1,nd1) = assignmentExpression (ts0, AllowColon, AllowIn)
            in case ts1 of
                (Comma, _) :: _ =>
                    let
                        val (ts2,nd2) = elementList (tl ts1)
                    in
                        trace (["<< elements with next=", tokenname (hd (ts2))]);
                        (ts2, nd1::nd2)
                    end
              | _ => 
                    let
                    in
                        trace (["<< elements with next=", tokenname (hd (ts1))]);
                        (ts1, nd1::[])
                    end
            end
    end

(*
    ElementComprehension
        ForInExpressionList
        ForInExpressionList  if  ParenListExpression

    ForInExpressionList 
        ForInExpression
        ForInExpressionList  ForInExpression
*)
  
and elementComprehension (ts0: TOKENS, nd0: Ast.EXPRESSION)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace([">> expressionComprehension with next=", tokenname(hd ts0)])
        val (ts1, nd1) = forInExpressionList (ts0)
    in case ts1 of
        (If, _) :: _ =>
            let 
                val (ts2, nd2) = parenListExpression (tl ts1)
            in 
                trace(["<< forInExpression with next=", tokenname(hd ts2)]);
                (ts2, Ast.Comprehension (nd0, nd1, SOME nd2))
            end
      | _ => 
            let
            in
                trace(["<< forInExpression with next=", tokenname(hd ts0)]);
                (ts1, Ast.Comprehension (nd0, nd1, NONE))
            end
    end

and forInExpressionList (ts0: TOKENS)
    : (TOKENS * Ast.FOR_ENUM_HEAD list) =
    let
        fun forInExpressionList' (ts0: TOKENS)
            : (TOKENS * Ast.FOR_ENUM_HEAD list) =
            let val _ = trace([">> forInExpressionList' with next=",tokenname(hd ts0)])
            in case ts0 of
                (For, _) :: _ =>
                    let
                        val (ts1, nd1) = forInExpression (ts0)
                        val (ts2, nd2) = forInExpressionList' (ts1)
                    in
                        trace(["<< forInExpressionList' with next=", tokenname (hd (ts2))]);
                        (ts2, nd1 :: nd2)
                    end
              | _ =>
                    let
                    in
                        trace(["<< forInExpressionList' with next=",tokenname (hd (ts0))]);
                        (ts0, [])
                    end
            end
        val (ts1, nd1) = forInExpression (ts0)
        val (ts2, nd2) = forInExpressionList' (ts1)
    in
        (ts2, nd1::nd2)
    end

(*    
    ForInExpression 
        for  (  ForInBinding  in  ListExpression(AllowColon, AllowIn)  )
        for  each  (  ForInBinding  in  ListExpression(AllowColon, AllowIn)  )
*)

and forInExpression (ts0: TOKENS)
    : (TOKENS * Ast.FOR_ENUM_HEAD) =
    let val _ = trace([">> forInExpression with next=", tokenname(hd ts0)])
        val (ts0,nd0) = case ts0 of (For, _) :: (Each, _) :: _ => (tl (tl ts0),true) | _ => (tl ts0,false)
        val ts0 = case ts0 of (LeftParen, _) :: _ => tl ts0 | _ => error ["found " ^ tokenname (hd ts0) ^ " expected token '('"]
        val (ts1,nd1) = forInBinding (ts0)
        val ts1 = case ts1 of (In, _) :: _ => tl ts1 | _ => error ["found " ^ tokenname (hd ts1) ^ " expected token 'in'"]
        val (ts2,nd2,_) = listExpression (ts1, AllowColon, AllowIn)
        val ts2 = case ts2 of (RightParen, _) :: _ => tl ts2 | _ => error ["found " ^ tokenname (hd ts2) ^ " expected token ')'"]
    in
        (ts2, {isEach=nd0, bindings=nd1, expr=nd2})
    end

(*
    PrimaryExpression(alpha,beta)
        null
        true
        false
        NumberLiteral
        StringLiteral
        RegularExpression
        XMLInitialiser
        ArrayLiteral(alpha)
        ObjectLiteral(alpha)
        FunctionExpression(alpha,beta)
        ThisExpression
        ParenListExpression
        LetExpression(a,b)
        PrimaryName
*)

and primaryExpression (ts0:TOKENS, a:ALPHA, b:BETA)
  : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> primaryExpression with next=", tokenname(hd ts0)])
    in case ts0 of
        (Null, _) :: ts1 => (ts1, Ast.LiteralNull)
      | (True, _) :: ts1 => (ts1, Ast.LiteralBoolean true)
      | (False, _) :: ts1 => (ts1, Ast.LiteralBoolean false)

      | (DecimalLiteral n, _) :: ts1 => (ts1, Ast.LiteralDecimal n)
      | (DoubleLiteral n, _) :: ts1 => (ts1, Ast.LiteralDouble n)

      (* FIXME: may wish to allow "string"::foo as a primary expression form? *)
      | (StringLiteral s,_) :: ts1 => (ts1, Ast.LiteralString s)
      | (This, _) :: _ => 
            let
            in case (tl ts0) of
                (Function, _) :: _ => (tl (tl ts0), Ast.ThisExpr (SOME Ast.FunctionThis))
              | (Generator, _) :: _ => (tl (tl ts0), Ast.ThisExpr (SOME Ast.GeneratorThis))
              | _ => (tl ts0, Ast.ThisExpr NONE)
            end
      | (LeftParen, _) :: _ => parenListExpression (ts0)
      | (LeftBracket, _) :: _ =>
            let
                val (ts1,nd1) = arrayLiteral (ts0, a)
            in
                (ts1,nd1)
            end
      | (LeftBrace, _) :: _ =>
            let
                val (ts1,nd1) = objectLiteral (ts0, a)
            in
                (ts1,nd1)
            end
      | (Function, _) :: _ => functionExpression (ts0,a,b)
      | (LexBreakDiv thunks, _) :: _ =>
        (case (#lex_regexp thunks)() of
             (RegexpLiteral str, _) :: rest =>
             (rest, Ast.LiteralRegExp {str=str})
           | _ => error ["non-regexp-literal token after '/' lexbreak"])

      | (Let, _) :: _ =>
            let
                val (ts1, nd1) = letExpression (ts0, a, b)
            in
                (ts1, nd1)
            end
      | _ =>
            let
                val (ts1,nd1) = nameExpression ts0 false
            in
                (ts1,Ast.LexicalReference {name=nd1, loc=locOf ts0})
            end
    end

(*
    LetExpression(a,b)
        let  (  LetBindingList  )  AssignmentExpression(a,b)
*)

and letExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> letExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (Let, _) :: (LeftParen, _) :: _ =>
            let
                val (ts1, nd1) = letBindingList (tl (tl ts0))
            in case ts1 of
                (RightParen, _) :: _ =>
                    let
                        val (ts2, nd2) = assignmentExpression(tl ts1, alpha, beta)
                    in
                        (trace (["<< letExpression with next=", tokenname (hd (ts2))]);
                        (ts2, Ast.LetExpr {defs=nd1, body=nd2, head=NONE}))
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
        VariableBinding(allowIn)
        VariableBinding(allowIn)  ,  NonemptyLetBindingList
*)

and letBindingList (ts0:TOKENS)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace ([">> letBindingList with next=", tokenname (hd (ts0))])
        fun nonemptyLetBindingList (ts0) 
            : (TOKENS * Ast.BINDINGS) =
            let
                val (ts1, (b1, i1)) = variableBinding (ts0, AllowIn)
            in case ts1 of
                (RightParen, _) :: _ => (ts1, (b1, i1))
              | (Comma, _) :: _ =>
                    let
                        val (ts2, (b2, i2)) = nonemptyLetBindingList (tl ts1)
                    in
                        trace (["<< nonemptyLetBindingList with next=", tokenname (hd ts2)]);
                        (ts2, (b1@b2, i1@i2))
                    end
              | _ => error ["unknown token in letBindingList"]
            end
    in case ts0 of
        (RightParen, _) :: _ =>
            (trace (["<< nonemptyLetBindingList with next=", tokenname (hd ts0)]);
            (ts0, ([], [])))
      | _ =>
            let
                val (ts1, nd1) = nonemptyLetBindingList ts0
            in
                trace (["<< letBindingList with next=", tokenname (hd ts1)]);
                (ts1, nd1)
            end
    end

(*
    SuperExpression
        super
        super  ParenExpression
*)

and superExpression (ts0: TOKENS)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> superExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (Super, _) :: _ =>
            let
            in case tl ts0 of
                (LeftParen, _) :: _ =>
                    let
                        val (ts1, nd1) = parenExpression (tl ts0)
                    in
                        (ts1, Ast.SuperExpr (SOME (nd1)))
                    end
                | _ =>
                    (tl ts0, Ast.SuperExpr (NONE))
            end
      | _ => error ["unknown token in superExpression"]
    end

(*
    Arguments
        (  )
        (  ArgumentList  )
*)

and arguments (ts0: TOKENS)
    : (TOKENS * Ast.EXPRESSION list) =
    let val _ = trace ([">> arguments with next=", tokenname (hd (ts0))])
    in case ts0 of
        (LeftParen, _) :: (RightParen, _) :: _ =>
            let
            in
                trace (["<< arguments with next=", tokenname (hd (tl (tl ts0)))]);
                (tl (tl ts0), [])
            end
      | (LeftParen, _) :: _ =>
            let
                val (ts1,nd1) = argumentList (tl ts0)
            in case ts1 of
                (RightParen, _) :: _ =>
                    let
                    in
                        trace (["<< arguments with next=", tokenname (hd (tl ts1))]);
                        (tl ts1, nd1)
                    end
              | _ => error ["unknown token in arguments"]
            end
      | _ => error ["unknown token in arguments"]
    end

(*
    ArgumentList
        AssignmentExpression(AllowColon, AllowIn)
        AssignmentExpression(AllowColon, AllowIn)  ,  ArgumentList
        SpreadExpression

    refactored:

    ArgumentList
        AssignmentExpression(AllowColon,AllowIn)  ArgumentListPrime
        SpreadExpression

    ArgumentListPrime
        empty
        , AssignmentExpression(AllowColon,AllowIn)  ArgumentListPrime
        , SpreadExpression
*)

and argumentList (ts0: TOKENS)
    : (TOKENS * Ast.EXPRESSION list) =
    let val _ = trace ([">> argumentList with next=", tokenname (hd (ts0))])
        fun argumentList' (ts0) : (TOKENS * Ast.EXPRESSION list) =
            let val _ = trace ([">> argumentList' with next=", tokenname (hd (ts0))])
            in case ts0 of
                (Comma, _) :: (TripleDot, _) :: _ =>
                    spreadExpression (tl ts0)
              | (Comma, _) :: _ =>
                    let
                        val (ts1, nd1) = assignmentExpression (tl ts0, AllowColon, AllowIn)
                        val (ts2, nd2) = argumentList' (ts1)
                    in
                        (ts2, nd1::nd2)
                    end
              | (RightParen, _) :: _ =>
                    (ts0, [])
              | _ =>
                (trace ["*syntax error*: expect '", tokenname (RightParen, 0), "' before '", tokenname (hd ts0), "'"];
                 error ["unknown token in argumentList"])
            end
    in
        case ts0 of
            (TripleDot, _) :: _ =>
                spreadExpression ts0
          | _ =>
            let
                val (ts1, nd1) = assignmentExpression (ts0, AllowColon, AllowIn)
                val (ts2, nd2) = argumentList' (ts1)
            in
                (ts2, nd1::nd2)
            end
    end

(*
    PropertyOperator
        .  ReservedIdentifier
        .  NameExpression
        Brackets

    TypeApplication
        .<  TypeExpressionList  >
*)

and propertyOperator (ts0: TOKENS, nd0: Ast.EXPRESSION)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> propertyOperator with next=", tokenname (hd (ts0))])
    in case ts0 of
           (Dot, _) :: _ =>
           
           if isreserved(hd (tl ts0)) 
           then
               let
                   val (ts1,nd1) = reservedIdentifier (tl ts0)
               in
                   (ts1,Ast.ObjectNameReference
                            { object=nd0,
                              name=Ast.UnqualifiedName
                                       { identifier=nd1,
                                         openNamespaces=[] },
                              loc=locOf ts0})
               end 
           else
               let
                   val (ts1,nd1) = nameExpression (tl ts0) false
               in
                   (ts1, Ast.ObjectNameReference {object=nd0, name=nd1, loc=locOf ts0})
               end
               
      | (LeftBracket, _) :: _ =>
        bracketsOrSlice (ts0, nd0)
        
      | (LeftDotAngle, _) :: _ =>
        let
            val (ts1, nd1) = typeExpressionList (tl ts0)
        in 
            case ts1 of
                (* FIXME: what about >> and >>> *)
                (GreaterThan, _) :: _ => (tl ts1, Ast.ApplyTypeExpression {expr=nd0, actuals=nd1})
              | _ => error ["unknown final token of parametric type expression"]
        end
        
      | _ => error ["unknown token in propertyOperator"]
    end

(*
    Brackets
        [  ListExpression(AllowColon,AllowIn)  ]
        [  SliceExpression   ]

    SliceExpression
        OptionalExpression  :  OptionalExpression
        OptionalExpression  :  OptionalExpression  :  OptionalExpression

    OptionalExpression
        ListExpression(AllowColon,AllowIn)
        empty
*)

and brackets (ts0:TOKENS)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> brackets with next=", tokenname (hd (ts0))])
    in case ts0 of
        (LeftBracket, _) :: _ =>
            let
                val (ts1, nd1, _) = listExpression (tl ts0, AllowColon, AllowIn)
            in case ts1 of
                (RightBracket, _) :: _ => (tl ts1, nd1)
              | _ => error ["unknown token in brackets"]
            end
      | _ => error ["unknown token in brackets"]
    end

and bracketsOrSlice (ts0:TOKENS, nd0:Ast.EXPRESSION)
    : (TOKENS * Ast.EXPRESSION) = 
    let val _ = trace ([">> brackets with next=", tokenname (hd (ts0))])

        fun asBracket e = 
            Ast.ObjectIndexReference
                { object = nd0,
                  index = e,
                  loc = locOf ts0}

        fun asSlice a b c = 
            Ast.CallExpr 
                { func = Ast.ObjectNameReference { object = nd0,
                                                   name = Ast.QualifiedName 
                                                              { namespace = Ast.Namespace Name.intrinsicNS,
                                                                identifier = Ustring.slice_ },
                                                   loc = locOf ts0 },
              actuals = [ a, b, c ] }

        val none = Ast.LiteralDouble (0.0 / 0.0)  (* NaN *)

        fun slice2 ts0 nd1 nd2 = 
            case ts0 of 
                (RightBracket, _) :: _ => (tl ts0, asSlice nd1 nd2 none)
              | _ => 
                let
                    val (ts3, nd3, _) = listExpression (ts0, AllowColon, AllowIn)
                in
                    case ts3 of 
                        (RightBracket, _) :: _ => (tl ts3, asSlice nd1 nd2 nd3)
                      | _ => error ["unknown token in slice"]                                                    
                end

        fun slice1 ts0 nd0 = 
            case ts0 of 
                (RightBracket, _) :: _ => (tl ts0, asSlice nd0 none none)
              | (Colon, _) :: _ => slice2 (tl ts0) nd0 none
              | _ => 
                let
                    val (ts1,nd1,_) = listExpression (ts0, AllowColon, AllowIn)
                in
                    case ts1 of
                        (RightBracket, _) :: _ => (tl ts1, asSlice nd0 nd1 none)
                      | (Colon, _) :: _ => slice2 (tl ts1) nd0 nd1
                      | _ => error ["unknown token in slice"]
                end
    in
        case ts0 of
            (LeftBracket, _) :: (Colon, _) :: _ => slice1 (tl (tl ts0)) none 
          | (LeftBracket, _) :: (DoubleColon, x) :: _ => slice1 ((Colon, x) :: (tl (tl ts0))) none 
          | (LeftBracket, _) :: _ => 
            let
                val (ts1, nd1, _) = listExpression (tl ts0, AllowColon, AllowIn)
            in 
                case ts1 of
                    (Colon, _) :: _ => slice1 (tl ts1) nd1
                  | (DoubleColon, x) :: _ => slice1 ((Colon, x) :: (tl ts1)) nd1
                  | (RightBracket, _) :: _ => (tl ts1, asBracket nd1)
                  | _ => error ["unknown token in brackets"]
            end
          | _ => error ["unknown token in brackets"]
    end
    
(*
    MemberExpression
        PrimaryExpression
        new  MemberExpression  Arguments
        SuperExpression  PropertyOperator
        MemberExpression  PropertyOperator

    Refactored:

    MemberExpression
        PrimaryExpression MemberExpressionPrime
        new MemberExpression Arguments MemberExpressionPrime
        SuperExpression  PropertyOperator  MemberExpressionPrime

    MemberExpressionPrime
        PropertyOperator MemberExpressionPrime
        empty
*)

and memberExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> memberExpression with next=", tokenname (hd (ts0))])

        fun memberExpression' (ts0:TOKENS, nd0:Ast.EXPRESSION, alpha:ALPHA, beta:BETA)
            : (TOKENS * Ast.EXPRESSION) =
            let val _ = trace ([">> memberExpression' with next=", tokenname (hd (ts0))])
                fun withPropertyOperator () =
                    let
                        val (ts1, nd1) = propertyOperator(ts0, nd0)
                        val (ts2, nd2) = memberExpression' (ts1, nd1, alpha, beta)
                    in
                        trace (["<< memberExpression' with next=", tokenname (hd (ts2))]);
                        (ts2, nd2)
                    end
            in case ts0 of
                (LeftBracket, _) :: _ => withPropertyOperator ()
              | (Dot, _) :: _ => withPropertyOperator ()
              | (LeftDotAngle, _) :: _ => withPropertyOperator ()
              | _ =>
                    let
                    in
                        trace (["<< memberExpression' with next=", tokenname (hd (ts0))]);
                        (ts0, nd0)
                    end
            end

    in case ts0 of
        (New, _) :: _ =>
            let
                val (ts1, nd1) = memberExpression (tl ts0, alpha, beta)
            in case ts1 of
                (LeftParen, _) :: _ =>
                    let
                        val (ts2, nd2) = arguments (ts1)
                        val (ts3, nd3) = memberExpression' (ts2, Ast.NewExpr {obj=nd1, actuals=nd2}, alpha, beta)
                    in
                        (ts3, nd3)
                    end
              | _ =>
                    let
                    in
                        (ts1, Ast.NewExpr {obj=nd1, actuals=[]}) (* short new, we're done *)
                    end
            end
      | (Super, _) :: _ =>
            let
                val (ts1, nd1) = superExpression (ts0)
                val (ts2, nd2) = propertyOperator (ts1, nd1)
                val (ts3, nd3) = memberExpression' (ts2, nd2, alpha, beta)
            in
                (ts3,nd3)
            end

      | _ =>
            let
                val (ts1, nd1) = primaryExpression (ts0, alpha, beta)
                val (ts2, nd2) = memberExpression' (ts1, nd1, alpha, beta)
            in
                trace (["<< memberExpression with next=", tokenname (hd ts2)]);
                (ts2, nd2)
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

and callExpression' (ts0:TOKENS, nd0:Ast.EXPRESSION, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> callExpression' with next=", tokenname (hd (ts0))])
        fun withPropertyOperator () =
            let
                val (ts1, nd1) = propertyOperator (ts0, nd0)
                val (ts2, nd2) = callExpression' (ts1, nd1, alpha, beta)
            in
                trace (["<< callExpressionPrime with next=", tokenname (hd (ts2))]);
                (ts2, nd2)
            end
    in case ts0 of
        (LeftBracket, _) :: _ => withPropertyOperator ()
      | (Dot, _) :: _ => withPropertyOperator ()
      | (LeftParen, _) :: _ =>
            let
                val (ts1, nd1) = arguments (ts0)
                val (ts2, nd2) = callExpression' (ts1, Ast.CallExpr ({func=nd0, actuals=nd1}), alpha, beta)
            in
                trace (["<< callExpression' with next=", tokenname (hd (ts2))]);
                (ts2, nd2)
            end
      | _ =>
            let
            in
                trace (["<< callExpression' with next=", tokenname (hd (ts0))]);
                (ts0, nd0)
            end
    end

(*
    NewExpression(a,b)
        MemberExpression(a,b)
        new  NewExpression(a,b)
*)

and newExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> newExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (New, _) :: (New, _) :: _ =>
            let
                val (ts1, nd1) = newExpression(tl ts0, alpha, beta)  (* eat only the first new *)
            in
                trace (["<< newExpression new new with next=", tokenname (hd (ts1))]);
                (ts1, Ast.NewExpr ({obj=nd1,actuals=[]}))
            end
      | (New, _) :: _ =>
            let
                val (ts1, nd1) = memberExpression (ts0, alpha, beta)  (* don't eat new, let memberExpr eat it *)
            in
                trace (["<< newExpression with next=", tokenname (hd (ts1))]);
                (ts1, nd1)
            end
      | _ =>
            let
                val (ts1, nd1) = memberExpression (ts0, alpha, beta)
            in case ts1 of
                (LeftParen, _) :: _ =>
                    let
                        val (ts2, nd2) = callExpression' (ts0, nd1, alpha, beta)
                    in
                        (ts2, nd2)
                    end
              | _ =>
                    let
                    in
                        (ts1, nd1)
                    end
            end
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

and leftHandSideExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> leftHandSideExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (New, _) :: (New, _) :: _ =>
            let
                val (ts1, nd1) = newExpression(ts0, alpha, beta)
            in
                (ts1, nd1)
            end
      | _ =>
            let
                val (ts1, nd1) = memberExpression (ts0, alpha, beta)
            in case ts1 of
                (LeftParen, _) :: _ =>
                    let
                        val (ts2,nd2) = arguments (ts1)
                        val (ts3,nd3) = callExpression' (ts2, Ast.CallExpr {func=nd1, actuals=nd2}, alpha, beta)
                    in
                        trace (["<< leftHandSideExpression with next=", tokenname (hd (ts3))]);
                        (ts3, nd3)
                    end
              | _ =>
                    let
                    in
                        trace(["<< leftHandSideExpression with next=",tokenname (hd (ts1))]);
                        (ts1, nd1)
                    end
            end
    end

(*
    PostfixExpression(a, b)
        LeftHandSideExpression(a, b)
        LeftHandSideExpression(a, b)  [no line break]  ++
        LeftHandSideExpression(a, b)  [no line break]  --
*)

and postfixExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> postfixExpression with next=", tokenname (hd (ts0))])
        val (ts1, nd1) = leftHandSideExpression (ts0, alpha, beta)
    in case ts1 of
        (PlusPlus, _) :: ts2 => (ts2, Ast.UnaryExpr (Ast.PostIncrement, nd1))
      | (MinusMinus, _) :: ts2 => (ts2, Ast.UnaryExpr (Ast.PostDecrement, nd1))
      | _ => (trace (["<< postfixExpression"]); (ts1, nd1))
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
*)

and unaryExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> unaryExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (Delete, _) :: _ =>
            let
                val (ts1, nd1) = postfixExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.Delete, nd1))
            end
      | (Void, _) :: _ =>
            let
                val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.Void, nd1))
            end
      | (TypeOf, _) :: _ =>
            let
                val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.Typeof, nd1))
            end
      | (PlusPlus, _) :: _ =>
            let
                val (ts1, nd1) = postfixExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.PreIncrement, nd1))
            end
      | (MinusMinus, _) :: _ =>
            let
                val (ts1, nd1) = postfixExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.PreDecrement, nd1))
            end
      | (Plus, _) :: _ =>
            let
                val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.UnaryPlus, nd1))
            end
      | (Minus, _) :: _ =>
            let
                val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.UnaryMinus, nd1))
            end
      | (BitwiseNot, _) :: _ =>
            let
                val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.BitwiseNot, nd1))
            end
      | (Not, _) :: _ =>
            let
                val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
            in
                (ts1, Ast.UnaryExpr (Ast.LogicalNot, nd1))
            end
      | _ =>
            let
                val (ts1, nd1) = postfixExpression (ts0, alpha, beta)
            in
                trace (["<< unaryExpression with next=", tokenname (hd (ts1))]);
                (ts1, nd1)
            end
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

and multiplicativeExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> multiplicativeExpression with next=", tokenname (hd ts0)])

        fun multiplicativeExpression' (ts0, nd0, alpha, beta) =
            let val _ = trace ([">> multiplicativeExpression' with next=", tokenname (hd ts0)])
            in
            case ts0 of
                (Mult, _) :: _ =>
                    let
                        val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
                    in
                        multiplicativeExpression' (ts1, Ast.BinaryExpr (Ast.Times, nd0, nd1), alpha, beta)
                    end

              | (LexBreakDiv x, _) :: _ =>
                    let
                        val ts0 = (#lex_initial x) ()  (* scan until next slash *)
                    in case ts0 of
                        (Div, _) :: _ =>
                            let
                                val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
                            in
                                multiplicativeExpression' (ts1, Ast.BinaryExpr (Ast.Divide, nd0, nd1), alpha, beta)
                            end
                      | (DivAssign, divAssignLoc) :: _ =>
                            let
                            in
                                trace (["<< multiplicative"]);
                                ((DivAssign, divAssignLoc) :: (tl ts0), nd0)
                            end
                      | _ => error ["missing token in multiplicativeExpression"]
                    end

              | (Modulus, _) :: _ =>
                    let
                        val (ts1, nd1) = unaryExpression (tl ts0, alpha, beta)
                    in
                        multiplicativeExpression' (ts1, Ast.BinaryExpr (Ast.Remainder, nd0, nd1), alpha, beta)
                    end
              | _ => (trace (["<< multiplicativeExpression'"]); (ts0, nd0))
            end
    in case ts0 of
        (Type, _) :: _ =>
            let
                val (ts1, nd1) = unaryTypeExpression (ts0, alpha, beta)
            in
                (ts1, nd1)
            end
      | _ =>
            let
                val (ts1, nd1) = unaryExpression (ts0, alpha, beta)
                val (ts2, nd2) = multiplicativeExpression' (ts1, nd1, alpha, beta)
            in
                trace (["<< multiplicativeExpression with next=", tokenname (hd ts2)]);
                (ts2, nd2)
            end
    end

(*
    AdditiveExpression
        MultiplicativeExpression
        AdditiveExpression  +  MultiplicativeExpression
        AdditiveExpression  -  MultiplicativeExpression

    right recursive: (see pattern of MultiplicativeExpression)
*)

and additiveExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> additiveExpression with next=", tokenname (hd (ts0))])

        fun additiveExpression' (ts0, nd0, alpha, beta) =
            case ts0 of
                (Plus, _) :: _ =>
                    let
                        val (ts1, nd1) = multiplicativeExpression (tl ts0, alpha, beta)
                    in
                        additiveExpression' (ts1, Ast.BinaryExpr (Ast.Plus, nd0, nd1), alpha, beta)
                    end
              | (Minus, _) :: _ =>
                    let
                        val (ts1, nd1) = multiplicativeExpression (tl ts0, alpha, beta)
                    in
                        additiveExpression' (ts1, Ast.BinaryExpr (Ast.Minus, nd0, nd1), alpha, beta)
                    end
              | _ =>
                    (trace(["<< additiveExpression"]);
                    (ts0, nd0))

        val (ts1, nd1) = multiplicativeExpression (ts0, alpha, beta)
    in
        additiveExpression' (ts1, nd1, alpha, beta)
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
    : (TOKENS * Ast.EXPRESSION) =
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
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace([">> relationalExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = shiftExpression (ts,a,b)
        fun relationalExpression' (ts1,nd1,a,b) =
            case (ts1,b) of
                (((LexBreakLessThan x),_) :: _,_) =>
                    let
                        val ts1 = (#lex_initial x)()
                    in case ts1 of
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
    : (TOKENS * Ast.EXPRESSION) =
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
    : (TOKENS * Ast.EXPRESSION) =
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
    : (TOKENS * Ast.EXPRESSION) =
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
    : (TOKENS * Ast.EXPRESSION) =
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
        bitwiseOrExpression' (ts1, nd1)
    end

(*
    LogicalAndExpression(b)
        BitwiseOrExpression(b)
        LogicalAndExpression(b)  &&  BitwiseOrExpression(b)
*)

and logicalAndExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPRESSION) =
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
    LogicalOrExpression(a,b)
        LogicalAndExpression(a,b)
        LogicalOrExpression(a,b)  ||  LogicalAndExpression(a,b)

*)

and logicalOrExpression (ts:TOKENS, a:ALPHA, b:BETA)
    : (TOKENS * Ast.EXPRESSION) =
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
    ConditionalExpression(a,b)
        UnaryTypeExpression
        YieldExpression(a,b)
        LogicalOrExpression(a,b)
        LogicalOrExpression(a,b)  ?  AssignmentExpression(NoColon,b)
                                  :  AssignmentExpression(a,b)

*)

and conditionalExpression (ts0:TOKENS, alpha: ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> conditionalExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (Type, _) :: _ => unaryTypeExpression(ts0, alpha, beta)
      | (Yield, _) :: _ =>
            let
                val (ts1, nd1) = yieldExpression (ts0, alpha, beta)
            in
                (ts1, nd1)
            end
      | _ =>
            let
                val (ts1, nd1) = logicalOrExpression (ts0, alpha, beta)
            in case ts1 of
                (QuestionMark, _) :: _ =>
                    let
                        val (ts2, nd2) = assignmentExpression (tl ts1, NoColon, beta)
                    in case ts2 of
                        (Colon, _) :: _ =>
                            let
                                val (ts3, nd3) = assignmentExpression (tl ts2, alpha, beta)
                            in
                                (ts3, Ast.ConditionalExpression (nd1, nd2, nd3))
                            end
                      | _ => error ["unknown token in conditionalExpression"]
                    end
              | _ =>
                    (trace (["<< conditionalExpression  with next=", tokenname (hd (ts1))]);
                    (ts1, nd1))
            end
        end

(*
    NonAssignmentExpression(a, b)
        UnaryTypeExpression
        YieldExpression(a,b)
        LogicalOrExpression(a,b)
        LogicalOrExpression(a,b)  ?  NonAssignmentExpression(noColon, b)
                                  :  NonAssignmentExpression(a, b)

*)

and nonAssignmentExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> nonAssignmentExpression  with next=", tokenname (hd (ts0))])
    in case ts0 of
        (Type, _) :: _ => unaryTypeExpression(ts0, alpha, beta)
      | (Yield, _) :: _ =>
            let
                val (ts1, nd1) = yieldExpression (ts0, alpha, beta)
            in
                (ts1, nd1)
            end
      | _ =>
            let
                val (ts1, nd1) = logicalOrExpression (ts0, alpha, beta)
            in case ts1 of
                (QuestionMark, _) :: _ =>
                    let
                        val (ts2, nd2) = nonAssignmentExpression (tl ts1, NoColon, beta)
                    in case ts2 of
                        (Colon, _) :: _ =>
                            let
                                val (ts3, nd3) = nonAssignmentExpression (tl ts2, alpha, beta)
                            in
                                (ts3, nd3)
                            end
                      | _ => error ["unknown token in nonAssignmentExpression"]
                    end
              | _ =>
                    (trace (["<< nonAssignmentExpression  with next=", tokenname (hd (ts1))]);
                    (ts1, nd1))
            end
        end

(*
    UnaryTypeExpression(a, b)
        type  TypeExpression
*)

and unaryTypeExpression (ts0:TOKENS, alpha:ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> unaryTypeExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (Type, _) :: _ =>
            let
                val (ts1, nd1) = typeExpression (tl ts0)
            in
                (ts1, Ast.TypeExpr nd1)
            end
      | _ => error ["expecting 'type'"]
     end

(*
    YieldExpression(a,b)
        yield
        yield  [no line break]  AssignmentExpression(a,b)
*)

and yieldExpression (ts0:TOKENS, alpha: ALPHA, beta:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace ([">> yieldExpression with next=", tokenname (hd (ts0))])
    in case ts0 of
        (Yield, _) :: _ =>
            let
            in case (tl ts0) of
                (SemiColon,  _) :: _ => (tl ts0, Ast.YieldExpr NONE)
              | (RightBrace, _) :: _ => (tl ts0, Ast.YieldExpr NONE)
              | (RightParen, _) :: _ => (tl ts0, Ast.YieldExpr NONE)
              | _ =>
                    let
                        val (ts1,nd1) = assignmentExpression(tl ts0, alpha, beta)
                    in
                        (ts1, Ast.YieldExpr (SOME nd1))
                    end
            end
      | _ => error ["unknown token in yieldExpression"]
    end

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
    : (TOKENS * Ast.EXPRESSION) =
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
                val (binds,inits) = desugarPattern (locOf ts) p Ast.AnyType (SOME nd2) 0  
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

and listExpression (ts: TOKENS, a: ALPHA, b: BETA)
    : (TOKENS * Ast.EXPRESSION * (Ast.LOC option)) =
    let
        val _ =    trace([">> listExpression with next=",tokenname(hd ts)])
        fun listExpression' (ts,a,b) : (TOKENS * Ast.EXPRESSION list) =
            let
                val _ =    trace([">> listExpression' with next=",tokenname(hd ts)])
            in case ts of
                (Comma, _) :: _ =>
                    let
                        val (ts1,nd1) = assignmentExpression(tl ts,AllowColon,b)
                        val (ts2,nd2) = listExpression'(ts1,a,b)
                    in
                        (trace(["<< listExpression' with next=",tokenname(hd(ts2))]);
                        (ts2, nd1 :: nd2))
                    end
              | _ =>
                    (trace(["<< listExpression' with next=",tokenname(hd(ts))]);
                    (ts, []))
            end
        val (ts1,nd1) = assignmentExpression(ts,AllowColon,b)
        val (ts2,nd2) = listExpression'(ts1,a,b)
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

and patternFromExpr (e:Ast.EXPRESSION)
    : PATTERN =
    let val _ = trace([">> patternFromExpr"])
    in case e of
        Ast.LiteralObject {...} => objectPatternFromExpr (e)
      | Ast.LiteralArray {...} => arrayPatternFromExpr (e)
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

and simplePatternFromExpr (e:Ast.EXPRESSION)
    : PATTERN =  (* only ever called from AllowExpr contexts *)
    let val _ = trace([">> simplePatternFromExpr"])
    in case e of
        Ast.ObjectNameReference _ => (trace(["<< simplePatternFromExpr"]); SimplePattern e)
      | Ast.ObjectIndexReference _ => (trace(["<< simplePatternFromExpr"]); SimplePattern e)
      | Ast.LexicalReference _ => (trace(["<< simplePatternFromExpr"]); SimplePattern e)
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

and objectPatternFromExpr (e:Ast.EXPRESSION)
    : PATTERN =
    let val _ = trace([">> objectPatternFromExpr"])
    in case e of
        (Ast.LiteralObject {expr,ty}) =>
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
        NonAttributeQualifiedIdentifier  :  Pattern(AllowColon,AllowIn,g)
*)

and destructuringField (ts:TOKENS, g:GAMMA)
    : (TOKENS * FIELD_PATTERN) =
    let
        val (ts1,nd1) = nameExpression ts true
    in case ts1 of
        (Colon, _) :: _ =>
            let
                val (ts2,nd2) = pattern (tl ts1,AllowColon,AllowIn,g)
            in
                (ts2,{name=nd1,pattern=nd2})
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
        {name=name,pattern=p}
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

and arrayPatternFromExpr (e:Ast.EXPRESSION)
    : PATTERN =
    let val _ = trace([">> arrayPatternFromExpr"])
    in case e of
        Ast.LiteralArray {exprs,ty} =>
            let
                val el = case exprs of Ast.ListExpr el => el | _ => error ["expecting list expr in Array pattern"]
                val p = destructuringElementListFromExpr el
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
                (ts1,SimplePattern(Ast.LiteralUndefined) :: nd1)
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

and destructuringElementListFromExpr (e:Ast.EXPRESSION list)
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
        Pattern(AllowCollon,AllowIn,g)
*)

and destructuringElement (ts:TOKENS, g:GAMMA)
    : (TOKENS * PATTERN) =
    let val _ = trace([">> destructuringElement with next=",tokenname(hd(ts))])
    in
        pattern (ts,AllowColon,AllowIn,g)
    end

and destructuringElementFromExpr (e:Ast.EXPRESSION)
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
    : TOKENS * (PATTERN * Ast.TYPE) =
    let val _ = trace([">> typedIdentifier with next=",tokenname(hd(ts))])
        val (ts1,nd1) = simplePattern (ts,AllowColon,NoIn,NoExpr)
    in case ts1 of
        (Colon, _) :: _ =>
            let
                val (ts2,nd2) = typeExpression (tl ts1)
            in
                (ts2,(nd1,nd2))
            end
      | _ =>
            let
            in
                trace ["<< typedIdentifier with next=", tokenname(hd(ts1))];
                (ts1,(nd1,Ast.AnyType))
            end

    end

(*
    TypedPattern(a,b,g)
        SimplePattern(a,b,g)
        SimplePattern(a,b,g)  :  TypeExpression
        ObjectPattern
        ObjectPattern  :  RecordType
        ArrayPattern
        ArrayPattern  :  ArrayType
*)

and typedPattern (ts:TOKENS, b:BETA)
    : (TOKENS * (PATTERN * Ast.TYPE)) =
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
                        (ts2,(nd1,nd2))
                    end
              | _ =>
                (ts1,(nd1,Ast.AnyType))  (* FIXME: this could be {*:*} to be more specific *)
            end
      | (LeftBracket, _) :: _ =>
            let
                val (ts1,nd1) = arrayPattern (ts,NoExpr)
            in case ts1 of
                (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in
                        (ts2,(nd1,nd2))
                    end
              | _ =>
                    (ts1,(nd1,Ast.ArrayType ([],NONE)))
            end
      | _ =>
            let
                val (ts1,nd1) = simplePattern (ts,AllowColon,b,NoExpr)
            in case ts1 of
                (Colon, _) :: _ =>
                    let
                        val (ts2,nd2) = typeExpression (tl ts1)
                    in
                        (ts2,(nd1,nd2))
                    end
              | _ =>
                    (trace ["<< typedPattern with next=",tokenname(hd ts1)];
                     (ts1,(nd1,Ast.AnyType)))
            end
    end

(*
    TYPE EXPRESSIONS
*)

(*
    TypeExpression
        NonNullTypeExpression
        like  NonNullTypeExpression

    NonNullTypeExpression
        BasicTypeExpression
        BasicTypeExpression  ?
        BasicTypeExpression  !

    BasicTypeExpression
        *
        null
        undefined
        FunctionType
        UnionType
        RecordType
        ArrayType
        PrimaryName
*)

and typeExpression (ts0:TOKENS)
    : (TOKENS * Ast.TYPE) =
    let val _ = trace ([">> typeExpression with next=", tokenname (hd ts0)])
    in case ts0 of
(*        (Like, _) :: _ =>
            let
                val (ts1, nd1) = nullableTypeExpression (tl ts0)
            in
                trace (["<< typeExpression with next=", tokenname (hd ts1)]);
                (ts1, Ast.LikeType nd1)
            end
      |*)  _ =>
            let
                val (ts1, nd1) = nullableTypeExpression (ts0)
            in
                trace (["<< typeExpression with next=", tokenname (hd ts1)]);
                (ts1, nd1)
            end
    end

and nullableTypeExpression (ts0:TOKENS)
    : (TOKENS * Ast.TYPE) =
    let val _ = trace ([">> nullableTypeExpression with next=", tokenname (hd ts0)])
        val (ts1, nd1) = basicTypeExpression ts0
    in case ts1 of
        (Not, _) :: _ =>
            (trace (["<< nullableTypeExpression with next=", tokenname (hd ts0)]);
            (tl ts1, Ast.NonNullType nd1))
      | (QuestionMark, _) :: _ =>
            (trace (["<< nullableTypeExpression with next=", tokenname (hd ts0)]);
            (tl ts1, Ast.UnionType [nd1, Ast.NullType]))
      | _ =>
            (trace (["<< nullableTypeExpression with next=", tokenname (hd ts0)]);
            (ts1, nd1))
    end

and basicTypeExpression (ts0:TOKENS)
    : (TOKENS * Ast.TYPE) =
    let val _ = trace ([">> basicTypeExpression with next=", tokenname (hd ts0)])
    in case ts0 of
        (Mult, _) :: _ => (tl ts0, Ast.AnyType)
      | (Null, _) :: _ => (tl ts0, Ast.NullType)
      | (Undefined, _) :: _ => (tl ts0, Ast.UndefinedType)
      | (Function, _) :: _ => functionType ts0
      | (LeftParen, _) :: _ => unionType ts0
      | (LeftBrace, _) :: _ => objectType ts0
      | (LeftBracket, _) :: _ => arrayType ts0
      | _ =>
            let
                val (ts1, nd1) = nameExpression ts0 false
            in case ts1 of
                (LeftDotAngle, _) :: _ => 
                    let
                        val (ts2,nd2) = typeExpressionList (tl ts1)
                    in case ts2 of
                        (GreaterThan, _) :: _ =>   (* FIXME: what about >> and >>> *)
                        (tl ts2, Ast.AppType ( typeName nd1, nd2 ))
                      | _ => error ["unknown final token of AppType type expression"]
                    end
              | _ => (ts1, typeName nd1)
            end
    end

(*
    FunctionType
        function  FunctionSignature
*)

and functionType (ts0:TOKENS)
    : (TOKENS * Ast.TYPE) =
    let val _ = trace ([">> functionType with next=", tokenname (hd (ts0))])
    in case ts0 of
        (Function, _) :: _ =>
            let
                val (ts1, nd1) = functionSignatureType (tl ts0)
            in
                trace (["<< functionType with next=", tokenname (hd ts1)]);
                (ts1, (functionTypeFromSignature nd1))
            end
      | _ => error ["unknown token in functionType"]
    end

and functionTypeFromSignature (fsig:Ast.FUNC_SIG)
    : Ast.TYPE =
    let
        val Ast.FunctionSignature {typeParams,
                                   params,
                                   paramTypes,
                                   returnType,
                                   thisType,
                                   hasRest,
                                   defaults,...} = fsig
        val (b,i) = params
        val thisType = case thisType of SOME x => x | NONE => Ast.AnyType
        val typeExpr = Ast.FunctionType {typeParams=typeParams,
                                         params=paramTypes,
                                         result=returnType,
                                         thisType=thisType,
                                         hasRest=hasRest,
                                         minArgs=(length paramTypes)-(length defaults)}
    in
        typeExpr
    end

(*
    UnionType
        (  TypeUnionList  )

    TypeUnionList
        TypeExpression
        TypeUnionList  |  TypeExpression

    refactored:

    TypeUnionList
        TypeExpression TypeUnionList'

    TypeUnionList'
        empty
        | TypeExpression TypeUnionList'

*)

and unionType (ts0:TOKENS)
    : (TOKENS * Ast.TYPE)  =
    let val _ = trace ([">> unionType with next=", tokenname (hd (ts0))])
    in case ts0 of
        (LeftParen, _) :: (RightParen, _) :: _ =>  (* unit type *)
            (tl (tl ts0), Ast.UnionType [])
      | (LeftParen, _) :: _ =>
            let
                val (ts1, nd1) = typeUnionList (tl ts0)
            in case ts1 of
                (RightParen, _) :: _ =>
                    (tl ts1, Ast.UnionType nd1)
              | _ => error ["unknown token in unionType"]
            end
      | _ => error ["unknown token in unionType"]
    end

and typeUnionList (ts0:TOKENS)
    : (TOKENS * Ast.TYPE list) =
    let
        fun typeUnionList' (ts0) =
            let
            in case ts0 of
                (BitwiseOr, _) :: _ =>
                    let
                        val (ts1, nd1) = typeExpression (tl ts0)
                        val (ts2, nd2) = typeUnionList' ts1
                    in
                        (ts2, nd1::nd2)
                    end
              | _ => (ts0, [])
            end
            
        val _ = trace ([">> typeExpressionList with next=", tokenname (hd (ts0))])
        val (ts1, nd1) = typeExpression ts0
        val (ts2, nd2) = typeUnionList' ts1
    in
        trace (["<< typeExpressionList with next=", tokenname(hd(ts0))]);
        (ts2, nd1::nd2)
    end

(*
    RecordType
        {  FieldTypeList  }


RecordType  
    {  FieldTypeList  }
    
FieldTypeList   
    empty
    FieldType
    FieldType  ,  FieldTypeList
    
FieldType   
    FieldName
    FieldName  :  TypeExpression

*)

and objectType (ts0:TOKENS)
    : (TOKENS * Ast.TYPE) =
    let val _ = trace ([">> objectType with next=", tokenname (hd (ts0))])
    in case ts0 of
        (LeftBrace, _) :: ts1 =>
            let
                val (ts2,nd2) = fieldTypeList ts1
            in case ts2 of
                (RightBrace, _) :: ts3 =>
                    (trace (["<< objectType with next=", tokenname (hd (ts3))]);
                    (ts3, Ast.RecordType nd2))
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

and fieldTypeList (ts0:TOKENS)
    : (TOKENS * (Ast.FIELD_TYPE list)) =
    let val _ = trace([">> fieldTypeList with next=", tokenname (hd (ts0))])
        fun nonemptyFieldTypeList (ts0) =
            let
                val (ts1,nd1) = fieldType(ts0)
            in case ts1 of
                (Comma, _) :: _ =>
                    let
                        val (ts2,nd2) = nonemptyFieldTypeList (tl ts1)
                    in
                        (ts2, nd1::nd2)
                    end
              | _ => (ts1, nd1::[])
            end
    in case ts0 of
        (RightBrace, _) :: _ => (ts0, [])
      | _ =>
        let
            val (ts1, nd1) = nonemptyFieldTypeList (ts0)
        in
            (ts1, nd1)
         end
    end

(*
    FieldType
        NameExpression  :  TypeExpression
*)

and fieldType (ts0:TOKENS)
    : (TOKENS * Ast.FIELD_TYPE) =
    let val _ = trace ([">> fieldType with next=", tokenname (hd (ts0))])
        val (ts1, nd1) = nameExpression ts0 true
    in case ts1 of
        (Colon, _) :: _ =>
            let
                val (ts2, nd2) = typeExpression (tl ts1)
            in
                (ts2, (nd1, nd2))
            end
      | _ => error ["unknown token in fieldType"]
    end

(*
    ArrayType
        [  ElementTypeList  ]
*)

and arrayType (ts0:TOKENS)
    : (TOKENS * Ast.TYPE) =
    let val _ = trace ([">> arrayType with next=", tokenname (hd (ts0))])
    in case ts0 of
        (LeftBracket, _) :: _ =>
            let
                val (ts1, nd1) = elementTypeList (tl ts0)
            in case ts1 of
                (RightBracket, _) :: _ => (tl ts1, Ast.ArrayType (nd1,NONE))
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

and elementTypeList (ts0:TOKENS)
    : (TOKENS * Ast.TYPE list) =
    let val _ = trace ([">> elementTypeList with next=", tokenname (hd (ts0))])
    in case ts0 of
        (RightBracket, _) :: _ => (ts0, [])
      | (Comma, _) :: _ =>
            let
                val (ts1, nd1) = elementTypeList (tl ts0)
            in
                (ts1, Ast.AnyType :: nd1)
            end
      | _ =>
            let
                val (ts1, nd1) = typeExpression (ts0)
            in case ts1 of
                (Comma, _) :: _ =>
                    let
                        val (ts2, nd2) = elementTypeList (tl ts1)
                    in
                        (ts2, nd1::nd2)
                    end
              | _ => (ts1, nd1::[])
            end
    end

(*
    TypeExpressionList
        TypeExpression
        TypeExpressionList  ,  TypeExpression
*)

and typeExpressionList (ts0:TOKENS)
    : (TOKENS * Ast.TYPE list) =
    let
        fun typeExpressionList' (ts0) =
            let
            in case ts0 of
                (Comma, _) :: _ =>
                    let
                        val (ts1, nd1) = typeExpression (tl ts0)
                        val (ts2, nd2) = typeExpressionList' ts1
                    in
                        (ts2, nd1::nd2)
                    end
              | _ => (ts0, [])
            end
        val _ = trace ([">> typeExpressionList with next=", tokenname(hd(ts0))])
        val (ts1, nd1) = typeExpression ts0
        val (ts2, nd2) = typeExpressionList' ts1
    in
        trace (["<< typeExpressionList with next=", tokenname(hd(ts0))]);
        (ts2, nd1::nd2)
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
    : (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
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
  (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
    let val _ = trace([">> expressionStatement with next=", tokenname(hd ts)])
        val (ts1,nd1,_) = listExpression(ts, AllowColon, AllowIn)
    in
        trace(["<< expressionStatement with next=", tokenname(hd ts1)]);
        (ts1,Ast.ExprStmt(nd1))
    end

(*
    WithStatement(w)
        with  TypedExpression  Substatement(w)
*)

and withStatement (ts:TOKENS, w:OMEGA)
    : (TOKENS * Ast.STATEMENT) =
    let val _ = trace([">> withStatement with next=", tokenname(hd ts)])
    in case ts of
        (With, _) :: (LeftParen, _) :: _ =>
            let
                val (ts1,(e1,t1)) = typedExpression (tl ts)
                val (ts2,nd2) = substatement (ts1,w)
            in
                (ts2,Ast.WithStmt {obj=e1,ty=t1,body=nd2})
            end
      | _ => error ["unknown token in withStatement"]
    end

(*
    TypedExpression
        ParenListExpression
        ParenListExpression  :  NonNullTypeExpression
*)

and typedExpression (ts:TOKENS)
    : (TOKENS * (Ast.EXPRESSION*Ast.TYPE)) =
    let val _ = trace([">> parenListExpression with next=",tokenname(hd(ts))])
        val (ts1,nd1) = parenListExpression (ts)
    in case ts1 of
        (Colon, _) :: _ =>
            let
                val (ts2,nd2) = typeExpression (tl ts1)
            in
                (ts2,(nd1,nd2))
            end
      | _ => (ts1,(nd1,Ast.AnyType))
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
    : (TOKENS * Ast.STATEMENT) =
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
                        (RightBrace, _) :: _ => (tl ts2,Ast.SwitchTypeStmt{cond=e1,ty=t1,cases=nd2})
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

and isDefaultCase (x:Ast.EXPRESSION option) =
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
    : (TOKENS * Ast.EXPRESSION option) =
    let val _ = trace([">> caseLabel with next=", tokenname(hd ts)])
    in case (ts,has_default) of
        ((Case, _) :: _,_) =>
            let
                val (ts1,nd1,_) = listExpression (tl ts,AllowColon,AllowIn)
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
                        val (ts2,nd2) = variableInitialisation(ts1,AllowIn)
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

and isDefaultTypeCase (x:Ast.TYPE) 
    : bool =
    case x of
        Ast.AnyType => true
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
                                                               (isDefaultTypeCase (#ty nd1)))
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
        val (ts2,nd2) = typeCaseElements' (ts1,isDefaultTypeCase (#ty nd1))
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
                             ty=ty,
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
                     ty=Ast.AnyType,
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
    (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
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
                fun desugarForInInit (init:Ast.EXPRESSION) : Ast.EXPRESSION =
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
                                                           Ast.AnyType
                                                           (SOME (Ast.GetParam 0)) 0
                                                                          (* type is meaningless *)
                        val (inits,assigns) = List.partition isInitStep inits
                                                         (* separate init steps and assign steps *)
                        val sets = map makeSetExpr assigns
                        val paramBind = Ast.Binding {ident=Ast.ParamIdent 0,
                                                     ty=Ast.AnyType}
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
                val (ts2,nd2,_) = listExpression (tl ts1,AllowColon,AllowIn)
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
    : (TOKENS * Ast.VAR_DEFN option * Ast.STATEMENT list) =
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
                val (ts1,nd1,_) = listExpression (ts,AllowColon,NoIn)
            in
                trace ["<< forInitialiser with next=", tokenname(hd ts1)];
                (ts1,NONE,[Ast.ExprStmt nd1])
            end
    end

and optionalExpression (ts:TOKENS)
    : (TOKENS * Ast.EXPRESSION) =
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
                val (ts1,nd1,_) = listExpression (ts,AllowColon,NoIn)
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
                val (ts2,(b,i)) = variableBinding (ts1,NoIn)
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
                val (ts1,nd1) = pattern (ts,AllowColon,NoIn,AllowExpr)
                val (b,i) = desugarPattern (locOf ts) nd1 Ast.AnyType (SOME (Ast.GetTemp 0)) 0
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
    : (TOKENS * Ast.STATEMENT) =
    let val _ = trace([">> letStatement with next=", tokenname(hd ts)])
    in case ts of
        (Let, _) :: (LeftParen, _) :: _ =>
            let
                val (ts1,nd1) = letBindingList (tl (tl ts))
                val defn = Ast.VariableDefn {kind=Ast.LetVar,
                                             ns=SOME (Ast.Namespace Name.publicNS),
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
                        val (ts2,nd2,_) = listExpression(tl ts1, AllowColon, AllowIn)
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
    : (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
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
    : (TOKENS * Ast.STATEMENT) =
    let
    in case ts of
        (Return, _) :: (SemiColon, _) :: _ => (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
      | (Return, _) :: (RightBrace, _) :: _ => (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
      | (Return, _) :: _ =>
            if newline(tl ts) then
                (tl ts,Ast.ReturnStmt (Ast.ListExpr []))
            else
                let
                    val (ts1,nd1,_) = listExpression(tl ts, AllowColon, AllowIn)
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
    : (TOKENS * Ast.STATEMENT) =
    let
    in case ts of
        (Throw, _) :: _ =>
            let
                val (ts1,nd1,_) = listExpression(tl ts, AllowColon, AllowIn)
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
    : (TOKENS * Ast.STATEMENT) =
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
                 ty:Ast.TYPE,
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
                 ty:Ast.TYPE,
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
                              ty=ty,
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
    : (TOKENS * Ast.STATEMENT) =
    let val _ = trace([">> defaultXmlNamespaceStatement with next=", tokenname(hd ts)])
    in case ts of
        (Default, _) :: (Xml, _) :: (Namespace, _) :: (Assign, _) :: _ =>
            let
                val (ts1,nd1) = nonAssignmentExpression ((tl (tl (tl (tl ts)))),AllowColon,AllowIn)
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
            { ns = SOME (Ast.Namespace Name.publicNS)
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
        nameExpression
        dynamic
        final
        native
        override
        prototype
        static
        [  AssignmentExpressionallowList, allowIn  ]

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
                val (ts1,nd1) = nameExpression (ts) false
            in
                (tl ts,{
                        ns = SOME (Ast.NamespaceName nd1),
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
                val (ts1,nd1) = nameExpression (ts) false
            in
                (tl ts,{
                        ns = SOME (Ast.NamespaceName nd1),
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
          | (Native,       _) :: _ => (error(["attributes not allowed on local definitions"]))
          | (Override,     _) :: _ => (error(["attributes not allowed on local definitions"]))
          | (Prototype,    _) :: _ => (error(["attributes not allowed on local definitions"]))
          | (Static,       _) :: _ => (error(["attributes not allowed on local definitions"]))
          | (Identifier _, _) :: _ => (error(["attributes not allowed on local definitions"]))
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

and variableDefinition (ts:TOKENS, ns:Ast.NAMESPACE_EXPRESSION option, prototype:bool, static:bool, b:BETA, t:TAU)
    : (TOKENS * Ast.DIRECTIVES) =
    let val _ = trace([">> variableDefinition with next=", tokenname(hd ts)])
        val (ts1,nd1) = variableDefinitionKind(ts)
        val (ts2,(b,i)) = variableBindingList (ts1,b)

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

and variableBindingList (ts:TOKENS, b:BETA)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> variableBindingList with next=", tokenname(hd ts)])
        fun variableBindingList' (ts,b) : (TOKENS * Ast.BINDINGS) =
            let val _ = trace([">> variableBindingList' with next=", tokenname(hd ts)])
            in case ts of
                (Comma, _) :: _ =>
                    let
                        val (ts1,(d1,s1)) = variableBinding(tl ts,b)
                        val (ts2,(d2,s2)) = variableBindingList'(ts1,b)
                    in case (ts2,b) of
                        ((In,_)::_, NoIn) => error ["too many for-in bindings"]
                      | _ =>
                            (trace(["<< variableBindingList' with next=", tokenname(hd ts2)]);
                            (ts2,(d1@d2,s1@s2)))
                    end
              | _ => (ts,([],[]))
            end
        val (ts1,(d1,s1)) = variableBinding(ts,b)
        val (ts2,(d2,s2)) = variableBindingList'(ts1,b)
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

and variableBinding (ts:TOKENS, beta:BETA)
    : (TOKENS * Ast.BINDINGS) =
    let val _ = trace([">> variableBinding with next=", tokenname(hd ts)])
        val (ts1,(p,t)) = typedPattern (ts,beta)  (* parse the more general syntax *)
    in case (ts1,p,beta) of
            ((Assign, _) :: _,_,_) =>
                let
                    val (ts2,nd2) = variableInitialisation (ts1,beta)
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
                    val init = Ast.InitStep (Ast.ParamIdent 0, Ast.LiteralUndefined)  (* for symmetry with above for-in case *)
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

and variableInitialisation (ts:TOKENS, b:BETA)
    : (TOKENS * Ast.EXPRESSION) =
    let val _ = trace([">> variableInitialisation with next=", tokenname(hd ts)])
    in case ts of
        (Assign, _) :: _ =>
            let
                val (ts1,nd1) = assignmentExpression (tl ts,AllowColon,b)
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
                                                              generator=false,
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
                                                                           generator=isGeneratorFunction nd4,
                                                                           loc=unionLoc (locOf ts) blockLoc,
                                                                           block=SOME nd4}})],
                              body=[],
                              head=NONE,
                              loc=locOf ts})
                    end
              | _ =>
                    let
                        val (ts4,nd4,listLoc) = listExpression (ts3, AllowColon, AllowIn)
                        val (ts4,nd4) = (semicolon (ts4,Full),nd4)
                        val block = Ast.Block { pragmas=[],
                                                defns=[],
                                                body=[Ast.ReturnStmt nd4],
                                                head=NONE,
                                                loc=listLoc}
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
                                                             generator=isGeneratorFunction block,
                                                             loc=unionLoc (locOf ts) listLoc,
                                                             block=SOME block}})],
(*
                                                                        (Ast.Block
                                                                             {pragmas=[],
                                                                              defns=[],
                                                                              body=[Ast.ReturnStmt nd4],
                                                                              head=NONE,
                                                                              loc=listLoc})}})],
*)
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
                                             generator=isGeneratorFunction nd4,
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
                        val (ts4,nd4,listLoc) = listExpression (ts3, AllowColon, AllowIn)
                        val (ts4,nd4) = (semicolon (ts4,Full),nd4)
                        val block = Ast.Block { pragmas=[],
                                                defns=[],
                                                body=[Ast.ReturnStmt nd4],
                                                head=NONE,
                                                loc=listLoc}
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
                                                                      generator=isGeneratorFunction block,
                                                                      loc=unionLoc (locOf ts) listLoc,
                                                                      block=SOME block}}],
(*
                                                                                 (Ast.Block { pragmas=[],
                                                                                              defns=[],
                                                                                              body=[Ast.ReturnStmt nd4],
                                                                                              head=NONE,
                                                                                              loc=listLoc})}}],
*)
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
                                             generator=isGeneratorFunction nd4,
                                             block=SOME nd4}
                        val initSteps = [Ast.InitStep (Ast.PropIdent ident, Ast.LiteralFunction func)]
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
                        val (ts4,nd4,listLoc) = listExpression (ts3, AllowColon, AllowIn)
                        val (ts4,nd4) = (semicolon (ts4,Full),nd4)
                        val ident = (#ident nd2)
                        val block = Ast.Block { pragmas=[],
                                                defns=[],
                                                body=[Ast.ReturnStmt nd4],
                                                head=NONE,
                                                loc=listLoc }
                        val func = Ast.Func {name=nd2,
                                             fsig=nd3,
                                             param=Ast.Head ([],[]),
                                             defaults=[],
                                             ty=functionTypeFromSignature nd3,
                                             loc=unionLoc (locOf ts) listLoc,
                                             native=false,
                                             generator=isGeneratorFunction block,
                                             block=SOME block}
(*
                                                        (Ast.Block {pragmas=[],
                                                                    defns=[],
                                                                    body=[Ast.ReturnStmt nd4],
                                                                    head=NONE,
                                                                    loc=listLoc})}
*)
                        val initSteps = [Ast.InitStep (Ast.PropIdent ident,
                                                       Ast.LiteralFunction func)]
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
                             generator=isGeneratorFunction nd4,
                             block=SOME nd4}

        fun hasNonStar (ts) : bool =
            case ts of
                [] => false
              | Ast.AnyType :: _ => hasNonStar (tl ts)
              | _ => true

        val hasNonStarAnno = true (* (not (Type.isGroundType ty)) *)
                             orelse hasNonStar (AstQuery.paramTysOfFuncTy ty) 
                             orelse hasNonStar [(AstQuery.resultTyOfFuncTy ty)]

        fun unconst Ast.Const = Ast.Var
          | unconst Ast.LetConst = Ast.LetVar
          | unconst x = x

    in
        (ts4,{pragmas=[],
              defns=[Ast.FunctionDefn {kind=unconst nd1, (* dynamic function are writable *)
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

and typeName (nd:Ast.NAME_EXPRESSION) =
    Ast.TypeName (nd, SOME (nextAstNonce()))


(*
    FunctionSignature
        TypeParameters  (  Parameters  )  ResultType
        TypeParameters  (  this  :  TypeIdentifier  ,  Parameters  )  ResultType
*)

and functionSignature (ts) : ((TOKEN * Ast.LOC) list * Ast.FUNC_SIG) =
    let val _ = trace([">> functionSignature with next=",tokenname(hd(ts))])
        val (ts1,nd1) = typeParameters ts
    in case ts1 of
        (LeftParen, _) :: (This, _) :: (Colon, _) ::  _ =>
            let
                val (ts2,nd2) = typeExpression (tl (tl (tl ts1)))
                val temp = Ast.Binding {ident=Ast.ParamIdent 0, ty=Ast.AnyType}
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
                                      thisType=SOME nd2,
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
                                  thisType=SOME nd2,
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
                val (ts2,nd2) = nameExpression (tl (tl (tl ts1))) false
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
                                          thisType=SOME (typeName nd2),
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
                                   thisType=SOME (typeName nd2),
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
        NonemptyParameters

    NonemptyParameters
        ParameterInit
        ParameterInit  ,  NonemptyParameters
        RestParameter
*)

and nonemptyParameters (ts) (n) (initRequired)
    : (TOKENS * (Ast.BINDINGS * Ast.EXPRESSION list * Ast.TYPE list) * bool) =
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
    : (TOKENS * (Ast.EXPRESSION list * Ast.TYPE list)) =
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
    : (TOKENS * (Ast.BINDINGS * Ast.EXPRESSION list * Ast.TYPE list) * bool) =
    let val _ = trace([">> parameters with next=",tokenname(hd(ts))])
    in case ts of
        (RightParen, _) :: ts1 => (ts,(([],[]),[],[]),false)
      | _ => nonemptyParameters ts 0 false
    end

and parametersType (ts)
    : (TOKENS * (Ast.EXPRESSION list * Ast.TYPE list))=
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
    : (TOKENS * (Ast.BINDINGS * Ast.EXPRESSION list * Ast.TYPE list)) =
    let val _ = trace([">> parameterInit with next=",tokenname(hd(ts))])
        val (ts1,(temp,nd1)) = parameter ts n
    in case (ts1,initRequired) of
        ((Assign, _) :: _,_) =>
            let
                val {pattern,ty,...} = nd1
                val (ts2,nd2) = nonAssignmentExpression (tl ts1,AllowColon,AllowIn)
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
    : (TOKENS * (Ast.EXPRESSION list * Ast.TYPE list)) =
    let val _ = trace([">> parameterInitType with next=",tokenname(hd(ts))])
        val (ts1,nd1) = parameterType ts
    in case ts1 of
        (Assign,_) :: _ =>
            let
                val (ts2,init) = (tl ts1, Ast.LiteralUndefined)
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
    : (TOKENS * (Ast.BINDING * {pattern:PATTERN, ty:Ast.TYPE})) =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))])
        val (ts1,nd1) = parameterKind (ts)
        val (ts2,(p,t)) = typedPattern (ts1,AllowIn)
        val temp = Ast.Binding {ident=Ast.ParamIdent n,ty=t}
    in
        trace(["<< parameter with next=",tokenname(hd(ts2))]);
        (ts2,(temp,{pattern=p,ty=t}))
    end

and parameterType (ts) =
    let val _ = trace([">> parameter with next=",tokenname(hd(ts))])
        val (ts2,t) = typeExpression ts
    in
        trace(["<< parameter with next=",tokenname(hd(ts2))]);
        (ts2, t)
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

and restParameter (ts) (n): (TOKENS * (Ast.BINDINGS * Ast.EXPRESSION list * Ast.TYPE list)) =
    let val _ = trace([">> restParameter with next=",tokenname(hd(ts))])
    in case ts of
        (TripleDot, _) :: _ =>
            let
            in case tl ts of
                (RightParen, _) :: _ =>
                    (tl ts, (([Ast.Binding{ident=Ast.PropIdent Ustring.empty,ty=Ast.AnyType}],[]),[],[Ast.ArrayType ([Ast.AnyType],NONE)]))
              | _ =>
                    let
                        val (ts1,(temp,{pattern,ty,...})) = parameter (tl ts) n
                        val (b,i) = desugarPattern (locOf ts) pattern ty (SOME (Ast.GetParam n)) (0)
                    in
                        (ts1, ((temp::b,i),[Ast.LiteralArray {exprs=Ast.ListExpr [],ty=NONE}],[Ast.ArrayType ([Ast.AnyType],NONE)]))
                    end
            end
      | _ => error ["unknown token in restParameter"]
    end

and restParameterType (ts) : (TOKENS * (Ast.EXPRESSION list * Ast.TYPE list)) =
    let val _ = trace([">> restParameter with next=",tokenname(hd(ts))])
    in case ts of
        (TripleDot, _) :: _ =>
            let
            in case tl ts of
                (RightParen, _) :: _ =>
                    (tl ts,([],[Ast.ArrayType ([],NONE)]))
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
    : (TOKENS * Ast.TYPE option) =
    let val _ = trace([">> resultType with next=",tokenname(hd(ts))])
    in case ts of
        (Colon, _) :: (Void, _) :: ts1 => (ts1,NONE)
      | (Colon, _) :: _ =>
            let
                val (ts1,nd1) = typeExpression (tl ts)
            in
                trace ["<< resultType with next=",tokenname(hd ts1)];
                (ts1, SOME nd1)
            end
      | ts1 => (ts1, SOME Ast.AnyType)
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
                                      returnType=NONE,
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
                                      returnType=NONE,
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
    : (TOKENS * (Ast.BINDINGS * Ast.EXPRESSION list)) =
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
        val (ts1,nd1) = pattern (ts,AllowColon,NoIn,AllowExpr)
    in case (ts1) of
            (Assign, _) :: _ =>
                let
                    val (ts2,nd2) = variableInitialisation (ts1,NoIn)
                in
                    trace(["<< initialiser with next=", tokenname(hd ts2)]);
                    (ts2, desugarPattern (locOf ts) nd1 Ast.AnyType (SOME nd2) 0) (* type meaningless *)
                end
          | _ => (error(["constructor initialiser without assignment"]); error ["unknown token in initialiser"])
    end

and superInitialiser (ts:TOKENS)
    : (TOKENS * Ast.EXPRESSION list) =
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
                val (ts1,nd1,listLoc) = listExpression (ts, AllowColon, AllowIn)
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

                fun isInstanceInit (s:Ast.STATEMENT)
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

                val privateNS = Name.newOpaqueNS ()
                val protectedNS = Name.newOpaqueNS ()
                val classDefn = Ast.ClassDefn {ident=ident,
                                               nonnullable=nonnullable,
                                               ns=ns,
                                               privateNS=privateNS,
                                               protectedNS=protectedNS,
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
                                 privateNS=privateNS,
                                 protectedNS=protectedNS,
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
    : (TOKENS * {ident:Ast.IDENTIFIER,
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
    : (TOKENS * {ident:Ast.IDENTIFIER,
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
    : (TOKENS * {extends:Ast.TYPE option,
                 implements:Ast.TYPE list}) =
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
                val (ts1,nd1) = typeExpressionList (tl ts)
            in
                (ts1,{extends=NONE,implements=nd1})
            end
      | _ => (ts,{extends=NONE,implements=[]})
    end

and typeIdentifierList (ts:TOKENS)
    : (TOKENS * Ast.NAME_EXPRESSION list) =
    let val _ = trace([">> typeIdentifierList with next=", tokenname(hd ts)])
        fun typeIdentifierList' (ts) =
            let
            in case ts of
                (Comma, _) :: _ =>
                    let
                        val (ts1,nd1) = nameExpression (tl ts) false
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
        val (ts1,nd1) = nameExpression (ts) false
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
    : (TOKENS * {extends:Ast.TYPE list}) =
    let val _ = trace([">> interfaceInheritance with next=", tokenname(hd ts)])
    in case ts of
        (Extends, _) :: _ =>
            let
                val (ts1,nd1) = typeExpressionList (tl ts)
            in
                (ts1,{extends=nd1})
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
        =  NamespaceExpression
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

and namespaceExpression (ts:TOKENS)
    : (TOKENS * Ast.NAMESPACE_EXPRESSION) =
    case ts of 
        (StringLiteral s, _) :: (DoubleColon, _) :: _ =>
        let
            val (ts1, nd1) = nameExpression ts false
        in
            (ts1, Ast.NamespaceName nd1)
        end
      | (StringLiteral s, _) :: _ => (tl ts, Ast.Namespace (Ast.TransparentNamespace s))
      | _ => 
        let
            val (ts1, nd1) = nameExpression ts false
        in
            (ts1, Ast.NamespaceName nd1)
        end

and namespaceInitialisation (ts:TOKENS)
    : (TOKENS * Ast.NAMESPACE_EXPRESSION option) =
    let val _ = trace([">> namespaceInitialisation with next=", tokenname(hd ts)])
    in 
        case ts of
            (Assign, _) :: _ =>
            let
                val (ts1,nd1) = namespaceExpression (tl ts) 
            in
                trace(["<< namespaceInitialisation StringLiteral with next=", tokenname(hd ts1)]);
                (ts1,SOME nd1)
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
            in
                trace(["<< typeDefinition with next=", tokenname(hd ts3)]);
                (ts3,{pragmas=[],
                      body=[],
                      defns=[Ast.TypeDefn {ns=ns,
                                           ident=nd1,
                                           typeParams = nd2,
                                           init=nd3}],
                      head=NONE,
                      loc=locOf ts})
            end
      | _ => error ["unknown token in typeDefinition"]
    end

and typeInitialisation (ts:TOKENS)
    : (TOKENS * Ast.TYPE) =
    let val _ = trace([">> typeInitialisation with next=", tokenname(hd ts)])
    in case ts of
        (Assign, _) :: _ =>
            let
                val (ts1,nd1) = typeExpression (tl ts)
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
        standard
        strict
        default namespace SimpleTypeIdentifier
        namespace SimpleTypeIdentifier
*)

and pragmaItem (ts:TOKENS)
    : (TOKENS * Ast.PRAGMA) =
    let val _ = trace([">> pragmaItem with next=", tokenname(hd ts)])
    in 
        case ts of
        (Standard, _) :: _ => (tl ts,Ast.UseStandard)
      | (Strict, _) :: _ => (tl ts,Ast.UseStrict)
      | (Default, _) :: (Namespace, _) :: _ =>
        let
            val (ts1,nd1) = namespaceExpression (tl (tl ts))
        in
            (ts1, Ast.UseDefaultNamespace nd1)
        end
      | (Namespace, _) :: _ =>
        let
            val (ts1,nd1) = namespaceExpression (tl ts)
        in
            (ts1, Ast.UseNamespace nd1)
        end
      | _ =>
        LogErr.parseError ["invalid pragma"]
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
    let
        val (ts1, nd1) = directives (ts, GlobalScope)
    in
        (ts1, Ast.Anon (Ast.Block nd1))
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

fun parse ts = 
    let
        val (residual, frag) = fragment ts
    in
        case residual of 
            [(Eof, _)] => frag
          | [] => frag
          | tok::_ => error ["residual token seen after parsing: ", tokenname tok]
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
