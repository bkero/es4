(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Defn = struct

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.

   To be specific, the definition phase completes the following tasks:
   - fold type expressions
   - fold namespace aliases
   - de-sugar patterns
   - translate defnitions to fixtures + initialisers
   - check for conflicting fixtures
   - hoist fixtures
   - inherit super classes and interfaces
   - evaluate pragmasx
   - capture open namespaces in unqualified identifiers
   - capture arithmetic modes in arithmetic operators

 *)

type CONTEXT = 
     { fixtures: Ast.FIXTURES,
       openNamespaces: Ast.NAMESPACE list list, 
       numericMode: Ast.NUMERIC_MODE }

type ENV = CONTEXT list

fun log ss = 
    (TextIO.print "log: "; 
     List.app TextIO.print ss;
     TextIO.print "\n")

val trace_on = true

fun trace ss =
    if trace_on then log ss else ()

fun dumpNamespaces (nss:Ast.NAMESPACE list) =
    let in
    TextIO.print("\n>>> Namespaces");    
    map Pretty.ppNamespace nss;
    TextIO.print("\n<<< Namespaces")
    end

fun dumpContext (ctx:CONTEXT) =
    let in
    TextIO.print("\n>>> Context\n>>> Fixtures");
    Pretty.ppFixtures (#fixtures ctx);
    TextIO.print("\n<<< Fixtures");
    map dumpNamespaces (#openNamespaces ctx);
    TextIO.print("\n<<< Context\n")
    end

fun dumpEnv env =
    let in
    TextIO.print("\n>>> Env");
    map dumpContext env;
    TextIO.print("\n<<< Env\n")
    end

val defaultNumericMode : Ast.NUMERIC_MODE =
    { numberType = Ast.Number,
      roundingMode = Ast.HalfEven,
      precision = 20 }

fun hasFixture (b:Ast.FIXTURES) 
               (n:Ast.NAME) 
    : bool = 
    let 
        fun search [] = false
          | search ((k,v)::bs) = 
            if k = n 
            then true
            else search bs
    in
        search b    end


fun getFixture (b:Ast.FIXTURES) 
               (n:Ast.NAME) 
    : Ast.FIXTURE = 
    let 
        fun search [] = LogErr.hostError ["fixture binding not found: ", 
                                          (#id n)]
          | search ((k,v)::bs) = 
            if k = n 
            then v
            else search bs
    in
        search b
    end


type DEFN_TYPE_CLASSIFICATION = 
     ((Ast.NAMESPACE_DEFN list) * (Ast.CLASS_DEFN list) * (Ast.DEFN list))


fun classifyDefnByType (curr:(Ast.DEFN * DEFN_TYPE_CLASSIFICATION))
    : DEFN_TYPE_CLASSIFICATION = 
    case curr of 
        (next, (n, c, d)) => 
        case next of 
            Ast.NamespaceDefn x => ((x::n), c, d)
          | Ast.ClassDefn x => (n, (x::c), d)
          | x => (n, c, (x::d))

                     
fun inr f (a, b) = (a, f b)

(*
    resolve a multiname to a fixture

    resolve a multiname to a name and then get the corresponding fixture

    for each nested context in the environment, look for a fixture that matches
    a multiname. see if a particular scope has a fixture with one of a list of 
    multinames see if a particular

    multiname = { nss : NAMESPACE list list, id: IDENT }
    name = { ns: NAMESPACE, id: IDENT }
*)

fun resolveMultinameToFixture (env:ENV) 
                   (mname:Ast.MULTINAME) 
    : Ast.FIXTURE =
    case env of 
        [] => LogErr.defnError ["unresolved fixture "^(LogErr.multiname mname)]
      | ({fixtures, ... }) :: parents => 
        let     
            val _ = trace(["looking for ",LogErr.multiname mname])
            val id = (#id mname)

            (* try each namespace in the set and accumulate matches *)

            fun tryName (matches:Ast.NAME list) [] = matches
              | tryName (matches:Ast.NAME list) (x::xs) : Ast.NAME list =
                let 
                    val n = { ns=x, id=id } 
                    val _ = trace(["trying ",LogErr.name n])
                in
                    if hasFixture fixtures n
                    then tryName (n::matches) xs
                    else tryName matches xs
                end

            (* try each of the nested namespace sets in turn to see
               if there is a match. raise an exception if there is
               more than one match. continue down the scope stack
               if there are none *)

            fun tryMultiname [] = NONE  
              | tryMultiname (x::xs:Ast.NAMESPACE list list) : Ast.NAME option = 
                let 
                    val matches = tryName [] x
                in case matches of
                    n :: [] => SOME n
                  | [] => tryMultiname xs
                  | _  => LogErr.defnError ["ambiguous reference "^(LogErr.multiname mname)]
                end
        in
            case tryMultiname (#nss mname) of 
                SOME n => getFixture fixtures n 
              | NONE => resolveMultinameToFixture parents mname
        end

(*
    Since we are in the definition phase the open namespaces have not been
    captured yet, so capture them before evaluating an unqualified namespace
    expression
*)

fun resolveExprToNamespace (env:ENV) 
                           (expr:Ast.EXPR) 
    : Ast.NAMESPACE = 
    case expr of 
        Ast.LiteralExpr (Ast.LiteralNamespace ns) => ns
      | Ast.LexicalRef {ident = Ast.Identifier {ident,...} } => 
        let 
            val mname = {nss = (#openNamespaces (hd env)), id = ident}
        in
            case resolveMultinameToFixture env mname of 
                Ast.NamespaceFixture ns => ns
              | _ => LogErr.defnError ["namespace expression resolved ",
                                       "to non-namespace fixture"]
        end
      | _ => LogErr.defnError ["unexpected expression type ",
                               "in namespace context"]
             
fun newContext (env:ENV) 
               (newContext:Ast.FIXTURES) 
    : CONTEXT = 
    case env of 
        [] => { fixtures = newContext,
                openNamespaces = [[Ast.Internal ""]],
                numericMode = defaultNumericMode }
      | ({ numericMode, openNamespaces, ... } :: _) =>
        { fixtures = newContext,
          openNamespaces = openNamespaces, 
          numericMode = numericMode } 


type classBlockAnalysis = 
     { protoVars: Ast.VAR_DEFN list,
       protoMethods: Ast.FUNC_DEFN list,
       instanceVars: Ast.VAR_DEFN list,
       instanceMethods: Ast.FUNC_DEFN list,
       vars: Ast.VAR_DEFN list,
       methods: Ast.FUNC_DEFN list,
       constructor: Ast.FUNC_DEFN option,
       initializer: Ast.STMT list,
       classBlock: Ast.BLOCK,
       instanceBlock: Ast.BLOCK
}

(*

       instanceBlock
       classBlock

*)


fun analyzeClassBlock (env:ENV) 
                      (n:Ast.NAME) 
                      (b:Ast.BLOCK) 
    : classBlockAnalysis = 
    case b of 
        Ast.Block { pragmas, defns, stmts, ... } => 
        let

            fun isLet (k:Ast.VAR_DEFN_TAG) = (k=Ast.LetVar) orelse (k=Ast.LetConst)

            fun isBlock (d:Ast.DEFN) 
                : bool =
                case d of 
                    Ast.VariableDefn vd => isLet (#kind vd)
                  | Ast.FunctionDefn fd => false
                  | Ast.TypeDefn _ => false
                  | Ast.NamespaceDefn _ => false
                  | _ => LogErr.defnError ["illegal definition type in class"]

            fun isProto (d:Ast.DEFN) 
                : bool = 
                case d of 
                    Ast.VariableDefn vd => (#prototype vd)
                  | Ast.FunctionDefn fd => (#prototype fd)
                  | Ast.TypeDefn _ => false
                  | Ast.NamespaceDefn _ => false
                  | _ => LogErr.defnError ["illegal definition type in class"]

            fun isStatic (d:Ast.DEFN)
                : bool = 
                case d of 
                    Ast.VariableDefn vd => (#static vd)
                  | Ast.FunctionDefn fd => (#static fd)
                  | Ast.TypeDefn _ => true
                  | Ast.NamespaceDefn _ => true
                  | _ => LogErr.defnError ["illegal definition type in class"]
                         
            fun isInstance (d:Ast.DEFN) 
                : bool = 
                let
                    val _ = trace ["isInstance ", Bool.toString (not ((isProto d) orelse (isStatic d)))]
                in
                    not ((isProto d) orelse (isStatic d) orelse (isBlock d))
                end

            fun isInstanceInit (s:Ast.STMT)
                : bool =
                let
                in case s of
                    Ast.InitStmt {kind,static,prototype,...} =>
                        not ((kind=Ast.LetVar) orelse (kind=Ast.LetConst) orelse
                             prototype orelse static)
                  | _ => false                    
                end

            fun isCtor (d:Ast.DEFN) : bool = 
                case d of 
                    (* FIXME: this might be an incorrect algorithm for
                     * determining ctor-ness *)
                    Ast.FunctionDefn { ns, 
                                       func = Ast.Func { name = { ident, ... }, ... },
                                       ... } => 
                    let 
                        val fname = { id = ident, 
                                      ns = resolveExprToNamespace env ns }
                        val _ = trace ["isCtor ", Bool.toString (fname=n)]
                    in
                        fname = n
                    end
                  | _ => false

            fun getFunc (d:Ast.DEFN) : (Ast.FUNC_DEFN option) = 
                case d of 
                    Ast.FunctionDefn fd => SOME fd
                  | _ => NONE

            fun getVarDefn (d:Ast.DEFN) : Ast.VAR_DEFN option = 
                case d of 
                    Ast.VariableDefn vd => SOME vd
                  | _ => NONE

            (*
                partition the class definition into a class block
                and an instance block, and then define both blocks
            *)

            val protoDefns = List.filter isProto defns
            val staticDefns = List.filter isStatic defns
            val instanceDefns = List.filter isInstance defns
            val blockDefns = List.filter isBlock defns

            val e0 = defPragmas env pragmas

            (* separate instance init from non-instance init statements *)
            val (iinits,stmts) = List.partition isInstanceInit stmts

            val cinit = Ast.Block {pragmas=[],defns=blockDefns,stmts=stmts,fixtures=NONE,inits=NONE}

            val cblk = defRegionalBlock env (Ast.Block {pragmas=pragmas,
                                                        defns=staticDefns,
                                                        stmts=[Ast.BlockStmt cinit],
                                                        fixtures=NONE,inits=NONE})


            (* separate ctor from non-ctor definitions *)
            val (ctorDefns, nonCtorInstanceDefns) = List.partition isCtor instanceDefns

            val ctorDefn = case ctorDefns of 
                               [Ast.FunctionDefn fd] => SOME fd
                             | [] => NONE
                             | _ => LogErr.defnError ["illegal constructor definition(s)"]

            val iblk = defRegionalBlock env (Ast.Block {pragmas=pragmas,
                                                        defns=instanceDefns,   (* include the ctor *)
                                                        stmts=[],
                                                        inits=SOME iinits,
                                                        fixtures=NONE})

            val (_, newProtoDefns) = defDefns env protoDefns

        in
            { protoVars = List.mapPartial getVarDefn newProtoDefns,
              protoMethods = List.mapPartial getFunc newProtoDefns,
              instanceVars = [],  (* List.mapPartial getVarDefn nonCtorInstanceDefns,*)
              instanceMethods = [], (* List.mapPartial getFunc nonCtorInstanceDefns, *)
              vars = [], (* List.mapPartial getVarDefn staticDefns, *)
              methods = [], (* List.mapPartial getFunc staticDefns, *)
              constructor = ctorDefn,
              initializer = [],
              classBlock = cblk,
              instanceBlock = iblk }
    end

and mergeFixtureOpts (a:Ast.FIXTURES option) 
                     (b:Ast.FIXTURES option) :
    Ast.FIXTURES option =
    case (a,b) of
        (NONE, NONE) => NONE
      | (SOME x, NONE) => SOME x
      | (SOME x, SOME y) => SOME (x @ y)
      | (NONE, SOME x) => SOME x

and mergeClasses (base:Ast.CLASS_DEFN) 
                 (curr:Ast.CLASS_DEFN) 
    : Ast.CLASS_DEFN = 
    let
        
        fun mergeBlocks (Ast.Block b1:Ast.BLOCK) (Ast. Block b2:Ast.BLOCK) : Ast.BLOCK = 
            Ast.Block {pragmas=[],
                       defns=[],
                       stmts=(#stmts b2),
                       fixtures=(#fixtures b2),inits=(#inits b2)  
        }
    in
    (* FIXME: check for name collisions and respect override / final modifiers. *)
    { ident = (#ident curr),
      ns = (#ns curr),
      nonnullable = (#nonnullable curr),
      dynamic = (#dynamic curr),
      final = (#final curr),
      params = (#params curr),
      extends = (#extends curr),
      implements = (#implements curr) @ (#implements base),
      body = (#body curr),

      classBlock = (#classBlock curr),
      instanceBlock = SOME (mergeBlocks (valOf (#instanceBlock base)) (valOf (#instanceBlock curr)))
     }
    end
(*
    Analyze class to get its various kinds of definitions
    Translate those definitions into 'block' form
    Define the blocks, first the class block, then the instance block
    Merge fixtures in the inheritance chain
*)


and resolveClass (env:ENV)
                 (children:Ast.NAME list)
                 (curr:Ast.CLASS_DEFN) 
    : (Ast.NAME * Ast.CLASS_DEFN) = 
    let
        fun qualName (cd:Ast.CLASS_DEFN) = {id = (#ident cd), 
                                            ns = (resolveExprToNamespace env (#ns cd)) }
        val currName = qualName curr


        fun seenAsChild (n:Ast.NAME) = List.exists (fn ch => ch = n) children
        fun findBaseClassDef (n:Ast.MULTINAME) = 
            let
                val f = resolveMultinameToFixture env n
            in case f of 
                Ast.ClassFixture cd => cd
              | _ => LogErr.defnError ["base class reference to non-class fixture"]
            end

        fun identExprToName ie = 
            case ie of 
                Ast.Identifier {ident, ...} => {nss = (#openNamespaces (hd env)), 
                                                id = ident}
              | _ => LogErr.defnError ["unhandled form of identifier ",
                                       "expresison in class defn"]

        val _ = trace ["analyzing class block for ", 
                                      LogErr.name currName]

        val cba = analyzeClassBlock env currName (#body curr)

        val newExtends = case (#extends curr) of 
                                     SOME x => SOME (defIdentExpr env x)
                                   | NONE => NONE
        val newImplements = map (defIdentExpr env) (#implements curr)

        val analyzedCurrClassDef : Ast.CLASS_DEFN = 
                    { ident = (#ident curr),
                      ns = (#ns curr),
                      
                      nonnullable = (#nonnullable curr),
                      dynamic = (#dynamic curr),
                      final = (#final curr),                      
                      params = (#params curr),
                      extends = newExtends,
                      implements = newImplements,
                      
                      body = (#body curr),
                      classBlock = SOME (#classBlock cba),
                      instanceBlock = SOME (#instanceBlock cba) }

    in case (#extends curr) of 
        SOME baseIdentExpr => 
                    let 
                        val baseName = identExprToName baseIdentExpr
                        val resolvedBaseClass : Ast.FIXTURE = 
                            if false (* seenAsChild baseName *)
                            then LogErr.defnError ["cyclical class inheritence detected at ", 
                                                   LogErr.multiname baseName]
                            else (resolveMultinameToFixture env baseName)

                    in 
(*   FIXME                     (currName, mergeClasses resolvedBaseClassDef analyzedCurrClassDef) *)
                          (currName, analyzedCurrClassDef)
                    end
      | NONE => ((qualName curr), analyzedCurrClassDef)
    end

(*
    VAR_BINDING

    During parsing, variable definitions are split into a var binding
    and optional init stmt. During the definition phase, the var binding 
    is translated into a fixture and optional initialiser. The init stmt
    initalises the property value and its type

    var v : t = x

    A type annotation is an attribute of a fixture, evaluated
    before the fixture is used to create a property. Light weight
    implementations may evaluate type annotations when instantiating
    a block scope for the first time. This lazy evaluation of type
    annotations means that unresolved reference errors shall not 
    be reported until runtime in standard mode.

    The definition above translates into the following psuedo
    code during the definition phase

    Block = {
        fxtrs = [ val {id='v'} ]
        inits = [ ref(v).type = t ]
        stmts = [ v = x to ref(v).type ]
    }

    The fxtrs and inits of a var definition might be hoisted
    out of their defining block. It is a definition error for
    a type annotation to resolve to a non-type fixture. This is
    to avoid changing the meaning of type annotations by hoisting


*)

and defineVar (env:ENV) 
              (kind:Ast.VAR_DEFN_TAG)
              (ns:Ast.NAMESPACE)
              (var:Ast.VAR_BINDING) 
    : (Ast.FIXTURES * Ast.VAR_BINDING) = 
    case var of 
        Ast.Binding { init, pattern, ty } => 
        let
            val newInit = case init of 
                              NONE => NONE
                            | SOME e => SOME (defExpr env e)
            val newTy = case ty of 
                            NONE => NONE
                          | SOME t => SOME (defTyExpr env t)
            val patternType = case newTy of
                                NONE => Ast.SpecialType Ast.Any
                              | SOME t => t
            val fxtrs = defineBinding env kind ns pattern patternType
            val isReadOnly = case kind of 
                                 Ast.Const => true
                               | Ast.LetConst => true
                               | _ => false                
        in
            (fxtrs, 
             Ast.Binding { init = newInit,
                           pattern = pattern,
                           ty = newTy })
        end

(*
    VAR_BINDING list

    translate a list of variable bindings into a list of
    fixtures and a list of initialisers.
*)

and defVars (env:ENV) 
            (vars:Ast.VAR_BINDING list) 
    : (Ast.FIXTURES * (Ast.VAR_BINDING list)) = 
    defVarsFull env Ast.Var (Ast.Internal "") vars
    
and defVarsFull (env:ENV) 
                (kind:Ast.VAR_DEFN_TAG)
                (ns:Ast.NAMESPACE)
                (vars:Ast.VAR_BINDING list) 
    : (Ast.FIXTURES * (Ast.VAR_BINDING list)) = 
    let
        val (fbl, vbl) = ListPair.unzip (map (defineVar env kind ns) vars)
    in
        (List.concat fbl, vbl)
    end

(*
    FUNC_SIG

    A function signature defines the invocation interface of a callable
    object. One is translated into a list of value fixtures and a list
    of initialisers that implement the call sequence

    function f.<t,u,v>(x:t,y:u=10):v {...}

    Function = {
        fsig = ...  (* structural type *)
        fixtures
        inits 
        block = {
            fxtrs = [ 't' -> TypeVal,
                      'call' -> Block {
                          fxtrs = [ val {id='x'}, val {id='y'} ]
                          inits = [ ref(x).type = t,x=args[0] to ref(x).type,
                                    y=(args.length<2?10:args[1]) to ref(y).type ]
                          stmts = [ ... ]}, ... ]
            inits = [ t=targ[0],... ]
            stmts = [ ]
        }
    }

    var c = o.f<A,B,C>(a,b)

    let g = o.f         bindThis
    let h = g.<A,B,C>   bindTypes
    let c = h(a,b)      callFunction
*)

(*
    FUNC_SIG

    Compute: 
        type arity
        function type
        fixtures (type and regular)
        parameter initialisers

*)
    

and defFuncSig (env:ENV) 
               (fsig:Ast.FUNC_SIG)
    : (int * Ast.TYPE_EXPR * Ast.FIXTURES * Ast.STMT list) =
    case fsig of 
        Ast.FunctionSignature { typeParams, params, inits, 
                                returnType, thisType, 
                                hasBoundThis, hasRest } =>
        let 

            (* compute type fixtures (type parameters) *)

            val tyVarNs = Ast.Internal ""
            fun mkTypeVarFixture x = ({ns=tyVarNs, id=x}, Ast.TypeVarFixture)
            val typeParamFixtures = map mkTypeVarFixture typeParams
            val boundTypeFixtures = newContext env typeParamFixtures
            val typeEnv = (boundTypeFixtures :: env)

            (* compute val fixtures (parameters) *)

            val (paramFixtures, newParams) = defVars typeEnv params

(* todo
            val (initFixtures, newInits) = 
                case inits of 
                    NONE => ([], NONE)
                  | SOME i => 
                    let 
                        val (fixtures, newBindings) = defVars typeEnv (#b i)
                        val newInits = defExprs env (#i i)
                    in
                        (fixtures, SOME { b = newBindings, 
                                          i = newInits })
                    end

            val newFsig = 
                Ast.FunctionSignature 
                    { typeParams = typeParams,
                      params = newParams,
                      inits = newInits,
                      returnType = defTyExpr typeEnv returnType,
                      thisType = case thisType of 
                                     NONE => NONE
                                   | SOME t => SOME (defTyExpr typeEnv t),
                      hasBoundThis = hasBoundThis,
                      hasRest = hasRest }
            val selfFixtures = 
                case name of 
                    NONE => []
                  | SOME n => [(n, Ast.ValFixture 
                                       { ty = Ast.FunctionType newFsig,
                                         readOnly = true,
                                         isOverride = false
                                         init = NONE })]

            val allParamFixtures = paramFixtures @ initFixtures
            val funcFixtures = (allParamFixtures @ typeParamFixtures @ selfFixtures)
        in
            (newFsig, selfFixtures, funcFixtures)
*)
            val type_param_count = 0
            val fixtures = typeParamFixtures @ paramFixtures
            val ftype = Ast.FunctionType fsig
            val (inits,_) = defStmts env inits
        in
            (type_param_count,ftype,fixtures,inits)
        end

(*
    FUNC

    When is code separated from fixtures? Always during parsing, or sometimes
    during the definition phase. For example, the optional parameter expressions
    in parameter lists, or function expressions in function definitions could
    be refactored during parsing, but since there is no clear reason for doing
    it during parsing, let's do it as late a possible during definition.
    
*)

and defFunc (env:ENV) (func:Ast.FUNC)
    : (Ast.TYPE_EXPR * Ast.FUNC) =
    let
        val Ast.Func {name, fsig, body,...} = func
        val (arity,ftype,fxtrs,inits) = defFuncSig env fsig
        val funcCtx = newContext env fxtrs
        val (body,hoisted) = defBlock (funcCtx :: env) body
    in
        (ftype,
         Ast.Func {name = name,
                   fsig = fsig,
                   body = body,
                   fixtures = SOME (fxtrs@hoisted),
                   inits = inits})
    end

(*
    FUNC_DEFN

    A function can be bound or unbound, writable or readonly, have constrained or
    unconstrainted 'this'. In all cases however a function definition specifies
    the function's name, type, and implementation

    Edition 3 style functions are unbound, writable, and have an unconstratined
    'this'. The fixture that holds one is of type *, and it is initialized by 
    an assignment expression in the preamble of the defining block.

*)

and defFuncDefn (env:ENV) 
            (f:Ast.FUNC_DEFN) 
    : (Ast.FIXTURES * Ast.FUNC_DEFN) = 
    case (#func f) of 
        Ast.Func { name, fsig, body, ... } =>
        let 
            val newNsExpr = defExpr env (#ns f)
            val qualNs = resolveExprToNamespace env newNsExpr
            val ident = case (#kind name) of 
                            Ast.Ordinary => (#ident name)
                          | Ast.Call => "call" (* FIXME: hack until parser fixed. *)
                          | _ => LogErr.unimplError ["defining unhandled type of function name"]
            val newName = { id = ident, ns = qualNs }
            val (ftype,newFunc) = defFunc env (#func f)
            val outerFixtures = [(newName, Ast.ValFixture 
                                       { ty = ftype,
                                         readOnly = true,
                                         isOverride = false,
                                         init = SOME (Ast.FunExpr newFunc) })]
        in

            (outerFixtures, { kind = (#kind f), 
                              ns = newNsExpr, 
                              final = (#final f),
                              native = (#native f),
                              override = (#override f),
                              prototype = (#prototype f),
                              static = (#static f),
                              func = newFunc })
        end

(*
    PRAGMA list

    Numeric mode flags and open namespaces are propagated to
    the expressions they modify via a Context value. Interpret
    each pragma and initialise a new context using the results
*)

and defPragmas (env:CONTEXT list)
               (pragmas:Ast.PRAGMA list)
    : CONTEXT list =
    let val ctx       = hd env
        val mode      = #numericMode ctx
        val numType   = ref (#numberType mode)
        val rounding  = ref (#roundingMode mode)
        val precision = ref (#precision mode)
        val opennss   = ref []
    in
        ( List.app (fn x => (* definePragma *)
            case x of 
                Ast.UseNumber n => numType := n
              | Ast.UseRounding m => rounding := m
              | Ast.UsePrecision (Ast.LiteralNumber p) => precision := Real.trunc p
              | Ast.UseNamespace ns =>
                    let
                        val namespace = resolveExprToNamespace env (Ast.LexicalRef {ident=ns})
                    in
                        opennss := (namespace :: !opennss)
                    end
              | _ => ()) pragmas ;
          { fixtures = [],
            openNamespaces = (case !opennss of 
                                 [] => (#openNamespaces ctx)   (* if opennss is empty, don't concat *)
                               | _  => !opennss :: (#openNamespaces ctx)),
            numericMode = { numberType = !numType, 
                            roundingMode = !rounding,
                            precision = !precision } } :: env )
    end

(*
    IDENT_EXPR

*)

and defIdentExpr (env:ENV) 
                 (ie:Ast.IDENT_EXPR) 
    : Ast.IDENT_EXPR = 
    let 
        val openNamespaces = 
            case env of 
                [] => []
              | ({ openNamespaces, ... }) :: _ => 
                openNamespaces

    in
        case ie of 
            Ast.Identifier { ident, ... } => 
            Ast.Identifier { ident=ident, 
                             openNamespaces=openNamespaces } 
            
          | Ast.AttributeIdentifier ai => 
            Ast.AttributeIdentifier (defIdentExpr env ai)

          | Ast.TypeIdentifier { ident, typeParams } => 
            Ast.TypeIdentifier { ident=(defIdentExpr env ident), 
                                 typeParams=typeParams }

          | Ast.QualifiedIdentifier { qual, ident } => 
            Ast.QualifiedIdentifier { qual = defExpr env qual, 
                                      ident = ident }

          | Ast.QualifiedExpression { qual, expr } => 
            Ast.QualifiedExpression { qual = defExpr env qual, 
                                      expr = defExpr env expr }
          | Ast.ExpressionIdentifier e => 
            Ast.ExpressionIdentifier (defExpr env e)
    end

(*
    EXPR

*)

and defExpr (env:ENV) 
            (expr:Ast.EXPR) 
    : Ast.EXPR = 
    let 
        fun sub e = defExpr env e
        fun subs e = defExprs env e
    in
        case expr of 
            Ast.TrinaryExpr (t, e1, e2, e3) => 
            Ast.TrinaryExpr (t, sub e1, sub e2, sub e3)
            
          | Ast.BinaryExpr (b, e1, e2) => 
            let val def = (SOME (#numericMode (hd env)))
                val opx = (case b of
                               Ast.Plus _ => Ast.Plus def
                             | Ast.Minus _ => Ast.Minus def
                             | Ast.Times _ => Ast.Times def
                             | Ast.Divide _ => Ast.Divide def
                             | Ast.Remainder _ => Ast.Remainder def
                             | Ast.Equals _ => Ast.Equals def
                             | Ast.NotEquals _ => Ast.NotEquals def
                             | Ast.StrictEquals _ => Ast.StrictEquals def
                             | Ast.StrictNotEquals _ => Ast.StrictNotEquals def
                             | Ast.Less _ => Ast.Less def
                             | Ast.LessOrEqual _ => Ast.LessOrEqual def
                             | Ast.Greater _ => Ast.Greater def
                             | Ast.GreaterOrEqual _ => Ast.GreaterOrEqual def
                             | _ => b)
            in
                Ast.BinaryExpr (opx, sub e1, sub e2)
            end
            
          | Ast.BinaryTypeExpr (b, e, te) => 
            Ast.BinaryTypeExpr (b, sub e, defTyExpr env te)

          | Ast.UnaryExpr (u, e) => 
            Ast.UnaryExpr (u, sub e)

          | Ast.TypeExpr t => 
            Ast.TypeExpr (defTyExpr env t)

          | Ast.NullaryExpr n => 
            Ast.NullaryExpr n

          | Ast.YieldExpr eso => 
            (case eso of 
                 NONE => Ast.YieldExpr NONE
               | SOME es => Ast.YieldExpr (SOME (subs es)))

          | Ast.SuperExpr eo => 
            (case eo of
                 NONE => Ast.SuperExpr NONE
               | SOME e => Ast.SuperExpr (SOME (sub e)))
            
          (* FIXME: possibly need to reinterpret literals given arithmetic modes. *)
          | Ast.LiteralExpr le => 
            Ast.LiteralExpr le
            
          | Ast.CallExpr {func, actuals} => 
            Ast.CallExpr {func = sub func,
                          actuals = map sub actuals }

          | Ast.ApplyTypeExpr { expr, actuals } =>
            Ast.ApplyTypeExpr { expr = sub expr,
                                actuals = map (defTyExpr env) actuals }

          | Ast.LetExpr { defs, body, fixtures } => 
            let
                val (f0, newDefs) = defVars env defs 
                val c0 = newContext env f0
                val newBody = defExprs (c0 :: env) body
            in
                Ast.LetExpr { defs = newDefs,
                              body = newBody,
                              fixtures = SOME f0 }
            end

          | Ast.NewExpr { obj, actuals } => 
            Ast.NewExpr { obj = sub obj,
                          actuals = subs actuals }

          | Ast.FunExpr func  => 
            let
                val (ftype,func) = defFunc env func
            in
                Ast.FunExpr func
            end

          | Ast.ObjectRef { base, ident } =>
            Ast.ObjectRef { base = sub base,
                            ident = defIdentExpr env ident }

          | Ast.LexicalRef { ident } => 
            Ast.LexicalRef { ident = defIdentExpr env ident } 

          | Ast.SetExpr (a, p, e) => 
            let 
            in
                Ast.ListExpr (defineAssignment env p e)
            end

          | Ast.AllocTemp (i,e) => 
            let 
            in
                Ast.AllocTemp (i,e)
            end

          | Ast.ListExpr es => 
            Ast.ListExpr (subs es) 

          | Ast.SliceExpr (a, b, c) => 
            Ast.SliceExpr (subs a, subs b, subs c) 

    end


and defExprs (env:ENV) 
             (expr:Ast.EXPR list) 
    : Ast.EXPR list = 
    map (defExpr env) expr

(*
    TYPE_EXPR

*)

and defTyExpr (env:ENV)
              (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR = 
    (* FIXME *)
    ty


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

    which gets rewritten by 'defineBinding' and 'defineInit' as,

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

    gets rewriten by 'defineAssignment' as,

        <temp> t = o
        ns::x = t[0]
        ns::y = t[1]

    Note: the difference between defineAssignment and defineInit is that
    defineInit creates qualified identifiers using the namespace and the
    identifiers on the left side of each assignment.
*)

and defineInit (env: ENV) (ns: Ast.NAMESPACE) (init: Ast.EXPR)
    : Ast.EXPR list =
    let
    in case init of
        (Ast.SetExpr (_,pattern,expr)) =>
            definePatternAssign env ns pattern expr 0
      | _ => LogErr.defnError ["internal definition error in defineInit"]
    end

and defineAssignment (env:ENV) (pattern: Ast.PATTERN) (expr: Ast.EXPR)
    : Ast.EXPR list =
    let
        val level = 0  (* to start with *)
        val ns = Ast.Intrinsic  (* unused since there are no IdentifierPatterns in this context *)
    in
        definePatternAssign env ns pattern expr level
    end

(*
    var x = o
    
    temp t = o
    x = t
*)

and definePatternAssign (env:ENV) (ns: Ast.NAMESPACE) (pattern: Ast.PATTERN) (expr: Ast.EXPR) (level:int)
    : Ast.EXPR list =
    let
        fun defineIdentifierAssign (id)
            : Ast.EXPR list =
            let
                val expr = defExpr env expr
            in
                [Ast.SetExpr (Ast.Assign,Ast.SimplePattern (Ast.LexicalRef 
                    {ident=Ast.QualifiedIdentifier 
                        {qual=Ast.LiteralExpr (Ast.LiteralNamespace ns),ident=id}}),expr)]
            end

        fun defineSimpleAssign (ex)
            : Ast.EXPR list =
            let
                val expr = defExpr env expr
            in
                [Ast.SetExpr (Ast.Assign,pattern,expr)]
            end

        (*
            [x,y] = o
            let [i,s]:[int,String] = o

            ISSUE: Are partial type annotations allowed? let [i,s]:[int]=...
                   If so, what is the type of 's' here? int or *?
        *)

        fun defineArrayAssign (elements:Ast.PATTERN list) (temp: Ast.EXPR) (n:int)
            : Ast.EXPR list =
            let
            in case elements of
                p::plist =>
                    let
                        val expr   = Ast.ObjectRef {base=temp, ident=Ast.ExpressionIdentifier
                                        (Ast.LiteralExpr (Ast.LiteralString (Int.toString n)))}
                        val inits  = definePatternAssign env ns p expr (level+1)
                        val inits' = defineArrayAssign plist temp (n+1)
                    in
                        inits @ inits'
                    end
              | [] => []                    
            end

        
        (*
                ns <init> {i:x,j:y} = o

            becomes

                <temp> t = o
                ns::x = t["i"]
                ns::y = t["j"]
        *)
            
        fun defineObjectAssign (fields:Ast.FIELD_PATTERN list) (temp: Ast.EXPR)
            : Ast.EXPR list =

            let
            in case fields of
                {name,ptrn}::plist =>
                   let
                        val expr   = Ast.ObjectRef {base=temp, ident=name}
                        val inits  = definePatternAssign env ns ptrn expr (level+1)
                        val inits' = defineObjectAssign plist temp
                    in
                        inits @ inits'
                    end
              | [] => []  
            end                  

        val temp_ident = "$t"^(Int.toString level)
        val init = Ast.AllocTemp (Ast.QualifiedIdentifier 
                                        {qual=Ast.LiteralExpr (Ast.LiteralNamespace Ast.Intrinsic),
                                         ident=temp_ident},expr)
        val temp_base = Ast.LexicalRef {ident=Ast.QualifiedIdentifier 
                            {qual=Ast.LiteralExpr (Ast.LiteralNamespace Ast.Intrinsic),
                             ident=temp_ident}}
    in case (pattern) of
        (Ast.ObjectPattern fields) =>
            let
                val inits = defineObjectAssign fields temp_base
            in
                init::inits
            end
      | (Ast.ArrayPattern elements) => 
            let
                val temp_index = 0    (* use register indexes to keep track of temps *)
                val inits = defineArrayAssign elements temp_base temp_index
            in
                init::inits
            end
      | (Ast.SimplePattern expr) =>
            defineSimpleAssign expr
      | (Ast.IdentifierPattern id) => 
            defineIdentifierAssign id
    end

and defineBinding (env: ENV) (kind: Ast.VAR_DEFN_TAG) (ns: Ast.NAMESPACE)
                  (pattern: Ast.PATTERN) (ty: Ast.TYPE_EXPR)
    : Ast.FIXTURES =
    let
        fun defineIdentifierBinding (id) (ty) 
            : Ast.FIXTURES =
            let
                val readOnly = if kind = Ast.Const then true else false
            in
                [({ns=ns, id=id}, Ast.ValFixture { ty = ty, readOnly = readOnly, isOverride = false, init = NONE })]
            end

        (*
            [x,y] = o
            let [i,s]:[int,String] = o

            ISSUE: Are partial type annotations allowed? let [i,s]:[int]=...
                   If so, what is the type of 's' here? int or *?
        *)

        fun defineArrayBinding (elements:Ast.PATTERN list) (element_types:Ast.TYPE_EXPR list)
            : Ast.FIXTURES =
            let
            in case (elements,element_types) of
                (e::elist,t::tlist) =>
                    let
                        val fxtrs = defineBinding env kind ns e t
                        val fxtrs' = defineArrayBinding elist tlist
                    in
                        fxtrs @ fxtrs'
                    end
              | (e::elist,_) =>
                    let
                        val fxtrs = defineBinding env kind ns e (Ast.SpecialType Ast.Any)
                        val fxtrs' = defineArrayBinding elist []
                    in
                        fxtrs @ fxtrs'
                    end
              | ([],_) => []                    
            end

        (*
            let {i:x,j:y} : {i:int,j:string}
        *)


        fun defineObjectBinding (fields:Ast.FIELD_PATTERN list) (field_types:Ast.FIELD_TYPE list)
            : Ast.FIXTURES =
            let
            in case fields of
                p::plist =>
                    let
                        val fxtrs = defineFieldBinding p field_types
                        val fxtrs' = defineObjectBinding plist field_types
                    in
                        fxtrs @ fxtrs'
                    end
              | [] => []                    
            end

        (*
            define a field pattern - use the field name to get the field type and
            associate that field type with the field's pattern
        *)

        and defineFieldBinding (field_pattern: Ast.FIELD_PATTERN) 
                               (field_types: Ast.FIELD_TYPE list)
            : Ast.FIXTURES =
            let
                val {name,ptrn} = field_pattern
                val ident_expr  = defIdentExpr env name
            in case (field_types,ident_expr) of
                (_::_,Ast.Identifier {ident,...}) =>  
                        (* if the field pattern is typed, it must have a identifier for
                           its name so we can do the mapping to its field type *)
                    let
                        val ty  = getFieldType ident field_types
                    in
                        defineBinding env kind ns ptrn ty
                    end
              | ([],_) => defineBinding env kind ns ptrn (Ast.SpecialType Ast.Any)
              | (_,_)  => LogErr.defnError ["Typed patterns must have sub patterns with names that are known at definition time"]
            end

        (*
            Get the type of a field in a field pattern - lookup in a list of field types a field type 
            with associated a name. if the list is empty then return type '*'. if the list is not
            empty and the sought name is not found, then report a syntax error.
        *)

        and getFieldType (name : Ast.IDENT) (field_types: Ast.FIELD_TYPE list)
            : Ast.TYPE_EXPR =
            let
            in case field_types of
                [] => Ast.SpecialType Ast.Any
              | field_type :: field_type_list =>
                let
                    val {name=field_type_name,ty} = field_type
                in 
                    if field_type_name = name 
                        then ty
                        else getFieldType name field_type_list
                end
            end

    in case (pattern,ty) of
        (Ast.ObjectPattern fields,Ast.ObjectType field_types) =>
            defineObjectBinding fields field_types
      | (Ast.ArrayPattern elements,Ast.ArrayType element_types) =>
            defineArrayBinding elements element_types
      | (Ast.ArrayPattern elements,(Ast.SpecialType Ast.Any)) => 
            defineArrayBinding elements []
      | (Ast.SimplePattern expr,_) =>
            LogErr.defnError ["internal error: simple pattern found in binding context"]
      | (Ast.IdentifierPattern id,_) => 
            defineIdentifierBinding id ty
      | (_,_) => 
            LogErr.defnError ["Pattern with incompatible type"]
    end


(*
    STMT

    Define a statement and return the elaborated statement and a list
    of hoisted fixtures.
*)

and defStmt (env:ENV) 
            (stmt:Ast.STMT) 
    : (Ast.STMT * Ast.FIXTURES) = 
    let
        fun reconstructForEnumStmt (fe:Ast.FOR_ENUM_STMT) = 
            case fe of 
                { ptrn, obj, defns, contLabel, body, fixtures } => 
                let
                    val newPtrn = 
                        case ptrn of 
                            NONE => NONE
                          | SOME p => SOME (defineBinding env Ast.Var (Ast.Internal "") p (Ast.SpecialType Ast.Any))
                    val newObj =  defExprs env obj
                    val (f0, newDefns) = defVars env defns
                    val c0 = newContext env f0
                    val (newBody,hoisted) = defStmt (c0 :: env) body
                in
                    ({ ptrn = ptrn, 
                      obj = newObj,
                      defns = newDefns,
                      contLabel = contLabel,
                      body = newBody, 
                      fixtures = SOME f0 })
                end
        fun reconstructWhileStmt (w:Ast.WHILE_STMT) = 
            case w of 
                { cond, body, contLabel } => 
                let 
                    val newCond = defExpr env cond
                    val (newBody,hoisted) = defStmt env body
                in
                    { cond=newCond, 
                      body=newBody, 
                      contLabel=contLabel }
                end

        fun reconstructForStmt { defns, init, cond, update, contLabel, body, fixtures } =
            let
                val (f0, newDefns) = defVars env defns
                val c0 = newContext env f0
                val newEnv = c0 :: env
                val newInit = defExprs newEnv init
                val newCond = defExprs newEnv cond
                val newUpdate = defExprs newEnv update
                val (newBody,hoisted) = defStmt newEnv body
            in
                Ast.ForStmt { defns = newDefns,
                              init = newInit,
                              cond = newCond,
                              update = newUpdate,
                              contLabel = contLabel,
                              body = newBody,
                              fixtures = SOME f0}
            end
            
        fun reconstructCatch { bind, fixtures, body } =
            let 
                val (f0, newBind) = defineVar env Ast.Var (Ast.Internal "") bind
                val c0 = newContext env f0
                val (body,fixtures) = defBlock (c0 :: env) body
            in                     
                { bind = newBind, 
                  body = body,
                  fixtures = SOME f0 }
            end            

        fun reconstructCase { label, body } =
            let
                val (body,hoisted) = defBlock env body
            in
                { label = (case label of 
                           NONE => NONE 
                         | SOME e => SOME (defExprs env e)),
                  body = body }
            end
            
        fun reconstructTyCase { ptrn, body } =
            let 
                val (b0, newPtrn) = 
                    case ptrn of NONE => 
                                 ([], NONE)
                               | SOME b => 
                                 inr (SOME) (defineVar env Ast.Var (Ast.Internal "") b)
                val f0 = newContext env b0
                val (body,hoisted) = defBlock (f0 :: env) body
            in
                { ptrn = newPtrn,
                  body = body }
            end            
    in
        case stmt of
            Ast.EmptyStmt => 
            (Ast.EmptyStmt,[])
            
          | Ast.ExprStmt es => 
            (Ast.ExprStmt (defExprs env es),[])
            
          | Ast.InitStmt {ns,inits,...} => 
                let
                    val ns0 = resolveExprToNamespace env ns
                in
                    ((Ast.ExprStmt (List.concat (map (defineInit env ns0) inits))),[])
                end

          | Ast.ForEachStmt fe => 
            (Ast.ForEachStmt (reconstructForEnumStmt fe),[])
            
          | Ast.ForInStmt fe => 
            (Ast.ForInStmt (reconstructForEnumStmt fe),[])
            
          | Ast.ThrowStmt es => 
            (Ast.ThrowStmt (defExprs env es),[])
            
          | Ast.ReturnStmt es => 
            (Ast.ReturnStmt (defExprs env es),[])
            
          | Ast.BreakStmt i => 
            (Ast.BreakStmt i,[])

          | Ast.ContinueStmt i => 
            (Ast.ContinueStmt i,[])
                                  
          | Ast.BlockStmt b =>
            let
                val (body,hoisted) = defBlock env b 
            in
                (Ast.BlockStmt body,hoisted)
            end
            
          | Ast.LabeledStmt (id, s) =>
            let
                val (stmt,hoisted) = defStmt env s
            in
                (Ast.LabeledStmt (id, stmt),hoisted)
            end 
            
          | Ast.LetStmt (vbs, stmt) => 
            let
                val (b0, newVbs) = defVars env vbs
                val f0 = newContext env b0
                val (newStmt,hoisted) = defStmt (f0 :: env) stmt
            in
                (Ast.LetStmt (newVbs, newStmt),hoisted)
            end
            
          | Ast.SuperStmt es => 
            (Ast.SuperStmt (defExprs env es),[])
            
          | Ast.WhileStmt w => 
            (Ast.WhileStmt (reconstructWhileStmt w),[])
            
          | Ast.DoWhileStmt w => 
            (Ast.DoWhileStmt (reconstructWhileStmt w),[])
            
          | Ast.ForStmt f => 
            (reconstructForStmt f,[])
            
          | Ast.IfStmt { cnd, thn, els } => 
            let
                val cnd = defExpr env cnd
                val (thn,thn_hoisted) = defStmt env thn
                val (els,els_hoisted) = defStmt env els
            in

                (Ast.IfStmt { cnd = cnd,
                              thn = thn,
                              els = els }, thn_hoisted@els_hoisted)
            end
            
          | Ast.WithStmt { obj, ty, body } =>
            let
                val (body,hoisted) = defStmt env body
            in 
                (Ast.WithStmt { obj = (defExprs env obj),
                           ty = (defTyExpr env ty),
                           body = body }, hoisted)
            end
        
          | Ast.TryStmt { body, catches, finally } => 
            let
                val (body,hoisted) = defBlock env body
            in
                (Ast.TryStmt { body = body,
                          catches = map reconstructCatch catches,
                          finally = case finally of 
                                        NONE => 
                                        NONE
                                      | SOME b =>
                                        let val (body,hoisted) = defBlock env b
                                        in SOME body end }, hoisted)
            end
        
          | Ast.SwitchStmt { cond, cases } => 
            (Ast.SwitchStmt { cond = defExprs env cond,
                             cases = map reconstructCase cases },[])
            
          | Ast.SwitchTypeStmt { cond, ty, cases } =>
            (Ast.SwitchTypeStmt { cond = defExprs env cond,
                                 ty = defTyExpr env ty,
                                 cases = map reconstructTyCase cases },[])
            
          | Ast.Dxns { expr } => 
            (Ast.Dxns { expr = defExpr env expr },[])
    end            

and defStmts (env) (stmts)
    : (Ast.STMT list * Ast.FIXTURES) = 
    let
        val (stmts,hoisted) = ListPair.unzip (map (defStmt env) stmts)
    in
        (stmts,List.concat hoisted)
    end

(*
    NAMESPACE_DEFN

    Translate a namespace definition into a namespace fixture. Namespaces 
    are not hoisted. The initialiser is resolved at this time and so must 
    be either a literal string or a reference to a previously defined 
    namespace.
*)

and defNamespace (env:ENV) 
                 (nd:Ast.NAMESPACE_DEFN)
    : (Ast.FIXTURES * Ast.NAMESPACE_DEFN) = 
    case nd of 
        { ident, ns, init } => 
        let
            val qualNs = resolveExprToNamespace env ns
            val newNs = case init of 
                            (* FIXME: a nonce perhaps? *)
                            NONE => Ast.UserNamespace ident 
                          | SOME (Ast.LiteralExpr (Ast.LiteralString s)) => 
                            Ast.UserNamespace s
                          | SOME (Ast.LexicalRef {ident}) => 
                            resolveExprToNamespace env (Ast.LexicalRef {ident=ident})
                          | _ => LogErr.evalError ["illegal form of namespace initializer"]
            val fixtureName = { ns = qualNs, id = ident } 
            val newNd = { ident = ident,
                          ns = ns,
                          init = init }
        in
            ([(fixtureName, Ast.NamespaceFixture newNs)], newNd)
        end

(*
    DEFN

    Translate a definition into a list of fixtures and a (possibly
    empty) list of initialisers.
*)

and defDefn (env:ENV) 
            (defn:Ast.DEFN) 
    : (Ast.FIXTURES * Ast.FIXTURES) = (* unhoisted, hoisted *)
    case defn of 
        Ast.VariableDefn { kind, ns, static, prototype, bindings } =>
            let
                val ns0 = resolveExprToNamespace env ns
                val (fxtrs, newBindings) = defVarsFull env kind ns0 bindings
            in case kind of
                (Ast.Var | Ast.Const) => ([],fxtrs)  (* fxtrs are hoisted *)
              | (Ast.LetVar | Ast.LetConst) => (fxtrs,[])  (* fxtrs are unhoisted *)
            end
      | Ast.FunctionDefn fd => 
            let
                val (hoisted,def) = defFuncDefn env fd
            in
                ([],hoisted)
            end
      | Ast.NamespaceDefn nd => 
            let
                val (unhoisted,def) = defNamespace env nd
            in
                (unhoisted,[])
            end
      | Ast.ClassDefn cd =>
            let
                val (hoisted,def) = defClass env cd
            in
                (hoisted,[])
            end
      | _ => ([],[])

(*
    DEFN list

    Process each definition. 
*)

and updateFixtures (cx::ex) (fxtrs:Ast.FIXTURES) : ENV =
        { fixtures = (fxtrs @ (#fixtures cx)),
          openNamespaces = (#openNamespaces cx), 
          numericMode = (#numericMode cx) } :: ex
  | updateFixtures ([]) (fxtrs:Ast.FIXTURES) : ENV = 
            LogErr.defnError ["cannot extend empty environment"]

and defDefinitions (env:ENV) 
                   (unhoisted:Ast.FIXTURES)
                   (hoisted:Ast.FIXTURES)
                   (defns:Ast.DEFN list)
    : (Ast.FIXTURES * Ast.FIXTURES) = (* unhoisted, hoisted *)
    let val _ = trace([">> defDefinitions"])
    in case defns of
        [] => (trace(["<< defDefinitions"]);(unhoisted,hoisted))
      | d::ds =>
            let
                val (u,h) = defDefn env d    
                val env'  = updateFixtures env u  
                            (* add the new unhoisted fxtrs to the current env *)
            in
                defDefinitions env' (u@unhoisted) (h@hoisted) ds
            end
    end

(*** deprecated ***)
and defDefns (env:ENV)
             (defns:Ast.DEFN list) 
    : (Ast.FIXTURES * (Ast.DEFN list)) = 
    let
        val (unhoisted, hoisted) = ListPair.unzip (map (defDefn env) defns)
    in
        (List.concat hoisted, defns)
    end

(*
    CLASS_DEFN

    The class definer analyzes the class definition into two blocks,
    a class block that implements the class object and an instance
    block that implements the instance objects.

    Class = {
        csig  = ...  (* structural type of this class *)
        isig  = ...  (* structural type of instances *)
        extends = ...
        implements = ...
        cblk = {
            fxtrs = [ 'construct' => Block {
                          fxtrs = ...  (* ctor vars *)
                          inits = ...  (* ctor inits *)
                          stmts = ...  (* ctor code *) }, ... ]
            inits = ...  (* static inits,  empty? *)
            stmts = ...  (* static initialiser *)
        }
        iblk = { ... }
    }

    The steps taken are:
    * analyse the class definition
    * resolve and inherit from the base class and interfaces
    * define the class and instance blocks

*)


and defClass (env: ENV) (cdef: Ast.CLASS_DEFN)
    : (Ast.FIXTURES * Ast.CLASS_DEFN) =
    let
        val tmp:(Ast.CLASS_DEFN list) ref = ref []
        val (cname,cdef) = resolveClass env [] cdef
        val cfx = (cname,Ast.ClassFixture {classBlock=valOf (#classBlock cdef),
                                           instanceBlock=valOf (#instanceBlock cdef)})
    in
        ([cfx],cdef)
    end

(*
    BLOCK

    Initialise a Block's fixtures and initialisers fields. Pragmas and
    definitions are not used after this definition phase. Traverse the
    statements so that embedded blocks (e.g. block statements, let 
    expressions) are initialised.
*)

and defBlock (env:ENV) 
             (b:Ast.BLOCK) 
    : (Ast.BLOCK * Ast.FIXTURES) =
    case b of 
        Ast.Block { pragmas, defns, stmts, inits,... } => 
        let 
            val env = defPragmas env pragmas
            val (unhoisted,defns_hoisted) = defDefinitions env [] [] defns
            val (inits,_) = defStmts env (case inits of NONE => [] | _ => valOf inits)  (* do inits in outer scope, inits never result in hoisted defs *)
            val env = updateFixtures env unhoisted
            val (stmts,stmts_hoisted) = defStmts env stmts
            val hoisted = defns_hoisted@stmts_hoisted
        in
            (Ast.Block { pragmas = pragmas,
                         defns = [], (* defns, *)
                         stmts = stmts,
                         fixtures = SOME unhoisted,
                         inits=SOME inits},
             hoisted)
        end

and defRegionalBlock env blk 
    : Ast.BLOCK =
        let
            val (Ast.Block {defns,stmts,fixtures,pragmas,inits},hoisted) = defBlock env blk
        in
            Ast.Block {pragmas=pragmas,
                       defns=defns,
                       stmts=stmts,
                       inits=inits,
                       fixtures=SOME (hoisted@(valOf fixtures))}
        end


(*
    PACKAGE

    Translate a package definition into a block with two implicit
    namespaces: one for 'public' and another for 'internal'. Both
    namespaces are open inside of the package block. The 'public'
    namespace is open outside of the package if the package is
    named in an import statement (e.g. import p.q.r.* opens the
    public package namespace 'p.q.r'
*)

and defPackage (env:ENV) 
               (package:Ast.PACKAGE) 
    : (Ast.PACKAGE * Ast.FIXTURES) =
        let
            val (body,hoisted) = defBlock env (#body package)
        in
            ({ name = (#name package),
              body = body }, hoisted)
        end


(*
    PROGRAM
*)

and defProgram (prog:Ast.PROGRAM) 
    : Ast.PROGRAM = 
    let 
        fun mkNamespaceFixtureBinding pkg = 
            case (pkg:Ast.PACKAGE) of 
                {name, ...} => ({ns=Ast.Internal "", id=name}, 
                                Ast.NamespaceFixture (Ast.Public name))

        val intrinsicBinding = ({ns=Ast.Internal "", id="intrinsic"}, 
                                Ast.NamespaceFixture Ast.Intrinsic)
        val topEnv = 
            [{ fixtures = intrinsicBinding::(map mkNamespaceFixtureBinding (#packages prog)),
               openNamespaces = [[Ast.Internal "", Ast.Public ""]],
               numericMode = defaultNumericMode }]
        val (packages,hoisted_pkg) = ListPair.unzip (map (defPackage topEnv) (#packages prog))
        val (body,hoisted_gbl) = defBlock topEnv (#body prog)
    in
        {packages=packages,body=body,fixtures=SOME ((List.concat hoisted_pkg)@hoisted_gbl)}
    end
end
