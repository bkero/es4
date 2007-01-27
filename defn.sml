(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Defn = struct

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.
 *)

type CONTEXT = 
     { fixtures: Ast.FIXTURES,
       openNamespaces: Ast.NAMESPACE list, 
       numberType: Ast.NUMBER_TYPE,
       roundingMode: Ast.ROUNDING_MODE }

type ENV = CONTEXT list

fun mergeOpts (a:('a list) option) 
              (b:('a list) option)
    : ('a list) option =
    case (a,b) of
        (NONE, NONE) => NONE
      | (SOME x, NONE) => SOME x
      | (SOME x, SOME y) => SOME (x @ y)
      | (NONE, SOME x) => SOME x

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
        search b
    end


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


fun resolveFixture (env:ENV) 
                   (mname:Ast.MULTINAME) 
    : Ast.FIXTURE =
    case env of 
        [] => LogErr.defnError ["unresolved fixture"]
      | ({fixtures, ... }) :: parents => 
        let     
            val id = (#id mname)
            fun tryName [] = NONE
              | tryName (x::xs) = 
                let 
                    val n = { ns=x, id=id } 
                in
                    if hasFixture fixtures n
                    then SOME n
                    else tryName xs
                end
        in
            case tryName (#nss mname) of 
                SOME n => getFixture fixtures n 
              | NONE => resolveFixture parents mname
        end


fun resolveExprToNamespace (env:ENV) 
                           (expr:Ast.EXPR) 
    : Ast.NAMESPACE = 
    case expr of 
        Ast.LiteralExpr (Ast.LiteralNamespace ns) => ns
      | Ast.LexicalRef {ident = Ast.Identifier {ident, openNamespaces} } => 
        let 
            val mname = {nss = openNamespaces, id = ident}
        in
            case resolveFixture env mname of 
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
                openNamespaces = [Ast.Internal ""],
                numberType = Ast.Number,
                roundingMode = Ast.HalfEven } 
      | ({ numberType, roundingMode, openNamespaces, ... } :: _) =>
        { fixtures = newContext,
          openNamespaces = openNamespaces, 
          numberType = numberType,
          roundingMode = roundingMode } 


type classBlockAnalysis = 
     { protoVars: Ast.VAR_DEFN list,
       protoMethods: Ast.FUNC_DEFN list,
       instanceVars: Ast.VAR_DEFN list,
       instanceMethods: Ast.FUNC_DEFN list,
       vars: Ast.VAR_DEFN list,
       methods: Ast.FUNC_DEFN list,
       constructor: Ast.FUNC_DEFN option,
       initializer: Ast.STMT list,
       ifixtures: Ast.FIXTURES, 
       fixtures: Ast.FIXTURES,
       iinitializers: Ast.INITIALIZERS, 
       pinitializers: Ast.INITIALIZERS, 
       initializers: Ast.INITIALIZERS
     }


fun analyzeClassBlock (env:ENV) 
                      (n:Ast.NAME) 
                      (b:Ast.BLOCK) 
    : classBlockAnalysis = 
    case b of 
        Ast.Block { pragmas, defns, stmts, ... } => 
        let
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
                not ((isProto d) orelse (isStatic d))

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

            val protoDefns = List.filter isProto defns
            val staticDefns = List.filter isStatic defns
            val instanceDefns = List.filter isInstance defns

            val f0 = List.concat (List.map (defPragma env) pragmas)
            val c0 = newContext env f0

            val (f1, newStaticDefns) = defDefns (c0 :: env) staticDefns
            val c1 = newContext (c0 :: env) f1

            val newStmts = map (defStmt (c1 :: env)) stmts

            val (f2, newInstanceDefns) = defDefns (c1 :: c0 :: env) instanceDefns
            val c2 = newContext (c1 :: c0 :: env) f2

            val (ctorDefns, nonCtorInstanceDefns) = List.partition isCtor newInstanceDefns
            val ctorDefn = case ctorDefns of 
                               [Ast.FunctionDefn fd] => SOME fd
                             | [] => NONE
                             | _ => LogErr.defnError ["illegal constructor definition(s)"]

            val (_, newProtoDefns) = defDefns (c2 :: c1 :: c0 :: env) protoDefns

        in 
            { protoVars = List.mapPartial getVarDefn newProtoDefns,
              protoMethods = List.mapPartial getFunc newProtoDefns,
              instanceVars = List.mapPartial getVarDefn nonCtorInstanceDefns,
              instanceMethods = List.mapPartial getFunc nonCtorInstanceDefns,
              vars = List.mapPartial getVarDefn newStaticDefns,
              methods = List.mapPartial getFunc newStaticDefns,
              constructor = ctorDefn,
              initializer = newStmts,               
              fixtures = f1,
              ifixtures = f2,
              iinitializers = [], 
              pinitializers = [], 
              initializers = [] }
    end


and mergeClasses (base:Ast.CLASS_DEFN) 
                 (curr:Ast.CLASS_DEFN) 
    : Ast.CLASS_DEFN = 
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

      name = (#name curr),
      classFixtures = (#classFixtures curr),

      instanceFixtures = (mergeOpts (#instanceFixtures base) 
                                    (#instanceFixtures curr)),
      instanceInitializers = (mergeOpts (#instanceInitializers curr)
                                        (#instanceInitializers base)),
      protoInitializers = (mergeOpts (#protoInitializers curr)
                                     (#protoInitializers base)),
      classInitializers = (mergeOpts (#classInitializers curr)
                                     (#classInitializers base)),

      protoVars = (#protoVars curr),
      protoMethods = (#protoMethods curr),
      instanceVars = (#instanceVars curr) @ (#instanceVars base),
      instanceMethods = (#instanceMethods curr) @ (#instanceMethods base),
      vars = (#vars curr),
      methods = (#methods curr),
      constructor = (#constructor curr),
      initializer = (#initializer curr) }


and resolveOneClass (env:ENV)
                    (unresolved:Ast.CLASS_DEFN list)
                    (resolved:(Ast.CLASS_DEFN list) ref)    
                    (children:Ast.NAME list)
                    (curr:Ast.CLASS_DEFN) 
    : (Ast.NAME * Ast.CLASS_DEFN) = 
    let 
        fun qualName (cd:Ast.CLASS_DEFN) = {id = (#ident cd), 
                                            ns = (resolveExprToNamespace env (#ns cd)) }
        val currName = qualName curr
        fun seenAsChild (n:Ast.NAME) = List.exists (fn ch => ch = n) children
        fun findResolved (n:Ast.NAME) = List.find (fn c => (qualName c) = n) (!resolved)
        fun findBaseClassDef (n:Ast.NAME) (cds:Ast.CLASS_DEFN list) = 
            case cds of 
                [] => LogErr.defnError ["unable to find class definition ", 
                                        LogErr.name n]
              | x::xs => if n = (qualName x) 
                         then (if (#final x)
                               then LogErr.defnError ["attempting to extend final class ", 
                                                      LogErr.name n]
                               else x)
                         else findBaseClassDef n xs
        fun identExprToName ie = 
            case ie of 
                Ast.Identifier {ident, ...} => {ns = Ast.Internal "", 
                                                id = ident}
              | _ => LogErr.defnError ["unhandled form of identifier ",
                                       "expresison in class defn"]
    in
        case findResolved currName of 
            SOME existingDefn => (currName, existingDefn)
          | NONE => 
            let 
                val _ = LogErr.trace ["analyzing class block for ", 
                                      LogErr.name currName]
                val cba = analyzeClassBlock env currName (#body curr)
                val newExtends = case (#extends curr) of 
                                     SOME x => SOME (defIdentExpr env x)
                                   | NONE => NONE
                val newImplements = map (defIdentExpr env) (#implements curr)
                val analyzedCurrClassDef = 
                    { ident = (#ident curr),
                      ns = (#ns curr),
                      
                      nonnullable = (#nonnullable curr),
                      dynamic = (#dynamic curr),
                      final = (#final curr),                      
                      params = (#params curr),
                      extends = newExtends,
                      implements = newImplements,
                      
                      classFixtures = SOME (#fixtures cba),
                      instanceFixtures = SOME (#ifixtures cba),
                      classInitializers = SOME (#initializers cba),
                      protoInitializers = SOME (#pinitializers cba),
                      instanceInitializers = SOME (#iinitializers cba),

                      body = (#body curr),
                      name = SOME currName,
                      
                      protoVars = (#protoVars cba),
                      protoMethods = (#protoMethods cba),
                      instanceVars = (#instanceVars cba),
                      instanceMethods = (#instanceMethods cba),
                      vars = (#vars cba),
                      methods = (#methods cba),
                      constructor = (#constructor cba),
                      initializer = (#initializer cba) }
            in
                case (#extends curr) of 
                    SOME baseIdentExpr => 
                    let 
                        val baseName = identExprToName baseIdentExpr
                        val unresolvedBaseClassDef = 
                            if seenAsChild baseName
                            then LogErr.defnError ["cyclical class inheritence detected at ", 
                                                   LogErr.name baseName]
                            else findBaseClassDef baseName unresolved 
                        val (_, resolvedBaseClassDef) = 
                            resolveOneClass 
                                env 
                                unresolved 
                                resolved
                                (currName :: children) 
                                unresolvedBaseClassDef
                    in
                        (currName, mergeClasses resolvedBaseClassDef 
                                                analyzedCurrClassDef)
                    end
                  | NONE => ((qualName curr), analyzedCurrClassDef)
            end
    end


and defVar (env:ENV) 
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
            val fixtureTy = case newTy of
                                NONE => Ast.SpecialType Ast.Any
                              | SOME t => t
            val newPattern = defPattern env pattern
            val isReadOnly = case kind of 
                                 Ast.Const => true
                               | Ast.LetConst => true
                               | _ => false                
            val fixtureBindings = 
                case newPattern of 
                    Ast.IdentifierPattern (Ast.Identifier { ident, ... }) => 
                    [({ns=ns, id=ident}, Ast.ValFixture { ty = fixtureTy, 
                                                          readOnly = isReadOnly,
                                                          isOverride = false })]
                  (* FIXME: do other pattern forms introduce fixtures? *)
                  | _ => []
        in
            (fixtureBindings, 
             Ast.Binding { init = newInit,
                           pattern = newPattern,
                           ty = newTy })
        end


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
        val (fbl, vbl) = ListPair.unzip (map (defVar env kind ns) vars)
    in
        (List.concat fbl, vbl)
    end


and defFuncSig (env:ENV) 
               (name:Ast.NAME option)
               (fsig:Ast.FUNC_SIG)
    : (Ast.FUNC_SIG * Ast.FIXTURES * Ast.FIXTURES) =
    case fsig of 
        Ast.FunctionSignature { typeParams, params, inits, 
                                returnType, thisType, 
                                hasBoundThis, hasRest } =>
        let 
            val tyVarNs = Ast.Internal ""
            fun mkTypeVarFixture x = ({ns=tyVarNs, id=x}, Ast.TypeVarFixture)
            val typeParamFixtures = map mkTypeVarFixture typeParams
            val boundTypeFixtures = newContext env typeParamFixtures
            val typeEnv = (boundTypeFixtures :: env)
            val (paramFixtures, newParams) = defVars typeEnv params
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
                                         isOverride = false })]
            val allParamFixtures = paramFixtures @ initFixtures
            val funcFixtures = (allParamFixtures @ typeParamFixtures @ selfFixtures)
        in
            (newFsig, selfFixtures, funcFixtures)
        end


and defFunc (env:ENV) 
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
            val (newFsig, outerFixtures, innerFixtures) = 
                defFuncSig env (SOME newName) fsig
            val funcCtx = newContext env innerFixtures
            val newFunc = Ast.Func { name = name, 
                                     fsig = newFsig, 
                                     body = defBlock (funcCtx :: env) body,
                                     typeParamFixtures = NONE,
                                     paramFixtures = NONE,
                                     bodyFixtures = SOME innerFixtures,
                                     paramInitializers = NONE,
                                     bodyInitializers = NONE }
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

        
and defPragma (env:ENV) 
              (pragma:Ast.PRAGMA) 
    : Ast.FIXTURES = 
    (* FIXME *)
    []


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
            Ast.BinaryExpr (b, sub e1, sub e2) 
            
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

          | Ast.LetExpr { defs, body, fixtures, initializers } => 
            let
                val (f0, newDefs) = defVars env defs 
                val c0 = newContext env f0
                val newBody = defExprs (c0 :: env) body
            in
                Ast.LetExpr { defs = newDefs,
                              body = newBody,
                              fixtures = SOME f0,
                              initializers = NONE }
            end

          | Ast.NewExpr { obj, actuals } => 
            Ast.NewExpr { obj = sub obj,
                          actuals = subs actuals }

          | Ast.FunExpr { ident, fsig, body, ...} => 
            let
                val (newFsig, _, innerFixtures) = 
                    case ident of 
                        (* FIXME: can function expressions have namespaces on their name? *)
                        SOME id => defFuncSig env 
                                              (SOME { ns=(Ast.Internal ""), 
                                                      id=id }) 
                                              fsig
                      | NONE => defFuncSig env NONE fsig
                val funcCtx = newContext env innerFixtures
            in
                Ast.FunExpr { ident = ident,
                              fsig = newFsig,
                              body = defBlock (funcCtx :: env) body,
                              typeParamFixtures = NONE,
                              paramFixtures = NONE,
                              bodyFixtures = SOME innerFixtures,
                              paramInitializers = NONE,
                              bodyInitializers = NONE }
            end
            
          | Ast.ObjectRef { base, ident } =>
            Ast.ObjectRef { base = sub base,
                            ident = defIdentExpr env ident }

          | Ast.LexicalRef { ident } => 
            Ast.LexicalRef { ident = defIdentExpr env ident } 

          | Ast.SetExpr (a, p, e) => 
            (* FIXME: probably need to do something complicated with temporary bindings here. *)
            let 
                val newPattern = defPattern env p
            in
                Ast.SetExpr (a, newPattern, sub e)
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


and defTyExpr (env:ENV)
              (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR = 
    (* FIXME *)
    ty

    
and defPattern (env:ENV)
               (pat:Ast.PATTERN) 
    : Ast.PATTERN = 
    case pat of 
        Ast.ObjectPattern fields => 
        Ast.ObjectPattern (map (fn { name, ptrn } => 
                                   { name = name, 
                                     ptrn = defPattern env ptrn }) fields)

      | Ast.ArrayPattern ptrns => 
        Ast.ArrayPattern (map (defPattern env) ptrns)

      | Ast.SimplePattern e => 
        Ast.SimplePattern (defExpr env e)

      | Ast.IdentifierPattern ie => 
        Ast.IdentifierPattern (defIdentExpr env ie)


and defStmt (env:ENV) 
            (stmt:Ast.STMT) 
    : (Ast.STMT) = 
    let
        fun reconstructForEnumStmt (fe:Ast.FOR_ENUM_STMT) = 
            case fe of 
                { ptrn, obj, defns, contLabel, body, 
                  fixtures, initializers } => 
                let
                    val newPtrn = 
                        case ptrn of 
                            NONE => NONE
                          | SOME p => SOME (defPattern env p)
                    val newObj =  defExprs env obj
                    val (f0, newDefns) = defVars env defns
                    val c0 = newContext env f0
                    val newBody = defStmt (c0 :: env) body
                in
                    { ptrn = newPtrn,
                      obj = newObj,
                      defns = newDefns,
                      contLabel = contLabel,
                      body = newBody, 
                      fixtures = SOME f0,
                      initializers = NONE }
                end
        fun reconstructWhileStmt (w:Ast.WHILE_STMT) = 
            case w of 
                { cond, body, contLabel } => 
                let 
                    val newCond = defExpr env cond
                    val newBody = defStmt env body
                in
                    { cond=newCond, 
                      body=newBody, 
                      contLabel=contLabel }
                end

        fun reconstructForStmt { defns, init, cond, update, contLabel, body, 
                                 fixtures, initializers } =
            let
                val (f0, newDefns) = defVars env defns
                val c0 = newContext env f0
                val newEnv = c0 :: env
                val newInit = defExprs newEnv init
                val newCond = defExprs newEnv cond
                val newUpdate = defExprs newEnv update
                val newBody = defStmt newEnv body
            in
                Ast.ForStmt { defns = newDefns,
                              init = newInit,
                              cond = newCond,
                              update = newUpdate,
                              contLabel = contLabel,
                              body = newBody,
                              fixtures = SOME f0,
                              initializers = NONE }
            end
            
        fun reconstructCatch { bind, body, fixtures, initializers } =
            let 
                val (f0, newBind) = defVar env Ast.Var (Ast.Internal "") bind
                val c0 = newContext env f0
            in                     
                { bind = newBind, 
                  body = defBlock (c0 :: env) body,
                  fixtures = SOME f0,
                  initializers = NONE }
            end            

        fun reconstructCase { label, body } =
            { label = (case label of 
                           NONE => NONE 
                         | SOME e => SOME (defExprs env e)),
              body = defBlock env body }

            
        fun reconstructTyCase { ptrn, body } =
            let 
                val (b0, newPtrn) = 
                    case ptrn of NONE => 
                                 ([], NONE)
                               | SOME b => 
                                 inr (SOME) (defVar env Ast.Var (Ast.Internal "") b)
                val f0 = newContext env b0
            in
                { ptrn = newPtrn,
                  body = defBlock (f0 :: env) body }
            end            
    in
        case stmt of
            Ast.EmptyStmt => 
            Ast.EmptyStmt
            
          | Ast.ExprStmt es => 
            Ast.ExprStmt (defExprs env es)
            
          | Ast.ForEachStmt fe => 
            Ast.ForEachStmt (reconstructForEnumStmt fe)
            
          | Ast.ForInStmt fe => 
            Ast.ForInStmt (reconstructForEnumStmt fe)
            
          | Ast.ThrowStmt es => 
            Ast.ThrowStmt (defExprs env es)
            
          | Ast.ReturnStmt es => 
            Ast.ReturnStmt (defExprs env es)
            
          | Ast.BreakStmt i => 
            Ast.BreakStmt i

          | Ast.ContinueStmt i => 
            Ast.ContinueStmt i
                                  
          | Ast.BlockStmt b =>
            Ast.BlockStmt (defBlock env b)
            
          | Ast.LabeledStmt (id, s) => 
            Ast.LabeledStmt (id, (defStmt env s))
            
          | Ast.LetStmt (vbs, stmt) => 
            let
                val (b0, newVbs) = defVars env vbs
                val f0 = newContext env b0
                val newStmt = defStmt (f0 :: env) stmt
            in
                Ast.LetStmt (newVbs, newStmt)
            end
            
          | Ast.SuperStmt es => 
            Ast.SuperStmt (defExprs env es)
            
          | Ast.WhileStmt w => 
            Ast.WhileStmt (reconstructWhileStmt w)
            
          | Ast.DoWhileStmt w => 
            Ast.DoWhileStmt (reconstructWhileStmt w)
            
          | Ast.ForStmt f => 
            reconstructForStmt f
            
          | Ast.IfStmt { cnd, thn, els } => 
            Ast.IfStmt { cnd = defExpr env cnd,
                         thn = defStmt env thn,
                         els = defStmt env els }
            
          | Ast.WithStmt { obj, ty, body } => 
            Ast.WithStmt { obj = (defExprs env obj),
                           ty = (defTyExpr env ty),
                           body = defStmt env body }
            
          | Ast.TryStmt { body, catches, finally } => 
            Ast.TryStmt { body = defBlock env body,
                          catches = map reconstructCatch catches,
                          finally = case finally of 
                                        NONE => 
                                        NONE
                                      | SOME b => 
                                        SOME (defBlock env b) }
            
          | Ast.SwitchStmt { cond, cases } => 
            Ast.SwitchStmt { cond = defExprs env cond,
                             cases = map reconstructCase cases }
            
          | Ast.SwitchTypeStmt { cond, ty, cases } =>
            Ast.SwitchTypeStmt { cond = defExprs env cond,
                                 ty = defTyExpr env ty,
                                 cases = map reconstructTyCase cases }
            
          | Ast.Dxns { expr } => 
            Ast.Dxns { expr = defExpr env expr }
    end            


and defNamespace (env:ENV) 
                 (nd:Ast.NAMESPACE_DEFN)
    : (Ast.FIXTURES * Ast.NAMESPACE_DEFN) = 
    case nd of 
        { ident, ns, init, name } => 
        let
            val qualNs = resolveExprToNamespace env ns
            val newNs = case init of 
                            (* FIXME: a nonce perhaps? *)
                            NONE => Ast.UserDefined ident 
                          | SOME (Ast.LiteralExpr (Ast.LiteralString s)) => 
                            Ast.UserDefined s
                          | SOME (Ast.LexicalRef {ident}) => 
                            resolveExprToNamespace env (Ast.LexicalRef {ident=ident})
                          | _ => LogErr.evalError ["illegal form of namespace initializer"]
            val fixtureName = { ns = qualNs, id = ident } 
            val newNd = { ident = ident,
                          ns = ns,
                          init = init,
                          name = SOME fixtureName }
        in
            ([(fixtureName, Ast.NamespaceFixture newNs)], newNd)
        end


and defDefn (env:ENV) 
            (defn:Ast.DEFN) 
    : (Ast.FIXTURES * Ast.DEFN) = 
    case defn of 
        Ast.VariableDefn { kind, ns, static, prototype, bindings } =>
        let
            val ns0 = resolveExprToNamespace env ns
            val (f0, newBindings) = defVarsFull env kind ns0 bindings
        in
            (f0, Ast.VariableDefn { kind = kind, 
                                    ns = ns,
                                    static = static, 
                                    prototype = prototype, 
                                    bindings = newBindings } )
        end
            
      | Ast.FunctionDefn fd => 
        inr (Ast.FunctionDefn) (defFunc env fd)

      | Ast.NamespaceDefn nd => 
        inr (Ast.NamespaceDefn) (defNamespace env nd)

      | Ast.ClassDefn cd => 
        LogErr.defnError ["class definition at non-top block"]

      | _ => ([], defn)


and defDefns (env:ENV) 
             (defns:Ast.DEFN list) 
    : (Ast.FIXTURES * (Ast.DEFN list)) = 
    let
        val (fbl, newDefns) = ListPair.unzip (map (defDefn env) defns)
    in
        (List.concat fbl, newDefns)
    end


and defTopBlock (env:ENV) 
                (block:Ast.BLOCK) 
    : Ast.BLOCK =
    defBlockFull true env block


and defBlock (env:ENV) 
             (block:Ast.BLOCK) 
    : Ast.BLOCK = 
    defBlockFull false env block


and defBlockFull 
        (classesOk:bool) 
        (env:ENV) 
        (b:Ast.BLOCK) 
    : Ast.BLOCK =
    case b of 
        Ast.Block { pragmas, defns, stmts, ... } => 
        let 
            val pragmaFixtures = List.concat (List.map (defPragma env) pragmas)
            val pragmaCtx = newContext env pragmaFixtures
            val e0 = pragmaCtx :: env

            val (nsDefns, classDefns, otherDefns) = List.foldl classifyDefnByType ([],[],[]) defns
            val (nsFixtures, newNsDefns) = defDefns e0 (map (Ast.NamespaceDefn) nsDefns)
            val nsCtx = newContext e0 nsFixtures
            val e1 = nsCtx :: e0

            val _ = if not ((List.null classDefns) orelse classesOk)
                    then LogErr.defnError ["classes found in illegal block context"]
                    else ()

            val tmp:(Ast.CLASS_DEFN list) ref = ref []
            val resolve = resolveOneClass e1 classDefns tmp []
            val newNamedClasses = map resolve classDefns
            val newClassDefns = map (fn (_, cd) => Ast.ClassDefn cd) newNamedClasses
            val classFixtures = map (inr (Ast.ClassFixture)) newNamedClasses
            val classCtx = if classesOk then (newContext e1 classFixtures) else nsCtx
            val e2 = if classesOk then (classCtx :: e1) else e1

            val (defnFixtures, newOtherDefns) = defDefns e2 otherDefns
            val defnCtx = newContext e2 (classFixtures @ defnFixtures)
            val e3 = defnCtx :: e2
        in
            Ast.Block { pragmas = pragmas,
                        defns = (newNsDefns @ newClassDefns @ newOtherDefns),
                        stmts = map (defStmt e3) stmts,
                        fixtures = SOME (pragmaFixtures @ 
                                         nsFixtures @ 
                                         classFixtures @ 
                                         defnFixtures),
                        initializers = NONE }
        end


and defPackage (env:ENV) 
               (package:Ast.PACKAGE) 
    : Ast.PACKAGE =
    { name = (#name package),
      body = defTopBlock env (#body package) }


and defProgram (prog:Ast.PROGRAM) 
    : Ast.PROGRAM = 
    let 
        fun mkNamespaceFixtureBinding pkg = 
            case (pkg:Ast.PACKAGE) of 
                {name, ...} => ({ns=Ast.Internal "", id=name}, 
                                Ast.NamespaceFixture (Ast.Public name))
        val topEnv = 
            [{ fixtures = map mkNamespaceFixtureBinding (#packages prog),
               openNamespaces = [Ast.Internal ""],
               numberType = Ast.Number,
               roundingMode = Ast.HalfEven }]
    in
        { packages = map (defPackage topEnv) (#packages prog),
          body = defTopBlock topEnv (#body prog) }
    end
end
