(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Defn = struct

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[eval] " :: ss) else ()
fun error ss = LogErr.defnError ss

(* 
 * The goal of the definition phase is to put together the fixtures
 * of the program, as well as insert class, function and interface
 * objects into the global object.

    To be specific, the definition phase completes the following tasks:
    - fold type expressions
    - fold namespace aliases
    - translate defnitions to fixtures + initialisers
    - check for conflicting fixtures
    - hoist fixtures
    - inherit super classes and interfaces
    - evaluate pragmas
    - capture open namespaces in unqualified identifiers
    - capture arithmetic modes in arithmetic operators
    - disambiguate package / object references

    TODO
    - fold types
    - inheritance checks
    - disambiguate package / object references
 *)


                   
datatype LABEL_KIND =
          IterationLabel
        | SwitchLabel
        | StatementLabel

type LABEL = (Ast.IDENT * LABEL_KIND)

type CONTEXT = 
     { fixtures: Ast.FIXTURES,
       openNamespaces: Ast.NAMESPACE list list, 
       numericMode: Ast.NUMERIC_MODE,
       labels: LABEL list,
       className: Ast.NAME option,
       packageName: Ast.IDENT option,
       defaultNamespace: Ast.NAMESPACE }

type ENV = CONTEXT list

fun dumpEnv (e:ENV) : unit =
    case e of
        {fixtures,...}::p => if (!doTrace) then (Pretty.ppFixtures fixtures; dumpEnv p) else ()
      | _ => ()

val defaultNumericMode : Ast.NUMERIC_MODE =
    { numberType = Ast.Number,
      roundingMode = Decimal.defaultRoundingMode,
      precision = Decimal.defaultPrecision }

fun hasFixture (b:Ast.FIXTURES) 
               (n:Ast.FIXTURE_NAME) 
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

(*
    FIXME: Move this to Mach or Eval. Something like this is used to eval Ast.FieldTypeRef
           and ElementTypeRef

    Get the type of a field in a field pattern - lookup in a list of field types a field type 
    with associated a name. if the list is empty then return type '*'. if the list is not
    empty and the sought name is not found, then report a syntax error.
*)

(*
fun getFieldType (name : Ast.IDENT) (field_types: Ast.FIELD_TYPE list)
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
*)

fun getFixture (b:Ast.FIXTURES) 
               (n:Ast.FIXTURE_NAME) 
    : Ast.FIXTURE = 
    let 
        fun search [] = LogErr.hostError ["fixture binding not found: ", 
                                          (LogErr.fname n)]
          | search ((k,v)::bs) = 
            if k = n 
            then v
            else search bs
    in
        search b
    end

fun inl f (a, b) = (f a, b)
fun inr f (a, b) = (a, f b)

fun isInstanceInit (s:Ast.STMT)
    : bool =
    let
    in case s of
           Ast.InitStmt {kind,static,prototype,...} =>
           not ((kind=Ast.LetVar) orelse (kind=Ast.LetConst) orelse
                prototype orelse static)
         | _ => false                    
    end

(*
    resolve a multiname to a fixture

    resolve a multiname to a name and then get the corresponding fixture

    for each nested context in the environment, look for a fixture that matches
    a multiname. see if a particular scope has a fixture with one of a list of 
    multinames

    multiname = { nss : NAMESPACE list list, id: IDENT }
    name = { ns: NAMESPACE, id: IDENT }
*)

fun resolveMultinameToFixture (env:ENV) 
                              (mname:Ast.MULTINAME) 
    : Ast.NAME * Ast.FIXTURE =
    let
        fun envHeadHasFixture ([],n) = false
          | envHeadHasFixture ((env:ENV),n) = hasFixture (#fixtures (List.hd env)) (Ast.PropName n) 
        fun getEnvParent [] = NONE
          | getEnvParent (x::[]) = NONE
          | getEnvParent (x::xs) = SOME xs
    in
        case Multiname.resolve mname env envHeadHasFixture getEnvParent of
            NONE => LogErr.defnError ["unresolved fixture ", LogErr.multiname mname]
          | SOME (({fixtures, ...}::_), n) => (n, getFixture fixtures (Ast.PropName n))
          | SOME _ => LogErr.defnError ["fixture lookup error ", LogErr.multiname mname]
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
                (_,Ast.NamespaceFixture ns) => ns
              | _ => LogErr.defnError ["namespace expression resolved ",
                                       "to non-namespace fixture"]
        end
      | _ => LogErr.defnError ["unexpected expression type ",
                               "in namespace context"]

and defaultNamespace (c::_) = (#defaultNamespace c)

and resolveExprOptToNamespace (env: ENV)
                              (ns : Ast.EXPR option)
    : Ast.NAMESPACE =
       case ns of
           NONE => defaultNamespace env
         | SOME n => resolveExprToNamespace env n

(*
    Create a new context initialised with the provided fixtures and
    inherited environment
*)
             
fun extendEnvironment (env:ENV) 
                      (fixtures:Ast.FIXTURES) 
    : ENV = 
    case env of 
        [] => (trace ["extending empty environment"];{ fixtures = fixtures,
                openNamespaces = [[Ast.Internal ""]],
                numericMode = defaultNumericMode,
                labels = [],
                className = NONE,
                packageName = NONE,
                defaultNamespace = Ast.Internal "" } :: [])
      | ({ numericMode, openNamespaces, labels, className, packageName, defaultNamespace, ... }) :: _ =>
        { fixtures = fixtures,
          openNamespaces = openNamespaces, 
          numericMode = numericMode,
          labels = labels,
          className = className,
          packageName = packageName,
          defaultNamespace = defaultNamespace } :: env

fun updateEnvironment (ctx::ex) (fxtrs:Ast.FIXTURES) 
    : ENV =
    let
    in
        { fixtures = (fxtrs @ (#fixtures ctx)),
          openNamespaces = (#openNamespaces ctx), 
          numericMode = (#numericMode ctx),
          labels = (#labels ctx),
          className = (#className ctx),
          packageName = (#packageName ctx),
          defaultNamespace = (#defaultNamespace ctx) } :: ex
    end
  | updateEnvironment ([]) (fxtrs:Ast.FIXTURES) 
    : ENV =
        LogErr.defnError ["cannot update an empty environment"]

fun dumpLabels labels = trace ["labels ", concat (map (fn (id,_) => id^" ") labels)]

(*
    Add a label to the current environment context. Report an error
    if there is a duplicate. Labels and switch and iteration statements
    have special meaning. They get lifted into the switch or iteration
    statement so the correct continuation value can be returned in case
    of break, and the correct control flow produced in case of continue.
    Also, the definition phase must distinguish between ordinary statement
    labels and iteration and switch labels to validate continue statements.
*)

fun addLabel (env:ENV) (label:LABEL)
    : ENV =
        let 
            fun checkLabel (env:ENV) ((labelId,labelKnd):LABEL) =
                case env of
                {labels,...}::_ =>
                    (dumpLabels labels;
                    if List.exists (fn (id,knd) => 
                            not (id = "") andalso   (* ignore empty labels *) 
                            id = labelId andalso    (* compare ids *)
                            knd = labelKnd) labels  (* and kinds *)
                    then LogErr.defnError ["duplicate label ",labelId]
                    else ())
        in
            checkLabel env label;
            case env of 
                ({fixtures,labels,openNamespaces,numericMode,className,packageName,defaultNamespace}::e) =>
                       { fixtures=fixtures,
                         labels=label::labels,
                         openNamespaces=openNamespaces,
                         numericMode=numericMode,
                         className = className,
                         packageName = packageName,
                         defaultNamespace = defaultNamespace } :: e
              | [] => LogErr.internalError ["addLabels: cannot update an empty environment"]
        end

fun addLabels (env:ENV) (labels:LABEL list)
    : ENV =
    let
    in case labels of
        [] => env
      | _ =>
        let
            val env' = addLabel env (hd labels)
        in
            addLabels env' (tl labels)
        end
    end

fun multinameOf (n:Ast.NAME) = 
    { nss = [[(#ns n)]], id = (#id n) }

(*
    Resolve an IDENT_EXPR to a multiname

    A qualified name with a literal namespace qualifier (including package qualified) 
    gets resolved to a multiname with a single namespace
*)

fun identExprToMultiname (env:ENV) (ie:Ast.IDENT_EXPR)
    : Ast.MULTINAME = 
    let
        val ie' = defIdentExpr env ie
    in case ie' of
        Ast.Identifier {ident, ...} => 
            let
            in
                {nss = (#openNamespaces (hd env)), id = ident}
            end
      | Ast.QualifiedIdentifier {ident, qual,...} => 
            let
            in case qual of
                Ast.LiteralExpr (Ast.LiteralNamespace ns ) =>
                    {nss = [[ns]], id = ident}
              | _ => LogErr.defnError ["unknown namespace value needed during definition phase"]
            end
      | _ => LogErr.defnError ["unhandled form of identifier expression in defIdentExpr"]
    end

(*
    CLASS_DEFN

    The class definer analyzes the class definition into two blocks,
    a class block that implements the class object and an instance
    block that implements the instance objects.

    ClassFixture = {
        extends = ...
        implements = ...
        cblk = {
            fxtrs = ...  (* static fixtures *) 
            inits = ...  (* static inits,  empty? static props are inited by statements *)
            body = ...   (* static initialiser *)
        }
        iblk = { ... }
    }

    The steps taken are:
    - analyze class body into instance and class blocks
    - resolve extends and implements and do inheritance
    - return a fixture binding for the class
*)

and defClass (env: ENV) 
             (cdef: Ast.CLASS_DEFN)
    : (Ast.FIXTURES * Ast.CLASS_DEFN) =
    let
        val _ = trace ["defining class ",(#ident cdef)]
        val class = analyzeClass env cdef
        val class = resolveClass env cdef class
        val Ast.Cls {name,...} = class
    in
        ([(Ast.PropName name, Ast.ClassFixture class)],cdef)
    end

(*
    analyzeClass

    The parser has already turned the class body into a block statement with init
    statements for setting the value of static and prototype fixtures. This class
    definition has a body whose only interesting statements left are init statements
    that set the value of instance vars.
*)

and analyzeClass (env:ENV)
                 (cdef:Ast.CLASS_DEFN)
    : Ast.CLS =
    case cdef of
        {ns, ident, instanceDefns, instanceStmts, classDefns, ctorDefn, (* block=Ast.Block { pragmas, body, ... },*) ...} =>
        let

            (*
                Partition the class definition into a class block
                and an instance block, and then define both blocks
            *)

            val ns = resolveExprOptToNamespace env ns
            val name = {id=ident, ns=ns}

            val (unhoisted,classFixtures,classInits) = defDefns env [] [] [] classDefns
            val env = extendEnvironment env classFixtures

            val ctor = 
                case ctorDefn of 
                    NONE => NONE 
                  | SOME c => SOME (defCtor env c)



            val (unhoisted,instanceFixtures,_) = defDefns env [] [] [] instanceDefns

            val (instanceStmts,_) = defStmts env instanceStmts  (* no hoisted fixture produced *)
            
            (* 
                The parser separates variable definitions into defns and stmts. The only stmts
                in this case will be InitStmts and what we really want is INITs so we translate
                to to INITs here.
            *)

            fun initsFromStmt stmt =
                case stmt of
                     Ast.ExprStmt (Ast.InitExpr (target,(temp_fxtrs,temp_inits),inits)) => (temp_fxtrs,temp_inits@inits)
                   | _ => LogErr.unimplError ["Defn.bindingsFromStmt"]
            
            val (fxtrs,inits) = ListPair.unzip(map initsFromStmt instanceStmts)
            val instanceInits = (List.concat fxtrs, List.concat inits)


            (*
                Add static prototype fixture
            *)

            val pf = Ast.ValFixture { ty=Ast.TypeName (Ast.QualifiedIdentifier 
                                                           {qual=Ast.LiteralExpr (Ast.LiteralNamespace ns),
                                                            ident=ident}),
                                     readOnly=true }

            val protoPropFixture = Ast.PropName {ns=Ast.Public "",id="prototype"}
            val classFixtures = (protoPropFixture, pf)::classFixtures

        in
            Ast.Cls {name=name,
                     extends = NONE,
                     implements = [],                     
                     classFixtures = classFixtures,
                     instanceFixtures = instanceFixtures,
                     instanceInits = instanceInits,
                     constructor = ctor,
                     classType = Ast.SpecialType Ast.Any,
                     instanceType = Ast.SpecialType Ast.Any }
        end

(*
    inheritFixtures

    Steps:

    - 

    Errors:

    - override by non-override fixture
    - override of final fixture
    - interface fixture not implemented
    
    Notes:

    don't check type compatibility yet; we don't know the value of type
    expressions until verify time. In standard mode we need to do a
    light weight verification to ensure that overrides are type compatible
    before a class is loaded.
*)

and inheritFixtures (base:Ast.FIXTURES)
                    (derived:Ast.FIXTURES)
    : Ast.FIXTURES =
    let
        (* 
           Recurse through the fixtures of a base class to see if the
           given fixture binding is allowed. if so, then add it
           return the updated fixtures 

           TODO: check for name conflicts:

                Any name can be overridden by any other name with the same 
                identifier and a namespace that is at least as visible. Visibility
                is an attribute of the builtin namespaces: private, protected,
                internal and public. These are related by:

                    private < protected
                    protected < public
                    private < internal
                    internal < public

                where < means less visible

                Note that protected and internal are overlapping namespaces and 
                therefore it is an error to attempt to override a name in one 
                with a name in another.

            It is an error for there to be a derived fixture with a name that is:

               - less visible than a base fixture
               - as visible or more visible than a base fixture but not overriding it 
                 (this case is caught by the override check below)
        *)

        fun inheritFixture ((n,fb):(Ast.FIXTURE_NAME * Ast.FIXTURE))
            : Ast.FIXTURES =
            let
                val targetFixture = if (hasFixture derived n)
                                        then SOME (getFixture derived n)
                                        else NONE

            in case targetFixture of
                NONE => (n,fb)::derived    (* not in the derived class, so inherit it *)
              | SOME fd => 
                case (canOverride fb fd) of
                    true => derived  (* return current fixtures *)
                  | _ => LogErr.defnError ["illegal override of ", LogErr.fname n]
            end                    

        (* 
           Given two fixtures, one base and other derived, check to see if
           the derived fixture can override the base fixture. Specifically,
           check that:

               - the base fixture is not 'final'
               - the derived fixture is 'override'
               - they have same number of type parameters and parameters
               - they both have the return type void or neither has the return type void
               - what else?
           
           Type compatibility of parameter and return types is done by the evaluator
           (or verifier in strict mode) and not here because type annotations can have
           forward references
        *)

        and canOverride (fb:Ast.FIXTURE) (fd:Ast.FIXTURE)
            : bool = 
            let
                fun isVoid ty = case ty of Ast.SpecialType Ast.VoidType => true 
                                         | _ => false
                                                
                val isCompatible = case (fb,fd) of 
                        (Ast.MethodFixture 
                             {ty=(Ast.FunctionType 
                                      {typeParams=tpb,params=pb,result=rtb,...}),...},
                         Ast.MethodFixture 
                             {ty=(Ast.FunctionType 
                                      {typeParams=tpd,params=pd,result=rtd,...}),...}) =>
                        (length tpb)=(length tpd) 
                        andalso (length pb)=(length pd) 
                        andalso (((isVoid rtb) andalso (isVoid rtd)) 
                                 orelse
                                 ((not (isVoid rtb)) andalso (not (isVoid rtd)))) 
                                (* FIXME: check compatibility of return types? *)
                      | _ => false
            
            in case (fb,fd) of
                (Ast.MethodFixture {final,...}, Ast.MethodFixture {override,...}) => 
                    (not final) andalso override andalso isCompatible
              | _ => false
            end

    in case base of
        [] => derived (* done *)
      | first::follows => inheritFixtures follows (inheritFixture first)
    end

and inheritFixtureOpts (base:Ast.FIXTURES option) 
                       (derived:Ast.FIXTURES option) 
    : Ast.FIXTURES option =
    case (base,derived) of
        (NONE, NONE) => NONE
      | (SOME x, NONE) => SOME x
      | (SOME x, SOME y) => SOME (inheritFixtures x y)
      | (NONE, SOME x) => SOME x

(*
    resolveClass

    Inherit instance fixtures from the base class. Check fixtures against
    interface fixtures
*)

and resolveClass (env:ENV)
                 ({extends,implements,...}: Ast.CLASS_DEFN)
                 (Ast.Cls {name,classFixtures,instanceFixtures,instanceInits,
                   constructor,classType,instanceType,...}:Ast.CLS)
    : Ast.CLS =
    let
        val _ = trace ["analyzing class block for ", LogErr.name name]
        val (extendsName, instanceFixtures) = resolveExtends env instanceFixtures extends [name]
        val (implementsNames, instanceFixtures) = resolveImplements env instanceFixtures implements
    in
        Ast.Cls {name=name, extends=extendsName,
                 implements=implementsNames,
                 classFixtures=classFixtures,
                 instanceFixtures=instanceFixtures,
                 instanceInits=instanceInits,
                 constructor=constructor,
                 classType=classType,
                 instanceType=instanceType}
    end

(*
    Resolve the base class

    Steps
    - resolve base class reference to a class fixture and its name
    - inherit fixtures from the base class

    Errors
    - base class not found
    - inheritance cycle detected

*)

and resolveExtends (env: ENV)
                   (currInstanceFixtures: Ast.FIXTURES) 
                   (extends: Ast.IDENT_EXPR option)
                   (children:Ast.NAME list)
    : (Ast.NAME option * Ast.FIXTURES) =
    let
        val _ = trace ["first child ", LogErr.name (hd children)]
        fun seenAsChild (n:Ast.NAME) = List.exists (fn ch => ch = n) children
        val extends:(Ast.IDENT_EXPR option) = case (extends,hd children) of 
                            (NONE, {id,...}) => 
                                if (id="Object") 
                                    then NONE 
                                    else SOME (Ast.Identifier {ident="Object",openNamespaces=[]})
                          | _ => extends 
    in case extends of
        SOME baseIdentExpr => 
            let 
                val baseClassMultiname = identExprToMultiname env baseIdentExpr
                val _ = trace ["inheriting from ",LogErr.multiname baseClassMultiname]
                val (baseClassName,baseClassFixture) = 
                        if false (* seenAsChild baseName *)
                        then LogErr.defnError ["cyclical class inheritence detected at ", 
                                           LogErr.multiname baseClassMultiname]
                        else (resolveMultinameToFixture env baseClassMultiname)
            in case baseClassFixture of
                Ast.ClassFixture (Ast.Cls {instanceFixtures=baseInstanceFixtures,...}) =>
                    (SOME baseClassName,inheritFixtures baseInstanceFixtures currInstanceFixtures)
              | _ => LogErr.defnError ["base class not found"]
            end
      | NONE => (NONE,currInstanceFixtures)
    end

(*
    Resolve each of the expressions in the 'implements' list to an interface
    fixture. check that each of the methods declared by each interface is
    implemented by the current set of  instance fixtures.

    Steps
    - resolve super interface references to interface fixtures
    - inherit fixtures from the super interfaces

    Errors
    - super interface fixture not found

*)

and resolveImplements (env: ENV) 
                      (currInstanceFixtures: Ast.FIXTURES)
                      (implements: Ast.IDENT_EXPR list)
    : (Ast.NAME list * Ast.FIXTURES) =
    let
        val implements = map (identExprToMultiname env) implements
    in
        ([],currInstanceFixtures)
    end

(*
    BINDING

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
    a type annotation to resolve to a local, non-type fixture. 
    This is to avoid changing the meaning of type annotations 
    by hoisting

    Here's a problem case:

    function f() 
    {
        type t = {i:I,s:S}

        { 
            type t = {i:int,s:string}
            var v : t = {i:10,s:"hi"}
            function g() : t {} {}
        }
    }

    Without a definition error, the meaning of 't' changes 
    after hoisting of 'v' and 'g'

*)

and defVar (env:ENV) 
           (kind:Ast.VAR_DEFN_TAG)
           (ns:Ast.NAMESPACE)
           (var:Ast.BINDING) 
    : (Ast.FIXTURE_NAME*Ast.FIXTURE) = 

    case var of 
        Ast.Binding { ident, ty } => 
        let
            val ty' = case ty of 
                           NONE => Ast.SpecialType Ast.Any
                         | SOME t => (defTyExpr env t)

            val readOnly' = case kind of 
                                 Ast.Const => true
                               | Ast.LetConst => true
                               | _ => false                

            val name' = fixtureNameFromPropIdent ns ident
        in
            (name', Ast.ValFixture {ty=ty',readOnly=readOnly'})
        end

(*
    (BINDING list * INIT_STEP list) -> (FIXTURES * INITS)

     and INIT_STEP =   (* used to encode init of bindings *)
         InitStep of (BINDING_IDENT * EXPR)
       | AssignStep of (EXPR * EXPR)

     and BINDING_IDENT = 
         TempIdent of int
       | PropIdent of IDENT

    -->

     and INITS    = (FIXTURE_NAME * EXPR) list

     and FIXTURE_NAME = TempName of int
                      | PropName of NAME

*)    

and fixtureNameFromPropIdent (ns:Ast.NAMESPACE) (ident:Ast.BINDING_IDENT) 
    : Ast.FIXTURE_NAME =
    case ident of
        Ast.TempIdent n =>  Ast.TempName n
      | Ast.PropIdent id => Ast.PropName {ns=ns,id=id}

and defInitStep (env:ENV)
            (ns:Ast.NAMESPACE)
            (step:Ast.INIT_STEP)
    : (Ast.FIXTURE_NAME * Ast.EXPR) = 
    let
    in case step of
        Ast.InitStep (ident,expr) =>
            let
                val name = fixtureNameFromPropIdent ns ident
                val expr = defExpr env expr
            in 
                (name,expr)
            end
      | Ast.AssignStep (left,right) =>
            LogErr.defnError ["unhandled init kind"]
    end

and defBindings (env:ENV)
                (kind:Ast.VAR_DEFN_TAG)
                (ns:Ast.NAMESPACE)
                ((binds,inits):Ast.BINDINGS) 
    : (Ast.FIXTURES * Ast.INITS) = 
    let
        val fxtrs:Ast.FIXTURES = map (defVar env kind ns) binds
        val inits:Ast.INITS = map (defInitStep env ns) inits
    in
        (fxtrs,inits)
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
                          body = [ ... ]}, ... ]
            inits = [ t=targ[0],... ]
            body = [ ]
        }
    }

    var c = o.f<A,B,C>(a,b)

    let g = o.f         bindThis
    let h = g.<A,B,C>   bindTypes
    let c = h(a,b)      callFunction

    paramBlock
    settingsBlock
    body
*)

and defFuncSig (env:ENV) 
               (fsig:Ast.FUNC_SIG)
    : (Ast.FIXTURES * Ast.INITS * Ast.EXPR list * Ast.FIXTURES * Ast.INITS * Ast.EXPR list) =

    case fsig of
        Ast.FunctionSignature { typeParams, params, defaults, ctorInits,
                                returnType, thisType, hasRest } =>
        let

(**** FIXME

            (* compute typeval fixtures (type parameters) *)
            val internalNs = Ast.Internal ""
            fun mkTypeVarFixture x = (Ast.PropName {ns=internalNs, id=x}, 
                                      Ast.TypeVarFixture)

            val typeParamFixtures = map mkTypeVarFixture typeParams
            val typeEnv = extendEnvironment env typeParamFixtures

            val thisType = case thisType of NONE => Ast.SpecialType Ast.Any
                                          | SOME x => x
            val thisBinding = (Ast.PropName {ns=internalNs, id="this"},
                               Ast.ValFixture
                                   { ty = thisType,
                                     readOnly = true })

****)

            fun isTempFixture (n,_) : bool =
                case n of
                   Ast.TempName _ => true
                 | _ => false


            val (paramFixtures,paramInits) = defBindings env Ast.Var (Ast.Internal "") params
            val ((settingsFixtures,settingsInits),superArgs) =
                    case ctorInits of
                        SOME (settings,args) => (defBindings env Ast.Var (Ast.Internal "") settings,
                                          defExprs env args)
                      | NONE => (([],[]),[])
            val settingsFixtures = List.filter isTempFixture settingsFixtures
        in
            (paramFixtures,
             paramInits,
             defaults,
             settingsFixtures,
             settingsInits,
             superArgs)
        end

(*
    FUNC

    The activation frame of a function is described by a BLOCK. The fixtures
    are the parameters (type and ordinary) and (hoisted) vars. The optional
    arguments are implemented using the inits. The body of the function is
    contained in the statements list.

    In addition to the block a function is described by its type tag, which
    is a function type expression.

    func : FUNC = {
        type =
        body : BLOCK = {
            fxtrs
            inits
            stmts = 
                [BlkStmt {
                    fxtrs
                    inits
                    stmts }
           ] 
        }
    }

    Assemble the normal form of the fun

    analyzeFuncSig
    defBlock

    function f(x,y,z) { var i=10; let j=20 }

    {f=[x,y,z,i],i=[],
      s=[{f=[j],i=[],
        s=[i=10,j=20]}]}
*)

and defFunc (env:ENV) (func:Ast.FUNC)
    : ((Ast.FIXTURES * Ast.INITS) * Ast.EXPR list * Ast.FUNC) =
    let
        val _ = trace [">> defFunc"]
        val Ast.Func {name, fsig, block, ty, isNative, ...} = func
        val (paramFixtures, paramInits, defaults, settingsFixtures, settingsInits, superArgs) = defFuncSig env fsig
        val newTy = defFuncTy env ty
        val defaults = defExprs env defaults
        val env = extendEnvironment env paramFixtures
        val (block, hoisted) = defBlock env block
        val _ = trace ["<< defFunc"]
    in 
        ((settingsFixtures, settingsInits), superArgs, 
         Ast.Func {name = name,
                   fsig = fsig,
                   block = block,
                   defaults = defaults,
                   ty = newTy,
                   param = (paramFixtures
                            @ hoisted,
                            paramInits),
                   isNative=isNative})
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

and defFuncDefn (env:ENV) (f:Ast.FUNC_DEFN) 
    : (Ast.FIXTURES * Ast.FUNC_DEFN) = 
    case (#func f) of
        Ast.Func { name, fsig, block, ty, isNative, ... } =>
        let
            val qualNs = resolveExprOptToNamespace env (#ns f)
            val ident = case (#kind name) of 
                            Ast.Ordinary => (#ident name)
                          | Ast.Call => "call" (* FIXME: hack until parser fixed. *)
                          | _ => LogErr.unimplError ["defining unhandled type of function name"]
            val newName = Ast.PropName { id = ident, ns = qualNs }
            val (_, _, newFunc) = defFunc env (#func f)
            val Ast.Func { ty, ... } = newFunc
            val (ftype, isReadOnly) = 
                if (#kind f) = Ast.Var 
                then (Ast.SpecialType Ast.Any,false)      (* e3 style writeable function *)
                else ((Ast.FunctionType ty),true)         (* read only, method *)

            val outerFixtures = [(newName, Ast.MethodFixture
                                       { func = newFunc,
                                         ty = ftype,
                                         readOnly = isReadOnly,
                                         final = (#final f),
                                         override = (#override f)})]
        in
            (outerFixtures, { kind = (#kind f),
                              ns = (#ns f),
                              final = (#final f),
                              override = (#override f),
                              prototype = (#prototype f),
                              static = (#static f),
                              func = newFunc })
        end


and defCtor (env:ENV) (ctor:Ast.CTOR)
    : Ast.CTOR =
    let
        val Ast.Ctor {func,...} = ctor
        val (settings,superArgs,newFunc) = defFunc env func
    in
        Ast.Ctor {settings=settings, superArgs=superArgs, func=newFunc} 
    end


(*
    PRAGMA list

    Numeric mode flags and open namespaces are propagated to
    the expressions they modify via a Context value. Interpret
    each pragma and initialise a new context using the results
*)

and defPragmas (env:ENV)
               (pragmas:Ast.PRAGMA list)
    : ENV =
    let val ctx : CONTEXT  = hd env
        val mode      = #numericMode ctx
        val numType   = ref (#numberType mode)
        val rounding  = ref (#roundingMode mode)
        val precision = ref (#precision mode)
        val defaultNamespace = ref (#defaultNamespace ctx)
        val opennss   = ref []
    in
        List.app (fn x => (* defPragma *)
            case x of 
                Ast.UseNumber n => numType := n
              | Ast.UseRounding m => rounding := m
              | Ast.UsePrecision p => precision := p
              | Ast.UseNamespace ns =>
                    let
                        val namespace = resolveExprToNamespace env ns
                    in
                        opennss := (namespace :: !opennss)
                    end
              | Ast.UseDefaultNamespace ns =>
                    let
                        val namespace = resolveExprToNamespace env ns
                        val _ = trace ["use default namespace ",LogErr.name {ns=namespace,id=""}]
                    in case namespace of
                        (Ast.Public _ | Ast.Protected _ | Ast.Private _ | Ast.Internal _) => 
                            (* these ones are already open *)
                            defaultNamespace := namespace
                      | _ =>
                            (opennss := (namespace :: !opennss);
                             defaultNamespace := namespace)
                    end
              | _ => ()) pragmas;

          { fixtures = [],
            labels = (#labels ctx),
            openNamespaces = (case !opennss of 
                                 [] => (#openNamespaces ctx)   (* if opennss is empty, don't concat *)
                               | _  => !opennss :: (#openNamespaces ctx)),
            numericMode = { numberType = !numType, 
                            roundingMode = !rounding,
                            precision = !precision },
            className = (#className ctx),
            packageName = (#packageName ctx),
            defaultNamespace = !defaultNamespace } :: env
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

and defContextualNumberLiteral (env:ENV) 
                               (n:string) 
                               (isIntegral:bool)
                               (isHex:bool)
    : Ast.LITERAL =
    let 
        val {numberType, roundingMode, precision} = (#numericMode (hd env))
        fun asDecimal _ = 
            Ast.LiteralDecimal (case Decimal.fromString precision roundingMode n of
                                    NONE => error ["failure converting '", n, 
                                                   "' to decimal literal "]
                                  | SOME d => d)
        fun asDouble _ = 
            Ast.LiteralDouble (case Real64.fromString n of
                                   NONE => error ["failure converting '", n, 
                                                  "' to double literal "]
                                 | SOME d => d)
        fun asInt _ = 
            Ast.LiteralInt (case Int32.fromString n of
                                NONE => error ["failure converting '", n, 
                                               "' to int literal "]
                              | SOME d => d)
        fun asUInt _ = 
            Ast.LiteralUInt (case Word32.fromString n of
                                 NONE => error ["failure converting '", n, 
                                                "' to uint literal "]
                               | SOME d => d)
    in
        if isHex andalso (not isIntegral)
        then error ["non-integral hex literal"]
        else
            case numberType of 
                Ast.Decimal => asDecimal ()                  
              | Ast.Double => asDouble ()
                              
              (* 
               * FIXME: The language in the draft spec describes
               * what happens to "integer literals" in the 
               * "use int" or "use uint" context. It says nothing 
               * about non-integral literals. What should we do
               * in those cases?
               *)
                              
              | Ast.Int => asInt ()
              | Ast.UInt => asUInt ()                  
                            
              (* This part is for ES3 backward-compatibility. *)
              | Ast.Number => 
                if isIntegral
                then 
                    let 
                        val v = valOf (IntInf.fromString n)
                        val uintMax = IntInf.pow(2, 32) - 1
                        val uintMin = IntInf.fromInt 0
                        val intMax = IntInf.pow(2, 31) - 1
                        val intMin = ~ (IntInf.pow(2, 31))
                    in
                        if isHex andalso uintMin <= v andalso v <= uintMax
                        then asUInt ()
                        else 
                            (if intMin <= v andalso v <= intMax
                             then asInt ()
                             else 
                                 (if 0 <= v andalso v <= uintMax
                                  then asUInt ()
                                  else asDouble ()))
                    end
                else
                    asDouble ()
    end                                         

and defLiteral (env:ENV) 
               (lit:Ast.LITERAL) 
    : Ast.LITERAL = 
    let 
        val _ = trace [">> defLiteral"]
    in 
        case lit of
            Ast.LiteralFunction func =>
            let
                val (_,_,func) = defFunc env func
            in
                Ast.LiteralFunction func
            end
          | Ast.LiteralContextualDecimalInteger n => 
            defContextualNumberLiteral env n true false

          | Ast.LiteralContextualDecimal n => 
            defContextualNumberLiteral env n false false

          | Ast.LiteralContextualHexInteger n => 
            defContextualNumberLiteral env n true true

          | Ast.LiteralArray {exprs, ty} => 
            Ast.LiteralArray {exprs = defExprs env exprs,
                              ty = case ty of 
                                       NONE => NONE
                                     | SOME t => SOME (defTyExpr env t) }
          | Ast.LiteralXML exprs => 
            Ast.LiteralXML (defExprs env exprs)

          | Ast.LiteralObject {expr, ty} => 
            Ast.LiteralObject {expr = List.map (fn { kind, name, init } => 
                                                   { kind = kind,
                                                     name = defIdentExpr env name, 
                                                     init = defExpr env init }) expr,
                               ty = case ty of 
                                        NONE => NONE
                                      | SOME t => SOME (defTyExpr env t) }

          | _ => lit   (* FIXME: other cases to handle here *)
    end

(*
    EXPR
*)    

and defExpr (env:ENV) 
            (expr:Ast.EXPR) 
    : Ast.EXPR = 
    let 
        fun sub e = defExpr env e
    in
        case expr of 
            Ast.TrinaryExpr (t, e1, e2, e3) => 
            Ast.TrinaryExpr (t, sub e1, sub e2, sub e3)
            
          | Ast.BinaryExpr (b, e1, e2) => 
            let 
                val m = (SOME (#numericMode (hd env)))
                val b' = (case b of
                               Ast.Plus _ => Ast.Plus m
                             | Ast.Minus _ => Ast.Minus m
                             | Ast.Times _ => Ast.Times m
                             | Ast.Divide _ => Ast.Divide m
                             | Ast.Remainder _ => Ast.Remainder m
                             | Ast.Equals _ => Ast.Equals m
                             | Ast.NotEquals _ => Ast.NotEquals m
                             | Ast.StrictEquals _ => Ast.StrictEquals m
                             | Ast.StrictNotEquals _ => Ast.StrictNotEquals m
                             | Ast.Less _ => Ast.Less m
                             | Ast.LessOrEqual _ => Ast.LessOrEqual m
                             | Ast.Greater _ => Ast.Greater m
                             | Ast.GreaterOrEqual _ => Ast.GreaterOrEqual m
                             | _ => b)
            in
                Ast.BinaryExpr (b', sub e1, sub e2)
            end
            
          | Ast.BinaryTypeExpr (b, e, te) => 
            Ast.BinaryTypeExpr (b, sub e, defTyExpr env te)

          | Ast.UnaryExpr (u, e) => 
            let 
                val m = (SOME (#numericMode (hd env)))
                val u' = (case u of
                               Ast.PreIncrement _ => Ast.PreIncrement m
                             | Ast.PreDecrement _ => Ast.PreDecrement m
                             | Ast.PostIncrement _ => Ast.PostIncrement m
                             | Ast.PostDecrement _ => Ast.PostDecrement m
                             | Ast.UnaryPlus _ => Ast.UnaryPlus m
                             | Ast.UnaryMinus _ => Ast.UnaryMinus m
                             | _ => u)
            in
                Ast.UnaryExpr (u', sub e)
            end
                         
          | Ast.TypeExpr t => 
            Ast.TypeExpr (defTyExpr env t)

          | Ast.ThisExpr => 
            Ast.ThisExpr

          | Ast.YieldExpr eo => 
            (case eo of 
                      NONE => Ast.YieldExpr NONE
                    | SOME e => Ast.YieldExpr (SOME (sub e)))

          | Ast.SuperExpr eo => 
            (case eo of
                      NONE => Ast.SuperExpr NONE
                    | SOME e => Ast.SuperExpr (SOME (sub e)))
            
          | Ast.LiteralExpr le => 
            Ast.LiteralExpr (defLiteral env le)
            
          | Ast.CallExpr {func, actuals} => 
            Ast.CallExpr {func = sub func,
                          actuals = map sub actuals }

          | Ast.ApplyTypeExpr { expr, actuals } =>
            Ast.ApplyTypeExpr { expr = sub expr,
                                actuals = map (defTyExpr env) actuals }

          | Ast.LetExpr { defs, body,... } => 
            let
                val (f,i)   = defBindings env Ast.Var (Ast.Internal "") defs
                val env     = extendEnvironment env f
                val newBody = defExpr env body
            in
                Ast.LetExpr { defs = defs,
                              body = newBody,
                              head = SOME (f,i) }
            end

          | Ast.NewExpr { obj, actuals } => 
            Ast.NewExpr { obj = sub obj,
                          actuals = map sub actuals }

          | Ast.ObjectRef { base, ident } =>
            Ast.ObjectRef { base = sub base,
                            ident = defIdentExpr env ident }

          | Ast.LexicalRef { ident } => 
            Ast.LexicalRef { ident = defIdentExpr env ident }

          | Ast.SetExpr (a, le, re) => 
            let
                val def = (SOME (#numericMode (hd env)))
                val a' = case a of
                             Ast.AssignPlus _ => Ast.AssignPlus def
                           | Ast.AssignMinus _ => Ast.AssignMinus def
                           | Ast.AssignTimes _ => Ast.AssignTimes def
                           | Ast.AssignDivide _ => Ast.AssignDivide def
                           | Ast.AssignRemainder _ => Ast.AssignRemainder def
                           | x => x
            in
                Ast.SetExpr (a', (sub le), (sub re))
            end

          | Ast.GetTemp n => 
            Ast.GetTemp n

          | Ast.ListExpr es => 
            Ast.ListExpr (map sub es)

          | Ast.SliceExpr (a, b, c) => 
            Ast.SliceExpr (sub a, sub b, sub c)
    end
    
    
and defExprs (env:ENV) 
             (exprs:Ast.EXPR list) 
    : Ast.EXPR list = 
    let
        val es = map (defExpr env) exprs
    in
        es
    end

(*
    TYPE_EXPR

*)

and defFuncTy (env:ENV)
              (ty:Ast.FUNC_TYPE)
    : Ast.FUNC_TYPE =
    (* FIXME *)
    ty

and defTyExpr (env:ENV)
              (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR = 
    case ty of 
        Ast.FunctionType t => 
        Ast.FunctionType (defFuncTy env t)
      (* FIXME *)
      | t => t


(*
    STMT

    Define a statement and return the elaborated statement and a list
    of hoisted fixtures.
*)

and defStmt (env:ENV) 
            (labelIds:Ast.IDENT list)
            (stmt:Ast.STMT) 
    : (Ast.STMT * Ast.FIXTURES) = 
    let
        val (ctx::_) = env

        fun reconstructForEnumStmt (fe:Ast.FOR_ENUM_STMT) = 
            case fe of 
                { obj, defn, labels, body, ... } => 
                let
                    val newObj =  defExpr env obj
                    val (f1, i1) = ([],[])  (* FIXME defVars env (valOf defn) *)
                    val env = updateEnvironment env f1
                    val (newBody,hoisted) = defStmt env [] body
                in
                    ({ obj = newObj,
                       defn = defn,
                       labels = labels,
                       body = newBody, 
                       fixtures = SOME f1,
                       inits = SOME i1 },
                     hoisted)
                end

        fun makeIterationLabel id = (id,IterationLabel)
        fun makeStatementLabel id = (id,StatementLabel)

        fun defWhileStmt (env) (w:Ast.WHILE_STMT) = 
            case w of 
                { cond, body, labels, fixtures } => (* FIXME: inits needed *)
                let 
                    val newCond = defExpr env cond
                    val (newBody, hoisted) = defStmt env [] body
                in
                    ({ cond=newCond, 
                       fixtures=NONE,
                       body=newBody, 
                       labels=""::labelIds}, hoisted)
                end

        (*
            for ( var x = 10; x > 0; --x ) ...
            for ( x=10; x > 0; --x ) ...
        *)

        fun defnForStmt env { defn, init, cond, update, labels, body, fixtures } =
            let
                fun defVarDefnOpt vd =
                    case vd of
                        SOME vd => defDefn env (Ast.VariableDefn vd)
                      | NONE => ([],[],[])
                val (uf,hf,_) = defVarDefnOpt defn
                val env' = updateEnvironment env (uf@hf)
                val (newInit,_) = defStmt env' [] init
                val newCond = defExpr env' cond
                val newUpdate = defExpr env' update
                val (newBody, hoisted) = defStmt env' [] body
            in
                trace ["<< reconstructForStmt"];
                ( Ast.ForStmt { defn = defn,
                                init = newInit,
                                cond = newCond,
                                update = newUpdate,
                                labels = ""::labels,  (* add the default break/continue label *)
                                body = newBody,
                                fixtures = SOME (uf) },
                  hf@hoisted )
            end

        fun reconstructCatch { bindings, fixtures, block, ty } =
            let 
                val (f0,i0) = defBindings env Ast.Var (Ast.Internal "") bindings
                val env = extendEnvironment env f0
                val (block,fixtures) = defBlock env block
            in                     
                { bindings = bindings,   (* FIXME: what about inits *)
                  block = block,
                  fixtures = SOME f0,
                  ty=ty }
            end            

        fun reconstructCase { label, body, inits } 
            : Ast.CASE * Ast.FIXTURES =
            let
                val (body,hoisted) = defBlock env body
                val label = 
                    case label of 
                        NONE => NONE
                      | SOME e => 
                        let 
                            val e' = defExpr env e
                        in 
                            SOME e'
                        end
            in
                ({label = label,
                  inits = SOME [],
                  body = body},
                [])
            end

        fun reconstructTyCase {ty: Ast.TYPE_EXPR option,
                               bindings: Ast.BINDINGS, 
                               body: Ast.BLOCK, inits: Ast.INITS option}
            : Ast.TYPE_CASE * Ast.FIXTURES = 
            let 
                val (fxtrs,inits) = defBindings env Ast.Var (Ast.Internal "") bindings
                val env = extendEnvironment env fxtrs
                val (body,hoisted) = defBlock env body
            in
                ({ ty=ty,
                  bindings=bindings,
                  inits=SOME inits,
                  body=body },
                 fxtrs)
            end

        fun findClass (n:Ast.NAME) =
            let
                val (n,f) = resolveMultinameToFixture env (multinameOf n)
            in case f of
                Ast.ClassFixture cd => cd
              | _ => LogErr.defnError ["reference to non-class fixture"]
            end

        fun reconstructClassBlock {ns, ident, block, name } =
            let
                val _ = trace ["reconstructing class block for ", ident]
                val Ast.Block { pragmas, defns, head, body } = block

                (* filter out instance initializers *)
                val (_,stmts) = List.partition isInstanceInit body

                val namespace = resolveExprOptToNamespace env ns
                val name = {ns=namespace, id=ident}

                val (block,hoisted) = defBlock env (Ast.Block {pragmas=pragmas,
                                                               defns=defns,
                                                               head=head,
                                                               body=body})
            in
                (Ast.ClassBlock { ns = ns,
                                 ident = ident,
                                 name = SOME name,
                                 block = block }, hoisted)
            end


        fun checkLabel labelIdOpt labelKnd =
            let
                val labelId = case labelIdOpt of NONE => "" | SOME i => i
                val _ = trace ["checkLabel ",labelId]
            in case env of
                {labels,...}::_ =>
                    (dumpLabels labels;
                    if List.exists (fn (id,knd) => 
                        id = labelId andalso    (* compare ids *)
                        knd = labelKnd) labels  (* and kinds *)
                    then true
                    else false)
            end

        fun checkBreakLabel (id:Ast.IDENT option) =
            (* 
                A break with an empty label shall only occur in an iteration
                statement or switch statement. A break with a non-empty label
                shall only occur in a statement with that label as a member of
                its label set
            *)
            if case id of
                NONE => (checkLabel id SwitchLabel) orelse (checkLabel id IterationLabel)
              | _ => (checkLabel id StatementLabel)
            then ()
            else LogErr.defnError ["invalid break label"]

        fun checkContinueLabel (id:Ast.IDENT option) =
            (*
                A continue statement with an empty label shall only occur in
                an iteration statement. A continue statement with a non-empty
                label shall only occur in an iteration statement with that
                label as a member of its label set
            *)

            if checkLabel id IterationLabel
            then ()
            else LogErr.defnError ["invalid label in continue statement"]

    in
        case stmt of
            Ast.EmptyStmt => 
            (Ast.EmptyStmt,[])
         
          | Ast.ExprStmt es => 
            let
            in
                (Ast.ExprStmt (defExpr env es),[])
            end

          | Ast.InitStmt {ns, temps, inits, prototype, static, kind} => 
            let
                val ns0 = resolveExprOptToNamespace env ns

                val target = case (kind, prototype, static) of
                                 (_,true,_) => Ast.Prototype
                               | (_,_,true) => Ast.Hoisted
                               | (Ast.Var,_,_) => Ast.Hoisted
                               | _ => Ast.Local
                val temps = defBindings env kind ns0 temps  (* ISSUE: kind and ns are irrelevant *)
            in
                (Ast.ExprStmt (Ast.InitExpr (target, temps, (map (defInitStep env ns0) inits))),[])
            end

          | Ast.ForEachStmt fe => 
            (inl Ast.ForEachStmt (reconstructForEnumStmt fe))
            
          | Ast.ForInStmt fe => 
            (inl Ast.ForInStmt (reconstructForEnumStmt fe))
            
          | Ast.ThrowStmt es => 
            (Ast.ThrowStmt (defExpr env es), [])
            
          | Ast.ReturnStmt es => 
            (Ast.ReturnStmt (defExpr env es), [])
            
          | Ast.BreakStmt i => 
            let
            in
                checkBreakLabel i;
                (Ast.BreakStmt i, [])
            end

          | Ast.ContinueStmt i =>
            let
            in
                checkContinueLabel i;
                (Ast.ContinueStmt i, [])
             end

          | Ast.BlockStmt b =>
            inl (Ast.BlockStmt) (defBlock env b)
            
          | Ast.ClassBlock cb =>
            reconstructClassBlock cb
            
          | Ast.LabeledStmt (id, s) =>
            let 
                val env' = addLabel env (makeStatementLabel id)
                val (s',f') = defStmt env' (id::labelIds) s
            in
                (Ast.LabeledStmt (id,s'),f')
            end
            
          | Ast.LetStmt b =>
            inl (Ast.LetStmt) (defBlock env b)
            
          | Ast.WhileStmt w => 
            let
                val env' = addLabels env (map makeIterationLabel labelIds)
                val env'' = addLabel env' (makeIterationLabel "")
            in
                inl (Ast.WhileStmt) (defWhileStmt env'' w)
            end
            
          | Ast.DoWhileStmt w => 
            let
                val env' = addLabels env (map makeIterationLabel labelIds);
                val env'' = addLabel env' (makeIterationLabel "")
            in
                inl (Ast.DoWhileStmt) (defWhileStmt env'' w)
            end
            
          | Ast.ForStmt f => 
            let
                val env' = addLabels env (map makeIterationLabel labelIds);
                val env'' = addLabel env' (makeIterationLabel "")
            in
                defnForStmt env'' f
            end
            
          | Ast.IfStmt { cnd, thn, els } => 
            let
                val cnd = defExpr env cnd
                val (thn,thn_hoisted) = defStmt env [] thn
                val (els,els_hoisted) = defStmt env [] els
            in

                (Ast.IfStmt { cnd = cnd,
                              thn = thn,
                              els = els }, 
                 (thn_hoisted @ els_hoisted))
            end
            
          | Ast.WithStmt { obj, ty, body } =>
            let
                val (body,hoisted) = defStmt env [] body
            in 
                (Ast.WithStmt { obj = (defExpr env obj),
                           ty = (defTyExpr env ty),
                           body = body }, hoisted)
            end
        
          | Ast.TryStmt { block, catches, finally } => 
            let
                val (block,hoisted) = defBlock env block
            in
                (Ast.TryStmt {block = block,
                              catches = map reconstructCatch catches,
                              finally = case finally of 
                                        NONE => 
                                        NONE
                                      | SOME b =>
                                        let val (block,hoisted) = defBlock env b
                                        in SOME block end }, hoisted)
            end
        
          | Ast.SwitchStmt { cond, cases } => 
            let
                val (cases,hoisted) = ListPair.unzip (map reconstructCase cases)
            in
                (Ast.SwitchStmt { cond = defExpr env cond,
                                  cases = cases}, List.concat hoisted)
            end
        
          | Ast.SwitchTypeStmt { cond, ty, cases } =>
            let
                val (cases,hoisted) = ListPair.unzip (map reconstructTyCase cases)
            in
                (Ast.SwitchTypeStmt {cond = defExpr env cond,
                                     ty = defTyExpr env ty,
                                     cases = cases}, List.concat hoisted)
            end
            
          | Ast.Dxns { expr } => 
            (Ast.Dxns { expr = defExpr env expr },[])
    end            

and defStmts (env) (stmts:Ast.STMT list)
    : (Ast.STMT list * Ast.FIXTURES) = 
    case stmts of
        (stmt::stmts) =>
            let 
                val (s1,f1):(Ast.STMT*Ast.FIXTURES) = defStmt env [] stmt

                (* Class definitions are embedded in the ClassBlock so we
                   need to update the environment in that case *)

                val env' = updateEnvironment env f1
                val (s2,f2) = defStmts env' stmts
            in
                (s1::s2,f1@f2)
            end
      | [] => ([],[])

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
            val qualNs = resolveExprOptToNamespace env ns
            val newNs = case init of 
                            (* FIXME: a nonce perhaps? *)
                            NONE => Ast.UserNamespace ident 
                          | SOME (Ast.LiteralExpr (Ast.LiteralString s)) => 
                            Ast.UserNamespace s
                          | SOME (Ast.LexicalRef {ident}) => 
                            resolveExprToNamespace env (Ast.LexicalRef {ident=ident})
                          | _ => LogErr.evalError ["illegal form of namespace initializer"]
            val fixtureName = Ast.PropName { ns = qualNs, id = ident } 
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
    : (Ast.FIXTURES * Ast.FIXTURES * Ast.INITS) = (* unhoisted, hoisted, inits *)
    case defn of 
        Ast.VariableDefn { kind, ns, static, prototype, bindings } =>
            let
                val ns = resolveExprOptToNamespace env ns
                val (fxtrs,inits) = defBindings env kind ns bindings
            in case kind of
                (Ast.Var | Ast.Const) => ([],fxtrs,inits)  (* hoisted fxtrs *)
              | (Ast.LetVar | Ast.LetConst) => (fxtrs,[],inits)  (* don't hoisted fxtrs *)
            end
      | Ast.FunctionDefn fd => 
            let
                val {kind,...} = fd
                val (fxtrs,def) = defFuncDefn env fd
            in case kind of
                (Ast.Var | Ast.Const) => ([],fxtrs,[])  (* hoisted fxtrs *)
              | (Ast.LetVar | Ast.LetConst) => (fxtrs,[],[])  (* don't hoisted fxtrs *)
            end
      | Ast.NamespaceDefn nd => 
            let
                val (unhoisted,def) = defNamespace env nd
            in
                (unhoisted,[],[])
            end
      | Ast.ClassDefn cd =>
            let
                val (hoisted,def) = defClass env cd
            in
                ([],hoisted,[])
            end  

      | _ => LogErr.unimplError ["defDefn"]

(*
    DEFN list

    Process each definition. 
*)

and defDefns (env:ENV) 
             (unhoisted:Ast.FIXTURES)
             (hoisted:Ast.FIXTURES)
             (inits:Ast.INITS)
             (defns:Ast.DEFN list)
    : (Ast.FIXTURES * Ast.FIXTURES * Ast.INITS) = (* unhoisted, hoisted, inits *)
    let val _ = trace([">> defDefns"])
    in case defns of
        [] => (trace(["<< defDefns"]);(unhoisted,hoisted,inits))
      | d::ds =>
            let
                val (unhoisted',hoisted',inits') = defDefn env d
                val temp = case d of Ast.ClassDefn _ => hoisted' | _ => []
                val env'  = updateEnvironment env (unhoisted'@temp)  
                            (* add the new unhoisted and temporarily, hoisted class fxtrs to the current env *)
            in
                defDefns env' (unhoisted@unhoisted') (hoisted@hoisted') (inits@inits') ds
            end
    end

(*
    BLOCK

    Initialise a Block's fixtures and initialisers fields. Pragmas and
    definitions are not used after this definition phase. Traverse the
    statements so that embedded blocks (e.g. block statements, let 
    expressions) are initialised.

    Class blocks have an outer scope that contain the class (static) 
    fixtures. When entering a class block, extend the environment with
    the class object and its base objects, in reverse order

*)

and defBlock (env:ENV) 
             (b:Ast.BLOCK) 
    : (Ast.BLOCK * Ast.FIXTURES) =
    case b of
        Ast.Block { pragmas, defns, body,... } => 
        let 
            val env : ENV = defPragmas env pragmas
            val (unhoisted_defn_fxtrs,hoisted_defn_fxtrs,inits) = defDefns env [] [] [] defns
            val env = updateEnvironment env (unhoisted_defn_fxtrs@hoisted_defn_fxtrs) (* so stmts can see them *)
            val (body,hoisted_body_fxtrs) = defStmts env body
            val hoisted = hoisted_defn_fxtrs@hoisted_body_fxtrs
        in
            (Ast.Block { pragmas = pragmas,
                         defns = [],  (* clear definitions, we are done with them *)
                         body = body,
                         head = SOME (unhoisted_defn_fxtrs,inits) },
             hoisted)
        end

and defRegionalBlock (env:ENV) (blk:Ast.BLOCK)
    : Ast.BLOCK =
        let
            val (Ast.Block {defns,body,head=head,pragmas},hoisted) = defBlock env blk
            val (fixtures,inits) = valOf head
        in
            Ast.Block {pragmas=pragmas,
                       defns=defns,
                       body=body,
                       head=(SOME ((hoisted @ fixtures),inits))}
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
            val (block,hoisted) = defBlock env (#block package)
        in
            ({ name = (#name package),
              block = block }, hoisted)
        end


(*
    PROGRAM
*)

and defProgram (prog:Ast.PROGRAM) 
    : Ast.PROGRAM = 
    let 
        fun mkNamespaceFixtureBinding pkg = 
            case (pkg:Ast.PACKAGE) of 
                {name, ...} => (Ast.PropName {ns=Ast.Internal "", id=name}, 
                                Ast.NamespaceFixture (Ast.Public name))

        val intrinsicBinding = (Ast.PropName {ns=Ast.Internal "", id="intrinsic"}, 
                                Ast.NamespaceFixture Ast.Intrinsic)
        val topEnv = 
            [{ fixtures = intrinsicBinding::(map mkNamespaceFixtureBinding (#packages prog)),
               openNamespaces = [[Ast.Internal "", Ast.Public ""]],
               numericMode = defaultNumericMode, labels = [], 
               className = NONE, packageName = NONE, defaultNamespace = Ast.Internal "" }]
        val (packages,hoisted_pkg) = ListPair.unzip (map (defPackage topEnv) (#packages prog))
        val (block,hoisted_gbl) = defBlock topEnv (#block prog)
        val result = {packages=packages,block=block,fixtures=SOME ((List.concat hoisted_pkg)@hoisted_gbl)}
    in
        trace ["definition complete"];
        (if !doTrace 
         then Pretty.ppProgram result
         else ());        
        result
    end
end
