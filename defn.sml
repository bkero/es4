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
    - evaluate pragmas
    - capture open namespaces in unqualified identifiers
    - capture arithmetic modes in arithmetic operators
    - disambiguate package / object references

    TODO
    - fold types
    - inheritance checks
    - rewrite prototype defns as statements in cinit
    - disambiguate package / object references
 *)

type CONTEXT = 
     { fixtures: Ast.FIXTURES,
       openNamespaces: Ast.NAMESPACE list list, 
       numericMode: Ast.NUMERIC_MODE,
       temp_count: int ref }

type ENV = CONTEXT list

val currentClassName : Ast.NAME ref = ref {id="",ns=Ast.Intrinsic}
    (* ISSUE: is there a beter way to manage this? *)

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
                SOME n => (n,getFixture fixtures n)
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
                (_,Ast.NamespaceFixture ns) => ns
              | _ => LogErr.defnError ["namespace expression resolved ",
                                       "to non-namespace fixture"]
        end
      | _ => LogErr.defnError ["unexpected expression type ",
                               "in namespace context"]

(*
    Create a new context initialised with the provided fixtures and
    inherited environment
*)
             
fun extendEnvironment (env:ENV) 
                      (fixtures:Ast.FIXTURES) 
    : ENV = 
    case env of 
        [] => { fixtures = fixtures,
                openNamespaces = [[Ast.Internal ""]],
                numericMode = defaultNumericMode,
                temp_count = ref 0 } :: []
      | ({ numericMode, openNamespaces, temp_count,... }) :: _ =>
        { fixtures = fixtures,
          openNamespaces = openNamespaces, 
          numericMode = numericMode,
          temp_count = temp_count } :: env

fun updateEnvironment (cx::ex) (fxtrs:Ast.FIXTURES) 
    : ENV =
        { fixtures = (fxtrs @ (#fixtures cx)),
          openNamespaces = (#openNamespaces cx), 
          numericMode = (#numericMode cx),
          temp_count = (#temp_count cx) } :: ex
  | updateEnvironment ([]) (fxtrs:Ast.FIXTURES) 
    : ENV =
        LogErr.defnError ["cannot update an empty environment"]

(* copied from eval.sml *)
fun multinameOf (n:Ast.NAME) = 
    { nss = [[(#ns n)]], id = (#id n) }



(*
    Resolve an IDENT_EXPR to a multiname

    A qualified name with a literal namespace qualifier (including package qualified) 
    gets resolved to a multiname with a single namespace
*)

fun identExprToMultiname (env:ENV) (ie:Ast.IDENT_EXPR)
    : Ast.MULTINAME = 
    case ie of
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
      | _ => LogErr.defnError ["unhandled form of identifier expression in identExprToMultiname"]

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
            stmts = ...  (* static initialiser *)
        }
        iblk = { ... }
    }

    The steps taken are:
    - analyze class body into instance and class blocks
    - resolve extends and implements and do inheritance
    - return a fixture binding for the class
*)

fun defClass (env: ENV) 
             (cdef: Ast.CLASS_DEFN)
    : (Ast.FIXTURES * Ast.CLASS_DEFN) =
    let
        val (className,class) = analyzeClass env cdef
        val class = resolveClass env cdef className class
        val _ = LogErr.trace ["defining class ",LogErr.name className]
    in
        ([(className,Ast.ClassFixture class)],cdef)
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
    : (Ast.NAME*Ast.CLS) =
    case cdef of
        {ns, ident, body=Ast.Block { pragmas, defns, stmts, ... },...} =>
        let
            fun isLet (d:Ast.DEFN)
                : bool =
                case d of
                    Ast.VariableDefn {kind,...} => (kind=Ast.LetVar) orelse (kind=Ast.LetConst)
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
                    not ((isProto d) orelse (isStatic d) orelse (isLet d))
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

            fun isCtor (n:Ast.NAME,d:Ast.DEFN) : bool = 
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

            (*
                Partition the class definition into a class block
                and an instance block, and then define both blocks
            *)

            val ns = resolveExprToNamespace env ns
            val name = {id=ident, ns=ns}
            val protoDefns = List.filter isProto defns
            val staticDefns = List.filter isStatic defns
            val instanceDefns = List.filter isInstance defns
            val letDefns = List.filter isLet defns

            val env = defPragmas env pragmas

            val (unhoisted,classFixtures) = defDefinitions env [] [] staticDefns
            val env = extendEnvironment env classFixtures
            val (unhoisted,instanceFixtures) = defDefinitions env [] [] instanceDefns

            (*
                Add static prototype fixture
            *)

            val pf = Ast.ValFixture {ty=Ast.NominalType {ident=Ast.QualifiedIdentifier 
                                                {qual=Ast.LiteralExpr (Ast.LiteralNamespace ns),
                                                 ident=ident}},
                                     readOnly=true,
                                     isOverride=false,
                                     isFinal=false,
                                     init=NONE }

            val classFixtures = ({ns=Ast.Public "",id="prototype"},pf)::classFixtures

            (* 
                Separate instance init from non-instance init statements 
            *)

            val (iinits,stmts) = List.partition isInstanceInit stmts
        in
            (name,Ast.Cls {extends = NONE,
                     implements = [],
                     classFixtures = classFixtures,
                     instanceFixtures = instanceFixtures,
                     instanceInits = iinits,
                     constructor = NONE,
                     classType = Ast.SpecialType Ast.Any,
                     instanceType = Ast.SpecialType Ast.Any })
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
        val _ = (trace ["inheritFixtures "];
                 Pretty.ppFixtures base;
                 Pretty.ppFixtures derived)

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

        fun inheritFixture ((n,fb):(Ast.NAME * Ast.FIXTURE))
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
                  | _ => LogErr.defnError ["illegal override of ", LogErr.name n]
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
                fun isVoid ty = case ty of Ast.SpecialType Ast.VoidType => true | _ => false

                val isCompatible = case (fb,fd) of 
                        (Ast.ValFixture {
                            ty=Ast.FunctionType 
                                (Ast.FunctionSignature 
                                    {typeParams=tpb,params=pb,returnType=rtb,...}),...},
                         Ast.ValFixture 
                            {ty=Ast.FunctionType 
                                (Ast.FunctionSignature 
                                    {typeParams=tpd,params=pd,returnType=rtd,...}),...}) =>
                            (length tpb)=(length tpd) andalso (length pb)=(length pd) andalso
                                (((isVoid rtb) andalso (isVoid rtd)) orelse
                                 ((not (isVoid rtb)) andalso (not (isVoid rtd)))) 
                      | _ => false
            
            in case (fb,fd) of
                (Ast.ValFixture {isFinal,...}, Ast.ValFixture {isOverride,...}) => 
                    (not isFinal) andalso isOverride andalso isCompatible
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
                 (name:Ast.NAME)
                 (Ast.Cls {classFixtures,instanceFixtures,instanceInits,
                   constructor,classType,instanceType,...}:Ast.CLS)
    : Ast.CLS =
    let
        val _ = trace ["analyzing class block for ", LogErr.name name]
        val (extendsName, instanceFixtures) = resolveExtends env instanceFixtures extends [name]
        val (implementsNames, instanceFixtures) = resolveImplements env instanceFixtures implements
    in
        Ast.Cls {extends=extendsName,
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
        val _ = LogErr.trace ["first child ",LogErr.name (hd children)]
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
        val implements = map (identExprToMultiname env) (map (defIdentExpr env) implements)
    in
        ([],currInstanceFixtures)
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
            val fxtrs = defBinding env kind ns pattern patternType
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

and defFuncSig (env:ENV) 
               (fsig:Ast.FUNC_SIG)
    : (Ast.TYPE_EXPR * Ast.FIXTURES * Ast.STMT list * int) =

    case fsig of 
        Ast.FunctionSignature { typeParams, params, inits, 
                                returnType, thisType, hasRest } =>
        let 

            (* compute typeval fixtures (type parameters) *)
            val tyVarNs = Ast.Internal ""
            fun mkTypeVarFixture x = ({ns=tyVarNs, id=x}, Ast.TypeVarFixture)
            val typeParamFixtures = map mkTypeVarFixture typeParams
            val typeEnv = extendEnvironment env typeParamFixtures
            val thisType = case thisType of NONE => Ast.SpecialType Ast.Any | _ => valOf thisType

            val thisBinding = ({ns=tyVarNs,id="this"}, Ast.ValFixture
                               { ty = thisType,
                                 readOnly = true,
                                 isOverride = false,
                                 isFinal = false,
                                 init = NONE })


            (* compute val fixtures (parameters) *)
            val (paramFixtures, newParams) = defVars typeEnv params

            val ftype = Ast.FunctionType (Ast.FunctionSignature { typeParams=typeParams, params=params, 
                                           inits=[], returnType=returnType, 
                                           thisType=SOME thisType, hasRest=hasRest })
            val (inits,_) = defStmts env inits
        in
            (ftype,typeParamFixtures @ (thisBinding::paramFixtures), inits, List.length params)
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
    : (Ast.TYPE_EXPR * Ast.FUNC) =
    let
        val Ast.Func {name, fsig, body,...} = func
        val (ftype,fxtrs,inits,param_count) = defFuncSig env fsig
        val env = extendEnvironment env fxtrs
        val (lblk,hoisted) = defBlock env body
        val fblk = lblk  (* Ast.Block { pragmas=[], defns=[], fixtures=SOME (fxtrs@hoisted), 
                               inits=SOME inits, stmts = [Ast.BlockStmt lblk]}*)
    in case ftype of
        Ast.FunctionType fsig =>
            ((#temp_count (hd env)) := (param_count+1);  
                (* FIXME: this is intentionally ugly. it is an impl detail that needs to be hidden *)
             (ftype, Ast.Func {name = name,
                   fsig = fsig,
                   body = fblk,
                   fixtures = SOME (fxtrs@hoisted),
                   inits = inits}))
      | _ => LogErr.defnError ["internal definition error in defFunc"]
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
            val (ftype,isReadOnly) = if (#kind f) = Ast.Var 
                            then (Ast.SpecialType Ast.Any,false)    (* e3 style writeable function *)
                            else (ftype,true)                      (* read only, method *)
            val outerFixtures = [(newName, Ast.ValFixture
                                       { ty = ftype,
                                         readOnly = false,
                                         isOverride = (#override f),
                                         isFinal = (#final f),
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
        ( List.app (fn x => (* defPragma *)
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
                            precision = !precision },
            temp_count = (#temp_count ctx) } :: env )
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

          | Ast.ThisExpr => 
            Ast.ThisExpr

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
                val env = extendEnvironment env f0
                val newBody = defExprs env body
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
                Ast.ListExpr (defAssignment env p e)
            end

          | Ast.AllocTemp (n,e) => 
            let 
            in
                Ast.AllocTemp (n,e)
            end

          | Ast.KillTemp n => 
            let 
            in
                Ast.KillTemp n
            end

          | Ast.GetTemp n => 
            let 
            in
                Ast.GetTemp n
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

and defInit (env: ENV) (ns: Ast.NAMESPACE) (prototype: bool) (static: bool) (init: Ast.EXPR) 
    : Ast.EXPR list =
    let
    in case init of
        (Ast.SetExpr (_,pattern,expr)) =>
            defPatternAssign env ns pattern expr prototype static 0
      | _ => LogErr.defnError ["internal definition error in defInit"]
    end

and defAssignment (env:ENV) (pattern: Ast.PATTERN) (expr: Ast.EXPR)
    : Ast.EXPR list =
    let
        val level = 0  (* to start with *)
        val ns = Ast.Intrinsic  (* unused since there are no IdentifierPatterns in this context *)
    in
        defPatternAssign env ns pattern expr false false level
    end

(*
    var x = o
    
    temp t = o
    x = t
*)

and defPatternAssign (env:ENV) (ns: Ast.NAMESPACE) (pattern: Ast.PATTERN) (expr: Ast.EXPR) (prototype:bool) (static:bool) (level:int)
    : Ast.EXPR list =
    let
        fun defIdentifierAssign (id)
            : Ast.EXPR list =
            let
                val expr = defExpr env expr
                val lref = if prototype (* A.prototype.x *)
                          then 
                                Ast.ObjectRef 
                                    {base=Ast.ObjectRef 
                                        {base=Ast.LexicalRef 
                                            {ident=Ast.QualifiedIdentifier
                                                {qual=Ast.LiteralExpr (Ast.LiteralNamespace (#ns (!currentClassName))),
                                                 ident=(#id (!currentClassName))}},
                                         ident=Ast.QualifiedIdentifier
                                            {qual=Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Public "")),   (* prototype props are always public *)
                                             ident="prototype"}},
                                     ident=Ast.QualifiedIdentifier
                                         {qual=Ast.LiteralExpr (Ast.LiteralNamespace ns),
                                          ident=id}}
                          else
                          if static
                          then Ast.ObjectRef (* A.x *)
                                {base=Ast.LexicalRef 
                                    {ident=Ast.QualifiedIdentifier
                                        {qual=Ast.LiteralExpr (Ast.LiteralNamespace (#ns (!currentClassName))),
                                         ident=(#id (!currentClassName))}},
                                 ident=Ast.QualifiedIdentifier
                                    {qual=Ast.LiteralExpr (Ast.LiteralNamespace ns), 
                                     ident=id}}
                          else Ast.LexicalRef
                                {ident=Ast.QualifiedIdentifier
                                    {qual=Ast.LiteralExpr (Ast.LiteralNamespace ns), ident=id}}
            in
                [Ast.SetExpr (Ast.Assign,Ast.SimplePattern lref,expr)]
            end

        fun defSimpleAssign (patternExpr)
            : Ast.EXPR list =
            let
                val patternExpr = defExpr env patternExpr
                val expr = defExpr env expr
            in
                [Ast.SetExpr (Ast.Assign,Ast.SimplePattern patternExpr,expr)]
            end

        (*
            [x,y] = o
            let [i,s]:[int,String] = o

            ISSUE: Are partial type annotations allowed? let [i,s]:[int]=...
                   If so, what is the type of 's' here? int or *?
        *)

        fun defArrayAssign (elements:Ast.PATTERN list) (temp: Ast.EXPR) (n:int)
            : Ast.EXPR list =
            let
            in case elements of
                p::plist =>
                    let
                        val expr   = Ast.ObjectRef {base=temp, ident=Ast.ExpressionIdentifier
                                        (Ast.LiteralExpr (Ast.LiteralString (Int.toString n)))}
                        val inits  = defPatternAssign env ns p expr prototype static (level+1)
                        val inits' = defArrayAssign plist temp (n+1)
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
            
        fun defObjectAssign (fields:Ast.FIELD_PATTERN list) (temp: Ast.EXPR)
            : Ast.EXPR list =

            let
            in case fields of
                {name,ptrn}::plist =>
                   let
                        val expr   = Ast.ObjectRef {base=temp, ident=name}
                        val inits  = defPatternAssign env ns ptrn expr prototype static (level+1)
                        val inits' = defObjectAssign plist temp
                    in
                        inits @ inits'
                    end
              | [] => []  
            end                  

        val temp_n = !(#temp_count (hd env))+level
        val init = Ast.AllocTemp (temp_n, expr)
        val temp_base = Ast.GetTemp temp_n

    in case (pattern) of
        (Ast.ObjectPattern fields) =>
            let
                val inits = defObjectAssign fields temp_base
            in
                init::inits
            end
      | (Ast.ArrayPattern elements) => 
            let
                val temp_index = 0    (* use register indexes to keep track of temps *)
                val inits = defArrayAssign elements temp_base temp_index
            in
                init::inits
            end
      | (Ast.SimplePattern expr) =>
            defSimpleAssign expr
      | (Ast.IdentifierPattern id) => 
            defIdentifierAssign id
    end

and defBinding (env: ENV) (kind: Ast.VAR_DEFN_TAG) (ns: Ast.NAMESPACE)
               (pattern: Ast.PATTERN) (ty: Ast.TYPE_EXPR)
    : Ast.FIXTURES =
    let
        fun defIdentifierBinding (id) (ty) 
            : Ast.FIXTURES =
            let
                val readOnly = if kind = Ast.Const then true else false
            in
                [({ns=ns, id=id}, Ast.ValFixture { ty = ty, readOnly = readOnly, isOverride = false, isFinal=true, init = NONE })]
            end

        (*
            [x,y] = o
            let [i,s]:[int,String] = o

            ISSUE: Are partial type annotations allowed? let [i,s]:[int]=...
                   If so, what is the type of 's' here? int or *?
        *)

        fun defArrayBinding (elements:Ast.PATTERN list) (element_types:Ast.TYPE_EXPR list)
            : Ast.FIXTURES =
            let
            in case (elements,element_types) of
                (e::elist,t::tlist) =>
                    let
                        val fxtrs = defBinding env kind ns e t
                        val fxtrs' = defArrayBinding elist tlist
                    in
                        fxtrs @ fxtrs'
                    end
              | (e::elist,_) =>
                    let
                        val fxtrs = defBinding env kind ns e (Ast.SpecialType Ast.Any)
                        val fxtrs' = defArrayBinding elist []
                    in
                        fxtrs @ fxtrs'
                    end
              | ([],_) => []                    
            end

        (*
            let {i:x,j:y} : {i:int,j:string}
        *)


        fun defObjectBinding (fields:Ast.FIELD_PATTERN list) (field_types:Ast.FIELD_TYPE list)
            : Ast.FIXTURES =
            let
            in case fields of
                p::plist =>
                    let
                        val fxtrs = defFieldBinding p field_types
                        val fxtrs' = defObjectBinding plist field_types
                    in
                        fxtrs @ fxtrs'
                    end
              | [] => []                    
            end

        (*
            def a field pattern - use the field name to get the field type and
            associate that field type with the field's pattern
        *)

        and defFieldBinding (field_pattern: Ast.FIELD_PATTERN) 
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
                        defBinding env kind ns ptrn ty
                    end
              | ([],_) => defBinding env kind ns ptrn (Ast.SpecialType Ast.Any)
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
            defObjectBinding fields field_types
      | (Ast.ArrayPattern elements,Ast.ArrayType element_types) =>
            defArrayBinding elements element_types
      | (Ast.ArrayPattern elements,(Ast.SpecialType Ast.Any)) => 
            defArrayBinding elements []
      | (Ast.SimplePattern expr,_) =>
            LogErr.defnError ["internal error: simple pattern found in binding context"]
      | (Ast.IdentifierPattern id,_) => 
            defIdentifierBinding id ty
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
                          | SOME p => SOME (defBinding env Ast.Var (Ast.Internal "") p (Ast.SpecialType Ast.Any))
                    val newObj =  defExprs env obj
                    val (f0, newDefns) = defVars env defns
                    val env = extendEnvironment env f0
                    val (newBody,hoisted) = defStmt env body
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
                val env = extendEnvironment env f0
                val newInit = defExprs env init
                val newCond = defExprs env cond
                val newUpdate = defExprs env update
                val (newBody,hoisted) = defStmt env body
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
                val (f0, newBind) = defVar env Ast.Var (Ast.Internal "") bind
                val env = extendEnvironment env f0
                val (body,fixtures) = defBlock env body
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
                                 inr (SOME) (defVar env Ast.Var (Ast.Internal "") b)
                val env = extendEnvironment env b0
                val (body,hoisted) = defBlock env body
            in
                { ptrn = newPtrn,
                  body = body }
            end            

        fun findClass (n:Ast.NAME) =
            let
                val (n,f) = resolveMultinameToFixture env (multinameOf n)
            in case f of 
                Ast.ClassFixture cd => cd
              | _ => LogErr.defnError ["reference to non-class fixture"]
            end


    in
        case stmt of
            Ast.EmptyStmt => 
            (Ast.EmptyStmt,[])
         
          | Ast.ExprStmt es => 
            (Ast.ExprStmt (defExprs env es),[])
            
          | Ast.InitStmt {ns,inits,prototype,static,...} => 
                let
                    val ns0 = resolveExprToNamespace env ns
                in
                    ((Ast.ExprStmt (List.concat (map (defInit env ns0 prototype static) inits))),[])
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
            
          | Ast.ClassBlock {ns,ident,block,...} =>
            let
                val name = {ns=resolveExprToNamespace env ns,id=ident}
                val _ = (currentClassName := name)
                val Ast.Cls cls = findClass name
                val fixtures = (#classFixtures cls)
                val extends = (#extends cls)
                val env = extendEnvironment env fixtures
                val block = defRegionalBlock env block
                val _ = (currentClassName := {ns=Ast.Intrinsic,id=""})
            in
                (Ast.ClassBlock {ns=ns,ident=ident,name=SOME name,extends=extends,
                                 fixtures=SOME fixtures,block=block},[])
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
                val env = extendEnvironment env b0
                val (newStmt,hoisted) = defStmt env stmt
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
                ([],hoisted)
            end  

      | _ => ([],[])

(*
    DEFN list

    Process each definition. 
*)

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
                val env'  = updateEnvironment env u  
                            (* add the new unhoisted fxtrs to the current env *)
            in
                defDefinitions env' (u@unhoisted) (h@hoisted) ds
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
    the class object and its base objects, in reverse order.

    

*)

and defBlock (env:ENV) 
             (b:Ast.BLOCK) 
    : (Ast.BLOCK * Ast.FIXTURES) =
    case b of 
        Ast.Block { pragmas, defns, stmts, inits,... } => 
        let 
            val env = defPragmas env pragmas
            val (unhoisted,defns_hoisted) = defDefinitions env [] [] defns
            
            val env  = updateEnvironment env (unhoisted@defns_hoisted )
            val (inits,_) = defStmts env (case inits of NONE => [] | _ => valOf inits)  
                                    (* do inits in outer scope, inits never result in hoisted defs *)
            val env = updateEnvironment env unhoisted
            val (stmts,stmts_hoisted) = defStmts env stmts
            val hoisted = defns_hoisted@stmts_hoisted
        in
            (Ast.Block { pragmas = pragmas,
                         defns = [],  (* clear definitions *)
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
               numericMode = defaultNumericMode, temp_count = ref 0 }]
        val (packages,hoisted_pkg) = ListPair.unzip (map (defPackage topEnv) (#packages prog))
        val (body,hoisted_gbl) = defBlock topEnv (#body prog)
    in
        {packages=packages,body=body,fixtures=SOME ((List.concat hoisted_pkg)@hoisted_gbl)}
    end
end
