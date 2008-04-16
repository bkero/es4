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

structure Defn = struct

(* Local tracing machinery *)

val doTrace = ref false
val doTraceSummary = ref false
fun log ss = LogErr.log ("[defn] " :: ss)
fun error ss = LogErr.defnError ss
fun trace ss = if (!doTrace) then log ss else ()
fun trace2 (s, us) = if (!doTrace) then log [s, (Ustring.toAscii us)] else ()

(* 
 * We use a globally unique numbering of type variables, assigned once when
 * we walk the AST during definition. They are inserted in any ribs formed
 * under a type binder. This includes the activation rib of any type-parametric
 * function, the instance rib of any type-parametric interface or class, and
 * -- depending on how we resolve the interaction of type parameters and 
 * class objects -- possibly the class rib of any type-parametric class.
 * 
 * We do not insetrt them anywhere in type terms, because those have no ribs.
 *)

val typeVarCounter = ref 0
fun mkTypeVarFixture _ = 
    (typeVarCounter := (!typeVarCounter) + 1;
     Ast.TypeVarFixture (!typeVarCounter))

fun mkParamRib (idents:Ast.IDENT list)
  : Ast.RIB = 
    map (fn id => (Ast.PropName (Name.nons id), (mkTypeVarFixture()))) idents

(*
 * The goal of the definition phase is to put together the ribs
 * of the program, as well as insert class, function and interface
 * objects into the global object.

    To be specific, the definition phase completes the following tasks:
    - fold type expressions
    - translate defnitions to rib + initialisers
    - check for conflicting fixtures
    - hoist rib
    - inherit super classes and interfaces
    - evaluate pragmas
    - capture open namespaces in unqualified identifiers
    - capture arithmetic modes in arithmetic operators
    - disambiguate package / object references

    TODO
    - fold types
    - inheritance checks
 *)


datatype LABEL_KIND =
          IterationLabel
        | SwitchLabel
        | StatementLabel


type LABEL = (Ast.IDENT * LABEL_KIND)


type ENV =
     { ribId: Ast.RIB_ID option,
       tempOffset: int,
       openNamespaces: Ast.NAMESPACE list list,
       labels: LABEL list,
       className: Ast.IDENT,
       packageName: Ast.IDENT list,
       defaultNamespace: Ast.NAMESPACE,
       program: Fixture.PROGRAM,
       func: Ast.FUNC option }    


fun dumpEnv (e:ENV) : unit =
    if (!doTrace) 
    then 
        let 
            val (ribs, closed) = Fixture.getRibs (#program e) (#ribId e)
        in
            List.app Fixture.printRib ribs
        end
    else ()

    
val (initRib:Ast.RIB) = [ (Ast.PropName Name.meta_, Ast.NamespaceFixture Name.metaNS),
                          (Ast.PropName Name.magic_, Ast.NamespaceFixture Name.magicNS),
                          (Ast.PropName Name.informative_, Ast.NamespaceFixture Name.informativeNS),
                          (Ast.PropName Name.ES4_, Ast.NamespaceFixture Name.ES4NS)]


fun makeTy (e:ENV) 
           (tyExpr:Ast.TYPE_EXPR) 
    : Ast.TY = 
    let
        val ty = Ast.Ty { expr = tyExpr,
                          ribId = #ribId e }
    in
        (* 
         * FIXME: controversial? This attempts to normalize each type term the first 
         * time we see it. We ignore type errors since it's not technically
         * our job to try this here; if someone wants early typechecking they can
         * use the verifier.
         *)        
        Type.normalize (#program e) [] ty
        handle LogErr.TypeError _ => ty
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

fun inl f (a, b) = (f a, b)

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
    resolve a multiname to a name and then get the corresponding fixture

    for each nested context in the environment, look for a fixture that matches
    a multiname. see if a particular scope has a fixture with one of a list of
    multinames

    multiname = { nss : NAMESPACE list list, id: IDENT }
    name = { ns: NAMESPACE, id: IDENT }
*)

fun resolve (env:ENV)
            (mname:Ast.MULTINAME)
    : (Ast.NAME * Ast.FIXTURE) =
    case Fixture.resolveToFixture (#program env) mname (#ribId env) of 
        SOME nf => nf
      | NONE => LogErr.defnError ["multiname did not resolve ", LogErr.multiname mname]

(*
    Since we are in the definition phase the open namespaces have not been
    captured yet, so capture them before evaluating an unqualified namespace
    expression
*)

fun packageIdentFromPath origPath
    : Ast.IDENT =
    let fun packageIdentFromPath_ [] ident = ident
          | packageIdentFromPath_ (pthid::pth) ident =
           let
               val dot = if ident=Ustring.empty then Ustring.empty else Ustring.dot
           in
               packageIdentFromPath_ pth (Ustring.append [ident, dot, pthid])
           end
    in
        packageIdentFromPath_ origPath Ustring.empty
    end


fun addNamespace (ns:Ast.NAMESPACE) 
                 (opennss:Ast.NAMESPACE list) =
    if List.exists (fn x => ns = x) opennss
    (* FIXME: should be an error to open namspaces redundantly *)
    then (trace ["skipping namespace ",LogErr.namespace ns]; opennss)   
    else (trace ["adding namespace ", LogErr.namespace ns]; ns :: opennss)

fun defNamespace (env:ENV)
                 (ns:Ast.NAMESPACE)
    : Ast.NAMESPACE =
        let
        in case ns of
            Ast.Public n =>
                let
                    val packageName = (#packageName env)
                    val ident = packageIdentFromPath packageName
                in
                    if n = Ustring.empty
                    then Ast.Public ident
                    else ns
                        (* if n is not empty, then it is a package qualifier,
                           so don't with the current package name *)
                end
          | Ast.Internal _ =>
                let
                    val packageName = (#packageName env)
                    val ident = packageIdentFromPath packageName
                in
(trace2 ("packageName=",ident);
                    Ast.Internal ident)
                end
          | Ast.Private _ =>
                let
                in
                    Ast.Private (#className env)
                end
          | Ast.Protected _ =>
                let
                in
                    Ast.Protected (#className env)  (* [JD] FIXME: not quite right *)
                end
          | _ => ns
        end

fun resolveExprToNamespace (env:ENV)
                           (expr:Ast.EXPR)
    : Ast.NAMESPACE =
    case expr of
        Ast.LiteralExpr (Ast.LiteralNamespace ns) =>
        let
        in
            defNamespace env ns
        end
      | Ast.LexicalRef {ident = Ast.Identifier {ident,...}, loc } =>
        let
            val _ = LogErr.setLoc loc
            val mname = {nss = (#openNamespaces env), id = ident}
        in
            case resolve env mname of
                (_, Ast.NamespaceFixture ns) => ns
              | _ => LogErr.defnError ["namespace expression did not resolve ",
                                       "to namespace fixture"]
        end
      | _ => LogErr.defnError ["unexpected expression type ",
                               "in namespace context"]

and resolveExprOptToNamespace (env: ENV)
                              (ns:Ast.EXPR option)
    : Ast.NAMESPACE =
    case ns of
        NONE => (#defaultNamespace env)
      | SOME n => resolveExprToNamespace env n

(*
    Create a new context initialised with the provided rib and
    inherited environment
*)

fun enterFragment (env:ENV)
                  (frag:Ast.FRAGMENT)
    : ENV =     
    let
        val { ribId, tempOffset, openNamespaces, 
              labels, className, packageName, 
              defaultNamespace, program, func } = env                                           
    
        val (newDefaultNs, newPkgName, newOpenNss) = 
            case frag of 
                Ast.Package { name, ... } => 
                let 
                    val packageIdent = packageIdentFromPath name
                in
                    Fixture.addPackageName program name;
                    (Ast.Internal packageIdent, name,
                     [[Ast.Internal packageIdent, Ast.Public packageIdent]] @ openNamespaces)
                end
              | _ => (defaultNamespace, packageName, openNamespaces)
    in
        { ribId = ribId, 
          tempOffset = tempOffset,
          openNamespaces = newOpenNss,
          labels = labels,
          className = className,
          packageName = newPkgName,
          defaultNamespace = newDefaultNs,
          program = program,
          func = func }
    end

    
fun extendEnvironment (env:ENV)
                      (rib:Ast.RIB)
    : ENV =
    let
        val { ribId, tempOffset, openNamespaces, 
              labels, className, packageName, 
              defaultNamespace, program, func } = env
        val newRibId = SOME (Fixture.allocGeneralRib program (#ribId env))
    in
        Fixture.saveRib program newRibId rib;
        { ribId = newRibId, 
          tempOffset = tempOffset,
          openNamespaces = openNamespaces,
          labels = labels,
          className = className,
          packageName = packageName,
          defaultNamespace = defaultNamespace,
          program = program,
          func = func }
    end


fun addToRib (env:ENV)
             (additions:Ast.RIB)
    : unit =
    let 
        val ribId = (#ribId env)
    in
        Fixture.extendRib (#program env) ribId additions (Type.matches (#program env) [])
    end


fun mergeRibs (program:Fixture.PROGRAM)
              (oldRib:Ast.RIB)
              (newRib:Ast.RIB)
    : Ast.RIB = 
    Fixture.mergeRibs (Type.matches program []) oldRib newRib


fun addPackageName ((newPkgName:Ast.IDENT list),(env:ENV))
    : unit =
    Fixture.addPackageName (#program env) newPkgName


fun updateTempOffset (env:ENV) (newTempOffset:int)
    : ENV =
    let
        val { ribId, tempOffset, openNamespaces, 
              labels, className, packageName, 
              defaultNamespace, program, func } = env
    in
        { ribId = ribId,
          tempOffset = newTempOffset,
          openNamespaces = openNamespaces,
          labels = labels,
          className = className,
          packageName = packageName,
          defaultNamespace = defaultNamespace,
          program = program,
          func = func }
    end


fun clearPackageName (env:ENV)
    : ENV =
    let
        val { ribId, tempOffset, openNamespaces, 
              labels, className, packageName, 
              defaultNamespace, program, func } = env
    in
        { ribId = ribId,
          tempOffset = tempOffset,
          openNamespaces = openNamespaces,
          labels = labels,
          className = className,
          packageName = [],
          defaultNamespace = defaultNamespace,
          program = program,
          func = func }
    end


fun enterClass (env:ENV) (newClassName:Ast.NAME)
    : ENV =
    let
        val { ribId, tempOffset, openNamespaces, 
              labels, className, packageName, 
              defaultNamespace, program, func } = env
        val className = Name.mangle newClassName
    in
        { ribId = ribId,
          tempOffset = tempOffset,
          openNamespaces = [Ast.Private className]::openNamespaces,
          labels = labels,
          className = className,
          packageName = packageName,
          defaultNamespace = defaultNamespace,
          program = program,
          func = func }
    end

fun enterFuncBody (env:ENV) (newFunc:Ast.FUNC)
    : ENV = 
    let
        val { ribId, tempOffset, openNamespaces, 
              labels, className, packageName, 
              defaultNamespace, program, func } = env    
    in
        { ribId = ribId,
          tempOffset = tempOffset,
          openNamespaces = [Ast.Private className]::openNamespaces,
          labels = labels,
          className = className,
          packageName = packageName,
          defaultNamespace = defaultNamespace,
          program = program,
          func = SOME newFunc }
    end


fun dumpLabels (labels : LABEL list) = 
    trace ["labels ", concat (map (fn (id,_) => (Ustring.toAscii id) ^ " ") labels)]

fun dumpPath path = 
    trace ["path ", concat (map (fn (id) => (Ustring.toAscii id) ^ " ") path)]

(*
    Add a label to the current environment context. Report an error
    if there is a duplicate. Labels on switch and iteration statements
    have special meaning. They get lifted into the switch or iteration
    statement so the correct continuation value can be returned in case
    of break, and the correct control flow produced in case of continue.
    Also, the definition phase must distinguish between ordinary statement
    labels and iteration and switch labels to validate continue statements.
*)

fun addLabel ((label:LABEL),(env:ENV))
    : ENV =
        let
            val { ribId, tempOffset, openNamespaces, 
                  labels, className, packageName, 
                  defaultNamespace, program, func } = env
            val (labelId,labelKnd) = label
        in
            dumpLabels labels;
            if List.exists (fn ((id,knd):LABEL) =>
                               not (id = Ustring.empty) andalso  (* ignore empty labels *)
                               id = labelId andalso              (* compare ids *)
                               knd = labelKnd) labels            (* and kinds *)
            then LogErr.defnError ["duplicate label ", Ustring.toAscii labelId]
            else ();
            { ribId = ribId,
              tempOffset = tempOffset,
              openNamespaces = openNamespaces,
              labels = label::labels,
              className = className,
              packageName = packageName,
              defaultNamespace = defaultNamespace,
              program = program,
              func = func }
        end


fun addLabels (env:ENV) (labels:LABEL list)
    : ENV =
    List.foldl addLabel env labels 


fun multinameFromName (n:Ast.NAME) =
    { nss = [[(#ns n)]], id = (#id n) }

(*
    Resolve an IDENT_EXPR to a multiname

    A qualified name with a literal namespace qualifier (including package qualified)
                            gets resolved to a multiname with a single namespace
 *)
    
fun identExprToMultiname (env:ENV) 
                         (ie:Ast.IDENT_EXPR)
    : Ast.MULTINAME =
    let
        val ie' = defIdentExpr env ie
    in 
        case ie' of
            Ast.Identifier {ident, ...} =>
            let
            in
                {nss = (#openNamespaces env), id = ident}
            end
          | Ast.QualifiedIdentifier {ident, qual,...} =>
            let
                val ns = resolveExprToNamespace env qual
            in
                {nss = [[ns]], id = ident}
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
            fxtrs = ...  (* static rib *)
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
    : (Ast.RIB * Ast.CLASS_DEFN) =
    let
        val class = analyzeClassBody env cdef
        val class = resolveClassInheritance env cdef class
        val Ast.Cls {name,...} = class
    in
        ([(Ast.PropName name, Ast.ClassFixture class)],cdef)
    end

(*
    Defining an interface

    - unpack the interface definition
    - construct the interface name
    - resolve super interfaces
    - define current interface definitions
    - inherit base rib
    - construct instance type
    - construct interface
    - return interface fixture
*)

and defInterface (env: ENV)
                 (idef: Ast.INTERFACE_DEFN)
    : (Ast.RIB * Ast.INTERFACE_DEFN) =
    let
        val { ident, ns, nonnullable, params, extends, instanceDefns } = idef

        (* Make the interface name *)
        val name = {id = ident, ns = resolveExprOptToNamespace env ns} 

        (* Resolve base interface's super interfaces and rib *)
        val (superInterfaces:Ast.TY list, inheritedRib:Ast.RIB) = resolveInterfaces env extends

        val prog = (#program env)

        val groundSuperInterfaceExprs = map (Type.groundExpr o (Type.normalize prog [])) superInterfaces

        val env = enterClass env name

        val instanceEnv = extendEnvironment env []
        val (typeParamRib:Ast.RIB) = mkParamRib params
        val _ = addToRib instanceEnv typeParamRib

        (* Define the current rib *)
        val (unhoisted,instanceRib,_) = defDefns env instanceDefns

        (* Inherit rib and check overrides *)
        val instanceRib:Ast.RIB = inheritRib inheritedRib instanceRib

        (* Make the instance type and interface fixture *)
        val instanceType:Ast.TY = 
            makeTy env (Ast.InstanceType 
                            { name=name,
                              nonnullable=nonnullable,
                              typeParams=params,
                              typeArgs=[],
                              superTypes=groundSuperInterfaceExprs,
                              ty=Ast.SpecialType Ast.Any,  (* FIXME needs synthetic record type *)
                              dynamic=false}) (* interfaces are never dynamic *)
                        
        val iface:Ast.IFACE = 
            Ast.Iface { name=name, 
                        typeParams=params,
                        nonnullable=nonnullable, 
                        extends=superInterfaces, 
                        instanceRib=instanceRib,
                        instanceType=instanceType }
    in
        ([(Ast.PropName name, Ast.InterfaceFixture iface)],idef)
    end

(*
    inheritRib

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

(*
   Given two fixtures, one base and other derived, check to see if
   the derived fixture can override the base fixture. Specifically,
   check that:

       - the base fixture is not 'final'
       - the derived fixture is 'override'
       - they have same number of parameters
       - they both have the return type void or neither has the return type void
       - what else?

       - FIXME: perhaps we ought to permit overriding fixtures with
         parametric types; we used to but the problem is more complex with general 
         lambda types. Currently disabled.

   Type compatibility of parameter and return types is done by the evaluator
   (or verifier in strict mode) and not here because type annotations can have
   forward references
*)

and canOverride (fb:Ast.FIXTURE) (fd:Ast.FIXTURE)
    : bool =
    let
        fun isVoid ty = 
            case ty of 
                Ast.SpecialType Ast.VoidType => true
              | _ => false
                     
        val isCompatible = case (fb,fd) of
                (Ast.MethodFixture
                     {ty=Ast.Ty 
                             {expr=Ast.FunctionType
                                       {params=pb, 
                                        result=rtb, 
                                        minArgs=mb,
                                        ...}, 
                              ...},
                      func=Ast.Func 
                               {fsig=Ast.FunctionSignature 
                                         {defaults=db, 
                                          ...}, 
                                ...},
                      ...},
                 Ast.MethodFixture
                     {ty=Ast.Ty 
                             {expr=Ast.FunctionType
                                       {params=pd, 
                                        result=rtd, 
                                        minArgs=md,
                                        ...}, 
                              ...},
                       func=Ast.Func 
                                {fsig=Ast.FunctionSignature 
                                          {defaults=dd, 
                                           ...}, 
                                 ...},
                      ...}) =>
                    let
                        val _ = trace ["mb ",Int.toString mb, " md ",Int.toString md,"\n"]
                        val _ = trace ["length pb ",Int.toString (length pb),
                                       " length pd ",Int.toString (length pd),"\n"]
                        val _ = trace ["length db ",Int.toString (length db),
                                       " length dd ",Int.toString (length dd),"\n"]
                    in
                        ((isVoid rtb) andalso (isVoid rtd))
                        orelse
                        ((not (isVoid rtb)) andalso (not (isVoid rtd)))
                    (* FIXME: check compatibility of return types? *)
                    end
                    
              | _ => false
        val _ = trace ["isCompatible = ",Bool.toString isCompatible]

    in case (fb,fd) of
        (Ast.MethodFixture {final,func,...}, 
         Ast.MethodFixture {override,...}) =>
        (((not final) andalso override) orelse (AstQuery.funcIsAbstract func))
        andalso isCompatible

      (* FIXME: what are the rules for getter/setter overriding?
         1/base fixture is not final
         2/derived fixture is override
         3/getter is compatible
         4/setter is compatible
      *)
      | (Ast.VirtualValFixture vb,
         Ast.VirtualValFixture vd) =>
            let
                val _ = trace ["checking override of VirtualValFixture"]
            in
               true
            end
      | _ => LogErr.unimplError ["checkOverride"]
    end

and inheritRib (base:Ast.RIB)
               (derived:Ast.RIB)
    : Ast.RIB =
    let

        (*
           Recurse through the rib of a base class to see if the
           given fixture binding is allowed. if so, then add it
           return the updated rib

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

        fun inheritFixture (n:Ast.FIXTURE_NAME, fb:Ast.FIXTURE)
            : Ast.RIB =
            let
                fun targetFixture _ = if (Fixture.hasFixture derived n)
                                      then SOME (Fixture.getFixture derived n)
                                      else NONE
                val _ = trace ["checking override of ", LogErr.fname n]
            in
                case n of
                    (* Private fixtures never "inherit", so it's meaningless to ask. *)
                    Ast.PropName {ns=Ast.Private _, ...} => derived
                  | _ => case targetFixture () of
                             NONE => (n,fb)::derived    (* not in the derived class, so inherit it *)
                           | SOME fd =>
                             case (canOverride fb fd) of
                                 true => derived  (* return current fixtures *)
                               | _ => LogErr.defnError ["illegal override of ", LogErr.fname n]
            end

    in case base of
        [] => derived (* done *)
      | first::follows => inheritRib follows (inheritFixture first)
    end

(*
    implementFixtures

    Steps:

    -

    Errors:

    - interface fixture not implemented

*)

and implementFixtures (base:Ast.RIB)
                      (derived:Ast.RIB)
    : unit =
    let
        (*
        *)

        fun implementFixture ((n,fb):(Ast.FIXTURE_NAME * Ast.FIXTURE))
            : Ast.RIB =
            let
                fun targetFixture _ = if (Fixture.hasFixture derived n)
                                      then SOME (Fixture.getFixture derived n)
                                      else NONE
                val _ = trace ["checking implementation of ", LogErr.fname n]
            in
                case targetFixture () of
                    NONE => LogErr.defnError ["unimplemented interface method ", LogErr.fname n]
                  | SOME fd =>
                        case (canOverride fb fd) of
                            true => derived  (* return current fixtures *)
                          | _ => LogErr.defnError ["illegal implementation of ", LogErr.fname n]
            end

    in case base of
        [] => () (* done *)
      | first::follows => implementFixtures follows (implementFixture first)
    end

(*
    resolveClassInheritance

    Inherit instance fixtures from the base class. Check fixtures against
    interface fixtures
*)

and resolveClassInheritance (env:ENV)
                 ({extends,implements,...}: Ast.CLASS_DEFN)
                 (Ast.Cls {name,typeParams,nonnullable,dynamic,classRib,instanceRib,instanceInits,
                           constructor,classType,instanceType,...}:Ast.CLS)
    : Ast.CLS =
    let
        val _ = trace ["analyzing inheritance for ", LogErr.name name]

        val (extendsTy:Ast.TY option, 
             instanceRib0:Ast.RIB) = resolveExtends env instanceRib extends name

        val (implementsTys:Ast.TY list, 
             instanceRib1:Ast.RIB) = resolveImplements env instanceRib0 implements


        val instanceType = 
            let
                val (it, rid) = AstQuery.extractInstanceType instanceType
                val superTypes:Ast.TY list = 
                    case extendsTy of 
                        NONE => implementsTys
                      | SOME ty => ty :: implementsTys
                val prog = (#program env)
                val superTypes = map (Type.groundExpr o (Type.normalize prog [])) superTypes
                val te = Ast.InstanceType { name = (#name it),
                                            typeParams = (#typeParams it),
                                            typeArgs = (#typeArgs it),
                                            nonnullable = (#nonnullable it),
                                            superTypes = superTypes,
                                            ty = (#ty it),
                                            dynamic = (#dynamic it)}
                val te = case typeParams of 
                             [] => te
                           | params => Ast.LamType { params = params, 
                                                     body = te }
            in
                Ast.Ty { expr = te,
                         ribId = rid }
            end
                
    in
        Ast.Cls {name=name,
                 typeParams=typeParams,
                 nonnullable=nonnullable,
                 dynamic=dynamic,
                 extends=extendsTy,
                 implements=implementsTys,
                 classRib=classRib,
                 instanceRib=instanceRib1,
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

and multinameOf (n:Ast.NAME) =
    { nss = [[(#ns n)]], id = (#id n) }

and resolveExtends (env:ENV)
                   (currInstanceRib:Ast.RIB)
                   (extends:Ast.TYPE_EXPR option)
                   (currName:Ast.NAME) 
    : (Ast.TY option * Ast.RIB) =
    let
        val baseClassMultiname:Ast.MULTINAME option = 
            case extends of 
                NONE => if currName = Name.nons_Object
                        then NONE
                        else SOME (multinameOf Name.nons_Object)
              | SOME te => SOME (identExprToMultiname env (extractIdentExprFromTypeName te))

            
    in
        case baseClassMultiname of
            NONE => (NONE, currInstanceRib)
          | SOME bcm => 
            let
                val (baseClassName:Ast.NAME, 
                     baseClassFixture:Ast.FIXTURE) = 
                    resolve env bcm
            in
                (SOME (classInstanceType baseClassFixture),
                 inheritRib (classInstanceRib baseClassFixture) currInstanceRib)                 
            end
    end

and resolveImplements (env: ENV)
                      (instanceRib: Ast.RIB)
                      (implements: Ast.TYPE_EXPR list)
    : (Ast.TY list * Ast.RIB) =
    let
        val (superInterfaces, abstractRib) = resolveInterfaces env implements
        val _ = implementFixtures abstractRib instanceRib
    in
        (superInterfaces,instanceRib)
    end

(*
    Resolve each of the expressions in the 'implements' list to an interface
    fixture. check that each of the methods declared by each interface is
    implemented by the current set of instance ribs.

    Steps
    - resolve super interface references to interface fixtures
    - inherit ribs from the super interfaces

    Errors
    - super interface fixture not found

*)

and interfaceMethods (ifxtr:Ast.FIXTURE)
    : Ast.RIB =
    case ifxtr of
        Ast.InterfaceFixture (Ast.Iface {instanceRib,...}) => instanceRib
      |_ => LogErr.internalError ["interfaceMethods"]

and interfaceExtends (ifxtr:Ast.FIXTURE)
    : Ast.TY list =
    case ifxtr of
        Ast.InterfaceFixture (Ast.Iface {extends,...}) => extends
      |_ => LogErr.internalError ["interfaceExtends"]

and interfaceInstanceType (ifxtr:Ast.FIXTURE)
    : Ast.TY =
    case ifxtr of
        Ast.InterfaceFixture (Ast.Iface {instanceType,...}) => instanceType
      |_ => LogErr.internalError ["interfaceInstanceType"]

and classInstanceRib (cfxtr:Ast.FIXTURE)
    : Ast.RIB =
    case cfxtr of
        Ast.ClassFixture (Ast.Cls {instanceRib,...}) => instanceRib
      |_ => LogErr.internalError ["classInstanceRib"]

and classInstanceType (cfxtr:Ast.FIXTURE)
    : Ast.TY =
    case cfxtr of
        Ast.ClassFixture (Ast.Cls {instanceType,...}) => instanceType
      |_ => LogErr.internalError ["classInstanceType"]

(*
    resolve a list of interface names to their super interfaces and
    method ribs

    interface I { function m() }
    interface J extends I { function n() }  // [I],[m]
    interface K extends J {}  // [I,J] [m,n]

*)

(* FIXME: for the time being we're only going to handle inheriting from 
 * TYPE_EXPRs of a simple form: those which name a 0-parameter interface. 
 * Generalize later. 
 *)
and extractIdentExprFromTypeName (Ast.TypeName ie) : Ast.IDENT_EXPR = ie
  | extractIdentExprFromTypeName _ = 
    error ["can only presently handle inheriting from simple named interfaces"]

and resolveInterfaces (env: ENV)
                      (exprs: Ast.TYPE_EXPR list)
    : (Ast.TY list * Ast.RIB) =
    case exprs of        
        [] => ([],[])
      | _ =>
        let
            val mnames:Ast.MULTINAME list = map ((identExprToMultiname env) o extractIdentExprFromTypeName) exprs
            val fixs = (map (resolve env) mnames)
            (* val _ = List.map (fn (n,f) => Fixture.printFixture (Ast.PropName n, f)) fixs *)
            val (_, ifaces) = ListPair.unzip fixs
            val ifaceInstanceTypes:Ast.TY list = map interfaceInstanceType ifaces
            val superIfaceTypes:Ast.TY list = List.concat (map interfaceExtends ifaces)
            val methodRib:Ast.RIB = List.concat (map interfaceMethods ifaces)
        in
            (ifaceInstanceTypes @ superIfaceTypes, methodRib)
        end

(*
    analyzeClassBody

    The parser has already turned the class body into a block statement with init
    statements for setting the value of static and prototype fixtures. This class
    definition has a body whose only interesting statements left are init statements
    that set the value of instance vars.
*)

and analyzeClassBody (env:ENV)
                     (cdef:Ast.CLASS_DEFN)
    : Ast.CLS =
    let
        val {ns, ident, nonnullable, dynamic, params,                  
             classDefns, 
             instanceDefns, instanceStmts, 
             ctorDefn,  ...} = cdef
                               
        (*
         * Partition the class definition into a class block
         * and an instance block, and then define both blocks.
         *)

        val ns = resolveExprOptToNamespace env ns
        val name = {id=ident, ns=ns}
        val env = enterClass env name
        val _ = trace ["defining class ", LogErr.name name]

        val staticEnv = extendEnvironment env []
        val (unhoisted,classRib,classInits) = defDefns staticEnv classDefns
        val classRib = (mergeRibs (#program env) unhoisted classRib)

        (* namespace and type definitions aren't normally hoisted *)

        val instanceEnv = extendEnvironment staticEnv []

        (* FIXME: there is some debate about whether type parameters live in the 
         * instance rib or the class rib. This needs to be resolved. 
         *)
        val (typeParamRib:Ast.RIB) = mkParamRib params
        val _ = addToRib instanceEnv typeParamRib

        val (unhoisted,instanceRib,_) = defDefns instanceEnv instanceDefns

        val ctor:Ast.CTOR option =
            case ctorDefn of
                NONE => NONE
              | SOME c => SOME (defCtor instanceEnv c)

        val (instanceStmts,_) = defStmts staticEnv instanceStmts  (* no hoisted fixture produced *)
            
        (*
         * The parser separates variable definitions into defns and stmts. The only stmts
         * in this case will be InitStmts and what we really want is INITs so we translate
         * to to INITs here.
         *)

        fun initsFromStmt stmt =
            case stmt of
                Ast.ExprStmt (Ast.InitExpr (target,Ast.Head (temp_fxtrs,temp_inits),inits)) 
                => (temp_fxtrs,temp_inits@inits)
              | _ => LogErr.unimplError ["Defn.bindingsFromStmt"]

        val (fxtrs,inits) = ListPair.unzip(map initsFromStmt instanceStmts)
        val instanceInits = Ast.Head (List.concat fxtrs, List.concat inits)

        val instanceType = 
            makeTy env (Ast.InstanceType 
                            { name = name,
                              nonnullable = nonnullable,
                              typeParams = params,
                              typeArgs = [],
                              superTypes = [], (* set in resolveClassInheritence *)
                              ty = Ast.SpecialType Ast.Any,  (* FIXME needs synthetic record type *)
                              dynamic = dynamic})
    in
        Ast.Cls { name=name,
                  typeParams = params,
                  nonnullable = nonnullable,
                  dynamic = dynamic,
                  extends = NONE,
                  implements = [],
                  classRib = classRib,
                  instanceRib = instanceRib,
                  instanceInits = instanceInits,
                  constructor = ctor,
                  classType = makeTy env (Ast.ObjectType []),
                  instanceType = instanceType }
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
            val ty':Ast.TY = defTy env ty
            val readOnly' = case kind of
                                 Ast.Const => true
                               | Ast.LetConst => true
                               | _ => false
            val offset = (#tempOffset env)
            val name' = fixtureNameFromPropIdent env (SOME ns) ident offset
        in
            (name', Ast.ValFixture {ty=ty',readOnly=readOnly'})
        end

(*
    (BINDING list * INIT_STEP list) -> (RIB * INITS)

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

and fixtureNameFromPropIdent (env:ENV) (ns:Ast.NAMESPACE option) (ident:Ast.BINDING_IDENT) (tempOffset:int)
    : Ast.FIXTURE_NAME =
    case ident of
        Ast.TempIdent n =>  Ast.TempName (n+tempOffset)
      | Ast.ParamIdent n => Ast.TempName n
      | Ast.PropIdent id =>
        let
        in case ns of
            SOME ns => Ast.PropName {ns=ns,id=id}
          | _ =>
                let
                    val mname = identExprToMultiname env (Ast.Identifier {ident=id,openNamespaces=[]})
                    val ({ns,id},f) = resolve env mname
                in
                    Ast.PropName {ns=ns,id=id}
                end
        end

and defInitStep (env:ENV)
                (ns:Ast.NAMESPACE option)
                (step:Ast.INIT_STEP)
    : (Ast.FIXTURE_NAME * Ast.EXPR) =
    let
    in case step of
        Ast.InitStep (ident,expr) =>
            let
                val tempOffset = (#tempOffset env)
                val name = fixtureNameFromPropIdent env ns ident tempOffset
                val expr = defExpr env expr
            in
                (name,expr)
            end
      | Ast.AssignStep (left,right) => (* resolve lhs to fixture name, and rhs to expr *)
            let
                val ie = case left of Ast.LexicalRef {ident,...} => ident
                                    | _ => error ["invalid lhs in InitStep"]
                val mname = identExprToMultiname env ie
                val ({ns,id},f) = resolve env mname
            in
                (Ast.PropName {ns=ns,id=id}, defExpr env right)
            end
    end

and defBindings (env:ENV)
                (kind:Ast.VAR_DEFN_TAG)
                (ns:Ast.NAMESPACE)
                ((binds,inits):Ast.BINDINGS)
    : (Ast.RIB * Ast.INITS) =
    let
        val fxtrs:Ast.RIB = map (defVar env kind ns) binds
        val inits:Ast.INITS = map (defInitStep env (SOME ns)) inits
    in
        (fxtrs,inits)
    end

and defSettings (env:ENV)
                ((binds,inits):Ast.BINDINGS)
    : (Ast.RIB * Ast.INITS) =
    let
        val _ = trace [">> defSettings"]
        val fxtrs:Ast.RIB = map (defVar env Ast.Var Name.noNS) binds
        val inits:Ast.INITS = map (defInitStep env NONE) inits   (* FIXME: lookup ident in open namespaces *)
        val _ = trace ["<< defSettings"]
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
        rib
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
    : (Ast.RIB * Ast.INITS * Ast.EXPR list * Ast.RIB * Ast.INITS * Ast.EXPR list * Ast.TYPE_EXPR) =

    case fsig of
        Ast.FunctionSignature { typeParams, params, paramTypes, defaults, ctorInits,
                                returnType, thisType, hasRest } =>
        let

(**** FIXME

            (* compute typeval fixtures (type parameters) *)
            fun mkTypeVarFixture x = (Ast.PropName {ns=Name.internalNS, id=x},
                                      Ast.TypeVarFixture)

            val typeParamFixtures = map mkTypeVarFixture typeParams
            val typeEnv = extendEnvironment env typeParamFixtures 0
****)

            val thisType:Ast.TYPE_EXPR = 
                case thisType of 
                    NONE => Ast.SpecialType Ast.Any
                  | SOME x => defTypeExpr env x

            fun isTempFixture (n:Ast.FIXTURE_NAME, _) : bool =
                case n of
                   Ast.TempName _ => true
                 | _ => false


            val (paramRib,paramInits) = defBindings env Ast.Var (Name.noNS) params
            val ((settingsRib,settingsInits),superArgs) =
                    case ctorInits of
                        SOME (settings,args) => (defSettings env settings, defExprs env args)
                      | NONE => (([],[]),[])
            val settingsRib = List.filter isTempFixture settingsRib
        in
            (paramRib,
             paramInits,
             defaults,
             settingsRib,
             settingsInits,
             superArgs,
             thisType)
        end

(*
    FUNC

    The activation frame of a function is described by a BLOCK. The ribs
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
             
and defFunc (env:ENV) 
            (func:Ast.FUNC)
    : (Ast.HEAD * Ast.EXPR list * Ast.FUNC) = (* (settings, superArgs, func) *)    
    let
        val _ = trace [">> defFunc"]
        val Ast.Func {name, fsig, block, ty, native, loc, ...} = func
        val Ast.Ty { expr, ... } = ty 
        fun findFuncType env e = 
            case e of 
                Ast.LamType { params, body } => 
                findFuncType (extendEnvironment env (mkParamRib params)) body 
              | Ast.FunctionType fty => (env, fty)
              | _ => error ["unexpected primary type in function: ", LogErr.ty e]

        val newT = defTyFromTy env ty
        val (env, funcType) = findFuncType env expr                     
        val numParams = length (#params funcType)
        val env = updateTempOffset env numParams
        val (paramRib, paramInits, defaults, settingsRib, settingsInits, superArgs, thisType) = defFuncSig env fsig
        val defaults = defExprs env defaults
        val env = extendEnvironment env paramRib
        val env = enterFuncBody env func
        val (blockOpt:Ast.BLOCK option, hoisted:Ast.RIB) = 
            case block of 
                NONE => (NONE, [])
              | SOME b => 
                let 
                    val (block:Ast.BLOCK, hoisted:Ast.RIB) = defBlock env b
                in
                    (SOME block, hoisted)
                end
        val _ = trace ["<< defFunc"]
    in
        (Ast.Head (settingsRib, settingsInits), superArgs,
         Ast.Func {name = name,
                   fsig = fsig,
                   block = blockOpt,
                   defaults = defaults,
                   ty = newT,
                   param = Ast.Head (mergeRibs (#program env) paramRib hoisted, 
                                     paramInits),
                   native=native,
                   loc=loc})
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
    : Ast.RIB =
    case (#func f) of
        Ast.Func { name, fsig, block, ty, native, ... } =>
        let
            val qualNs = resolveExprOptToNamespace env (#ns f)
            val newName = Ast.PropName { id = (#ident name), ns = qualNs }
            val env = clearPackageName env
            val (_, _, newFunc) = defFunc env (#func f)
            val Ast.Func { ty, ... } = newFunc

            val fixture =
                case (#kind name) of
                    Ast.Get =>
                    Ast.VirtualValFixture
                        { ty = AstQuery.resultTyOfFuncTy ty,
                          getter = SOME newFunc,
                          setter = NONE }
                  | Ast.Set =>
                    Ast.VirtualValFixture
                        { ty = AstQuery.singleParamTyOfFuncTy ty,
                          getter = NONE,
                          setter = SOME newFunc }
                  | Ast.Ordinary =>
                    let
                        val (ftype, isReadOnly) =
                            if (#kind f) = Ast.Var                            
                            then  
                                (* e3 style writeable function *)
                                (makeTy env (Ast.SpecialType Ast.Any), false)  
                            else 
                                (* read only, method *)
                                (ty, true)                        
                    in
                        Ast.MethodFixture
                            { func = newFunc,
                              ty = ftype,
                              readOnly = isReadOnly,
                              final = (#final f),
                              override = (#override f)}
                    end
                  | Ast.Call =>
                    Ast.MethodFixture
                        { func = newFunc,
                          ty = ty,
                          readOnly = true,
                          final = true,
                          override = false}
                  | Ast.Has =>
                    Ast.MethodFixture
                        { func = newFunc,
                          ty = ty,
                          readOnly = true,
                          final = true,
                          override = false}
                  | Ast.Operator =>
                    LogErr.unimplError ["operator function not implemented"]
        in
            [(newName, fixture)]
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

    Some effects of pragmas are propagated to the expressions they modify via a
    Context value. Interpret each pragma and initialise a new env using the results
*)

and defPragmas (env:ENV)
               (pragmas:Ast.PRAGMA list)
    : (Ast.PRAGMA list * ENV * Ast.RIB) =
    let
        val program = #program env
        val ribId  = #ribId env
        val defaultNamespace = ref (#defaultNamespace env)
        val opennss = ref []
        val rib = ref []
        val tempOffset = #tempOffset env

        fun modifiedEnv _ = 
            { ribId = ribId,
              tempOffset = tempOffset,
              openNamespaces = (case !opennss of
                                    [] => (#openNamespaces env)   (* if opennss is empty, don't concat *)
                                  | _  => !opennss :: (#openNamespaces env)),
              labels = (#labels env),
              className = (#className env),
              packageName = (#packageName env),
              defaultNamespace = !defaultNamespace,
              program = (#program env),
              func = (#func env) }

        fun defPragma x =
            case x of
                Ast.UseNamespace ns =>
                    let
                        val env = modifiedEnv()
                        val ns = defExpr env ns
                        val namespace = resolveExprToNamespace env ns
                    in
                        opennss := addNamespace namespace (!opennss);
                        Ast.UseNamespace ns
                    end
              | Ast.UseDefaultNamespace ns =>
                    let
                        val env = modifiedEnv()
                        val ns = defExpr env ns
                        val namespace = resolveExprToNamespace env ns
                        val _ = trace ["use default namespace ",LogErr.name {ns=namespace,id=Ustring.empty}]
                    in
                        (case namespace of
                             (* these ones are already open *)
                             (Ast.Public _) => ()
                           | (Ast.Protected _) => ()
                           | (Ast.Private _) => ()
                           | (Ast.Internal _) => ()
                           | _ => opennss := (namespace :: !opennss));
                        defaultNamespace := namespace;
                        Ast.UseDefaultNamespace ns
                    end
              | Ast.Import {package,name} =>
                let
                    val id = packageIdentFromPath package
                    val ns = if name = Ustring.asterisk
                             then Ast.Public id
                             else Ast.LimitedNamespace (name,Ast.Public id)
                in
                    Fixture.addPackageName program package;
                    trace2 ("openning package ",id);
                    opennss  := addNamespace ns (!opennss);
                    Ast.Import {package=package, name=name}
                end

              | p => p

        val newPragmas = map defPragma pragmas                         
    in
        (newPragmas, modifiedEnv (), !rib)
    end

(*
    IDENT_EXPR
*)

and defIdentExpr (env:ENV)
                 (ie:Ast.IDENT_EXPR)
    : Ast.IDENT_EXPR =
    let
        val openNamespaces = (#openNamespaces env)
    in
        case ie of
            Ast.Identifier { ident, ... } =>
            Ast.Identifier { ident=ident,
                             openNamespaces=openNamespaces }

          | Ast.AttributeIdentifier ai =>
            Ast.AttributeIdentifier (defIdentExpr env ai)

          | Ast.QualifiedIdentifier { qual, ident } =>
            Ast.QualifiedIdentifier { qual = defExpr env qual,
                                      ident = ident }

          | Ast.QualifiedExpression { qual, expr } =>
            Ast.QualifiedExpression { qual = defExpr env qual,
                                      expr = defExpr env expr }
          | Ast.ExpressionIdentifier { expr, ...} =>
            Ast.ExpressionIdentifier { expr = (defExpr env expr),
                                       openNamespaces = openNamespaces }
          | Ast.WildcardIdentifier =>
            Ast.WildcardIdentifier

          | Ast.UnresolvedPath (p,i) =>
            LogErr.internalError ["unresolved UnresolvedPath ", Ustring.toAscii (hd p)]


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

          | Ast.LiteralArray {exprs, ty} =>
            Ast.LiteralArray {exprs = defExpr env exprs,
                              ty = case ty of
                                       NONE => NONE
                                     | SOME t => SOME (defTyFromTy env t) }
          | Ast.LiteralXML exprs =>
            Ast.LiteralXML (defExprs env exprs)

          | Ast.LiteralObject {expr, ty} =>
            Ast.LiteralObject {expr = List.map (fn { kind, name, init } =>
                                                   { kind = kind,
                                                     name = defIdentExpr env name,
                                                     init = defExpr env init }) expr,
                               ty = case ty of
                                        NONE => NONE
                                      | SOME t => SOME (defTyFromTy env t) }

          | Ast.LiteralNamespace ns =>
            Ast.LiteralNamespace (defNamespace env ns)

          | _ => lit   (* FIXME: other cases to handle here *)
    end

(*
    Resolve a path in an environment

    For each context in the environment: if there is a fixture that matches the
    head of the path, then resolve the path as an object path; if some prefix of
    the path matches a package name introduced in that context, then resolve the
    prefix as a package qualifier and the rest of the path as an object path,
    the head (if any) is qualified by the package qualifier.

    If no context has a matching fixture or package name, then resolve the path as
    as an object reference.
*)

and resolvePath (env:ENV) 
                (path:Ast.IDENT list)
    : Ast.EXPR =
    let
val _ = dumpPath path

        val (pkg,pth) = matchPackageName env path
val _ = trace2 ("resolvePath ",case pkg of NONE => Ustring.fromString "NONE" | _ => valOf pkg)
    in case (pkg,pth) of
             (SOME pk,[]) => Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Public pk))
           | (SOME pk,id::ids) =>
                let
                    val expr = Ast.LexicalRef
                                    {ident=Ast.QualifiedIdentifier
                                        {qual=Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Public pk)),
                                         ident=id},
                                     loc=NONE}
                in
                    resolveObjectPath env ids (SOME expr)
                end
           | (NONE,_) => resolveObjectPath env path NONE
    end

(*
    Return the package qualifier, if any, and the remaining path
*)

and getFullRibs (env:ENV)
    : Ast.RIBS = 
    let
        val (ribs, closed) = Fixture.getRibs (#program env) (#ribId env)
    in
        ribs
    end

and matchPackageName (env:ENV)
                     (path:Ast.IDENT list)
    : Ast.IDENT option * Ast.IDENT list =
    let
        val mname = 
            identExprToMultiname 
                env 
                (Ast.Identifier {ident=(hd path),openNamespaces=[]})
    in
        case Multiname.resolveInRibs mname (getFullRibs env) of
            SOME (_, n) => (NONE,path) (* head of path matches fixture *)
          | NONE => 
            (* head of path does not match fixture, *)
            (* try finding a package name that matches prefix of path *)
            let
                val { program, ... } = env
            in 
                case pathInPackageNames (Fixture.getPackageNames program) path of
                    (NONE,_) => (NONE, path)
                  | (SOME pkg, rest) => (SOME pkg, rest)
            end
    end

and pathInPackageNames (packageNames: Ast.IDENT list list) (path:Ast.IDENT list)
    : Ast.IDENT option * Ast.IDENT list =
    let
        fun matchPackage (package:Ast.IDENT list) (path:Ast.IDENT list) (prefix:Ast.IDENT list)
            : Ast.IDENT option * Ast.IDENT list =
            case (package,path,prefix) of
                ([],_,[]) =>
                    (NONE,path)
              | ([],_,_) =>
                    (SOME (packageIdentFromPath (List.rev prefix)),path)
              | (pkgid::pkg,pthid::pth,_) =>
                    if pkgid = pthid
                    then matchPackage pkg pth (pkgid::prefix)
                    else (NONE,path)
          | (_,[],_) => error ["resolving package portion of empty path"];

    in case packageNames of
        [] => (NONE,path)  (* no match *)
      | pkg::pkgs =>
        let
            val (ident,path) = matchPackage pkg path []
        in case ident of
            NONE => pathInPackageNames pkgs path (* no match yet, try again *)
          | _ => (ident,path) (* match, return the remainder of the path *)
        end
    end

and resolveObjectPath (env:ENV) (path:Ast.IDENT list) (expr:Ast.EXPR option)
    : Ast.EXPR =
    let
    in case (expr, path) of
        (NONE, ident::identList) =>
            let
                val base = Ast.LexicalRef {ident=Ast.Identifier {ident=ident,openNamespaces=[]},loc=NONE}
            in
                resolveObjectPath env identList (SOME base)
            end
      | (SOME expr, ident::identList) =>
            let
                val base = Ast.ObjectRef {base=expr, ident=Ast.Identifier {ident=ident,openNamespaces=[]},loc=NONE}
            in
                resolveObjectPath env identList (SOME base)
            end
      | (SOME expr, []) =>
            let
            in
                expr
            end
      | _ => LogErr.internalError ["unhandled case in resolveObjectPath"]
    end


and defExpr (env:ENV)
            (expr:Ast.EXPR)
    : Ast.EXPR =
    let
        fun sub e = defExpr env e
    in
        case expr of
            Ast.TernaryExpr (e1, e2, e3) =>
            Ast.TernaryExpr (sub e1, sub e2, sub e3)

          | Ast.BinaryExpr (b, e1, e2) =>
            Ast.BinaryExpr (b, sub e1, sub e2)

          | Ast.BinaryTypeExpr (b, e, ty) =>
            Ast.BinaryTypeExpr (b, sub e, defTyFromTy env ty)

          | Ast.UnaryExpr (u, e) =>
            Ast.UnaryExpr (u, sub e)

          | Ast.TypeExpr t =>
            Ast.TypeExpr (defTyFromTy env t)

          | Ast.ThisExpr k =>
            (case k of
                 SOME Ast.FunctionThis =>
                 (case (#func env) of
                      SOME (Ast.Func { name = { kind, ident }, ...}) => 
                      (case kind of
                           Ast.Get => error ["'this function' not allowed in getter"]
                         | Ast.Set => error ["'this function' not allowed in setter"]
                         | _ => Ast.ThisExpr k)
                    | _ => error ["'this function' not allowed in non-function context"])
               | _ => Ast.ThisExpr k)

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
                                actuals = map (defTyFromTy env) actuals }

          | Ast.LetExpr { defs, body,... } =>
            let
                val (f,i)   = defBindings env Ast.Var Name.noNS defs
                val env     = extendEnvironment env f
                val newBody = defExpr env body
            in
                Ast.LetExpr { defs = defs,
                              body = newBody,
                              head = SOME (Ast.Head (f,i)) }
            end

          | Ast.NewExpr { obj, actuals } =>
            Ast.NewExpr { obj = sub obj,
                          actuals = map sub actuals }

          | Ast.ObjectRef { base, ident, loc } =>
            (LogErr.setLoc loc;
             Ast.ObjectRef { base = sub base,
                             ident = defIdentExpr env ident,
                             loc = loc })

          | Ast.LexicalRef { ident, loc } =>
            let
                val _ = LogErr.setLoc loc
            in
                case ident of
                    Ast.UnresolvedPath (p,i) =>
                    let
                        val base = resolvePath env p
                    in case (base,i) of
                           (Ast.LiteralExpr _,Ast.Identifier {ident=id,...}) =>
                               Ast.LexicalRef {ident=Ast.QualifiedIdentifier {qual=(defExpr env base),
                                                                              ident=id},
                                               loc=loc}
                         | (Ast.LiteralExpr _,_) =>
                           LogErr.defnError ["invalid package qualification"]

                         | (_,_) =>
                           Ast.ObjectRef { base=(defExpr env base),
                                           ident=(defIdentExpr env i),
                                           loc=loc}
                    end
                  | _ =>
                    Ast.LexicalRef { ident = defIdentExpr env ident,
                                     loc = loc}
            end

          | Ast.SetExpr (a, le, re) =>
            Ast.SetExpr (a, (sub le), (sub re))

          | Ast.GetTemp n =>
            let
                val tempOffset = (#tempOffset env)
            in
                Ast.GetTemp (n+tempOffset)
            end

          | Ast.GetParam n =>
            Ast.GetTemp n

          | Ast.ListExpr es =>
            Ast.ListExpr (map sub es)

          | Ast.InitExpr ie =>
            Ast.InitExpr ie
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
        let
            val {params,result,thisType,hasRest,minArgs} = ty
            val params' = map (defTypeExpr env) params
            val thisType' = case thisType of 
                                SOME ty => SOME (defTypeExpr env ty) 
                              | _ => NONE
            val result' = defTypeExpr env result
        in
            {params=params',
             result=result',
             thisType=thisType',
             hasRest=hasRest,
             minArgs=minArgs}
        end


and defTypeExpr (env:ENV)
                (typeExpr:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR =
    case typeExpr of
        Ast.FunctionType t =>
        Ast.FunctionType (defFuncTy env t)
      | Ast.TypeName n =>
        let
        in
            case n of
                Ast.UnresolvedPath (p,i) =>
                let
                    val base = resolvePath env p
                in case (base,i) of
                   (Ast.LiteralExpr _,Ast.Identifier {ident=id,...}) =>
                   Ast.TypeName (Ast.QualifiedIdentifier {qual=(defExpr env base),
                                                          ident=id})
                 | (_,_) =>
                       LogErr.defnError ["invalid type expr ", Ustring.toAscii (hd p)]
                end
              | _ =>
                Ast.TypeName (defIdentExpr env n)
         end
      | Ast.UnionType tys =>
        Ast.UnionType (map (defTypeExpr env) tys)
      | Ast.ArrayType tys =>
        Ast.ArrayType (map (defTypeExpr env) tys)
      | Ast.ObjectType tys =>
        Ast.ObjectType (map (defFieldType env) tys)
      | Ast.LikeType ty => 
        Ast.LikeType (defTypeExpr env ty)
      | Ast.NullableType { expr, nullable } =>
        Ast.NullableType { expr = defTypeExpr env expr,
                           nullable = nullable }

      | Ast.ElementTypeRef (ty, n) =>
        Ast.ElementTypeRef (defTypeExpr env ty, n)

      | Ast.FieldTypeRef (ty, ident) =>
        Ast.FieldTypeRef (defTypeExpr env ty, ident)

      | Ast.AppType { base, args } => 
        Ast.AppType { base = defTypeExpr env base,
                      args = map (defTypeExpr env) args }
      | Ast.LamType { params, body } => 
        Ast.LamType { params = params,
                      body = defTypeExpr env body }

      (* FIXME *)
      | t => t


and defTy (env:ENV)
          (typeExpr:Ast.TYPE_EXPR)
    : Ast.TY = 
    makeTy env (defTypeExpr env typeExpr)


and defTyFromTy (env:ENV)
                (ty:Ast.TY)
    : Ast.TY =
    let
        val Ast.Ty { expr, ... } = ty
    in
        defTy env expr
    end


and defFieldType (env:ENV)
              (ty:Ast.FIELD_TYPE)
    : Ast.FIELD_TYPE =
    let
        val {name,ty} = ty
        val ty = defTypeExpr env ty
    in
        {name=name,ty=ty}
    end


(*
    STMT

    Define a statement and return the elaborated statement and a list
    of hoisted rib.
*)

and defStmt (env:ENV)
            (labelIds:Ast.IDENT list)
            (stmt:Ast.STMT)
    : (Ast.STMT * Ast.RIB) =
    let
        fun defForEnumStmt env (fe:Ast.FOR_ENUM_STMT) =
            let
                fun defVarDefnOpt vd =
                    case vd of
                        SOME vd => defDefn env (Ast.VariableDefn vd)
                      | NONE => ([],[],[])
            in
            case fe of
                { isEach, obj, defn, labels, body, next, ... } =>
                let
                    val newObj =  defExpr env obj
                    val (ur1,hr1,i1) = defVarDefnOpt defn
                    val env = extendEnvironment env ur1
                    val (newBody,hoisted) = defStmt env [] body
                    val tempEnv = updateTempOffset env 1   (* alloc temp for iteration value *)
                    val (newNext,_) = defStmt tempEnv [] next
                in
                    (Ast.ForInStmt { isEach=isEach,
                                     obj = newObj,
                                     defn = defn,
                                     labels = Ustring.empty::labelIds,
                                     body = newBody,
                                     rib = SOME ur1,
                                     next = newNext },
                     mergeRibs (#program env) hr1 hoisted)
                end
            end

        fun makeIterationLabel id = (id,IterationLabel)
        fun makeStatementLabel id = (id,StatementLabel)
        fun makeSwitchLabel id = (id,SwitchLabel)

        fun defWhileStmt (env) (w:Ast.WHILE_STMT) =
            case w of
                { cond, body, labels, rib } => (* FIXME: inits needed *)
                let
                    val newCond = defExpr env cond
                    val (newBody, hoisted) = defStmt env [] body
                in
                    ({ cond=newCond,
                       rib=NONE,
                       body=newBody,
                       labels=Ustring.empty::labelIds}, hoisted)
                end

        (*
            for ( var x = 10; x > 0; --x ) ...
            for ( x=10; x > 0; --x ) ...
        *)

        fun defForStmt env { defn, init, cond, update, labels, body, rib } =
            let
                fun defVarDefnOpt vd =
                    case vd of
                        SOME vd => defDefn env (Ast.VariableDefn vd)
                      | NONE => ([],[],[])
                val (ur,hr,_) = defVarDefnOpt defn
                val env = extendEnvironment env (mergeRibs (#program env) ur hr)
                val (newInit,_) = defStmts env init
                val newCond = defExpr env cond
                val newUpdate = defExpr env update
                val (newBody, hoisted) = defStmt env [] body
            in
                trace ["<< reconstructForStmt"];
                ( Ast.ForStmt { defn = defn,
                                init = newInit,
                                cond = newCond,
                                update = newUpdate,
                                labels = Ustring.empty::labelIds,
                                body = newBody,
                                rib = SOME (ur) },
                  (mergeRibs (#program env) hr hoisted) )
            end

        fun reconstructCatch { bindings, rib, inits, block, ty } =
            let
                val ty:Ast.TY = defTyFromTy env ty
                val (r0,i0) = defBindings env Ast.Var Name.noNS bindings
                val env = extendEnvironment env r0
                val (block:Ast.BLOCK, rib:Ast.RIB) = defBlock env block
            in
                ({ bindings = bindings,   (* FIXME: what about inits *)
                  block = block,
                  rib = SOME r0,
                  inits = SOME i0,
                  ty=ty }, rib)
            end

        fun defCase env { label, body, inits }
            : Ast.CASE * Ast.RIB =
            let
                val (body:Ast.BLOCK, hoisted) = defBlock env body
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
                hoisted)
            end

        fun findClass (n:Ast.NAME) =
            let
                val (n,f) = resolve env (multinameFromName n)
            in case f of
                   Ast.ClassFixture cd => cd
                 | _ => LogErr.defnError ["reference to non-class fixture"]
            end
            
        fun reconstructClassBlock {ns, ident:Ast.IDENT, block, name:Ast.NAME option } =
            let
                val _ = trace2 ("reconstructing class block for ", ident)
                val Ast.Block { pragmas, defns, head, body, loc } = block

                (* filter out instance initializers *)
                val (_,stmts) = List.partition isInstanceInit body

                val namespace = resolveExprOptToNamespace env ns
                val name = {ns=namespace, id=ident}

                val env = enterClass env name (* only top level package defns use package specific namespaces *)

                val (block,hoisted) = defBlock env (Ast.Block {pragmas=pragmas,
                                                               defns=defns,
                                                               head=head,
                                                               body=body,
                                                               loc=loc})
            in
                (Ast.ClassBlock { ns = ns,
                                 ident = ident,
                                 name = SOME name,
                                 block = block }, hoisted)
            end


        fun checkLabel labelIdOpt labelKnd =
            let
                val labelId = case labelIdOpt of NONE => Ustring.empty | SOME i => i
                val _ = trace2 ("checkLabel ",labelId)
                val { labels, ... } = env
            in
                dumpLabels labels;
                 if List.exists (fn (id,knd) =>
                                    id = labelId andalso    (* compare ids *)
                                    knd = labelKnd) labels  (* and kinds *)
                 then true
                 else false
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
                               | (Ast.Const,_,_) => Ast.Hoisted
                               | _ => Ast.Local
                val temps = defBindings env kind ns0 temps  (* ISSUE: kind and ns are irrelevant *)
            in
                (Ast.ExprStmt (Ast.InitExpr (target, Ast.Head temps, (map (defInitStep env (SOME ns0)) inits))),[])
            end

          | Ast.ForInStmt fe =>
            let
                val env' = addLabels env (map makeIterationLabel labelIds)
                val env'' = addLabel ((makeIterationLabel Ustring.empty), env')
            in
                defForEnumStmt env'' fe
            end

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
                val env' = addLabel ((makeStatementLabel id), env)
                val (s',f') = defStmt env' (id::labelIds) s
            in
                (Ast.LabeledStmt (id,s'),f')
            end

          | Ast.LetStmt b =>
            inl (Ast.LetStmt) (defBlock env b)

          | Ast.WhileStmt w =>
            let
                val env' = addLabels env (map makeIterationLabel labelIds)
                val env'' = addLabel ((makeIterationLabel Ustring.empty), env')
            in
                inl (Ast.WhileStmt) (defWhileStmt env'' w)
            end

          | Ast.DoWhileStmt w =>
            let
                val env' = addLabels env (map makeIterationLabel labelIds);
                val env'' = addLabel ((makeIterationLabel Ustring.empty), env')
            in
                inl (Ast.DoWhileStmt) (defWhileStmt env'' w)
            end

          | Ast.ForStmt f =>
            let
                val env' = addLabels env (map makeIterationLabel labelIds);
                val env'' = addLabel ((makeIterationLabel Ustring.empty), env')
            in
                defForStmt env'' f
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
                 mergeRibs (#program env) thn_hoisted els_hoisted)
            end

          | Ast.WithStmt { obj, ty, body } =>
            let
                val (body,hoisted) = defStmt env [] body
            in
                (Ast.WithStmt { obj = (defExpr env obj),
                           ty = (defTyFromTy env ty),
                           body = body }, hoisted)
            end

          | Ast.TryStmt { block, catches, finally } =>
            let
                val (block,thoisted) = defBlock env block
                val (catches,choisted) = ListPair.unzip (map reconstructCatch catches)
                val (finally,fhoisted) = case finally of
                                   NONE =>
                                   (NONE,[])
                                 | SOME b =>
                                   let val (block,hoisted) = defBlock env b
                                   in (SOME block,hoisted) end
            in
                (Ast.TryStmt {block = block,
                              catches = catches,
                              finally = finally}, thoisted@(List.concat choisted)@fhoisted)
            end

          | Ast.SwitchStmt { cond, cases, ... } =>
            let
                val env' = addLabels env (map makeSwitchLabel labelIds);
                val env'' = addLabel ((makeSwitchLabel Ustring.empty), env')
                val (cases,hoisted) = ListPair.unzip (map (defCase env'') cases)
            in
                (Ast.SwitchStmt { cond = defExpr env cond,
                                  cases = cases,
                                  labels=Ustring.empty::labelIds}, List.concat hoisted)
            end

          | Ast.SwitchTypeStmt { cond, ty, cases } =>
            let
                val (cases,hoisted) = ListPair.unzip (map reconstructCatch cases)
            in
                (Ast.SwitchTypeStmt {cond = defExpr env cond,
                                     ty = defTyFromTy env ty,
                                     cases = cases}, List.concat hoisted)
            end
          | Ast.DXNStmt { expr } =>
            (Ast.DXNStmt { expr = defExpr env expr },[])
    end

and defStmts (env) (stmts:Ast.STMT list)
    : (Ast.STMT list * Ast.RIB) =
    case stmts of
        (stmt::stmts) =>
            let
                val (s1,f1):(Ast.STMT*Ast.RIB) = defStmt env [] stmt

                (* Class definitions are embedded in the ClassBlock so we
                   need to update the environment in that case *)

                val _ = addToRib env f1
                val (s2, f2) = defStmts env stmts
            in
                (s1::s2,(mergeRibs (#program env) f1 f2))
            end
      | [] => ([],[])

(*
    NAMESPACE_DEFN

    Translate a namespace definition into a namespace fixture. Namespaces
    are not hoisted. The initialiser is resolved at this time and so must
    be either a literal string or a reference to a previously defined
    namespace.
*)

and defNamespaceDefn (env:ENV)
                 (nd:Ast.NAMESPACE_DEFN)
    : (Ast.RIB * Ast.NAMESPACE_DEFN) =
    case nd of
        { ident, ns, init } =>
        let
            val _ = trace [">> defNamespaceDefn"]
            val qualNs = resolveExprOptToNamespace env ns
            val newNs = case init of
                            (* FIXME: a nonce perhaps? *)
                            NONE => Ast.UserNamespace ident
                          | SOME (Ast.LiteralExpr (Ast.LiteralString s)) =>
                            Ast.UserNamespace s
                          | SOME (Ast.LexicalRef lref) =>
                            resolveExprToNamespace env (Ast.LexicalRef lref)
                          | _ => LogErr.evalError ["illegal form of namespace initializer"]
            val fixtureName = Ast.PropName { ns = qualNs, id = ident }
            val newNd = { ident = ident,
                          ns = ns,
                          init = init }
        in
            ([(fixtureName, Ast.NamespaceFixture newNs)], newNd)
        end


and defType (env:ENV)
            (td:Ast.TYPE_DEFN)
    : Ast.RIB =
    let
        val { ident, ns, init } = td
        val ns = case ns of
                     NONE => (#defaultNamespace env)
                   | SOME e => resolveExprToNamespace env e
        val n = { id=ident, ns=ns }
    in
        [(Ast.PropName n,
          Ast.TypeFixture (defTy env init))]
    end


(*
    DEFN

    Translate a definition into two ribs (hoisted and unhoisted) and a
    (possibly empty) list of initialisers.
*)


and defDefn (env:ENV)
            (defn:Ast.DEFN)
    : (Ast.RIB * Ast.RIB * Ast.INITS) = (* unhoisted, hoisted, inits *)
    case defn of
        Ast.VariableDefn { kind, ns, static, prototype, bindings } =>
            let
                val _ = trace ["defVar"]
                val ns = resolveExprOptToNamespace env ns
                val (fxtrs,inits) = defBindings env kind ns bindings
            in case kind of
              (* hoisted fxtrs *)
                Ast.Var => ([],fxtrs,inits)
              | Ast.Const => ([],fxtrs,inits)
              (* unhoisted fxtrs *)
              | Ast.LetVar => (fxtrs,[],inits)
              | Ast.LetConst => (fxtrs,[],inits)
            end
      | Ast.FunctionDefn fd =>
            let
                val {kind,...} = fd
                val fxtrs = defFuncDefn env fd
            in case kind of
              (* hoisted fxtrs *)
                Ast.Var => ([],fxtrs,[])
              | Ast.Const => ([],fxtrs,[])
              (* unhoisted fxtrs *)
              | Ast.LetVar => (fxtrs,[],[])
              | Ast.LetConst => (fxtrs,[],[])
            end
      | Ast.NamespaceDefn nd =>
            let
                val _ = trace ["defNamespaceDefn"]
                val (hoisted,def) = defNamespaceDefn env nd
            in
                ([],hoisted,[])
            end
      | Ast.ClassDefn cd =>
            let
                val _ = trace ["defClass"]
                (* FIXME: we use classes inside statements *)
                val (hoisted,def) = defClass env cd
            in
                ([],hoisted,[])
            end

      | Ast.InterfaceDefn cd =>
            let
                val (hoisted,def) = defInterface env cd
            in
                ([],hoisted,[])
            end

      | Ast.TypeDefn td =>
        let
            val unhoisted = defType env td
        in
            (unhoisted, [], [])
        end

      | _ => LogErr.unimplError ["defDefn"]

(*
    DEFN list

    Process each definition.
*)

and defDefns (env:ENV)
             (defns:Ast.DEFN list)
    : (Ast.RIB * Ast.RIB * Ast.INITS) = (* unhoisted, hoisted, inits *)
    let
        fun loop (defns:Ast.DEFN list)
                 (unhoisted:Ast.RIB)
                 (hoisted:Ast.RIB)
                 (inits:Ast.INITS)
            : (Ast.RIB * Ast.RIB * Ast.INITS) = (* unhoisted, hoisted, inits *)
            case defns of 
                [] => (unhoisted, hoisted, inits)
              | d::ds => 
                let
                    val { program, ... } = env
                    val (unhoisted', hoisted', inits') = defDefn env d
                    val _ = if (!doTrace) 
                            then (trace ["defDefns: unhoisted: "]; Fixture.printRib unhoisted;
                                  trace ["defDefns: hoisted: "]; Fixture.printRib hoisted;
                                  trace ["defDefns: unhoisted': "]; Fixture.printRib unhoisted';
                                  trace ["defDefns: hoisted': "]; Fixture.printRib hoisted')
                            else ()

                    val _ = trace(["defDefns: combining unhoisted ribs"]);                    
                    val combinedUnHoisted = mergeRibs program unhoisted unhoisted'
                    val _ = trace(["defDefns: combining hoisted ribs"]);        
                    val combinedHoisted = mergeRibs program hoisted hoisted'
                    val _ = trace(["defDefns: combining inits"])
                    val combinedInits = inits @ inits'
                in
                    addToRib env unhoisted';
                    addToRib env hoisted';
                    loop ds combinedUnHoisted combinedHoisted combinedInits
                end
        val _ = trace([">> defDefns"])
        val res = loop defns [] [] []
        val _ = trace(["<< defDefns"])
    in
        res
    end    
        
(*
    BLOCK

    Initialise a Block's rib and initialisers fields. Pragmas and
    definitions are not used after this definition phase. Traverse the
    statements so that embedded blocks (e.g. block statements, let
    expressions) are initialised.

    Class blocks have an outer scope that contain the class (static)
    rib. When entering a class block, extend the environment with
    the class object and its base objects, in reverse order

*)

and defBlock (env:ENV)
             (b:Ast.BLOCK)
    : (Ast.BLOCK * Ast.RIB) =
    defBlockFull env b false

and defTopBlock (env:ENV)
                (b:Ast.BLOCK) = 
    defBlockFull env b true

and defBlockFull (env:ENV)
                 (b:Ast.BLOCK)
                 (top:bool)
    : (Ast.BLOCK * Ast.RIB) =
    let
        val Ast.Block { pragmas, defns, body, loc, ... } = b
        val _ = LogErr.setLoc loc
        val env = if top 
                  then env
                  else extendEnvironment env []
        val (pragmas, env, unhoisted_pragma_fxtrs) = defPragmas env pragmas
        val (unhoisted_defn_fxtrs, hoisted_defn_fxtrs, inits) = defDefns env defns
        val (body, hoisted_body_fxtrs) = defStmts env body
        val unhoisted = mergeRibs (#program env) 
                                  unhoisted_defn_fxtrs 
                                  unhoisted_pragma_fxtrs
        val hoisted = mergeRibs (#program env) hoisted_defn_fxtrs hoisted_body_fxtrs
        val rib = if top
                  then mergeRibs (#program env) unhoisted hoisted
                  else unhoisted
    in
        (Ast.Block { pragmas = pragmas,
                     defns = [],  (* clear definitions, we are done with them *)
                     body = body,
                     head = SOME (Ast.Head (rib, inits)),
                     loc = loc},
         hoisted)
    end

(*
  FRAGMENT
*)                          


and defFragment (env:ENV) 
                (frag:Ast.FRAGMENT)
    : Ast.FRAGMENT =
    let
        val env = enterFragment env frag
    in
        case frag of 
            Ast.Package { name, fragments } => 
            Ast.Package { name = name, fragments = List.map (defFragment env) fragments }
          | Ast.Anon blk => 
            let 
                val (blk, _) = defTopBlock env blk
            in
                Ast.Anon blk
            end
    end

and mkTopEnv (prog:Fixture.PROGRAM) 
    : ENV =
    { ribId = NONE,
      tempOffset = 0,
      openNamespaces = (if (Fixture.getLangEd prog > 3)
                        then [[Name.noNS, Ast.Internal Ustring.empty], [Name.ES4NS]]
                        else [[Name.noNS, Ast.Internal Ustring.empty]]),
      labels = [],
      className = Ustring.fromString "",
      packageName = [],
      defaultNamespace = Name.noNS,
      program = prog,
      func = NONE }

and summarizeFragment (Ast.Package { name, fragments }) =
    (LogErr.log ["package ", (LogErr.join "." (map Ustring.toAscii name)), " { "];
     List.app summarizeFragment fragments;
     LogErr.log ["}"])
  | summarizeFragment (Ast.Anon (Ast.Block {head=(SOME (Ast.Head (rib, _))), ...})) =
    Fixture.printRib rib
  | summarizeFragment _ = ()

and defTopFragment (prog:Fixture.PROGRAM)
                   (frag:Ast.FRAGMENT)
    : (Fixture.PROGRAM * Ast.FRAGMENT) =
    let
        val topEnv = mkTopEnv prog
        val frag = defFragment topEnv frag
    in
        trace ["fragment definition complete"];
        (if !doTraceSummary
         then summarizeFragment frag
         else ());
        (if !doTrace
         then Pretty.ppFragment frag
         else ());
        (prog, frag)
    end

end
