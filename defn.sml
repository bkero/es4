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
fun log ss = LogErr.log ("[defn] " :: ss)
fun error ss = LogErr.defnError ss
fun trace ss = if (!doTrace) then log ss else ()
fun trace2 (s, us) = if (!doTrace) then log [s, (Ustring.toAscii us)] else ()

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
 *)


                   
datatype LABEL_KIND =
          IterationLabel
        | SwitchLabel
        | StatementLabel

type LABEL = (Ast.IDENT * LABEL_KIND)

type CONTEXT = 
     { fixtures: Ast.FIXTURES,
       tempOffset: int,
       openNamespaces: Ast.NAMESPACE list list, 
       numericMode: Ast.NUMERIC_MODE,
       labels: LABEL list,
       packageNames: Ast.IDENT list list,
       className: Ast.IDENT,
       packageName: Ast.IDENT list,
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

val (topFixtures:Ast.FIXTURES ref) = ref []
val (topPackageNames:Ast.IDENT list list ref) = ref []

fun resetTopFixtures _ = 
    topFixtures := [ (Ast.PropName Name.meta_, Ast.NamespaceFixture Name.metaNS),
                     (Ast.PropName Name.magic_, Ast.NamespaceFixture Name.magicNS) ]

fun resetTopPackageNames _ = 
    topPackageNames := []

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

fun hasNamespace (nl:Ast.NAMESPACE list) 
                 (n:Ast.NAMESPACE) 
    : bool = 
    List.exists (fn x => n = x) nl

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


fun replaceFixture (b:Ast.FIXTURES) 
                   (n:Ast.FIXTURE_NAME) 
                   (v:Ast.FIXTURE)
    : Ast.FIXTURES = 
    let 
        fun search [] = LogErr.hostError ["fixture binding not found: ", 
                                          (LogErr.fname n)]
          | search ((k,v0)::bs) = 
            if k = n 
            then (k,v)::bs
            else (k,v0) :: (search bs)
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
    resolve a multiname to a name and then get the corresponding fixture

    for each nested context in the environment, look for a fixture that matches
    a multiname. see if a particular scope has a fixture with one of a list of 
    multinames

    multiname = { nss : NAMESPACE list list, id: IDENT }
    name = { ns: NAMESPACE, id: IDENT }
*)


fun getEnvParent [] = NONE
  | getEnvParent (x::[]) = NONE
  | getEnvParent (x::xs) = SOME xs

fun getEnvFixtures [] = error ["getEnvFixtures on empty environment"]
  | getEnvFixtures (({fixtures, ...}:CONTEXT) :: _) = fixtures

fun resolve env mname = 
    Multiname.resolveInFixtures mname env getEnvFixtures getEnvParent

fun resolveMultinameToFixture (env:ENV)
                              (mname:Ast.MULTINAME)
    : Ast.NAME * Ast.FIXTURE =
    case resolve env mname of
        NONE => LogErr.defnError ["unresolved fixture ", LogErr.multiname mname]
      | SOME (fixtures, n) => (n, getFixture fixtures (Ast.PropName n))

fun multinameHasFixture (env:ENV) 
                        (mname:Ast.MULTINAME) 
    : bool =
    case resolve env mname of
        NONE => false
      | SOME (fixtures, n) => true

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

fun mergeVirtuals (fName:Ast.FIXTURE_NAME)
                  (vnew:Ast.VIRTUAL_VAL_FIXTURE)
                  (vold:Ast.VIRTUAL_VAL_FIXTURE) = 
    let 
        val ty = if (#ty vnew) = (#ty vold)
                 then (#ty vnew)
                 else (if (#ty vnew) = Ast.SpecialType Ast.Any
                       then (#ty vold)
                       else (if (#ty vold) = Ast.SpecialType Ast.Any
                             then (#ty vnew)
                             else error ["mismatched get/set types on fixture ", 
                                         LogErr.fname fName]))
        fun either a b = 
            case (a,b) of 
                (SOME x, NONE) => SOME x
              | (NONE, SOME x) => SOME x
              | (NONE, NONE) => NONE
              | _ => error ["multiply defined get/set functions on fixture ", 
                            LogErr.fname fName]
    in
        { ty = ty,
          getter = either (#getter vold) (#getter vnew),
          setter = either (#setter vold) (#setter vnew) }
    end

fun mergeFixtures ((newName,newFix),oldFixs) =
    if hasFixture oldFixs newName
    then 
        case (newFix, getFixture oldFixs newName) of
            (Ast.VirtualValFixture vnew,
             Ast.VirtualValFixture vold) => 
            replaceFixture oldFixs newName 
                           (Ast.VirtualValFixture 
                                (mergeVirtuals newName vnew vold))
          | (Ast.ValFixture new, Ast.ValFixture old) =>
                 if (#ty new) = (#ty old) andalso (#readOnly new) = (#readOnly old)
                 then (trace ["skipping fixture ",LogErr.fname newName]; oldFixs)
                 else error ["incompatible redefinition of fixture name: ", LogErr.fname newName]
          | (Ast.MethodFixture new, Ast.MethodFixture old) => 
            replaceFixture oldFixs newName (Ast.MethodFixture new) (* FIXME: types *)
          | (Ast.MethodFixture new, Ast.ValFixture old) => 
            replaceFixture oldFixs newName (Ast.MethodFixture new) (* FIXME: types *)
          | (Ast.ValFixture new, Ast.MethodFixture old) => 
            replaceFixture oldFixs newName (Ast.ValFixture new) (* FIXME: types *)
          | _ => error ["mergeFixtures: redefining fixture name: ", LogErr.fname newName]
    else
        (newName,newFix) :: oldFixs

fun addNamespace (ns,opennss) =
    if hasNamespace opennss ns
    then (trace ["skipping namespace ",LogErr.namespace ns]; opennss)   (* FIXME: should be an error to open namspaces redundantly *)
    else (trace ["adding namespace ", LogErr.namespace ns]; ns :: opennss)

fun defNamespace (env:ENV) 
                 (ns:Ast.NAMESPACE) 
    : Ast.NAMESPACE =
        let
        in case ns of
            Ast.Public n =>
                let
                    val packageName = (#packageName (hd env))
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
                    val packageName = (#packageName (hd env))
                    val ident = packageIdentFromPath packageName
                in
(trace2 ("packageName=",ident);
                    Ast.Internal ident)
                end
          | Ast.Private _ => 
                let
                in
                    Ast.Private (#className (hd env))
                end
          | Ast.Protected _ => 
                let
                in
                    Ast.Protected (#className (hd env))  (* [JD] FIXME: not quite right *)
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
            val mname = {nss = (#openNamespaces (hd env)), id = ident}
        in
            case resolveMultinameToFixture env mname of 
                (_,Ast.NamespaceFixture ns) => ns
              | _ => LogErr.defnError ["namespace expression resolved ",
                                       "to non-namespace fixture"]
        end
      | _ => LogErr.defnError ["unexpected expression type ",
                               "in namespace context"]

and defaultNamespace e = case e of c::_ => (#defaultNamespace c) | _ => LogErr.internalError ["empty environment in defaultNamespace"]

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
                tempOffset = 0,
                openNamespaces = [[Name.noNS]],
                numericMode = defaultNumericMode,
                labels = [],
                packageNames = [],
                className = Ustring.fromString "",
                packageName = [],
                defaultNamespace = Name.noNS } :: [])
      | ({ tempOffset, numericMode, openNamespaces, labels, packageNames, className, 
           packageName, defaultNamespace, ... }) :: _ =>
        { fixtures = fixtures,
          tempOffset = tempOffset,
          openNamespaces = openNamespaces, 
          numericMode = numericMode,
          labels = labels,
          packageNames = [],
          className = className,
          packageName = packageName,
          defaultNamespace = defaultNamespace } :: env

fun updateFixtures (ctx::ex) (fxtrs:Ast.FIXTURES)
    : ENV =
    let
    in
        { fixtures = List.foldl mergeFixtures fxtrs (#fixtures ctx),
          tempOffset = (#tempOffset ctx),
          openNamespaces = (#openNamespaces ctx), 
          numericMode = (#numericMode ctx),
          labels = (#labels ctx),
          packageNames = (#packageNames ctx),
          className = (#className ctx),
          packageName = (#packageName ctx),
          defaultNamespace = (#defaultNamespace ctx) } :: ex
    end
  | updateFixtures ([]) (fxtrs:Ast.FIXTURES) 
    : ENV =
        LogErr.defnError ["cannot update an empty environment"]

fun addPackageName ({name,...}:Ast.PACKAGE,(ctx::ex))
    : ENV =
    let
    in
        { fixtures = (#fixtures ctx),
          tempOffset = (#tempOffset ctx),
          openNamespaces = (#openNamespaces ctx), 
          numericMode = (#numericMode ctx),
          labels = (#labels ctx),
          packageNames = name::(#packageNames ctx),
          className = (#className ctx),
          packageName = (#packageName ctx),
          defaultNamespace = (#defaultNamespace ctx) } :: ex
    end
  | addPackageName (_,[])
    : ENV =
        LogErr.defnError ["cannot add package name to an empty environment"]

fun updateTempOffset (ctx::ex) (tempOffset:int)
    : ENV =
    let
    in
        { fixtures = (#fixtures ctx),
          tempOffset = tempOffset,
          openNamespaces = (#openNamespaces ctx), 
          numericMode = (#numericMode ctx),
          labels = (#labels ctx),
          packageNames = (#packageNames ctx),
          className = (#className ctx),
          packageName = (#packageName ctx),
          defaultNamespace = (#defaultNamespace ctx) } :: ex
    end
  | updateTempOffset ([]) (tempOffset:int)
    : ENV =
        LogErr.defnError ["cannot update an empty environment"]

fun clearPackageName (ctx::ex)
    : ENV =
    let
    in
        { fixtures = (#fixtures ctx),
          tempOffset = (#tempOffset ctx),
          openNamespaces = (#openNamespaces ctx), 
          numericMode = (#numericMode ctx),
          labels = (#labels ctx),
          packageNames = (#packageNames ctx),
          className = (#className ctx),
          packageName = [],
          defaultNamespace = (#defaultNamespace ctx) } :: ex
    end
  | clearPackageName ([])
    : ENV =
        LogErr.defnError ["cannot update an empty environment"]

fun enterClass (ctx::ex) (className)
    : ENV =
    let
        val className = Name.mangle className
    in
        { fixtures = (#fixtures ctx),
          tempOffset = (#tempOffset ctx),
          openNamespaces = [Ast.Private className]::(#openNamespaces ctx), 
          numericMode = (#numericMode ctx),
          labels = (#labels ctx),
          packageNames = (#packageNames ctx),
          className = className,
          packageName = [],
          defaultNamespace = (#defaultNamespace ctx) } :: ex
    end
  | enterClass [] _
    : ENV =
        LogErr.defnError ["cannot update an empty environment"]

fun dumpLabels (labels : LABEL list) = trace ["labels ", concat (map (fn (id,_) => (Ustring.toAscii id)^" ") labels)]
fun dumpPath path = trace ["path ", concat (map (fn (id) => (Ustring.toAscii id)^" ") path)]

(*
    Add a label to the current environment context. Report an error
    if there is a duplicate. Labels on switch and iteration statements
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
                {labels:LABEL list,...}::_ =>
                    (dumpLabels labels;
                    if List.exists (fn ((id,knd):LABEL) => 
                            not (id = Ustring.empty) andalso  (* ignore empty labels *) 
                            id = labelId andalso              (* compare ids *)
                            knd = labelKnd) labels            (* and kinds *)
                    then LogErr.defnError ["duplicate label ", Ustring.toAscii labelId]
                    else ())
                  | [] => LogErr.internalError ["empty environment in addLabel"]
        in
            checkLabel env label;
            case env of 
                ({fixtures,tempOffset,labels,packageNames,openNamespaces,numericMode,className,
                  packageName,defaultNamespace}::e) =>
                       { fixtures=fixtures,
                         tempOffset=tempOffset,
                         labels=label::labels,
                         packageNames=packageNames,
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

fun multinameFromName (n:Ast.NAME) = 
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
                val ns = resolveExprToNamespace env qual
            in
                {nss = [[ns]], id = ident}
            end
      | Ast.TypeIdentifier {ident,typeArgs} =>
            let
            in
                identExprToMultiname env ident
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
    - inherit base fixtures
    - construct instance type
    - construct interface
    - return interface fixture
*)

and defInterface (env: ENV) 
                 (idef: Ast.INTERFACE_DEFN)
    : (Ast.FIXTURES * Ast.INTERFACE_DEFN) =
    let
        val { ident, extends, instanceDefns, nonnullable, ... } = idef

        (* Make the interface name *)
        val name = Name.nons ident

        (* Resolve base interface's super interfaces and fixtures *)
        val (superInterfaces, inheritedFixtures) = resolveInterfaces env extends

        val env = enterClass env name

        (* Define the current fixtures *)
        val (unhoisted,instanceFixtures,_) = defDefns env [] [] [] instanceDefns

        (* Inherit fixtures and check overrides *)
        val instanceFixtures = inheritFixtures inheritedFixtures instanceFixtures

        (* Make the instance type and interface fixture *)
        val instanceType = {name=name,
                            nonnullable=nonnullable,
                            typeParams=[],
                            superTypes=superInterfaces,
                            ty=Ast.SpecialType Ast.Any,  (* FIXME needs record type *)
                            conversionTy=NONE,
                            dynamic=false} (* interfaces are never dynamic *)
        val iface = Ast.Iface { name=name, nonnullable=nonnullable, extends=superInterfaces, instanceFixtures=instanceFixtures, 
                                instanceType = instanceType }
    in
        ([(Ast.PropName name, Ast.InterfaceFixture iface)],idef)
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
                              {typeParams=tpb,params=pb,result=rtb,minArgs=mb,...}),
                      func=(Ast.Func {fsig=Ast.FunctionSignature {defaults=db, ...}, ...}),...},
                 Ast.MethodFixture 
                     {ty=(Ast.FunctionType 
                              {typeParams=tpd,params=pd,result=rtd,minArgs=md,...}),
                      func=(Ast.Func {fsig=Ast.FunctionSignature {defaults=dd, ...}, ...}),...}) =>
                    let
                        val _ = trace ["length tpb ",Int.toString (length tpb),
                                       " length tpd ",Int.toString (length tpd),"\n"]
                        val _ = trace ["mb ",Int.toString mb, " md ",Int.toString md,"\n"]
                        val _ = trace ["length pb ",Int.toString (length pb),
                                       " length pd ",Int.toString (length pd),"\n"]
                        val _ = trace ["length db ",Int.toString (length db),
                                       " length dd ",Int.toString (length dd),"\n"]
                    in
                        (length tpb)=(length tpd)
                        andalso (((isVoid rtb) andalso (isVoid rtd))
                                 orelse
                                ((not (isVoid rtb)) andalso (not (isVoid rtd))))
                        (* FIXME: check compatibility of return types? *)
                    end
                
              | _ => false
        val _ = trace ["isCompatible = ",Bool.toString isCompatible]
    
    in case (fb,fd) of
        (Ast.MethodFixture {final,abstract,...}, Ast.MethodFixture {override,...}) => 
            (((not final) andalso override) orelse (abstract)) 
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
                fun targetFixture _ = if (hasFixture derived n)
                                      then SOME (getFixture derived n)
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
      | first::follows => inheritFixtures follows (inheritFixture first)
    end

(*
    implementFixtures

    Steps:

    - 

    Errors:

    - interface fixture not implemented
    
*)

and implementFixtures (base:Ast.FIXTURES)
                      (derived:Ast.FIXTURES)
    : unit =
    let
        (* 
        *)

        fun implementFixture ((n,fb):(Ast.FIXTURE_NAME * Ast.FIXTURE))
            : Ast.FIXTURES =
            let
                fun targetFixture _ = if (hasFixture derived n)
                                      then SOME (getFixture derived n)
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
                 (Ast.Cls {name,nonnullable,dynamic,classFixtures,instanceFixtures,instanceInits,
                           constructor,classType,instanceType,...}:Ast.CLS)
    : Ast.CLS =
    let
        val _ = trace ["analyzing class block for ", LogErr.name name]
        val (extendsName, instanceFixtures) = resolveExtends env instanceFixtures extends [name]
        val (implementsNames, instanceFixtures) = resolveImplements env instanceFixtures implements
        val superTypes = case extendsName of NONE => implementsNames | _ => (valOf extendsName) :: implementsNames

        (* Make the instance type *)
        val instanceType = 
            let 
                val { name, nonnullable, typeParams, ty, conversionTy, dynamic, ... } = instanceType
            in
                { name = name,
                  nonnullable = nonnullable,
                  typeParams = typeParams,
                  superTypes = superTypes,
                  ty = ty,
                  conversionTy = conversionTy,
                  dynamic = dynamic }
            end
    in
        Ast.Cls {name=name,
                 extends=extendsName,
                 nonnullable=nonnullable,
                 dynamic=dynamic,
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
                                if (id=Ustring.Object_) 
                                    then NONE 
                                    else SOME (Ast.Identifier {ident=Ustring.Object_,openNamespaces=[]})
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

and resolveImplements (env: ENV)
                      (instanceFixtures: Ast.FIXTURES) 
                      (implements: Ast.IDENT_EXPR list)
    : (Ast.NAME list * Ast.FIXTURES) =
    let
        val (superInterfaces, abstractFixtures) = resolveInterfaces env implements
        val _ = implementFixtures abstractFixtures instanceFixtures
    in
        (superInterfaces,instanceFixtures)
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

and interfaceMethods (ifxtr)
    : Ast.FIXTURES = 
    case ifxtr of
        Ast.InterfaceFixture (Ast.Iface {instanceFixtures,...}) => instanceFixtures
      |_ => LogErr.internalError ["interfaceMethods"]

and interfaceExtends (ifxtr)
    : Ast.NAME list =
    case ifxtr of
        Ast.InterfaceFixture (Ast.Iface {extends,...}) => extends
      |_ => LogErr.internalError ["interfaceExtends"]

(*
    resolve a list of interface names to their super interfaces and
    method fixtures

    interface I { function m() }
    interface J extends I { function n() }  // [I],[m]
    interface K extends J {}  // [I,J] [m,n]

*)

and resolveInterfaces (env: ENV)
                      (exprs: Ast.IDENT_EXPR list)
    : (Ast.NAME list * Ast.FIXTURES) =
    case exprs of
        [] => ([],[])
      | _ =>
        let
            val mnames = map (identExprToMultiname env) exprs
            val (interfaceNames,interfaceFixtures) = ListPair.unzip (map (resolveMultinameToFixture env) mnames)
            val methodFixtures = List.concat (map interfaceMethods interfaceFixtures)
            val superInterfaceNames = List.concat (map interfaceExtends interfaceFixtures)
        in
            (interfaceNames@superInterfaceNames, methodFixtures)
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
    case cdef of
        {ns, ident, instanceDefns, instanceStmts, classDefns, ctorDefn, nonnullable, dynamic, ...} =>
        let

            (*
                Partition the class definition into a class block
                and an instance block, and then define both blocks
            *)

            val ns = resolveExprOptToNamespace env ns
            val name = {id=ident, ns=ns}
            val env = enterClass env name

            val (unhoisted,classFixtures,classInits) = defDefns env [] [] [] classDefns
            val classFixtures = (List.foldl mergeFixtures unhoisted classFixtures)
            val staticEnv = extendEnvironment env classFixtures
                             (* namespace and type definitions aren't normally hoisted *)

            val (unhoisted,instanceFixtures,_) = defDefns staticEnv [] [] [] instanceDefns
            val instanceEnv = extendEnvironment staticEnv instanceFixtures

            val ctor = 
                case ctorDefn of 
                    NONE => NONE 
                  | SOME c => SOME (defCtor instanceEnv c)

            val (instanceStmts,_) = defStmts staticEnv instanceStmts  (* no hoisted fixture produced *)

            val conversionTy = 
                let 
                    val fname = Ast.PropName Name.meta_convert
                in
                    if hasFixture classFixtures fname
                    then 
                        case getFixture classFixtures fname of
                            Ast.MethodFixture { ty=Ast.FunctionType { params=[pt], ... }, ... } => 
                            SOME pt
                          | _ => NONE
                    else NONE
                end
            
            (* 
                The parser separates variable definitions into defns and stmts. The only stmts
                in this case will be InitStmts and what we really want is INITs so we translate
                to to INITs here.
            *)

            fun initsFromStmt stmt =
                case stmt of
                     Ast.ExprStmt (Ast.InitExpr (target,Ast.Head (temp_fxtrs,temp_inits),inits)) => (temp_fxtrs,temp_inits@inits)
                   | _ => LogErr.unimplError ["Defn.bindingsFromStmt"]
            
            val (fxtrs,inits) = ListPair.unzip(map initsFromStmt instanceStmts)
            val instanceInits = Ast.Head (List.concat fxtrs, List.concat inits)

            val instanceType = {name=name, 
                                nonnullable=nonnullable, 
                                typeParams=[], (* FIXME: copy in from CLASS_DEFN *)
                                superTypes=[], (* set in resolveClassInheritence *)
                                ty=Ast.ObjectType [], (* FIXME: need a full record type *)
                                conversionTy=conversionTy,
                                dynamic=dynamic}
        in
            Ast.Cls {name=name,
                     nonnullable=nonnullable,
                     dynamic=dynamic,
                     extends = NONE,
                     implements = [],                     
                     classFixtures = classFixtures,
                     instanceFixtures = instanceFixtures,
                     instanceInits = instanceInits,
                     constructor = ctor,
                     classType = Ast.ObjectType [],
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
            val ty' = defTyExpr env ty
            val readOnly' = case kind of 
                                 Ast.Const => true
                               | Ast.LetConst => true
                               | _ => false           
            val offset = (#tempOffset (hd env))     
            val name' = fixtureNameFromPropIdent env (SOME ns) ident offset
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
                    val ({ns,id},f) = resolveMultinameToFixture env mname
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
                val tempOffset = (#tempOffset (hd env))
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
                val ({ns,id},f) = resolveMultinameToFixture env mname
            in
                (Ast.PropName {ns=ns,id=id}, defExpr env right)
            end
    end

and defBindings (env:ENV)
                (kind:Ast.VAR_DEFN_TAG)
                (ns:Ast.NAMESPACE)
                ((binds,inits):Ast.BINDINGS) 
    : (Ast.FIXTURES * Ast.INITS) = 
    let
        val fxtrs:Ast.FIXTURES = map (defVar env kind ns) binds
        val inits:Ast.INITS = map (defInitStep env (SOME ns)) inits
    in
        (fxtrs,inits)
    end

and defSettings (env:ENV)
                ((binds,inits):Ast.BINDINGS) 
    : (Ast.FIXTURES * Ast.INITS) = 
    let
        val _ = trace [">> defSettings"]
        val fxtrs:Ast.FIXTURES = map (defVar env Ast.Var Name.noNS) binds
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
    : (Ast.FIXTURES * Ast.INITS * Ast.EXPR list * Ast.FIXTURES * Ast.INITS * Ast.EXPR list * Ast.TYPE_EXPR) =

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

            val thisType = case thisType of NONE => Ast.SpecialType Ast.Any
                                          | SOME x => defTyExpr env x
            val thisBinding = (Ast.PropName Name.this,
                               Ast.ValFixture
                                   { ty = thisType,
                                     readOnly = true })


            fun isTempFixture (n,_) : bool =
                case n of
                   Ast.TempName _ => true
                 | _ => false


            val (paramFixtures,paramInits) = defBindings env Ast.Var (Name.noNS) params
            val ((settingsFixtures,settingsInits),superArgs) =
                    case ctorInits of
                        SOME (settings,args) => (defSettings env settings, defExprs env args)
                      | NONE => (([],[]),[])
            val settingsFixtures = List.filter isTempFixture settingsFixtures
        in
            (thisBinding::paramFixtures,
             paramInits,
             defaults,
             settingsFixtures,
             settingsInits,
             superArgs,
             thisType)
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
    : (Ast.HEAD * Ast.EXPR list * Ast.FUNC) =
    let
        val _ = trace [">> defFunc"]
        val Ast.Func {name, fsig, block, ty, native, ...} = func
        val paramTypes = (#params ty)
        val env = updateTempOffset env (length paramTypes)
        val (paramFixtures, paramInits, defaults, settingsFixtures, settingsInits, superArgs, thisType) = defFuncSig env fsig
        val newTy = defFuncTy env ty
        val defaults = defExprs env defaults
        val env = extendEnvironment env paramFixtures
        val (block, hoisted) = defBlock env block
        val _ = trace ["<< defFunc"]
    in 
        (Ast.Head (settingsFixtures, settingsInits), superArgs, 
         Ast.Func {name = name,
                   fsig = fsig,
                   block = block,
                   defaults = defaults,
                   ty = newTy,
                   param = Ast.Head (List.foldl mergeFixtures paramFixtures hoisted, paramInits),
                   native=native})
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
        Ast.Func { name, fsig, block, ty, native, ... } =>
        let
            val qualNs = resolveExprOptToNamespace env (#ns f)
            val newName = Ast.PropName { id = (#ident name), ns = qualNs }
            val env = clearPackageName env
            val (_, _, newFunc) = defFunc env (#func f)
            val Ast.Func { ty, ... } = newFunc
            val newDefn = { kind = (#kind f),
                            ns = (#ns f),
                            final = (#final f),
                            override = (#override f),
                            prototype = (#prototype f),
                            static = (#static f),
                            abstract = (#abstract f),
                            func = newFunc }

            val fixture = 
                case (#kind name) of 
                    Ast.Get => 
                    Ast.VirtualValFixture 
                        { ty = (#result ty), 
                          getter = SOME newDefn,
                          setter = NONE }
                  | Ast.Set => 
                    Ast.VirtualValFixture 
                        { ty = (case (#params ty) of 
                                    [t] => t
                                  | _ => error ["expected one parameter in setter for ", 
                                                LogErr.fname newName]), 
                          getter = NONE,
                          setter = SOME newDefn }
                  | Ast.Ordinary =>
                    let 
                        val (ftype, isReadOnly) = 
                            if (#kind f) = Ast.Var 
                            then (Ast.SpecialType Ast.Any,false)      (* e3 style writeable function *)
                            else ((Ast.FunctionType ty),true)         (* read only, method *)
                    in
                        Ast.MethodFixture
                            { func = newFunc,
                              ty = ftype,
                              readOnly = isReadOnly,
                              final = (#final f),
                              override = (#override f),
                              abstract = (#abstract f)}
                    end
                  | Ast.Call =>
                    Ast.MethodFixture
                        { func = newFunc,
                          ty = Ast.FunctionType ty,
                          readOnly = true,
                          final = true,
                          override = false,
                          abstract = false}
                  | Ast.Has =>
                    Ast.MethodFixture
                        { func = newFunc,
                          ty = Ast.FunctionType ty,
                          readOnly = true,
                          final = true,
                          override = false,
                          abstract = false}
                  | Ast.Operator =>
                    LogErr.unimplError ["operator function not implemented"]

            val outerFixtures = [(newName, fixture)]
        in
            (outerFixtures, newDefn)
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
    : (ENV * Ast.FIXTURES) =
    let 
        val ctx : CONTEXT  = hd env
        val mode      = #numericMode ctx
        val numType   = ref (#numberType mode)
        val rounding  = ref (#roundingMode mode)
        val precision = ref (#precision mode)
        val defaultNamespace = ref (#defaultNamespace ctx)
        val opennss   = ref []
        val packageNames  = ref (#packageNames ctx)
        val fixtures = ref []
        val tempOffset = #tempOffset ctx

        fun defPragma x =
            case x of 
                Ast.UseNumber n => numType := n
              | Ast.UseRounding m => rounding := m
              | Ast.UsePrecision p => precision := p
              | Ast.UseNamespace ns =>
                    let
                        val namespace = resolveExprToNamespace env ns
                    in
                        opennss := addNamespace (namespace, (!opennss))
                    end
              | Ast.UseDefaultNamespace ns =>
                    let
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
                        defaultNamespace := namespace
                    end
              | Ast.Import {package,name,alias} =>  
                    let   
                    in case alias of
                        NONE =>
                            let
                                val id = packageIdentFromPath package
                                val ns = if name = Ustring.asterisk
                                         then Ast.Public id
                                         else Ast.LimitedNamespace (name,Ast.Public id)
                            in
                                (packageNames := package::(!packageNames);
trace2 ("openning package ",id);
                                 opennss  := addNamespace (ns, (!opennss)))
                            end
                      | _ => 
                            let

                                (* FIXME: replace this boilerplate getter/setter code with a new kind of fixture for import aliasing?
                                   In the cases of ClassFixture and TypeFixture, the verifier has to evaluate the getter to see the 
                                   concrete type of the alias *)

                                fun makeAliasFixture env alias package ident =
                                    let
                                        val targetName = {ns=Ast.Public package,id=ident}
                                        val (n,targetFixture) = resolveMultinameToFixture env (multinameFromName targetName)
                                        val fixtureType = case targetFixture of 
                                                               Ast.ValFixture {ty,...} => ty 
                                                             | Ast.NamespaceFixture _ => (Name.typename Name.intrinsic_Namespace)
                                                             | Ast.ClassFixture _ => (Name.typename Name.intrinsic_Class)   
                                                                 (* ISSUE: this is the base type of the class object *)
                                                             | Ast.TypeFixture _ => (Name.typename Name.intrinsic_Type)
                                                             | Ast.MethodFixture {ty,...} => ty
                                                             | Ast.VirtualValFixture {ty,...} => ty
                                                             | _ => LogErr.unimplError ["unhandle fixture type"]
                                        val targetRef = defExpr env (Ast.LexicalRef {ident=Ast.QualifiedIdentifier {
                                                                                    qual=Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Public package)),
                                                                                    ident=name},loc=NONE})
                                        val getterDefn : Ast.FUNC_DEFN = 
                                                 {kind=Ast.Const,
                                                  ns=NONE, (*SOME (!defaultNamespace),*)
                                                  final=false,
                                                  override=false,
                                                  prototype=false,
                                                  static=false,
                                                  abstract=false,
                                                  func = Ast.Func {name={kind=Ast.Get,ident=Ustring.empty},
                                                          fsig=Ast.FunctionSignature {typeParams=[],params=([],[]),paramTypes=[],
                                                                        defaults=[],ctorInits=NONE,returnType=Ast.SpecialType Ast.Any,
                                                                        thisType=NONE,hasRest=false},
                                                          native=false,
                                                          block=Ast.Block {pragmas=[],defns=[],head=SOME (Ast.Head ([],[])),loc=NONE,
                                                                           body=[Ast.ReturnStmt targetRef]},
                                                          param=Ast.Head ([],[]),
                                                          defaults=[],
                                                          ty = {typeParams=[],
                                                                params=[],
                                                                result=fixtureType,
                                                                thisType=NONE,
                                                                hasRest=false,
                                                                minArgs=0}}}


                                        val setterDefn : Ast.FUNC_DEFN = 
                                                 {kind=Ast.Const,
                                                  ns=NONE, (*SOME (!defaultNamespace),*)
                                                  final=false,
                                                  override=false,
                                                  prototype=false,
                                                  static=false,
                                                  abstract=false,
                                                  func = Ast.Func {name={kind=Ast.Set,ident=Ustring.empty},
                                                          fsig=Ast.FunctionSignature {typeParams=[],params=([],[]),paramTypes=[],
                                                                        defaults=[],ctorInits=NONE,returnType=Ast.SpecialType Ast.Any,
                                                                        thisType=NONE,hasRest=false},
                                                          native=false,
                                              block = Ast.Block
                                                        { pragmas = [],
                                                          defns = [],
                                                          head = SOME (Ast.Head ([], [])),
                                                          body = [Ast.ExprStmt
                                                                      (Ast.SetExpr
                                                                         (Ast.Assign, targetRef,
                                                                           Ast.LexicalRef
                                                                             { ident = Ast.Identifier
                                                                                         { ident = Ustring.x_,
                                                                                           openNamespaces = [[Name.noNS]]},
                                                                               loc = NONE}))],
                                                          loc = NONE},
                                              param = Ast.Head
                                                      ([(Ast.TempName 0,
                                                          Ast.ValFixture
                                                            { ty = Ast.SpecialType
                                                                     Ast.Any,
                                                              readOnly = false}),
                                                         (Ast.PropName
                                                            { ns = Name.noNS,
                                                              id = Ustring.x_},
                                                           Ast.ValFixture
                                                             { ty = Ast.SpecialType
                                                                      Ast.Any,
                                                               readOnly = false})],
                                                        [(Ast.PropName
                                                            { ns = Name.noNS,
                                                              id = Ustring.x_},
                                                           Ast.GetTemp 0)]),

                                                          defaults=[],
                                                          ty = {typeParams=[],
                                                                params=[fixtureType],
                                                                result=Ast.SpecialType Ast.VoidType,
                                                                thisType=NONE,
                                                                hasRest=false,
                                                                minArgs=1}}}
                                    in
                                        Ast.VirtualValFixture {ty=fixtureType,
                                                               getter=SOME getterDefn,
                                                               setter=SOME setterDefn}
                                    end
                                val aliasFixture = makeAliasFixture env alias (packageIdentFromPath package) name
                                val aliasName = {ns=(!defaultNamespace),id=valOf alias}
                                val _ = trace ["aliasName ",LogErr.name aliasName]
                                val id = packageIdentFromPath package
                                val ns = if name=Ustring.asterisk then Ast.Public id
                                                                      else Ast.LimitedNamespace (name,Ast.Public id)
                            in
                                (fixtures := (Ast.PropName aliasName,aliasFixture)::(!fixtures);
                                 packageNames := package::(!packageNames);
                                 opennss  := addNamespace (ns, (!opennss)))
                            end
                    end
              | _ => ()

    in
        List.app defPragma pragmas;

          ({ fixtures = !fixtures,
            tempOffset = tempOffset,
            labels = (#labels ctx),
            packageNames = !packageNames,
            openNamespaces = (case !opennss of 
                                 [] => (#openNamespaces ctx)   (* if opennss is empty, don't concat *)
                               | _  => !opennss :: (#openNamespaces ctx)),
            numericMode = { numberType = !numType, 
                            roundingMode = !rounding,
                            precision = !precision },
            className = (#className ctx),
            packageName = (#packageName ctx),
            defaultNamespace = !defaultNamespace } :: env,
        !fixtures)
    end

(*
    IDENT_EXPR
*)

and defIdentExpr (env:ENV) 
                 (ie:Ast.IDENT_EXPR) 
    : Ast.IDENT_EXPR = 
    let 
        val _ = trace [">> defIdentExpr"]
        val openNamespaces = 
            case env of 
                [] => []
              | ({ openNamespaces, ... }) :: _ => 
                openNamespaces
        val _ = trace ["<< defIdentExpr"]
    in
        case ie of 
            Ast.Identifier { ident, ... } => 
            Ast.Identifier { ident=ident, 
                             openNamespaces=openNamespaces } 
            
          | Ast.AttributeIdentifier ai => 
            Ast.AttributeIdentifier (defIdentExpr env ai)

          | Ast.TypeIdentifier { ident, typeArgs } => 
            Ast.TypeIdentifier { ident=(defIdentExpr env ident), 
                                 typeArgs=typeArgs }

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

and defContextualNumberLiteral (env:ENV) 
                               (n:string) 
                               (isIntegral:bool)
                               (isHex:bool)
                               (isOct:bool)
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
            Ast.LiteralUInt (if isHex
                             then 
                                 (case Word32.fromString n of 
                                      NONE => error ["failure converting '", n, 
                                                     "' to uint literal "]
                                    | SOME x => x)
                             else if isOct
                             then 
                                 (case StringCvt.scanString (Word32.scan StringCvt.OCT) n of 
                                      NONE => error ["failure converting '", n, 
                                                     "' to uint literal "]
                                    | SOME x => x)
                             else
                                 (case LargeInt.fromString n of
                                      NONE => error ["failure converting '", n, 
                                                     "' to uint literal "]
                                    | SOME d => Word32.fromLargeInt d))
    in
        if isHex andalso (not isIntegral)
        then error ["non-integral hex literal"]
        else if isOct andalso (not isIntegral)
        then error ["non-integral oct literal"]
        else if isOct andalso isHex
        then error ["literal cannot be both hex and oct"]
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
                        fun v _ = valOf (LargeInt.fromString n)
                    in
                        if isHex orelse isOct
                        then asUInt ()
                        else 
                            (if Mach.fitsInInt (v())
                             then asInt ()
                             else 
                                 (if Mach.fitsInUInt (v())
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
            defContextualNumberLiteral env n true false false

          | Ast.LiteralContextualDecimal n => 
            defContextualNumberLiteral env n false false false

          | Ast.LiteralContextualHexInteger n => 
            defContextualNumberLiteral env n true true false

          | Ast.LiteralContextualOctInteger n => 
            defContextualNumberLiteral env n true false true

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

and resolvePath (env:ENV) (path:Ast.IDENT list)
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

and matchPackageName (env:ENV)
                     (path:Ast.IDENT list)
    : Ast.IDENT option * Ast.IDENT list =
    let
    in case env of
        [] => (NONE,path)
      | _ =>
        let
            fun parentEnv _ = NONE (* just do one scope at a time *)
            val mname = identExprToMultiname env (Ast.Identifier {ident=(hd path),openNamespaces=[]})
        in
            case Multiname.resolveInFixtures mname env getEnvFixtures parentEnv of
                SOME (fixtures, n) => (NONE,path)   (* head of path matches fixture *)
              | NONE => (* head of path does not match fixture, try finding a package name that matches prefix of path *)
                let
                    val { packageNames, ... } = hd env
                in case pathInPackageNames packageNames path of
                       (NONE,_) => matchPackageName (tl env) path
                     | (SOME pkg,rest) => (SOME pkg,rest)
                end
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
                         | (Ast.LiteralExpr _,Ast.TypeIdentifier {ident=Ast.Identifier {ident=id,...},typeArgs}) => 
                               Ast.LexicalRef {ident=Ast.TypeIdentifier {ident=Ast.QualifiedIdentifier {qual=(defExpr env base),
                                                                                                        ident=id},
                                                                         typeArgs=typeArgs},
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
            let
                val tempOffset = (#tempOffset (hd env))
            in
                Ast.GetTemp (n+tempOffset)
            end

          | Ast.GetParam n => 
            Ast.GetTemp n

          | Ast.ListExpr es => 
            Ast.ListExpr (map sub es)

          | Ast.SliceExpr (a, b, c) => 
            Ast.SliceExpr (sub a, sub b, sub c)

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
            val {typeParams,params,result,thisType,hasRest,minArgs} = ty
            val params' = map (defTyExpr env) params
            val thisType' = case thisType of SOME ty => SOME (defTyExpr env ty) | _ => NONE
            val result' = defTyExpr env result
        in
            {typeParams=typeParams,
             params=params',
             result=result',
             thisType=thisType',
             hasRest=hasRest,
             minArgs=minArgs}            
        end

and defTyExpr (env:ENV)
              (ty:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR = 
    case ty of 
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
                 | (Ast.LiteralExpr _,Ast.TypeIdentifier {ident=Ast.Identifier {ident=id,...},typeArgs}) => 
                       Ast.TypeName (Ast.TypeIdentifier {ident=Ast.QualifiedIdentifier {qual=(defExpr env base),
                                                                                                ident=id},
                                                                 typeArgs=typeArgs})
                 | (_,_) => 
                       LogErr.defnError ["invalid type expr"]
                end
              | _ => 
                Ast.TypeName (defIdentExpr env n)
         end
      | Ast.UnionType tys => 
        Ast.UnionType (map (defTyExpr env) tys)
      | Ast.ArrayType tys => 
        Ast.ArrayType (map (defTyExpr env) tys)
      | Ast.ObjectType tys => 
        Ast.ObjectType (map (defFieldType env) tys)
      | Ast.NullableType { expr, nullable } => 
        Ast.NullableType { expr = defTyExpr env expr,
                           nullable = nullable } 

      | Ast.ElementTypeRef (ty, n) => 
        Ast.ElementTypeRef (defTyExpr env ty, n)

      | Ast.FieldTypeRef (ty, ident) => 
        Ast.FieldTypeRef (defTyExpr env ty, ident)

      (* FIXME *)
      | t => t

and defFieldType (env:ENV)
              (ty:Ast.FIELD_TYPE)
    : Ast.FIELD_TYPE = 
    let
        val {name,ty} = ty
        val ty = defTyExpr env ty
    in
        {name=name,ty=ty}
    end


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
        val ctx = case env of ctx::_ => ctx | _ => LogErr.internalError ["empty evironment in defStmt"]

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
                    val (uf1,hf1,i1) = defVarDefnOpt defn
                    val env = updateFixtures env uf1
                    val (newBody,hoisted) = defStmt env [] body
                    val tempEnv = updateTempOffset env 1   (* alloc temp for iteration value *)
                    val (newNext,_) = defStmt tempEnv [] next
                in
                    (Ast.ForInStmt { isEach=isEach, 
                                     obj = newObj,
                                     defn = defn,
                                     labels = Ustring.empty::labelIds,
                                     body = newBody, 
                                     fixtures = SOME uf1,
                                     next = newNext },
                     List.foldl mergeFixtures hf1 hoisted)
                end
            end

        fun makeIterationLabel id = (id,IterationLabel)
        fun makeStatementLabel id = (id,StatementLabel)
        fun makeSwitchLabel id = (id,SwitchLabel)

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
                       labels=Ustring.empty::labelIds}, hoisted)
                end

        (*
            for ( var x = 10; x > 0; --x ) ...
            for ( x=10; x > 0; --x ) ...
        *)

        fun defForStmt env { defn, init, cond, update, labels, body, fixtures } =
            let
                fun defVarDefnOpt vd =
                    case vd of
                        SOME vd => defDefn env (Ast.VariableDefn vd)
                      | NONE => ([],[],[])
                val (uf,hf,_) = defVarDefnOpt defn
                val env' = updateFixtures env (List.foldl mergeFixtures uf hf)
                val (newInit,_) = defStmts env' init
                val newCond = defExpr env' cond
                val newUpdate = defExpr env' update
                val (newBody, hoisted) = defStmt env' [] body
            in
                trace ["<< reconstructForStmt"];
                ( Ast.ForStmt { defn = defn,
                                init = newInit,
                                cond = newCond,
                                update = newUpdate,
                                labels = Ustring.empty::labelIds,
                                body = newBody,
                                fixtures = SOME (uf) },
                  (List.foldl mergeFixtures hf hoisted) )
            end

        fun reconstructCatch { bindings, fixtures, inits, block, ty } =
            let 
                val ty = defTyExpr env ty
                val (f0,i0) = defBindings env Ast.Var Name.noNS bindings
                val env = extendEnvironment env f0
                val (block,fixtures) = defBlock env block
            in                     
                { bindings = bindings,   (* FIXME: what about inits *)
                  block = block,
                  fixtures = SOME f0,
                  inits = SOME i0,
                  ty=ty }
            end            

        fun defCase env { label, body, inits } 
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

        fun findClass (n:Ast.NAME) =
            let
                val (n,f) = resolveMultinameToFixture env (multinameFromName n)
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
            in case env of
                {labels,...}::_ =>
                    (dumpLabels labels;
                    if List.exists (fn (id,knd) => 
                        id = labelId andalso    (* compare ids *)
                        knd = labelKnd) labels  (* and kinds *)
                    then true
                    else false)
              | _ => LogErr.internalError ["checkLabel called with empty environment"]
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
                val env'' = addLabel env' (makeIterationLabel Ustring.empty)
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
                val env'' = addLabel env' (makeIterationLabel Ustring.empty)
            in
                inl (Ast.WhileStmt) (defWhileStmt env'' w)
            end
            
          | Ast.DoWhileStmt w => 
            let
                val env' = addLabels env (map makeIterationLabel labelIds);
                val env'' = addLabel env' (makeIterationLabel Ustring.empty)
            in
                inl (Ast.DoWhileStmt) (defWhileStmt env'' w)
            end
            
          | Ast.ForStmt f => 
            let
                val env' = addLabels env (map makeIterationLabel labelIds);
                val env'' = addLabel env' (makeIterationLabel Ustring.empty)
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
                 (List.foldl mergeFixtures thn_hoisted els_hoisted))
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
        
          | Ast.SwitchStmt { cond, cases, ... } => 
            let
                val env' = addLabels env (map makeSwitchLabel labelIds);
                val env'' = addLabel env' (makeSwitchLabel Ustring.empty)
                val (cases,hoisted) = ListPair.unzip (map (defCase env'') cases)
            in
                (Ast.SwitchStmt { mode = SOME (#numericMode ctx),
                                  cond = defExpr env cond,
                                  cases = cases,
                                  labels=Ustring.empty::labelIds}, List.concat hoisted)
            end
        
          | Ast.SwitchTypeStmt { cond, ty, cases } =>
            (Ast.SwitchTypeStmt {cond = defExpr env cond,
                                 ty = defTyExpr env ty,
                                 cases = map reconstructCatch cases}, [])
            
          | Ast.DXNStmt { expr } => 
            (Ast.DXNStmt { expr = defExpr env expr },[])
    end

and defStmts (env) (stmts:Ast.STMT list)
    : (Ast.STMT list * Ast.FIXTURES) = 
    case stmts of
        (stmt::stmts) =>
            let 
                val (s1,f1):(Ast.STMT*Ast.FIXTURES) = defStmt env [] stmt

                (* Class definitions are embedded in the ClassBlock so we
                   need to update the environment in that case *)

                val env' = updateFixtures env f1
                val (s2,f2) = defStmts env' stmts
            in
                (s1::s2,(List.foldl mergeFixtures f1 f2))
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
    : (Ast.FIXTURES * Ast.NAMESPACE_DEFN) = 
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
    : Ast.FIXTURES = 
    let
        val { ident, ns, init } = td 
        val ns = case ns of 
                     NONE => defaultNamespace env
                   | SOME e => resolveExprToNamespace env e
        val n = { id=ident, ns=ns }
    in
        [(Ast.PropName n, 
          Ast.TypeFixture (defTyExpr env init))]
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
                val (fxtrs,def) = defFuncDefn env fd
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
            (* FIXME: this should be unhoisted, but it causes type 'Numeric' to vanish. Why? *)
            val hoisted = defType env td
        in
            ([], hoisted, [])
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
    let 
        val _ = trace([">> defDefns"])
    in case defns of
           [] => (trace(["<< defDefns"]);(unhoisted,hoisted,inits))
         | d::ds =>
           let
               val (unhoisted',hoisted',inits') = defDefn env d
               val temp = case d of
                              Ast.NamespaceDefn _ => hoisted'
                            | Ast.ClassDefn _     => hoisted'
                            | Ast.InterfaceDefn _ => hoisted'
                            | _ => []
               val env'  = updateFixtures env (List.foldl mergeFixtures unhoisted' temp) 
           (* add the new unhoisted and temporarily, hoisted class fxtrs to the current env *)
           in
               defDefns env' 
                        (List.foldl mergeFixtures unhoisted unhoisted') 
                        (List.foldl mergeFixtures hoisted hoisted') 
                        (inits@inits') ds
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
    let 
        val Ast.Block { pragmas, defns, body, loc, ... } = b
        val _ = LogErr.setLoc loc
        val (env,unhoisted_pragma_fxtrs) = defPragmas env pragmas
        val (unhoisted_defn_fxtrs,hoisted_defn_fxtrs,inits) = defDefns env [] [] [] defns
        val env = updateFixtures env (List.foldl mergeFixtures unhoisted_defn_fxtrs hoisted_defn_fxtrs)
        val (body,hoisted_body_fxtrs) = defStmts env body
        val hoisted = List.foldl mergeFixtures hoisted_defn_fxtrs hoisted_body_fxtrs
    in
        (Ast.Block { pragmas = pragmas,
                     defns = [],  (* clear definitions, we are done with them *)
                     body = body,
                     head = SOME (Ast.Head (List.foldl mergeFixtures unhoisted_defn_fxtrs unhoisted_pragma_fxtrs,inits)),
                     loc = loc},
         hoisted)
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
            val packageName : Ast.IDENT list = (#name package)
            val packageIdent = packageIdentFromPath packageName
            val _ = trace2 ("packageIdent ",packageIdent)
            val env' = {fixtures = [],
                        tempOffset = 0,
                        openNamespaces = [[Ast.Internal packageIdent, Ast.Public packageIdent]]@(#openNamespaces (hd env)),
                        numericMode = defaultNumericMode, 
                        labels = [],
                        packageNames = [packageName],
                        className = Ustring.fromString "", 
                        packageName = packageName, 
                        defaultNamespace = Ast.Internal packageIdent } :: env

            val (block,hoisted) = defBlock env' (#block package)
        in
            ({ name = packageName,
              block = block }, hoisted)
        end


(*
    PROGRAM
*)

and topEnv _ = [ { fixtures = !topFixtures,
                   tempOffset = 0,
                   openNamespaces = [[Name.noNS,Ast.Internal Ustring.empty]],
                   numericMode = defaultNumericMode, 
                   labels = [],
                   packageNames = !topPackageNames,
                   className = Ustring.fromString "", 
                   packageName = [], 
                   defaultNamespace = Name.noNS } ]

and defProgram (prog:Ast.PROGRAM) 
    : Ast.PROGRAM = 
    let 
        val _ = LogErr.setLoc NONE
        val e = topEnv ()
        val (packages, hoisted_pkg) = ListPair.unzip (map (defPackage e) (#packages prog))
        val e = List.foldl addPackageName e packages
        val (block, hoisted_gbl) = defBlock (updateFixtures e (List.concat hoisted_pkg)) (#block prog)

        val fixtures = List.foldl mergeFixtures (List.concat hoisted_pkg) hoisted_gbl
        val result = {packages = packages,
                      block = block,
                      fixtures = SOME fixtures }
    in
        trace ["definition complete"];
        (if !doTrace 
         then Pretty.ppProgram result
         else ());
        topFixtures := (List.foldl mergeFixtures fixtures (!topFixtures));
        topPackageNames := (#packageNames (hd e));
        result
    end
end
