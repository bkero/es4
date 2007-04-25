(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Eval = struct 

(* Local tracing machinery *)


val traceStack = ref false
val (stack:string list list ref) = ref []
fun resetStack _ = 
    stack := []

val ArrayClassIdentity = ref (~1)
val FunctionClassIdentity = ref (~1)

fun join sep ss = 
    case ss of 
        [] => ""
      | [x] => x
      | x :: xs => x ^ sep ^ (join sep xs)

fun stackString _ =
    "[" ^ (join " | " (map String.concat (List.rev (!stack)))) ^ "]"

fun push ss = 
    (stack := (ss) :: (!stack);     
     if !traceStack
     then LogErr.log ("[stack] " :: [stackString ()])
     else ())

fun pop _ = 
    (stack := tl (!stack);
     if !traceStack
     then LogErr.log ("[stack] " :: [stackString()])
     else ())

fun log ss = LogErr.log ("[eval] " :: ss)

val doTrace = ref false
fun fmtName n = if (!doTrace) then LogErr.name n else ""
fun fmtMultiname n = if (!doTrace) then LogErr.multiname n else ""
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = 
    (LogErr.log ("[stack] " :: [stackString()]);
     LogErr.evalError ss)

(* Exceptions for object-language control transfer. *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL

exception InternalError

fun mathOp (v:Mach.VAL) 
           (decimalFn:(Decimal.DEC -> 'a) option)
           (doubleFn:(Real64.real -> 'a) option)
           (intFn:(Int32.int -> 'a) option)
           (uintFn:(Word32.word -> 'a) option)
           (default:'a)
    : 'a = 
    let 
        fun fnOrDefault fo v = case fo of 
                                   NONE => default
                                 | SOME f => f v
    in
        case v of 
            Mach.Object (Mach.Obj ob) => 
            (case !(#magic ob) of 
                 SOME (Mach.Decimal d) => fnOrDefault decimalFn d
               | SOME (Mach.Double d) => fnOrDefault doubleFn d
               | SOME (Mach.Int i) => fnOrDefault intFn i
               | SOME (Mach.UInt u) => fnOrDefault uintFn u
               | _ => default)
          | _ => default
    end

fun extendScope (p:Mach.SCOPE) 
                (ob:Mach.OBJ)
                (kind:Mach.SCOPE_KIND)
    : Mach.SCOPE = 
    Mach.Scope { parent = (SOME p), 
                 object = ob, 
                 temps = ref [], 
                 kind = kind }

fun extendScopeReg (r:Mach.REGS) 
                   (ob:Mach.OBJ)
                   (kind:Mach.SCOPE_KIND)
    : Mach.REGS = 
    let
        val {scope,this} = r
    in
        {scope=extendScope scope ob kind,
         this=this}
    end
         

fun getObjId (obj:Mach.OBJ) 
    : Mach.OBJ_IDENT = 
    case obj of 
        Mach.Obj { ident, ... } => ident


fun getScopeObj (scope:Mach.SCOPE) 
    : Mach.OBJ = 
    case scope of 
        Mach.Scope { object, ... } => object


fun getScopeId (scope:Mach.SCOPE) 
    : Mach.OBJ_IDENT = getObjId (getScopeObj scope)


fun getScopeTemps (scope:Mach.SCOPE) 
    : Mach.TEMPS = 
    case scope of 
        Mach.Scope { temps, ... } => temps


fun needNamespace (v:Mach.VAL) 
    : Ast.NAMESPACE = 
    case v of 
        Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of 
             SOME (Mach.Namespace n) => n
           | _ => error ["need namespace"])
      | _ => error ["need namespace"]


fun needObj (v:Mach.VAL) 
    : Mach.OBJ = 
    case v of 
        Mach.Object ob => ob
      | _ => error ["need object"]


(* 
 * The global object and scope.
 *)

val (globalObject:(Mach.OBJ option) ref) = ref NONE
val (globalScope:(Mach.SCOPE option) ref) = ref NONE

fun getGlobalObject _ 
    : Mach.OBJ = 
    case !globalObject of 
        NONE => error ["missing global object"]
      | SOME ob => ob

fun getGlobalScope _ 
    : Mach.SCOPE = 
    case !globalScope of 
        NONE => error ["missing global scope"]
      | SOME ob => ob


fun getInitialRegs _ = 
    { this = getGlobalObject (),
      scope = getGlobalScope () }

(* 
 * A small number of functions do not fully evaluate to Mach.VAL 
 * values, but instead to REFs; these are temporary artifacts of
 * evaluation, not first-class values in the language. 
 *)

type REF = (Mach.OBJ * Ast.NAME)

(* Fundamental object methods *)

(* FIXME: possibly move this to mach.sml *) 

fun allocFixtures (regs:Mach.REGS) 
                  (obj:Mach.OBJ) 
                  (this:Mach.OBJ option)
                  (temps:Mach.TEMPS)
                  (f:Ast.FIXTURES) 
    : unit =
    case obj of 
        Mach.Obj { props, ident, ... } => 
        let 
            val _ = trace ["allocating fixtures on object id #", Int.toString ident]
            val {scope, ...} = regs
            val methodScope = extendScope scope obj Mach.ActivationScope
            fun valAllocState (t:Ast.TYPE_EXPR) 
                : Mach.PROP_STATE = 

                (* Every value fixture has a type, and every type has an
                 * associated "allocated state". Note that
                 * this is *not* the same as saying that every type
                 * has an associated default value; for *some* types
                 * the allocated state is a default value; for
                 * types that are non-nullable, however, the allocated
                 * state is Mach.UninitProp. This property
                 * state should never be observable to a user. It is
                 * always a hard error to read a property in
                 * Mach.UninitProp state, and it is always a hard
                 * error to complete the initialization phase of an
                 * object with any properties remaining in
                 * Mach.UninitProp state. *)

                case t of 
                    Ast.SpecialType (Ast.Any) => 
                    Mach.ValProp (Mach.Undef)

                  | Ast.SpecialType (Ast.Null) => 
                    Mach.ValProp (Mach.Null)

                  | Ast.SpecialType (Ast.Undefined) => 
                    Mach.ValProp (Mach.Undef)

                  | Ast.SpecialType (Ast.VoidType) => 
                    error ["attempt to allocate void-type property"]

                  (* FIXME: is this correct? Maybe we need to check them all to be nullable? *)
                  | Ast.UnionType _ => 
                    Mach.ValProp (Mach.Null)

                  | Ast.ArrayType _ => 
                    Mach.ValProp (Mach.Null)

                  | Ast.TypeName ident => 
                    (* FIXME: resolve nominal type to class or interface, check to see if 
                     * it is nullable, *then* decide whether to set to null or uninit. *)
                    Mach.ValProp (Mach.Null)

                  | Ast.FunctionType _ => 
                    Mach.UninitProp

                  | Ast.ObjectType _ => 
                    Mach.ValProp (Mach.Null)

                  | Ast.AppType {base, ...} => 
                    valAllocState base

                  | Ast.NullableType { expr, nullable=true } => 
                    Mach.ValProp (Mach.Null)
                    
                  | Ast.NullableType { expr, nullable=false } => 
                    Mach.UninitProp

                  | Ast.ElementTypeRef _ => 
                    Mach.ValProp (Mach.Undef)  (* FIXME: should get the type of the element from the array type *)
 
                  | Ast.FieldTypeRef _ => (* FIXME: get type from object type *)
                    Mach.ValProp (Mach.Undef)

                  | _ => error ["Shouldn't happen: failed to match in Eval.allocFixtures#valAllocState."]

            fun tempPadding n =
                if n = 0
                    then []
                    else (Ast.SpecialType Ast.Any, Mach.UninitTemp)::(tempPadding (n-1))

            fun allocFixture (n, f) = 
                case n of 
                    Ast.TempName t => 
                    (case f of 
                         Ast.ValFixture { ty, ... } =>  (* FIXME: temp types are not needed, use the value tag for rt typechecking *)
                         (if t = (List.length (!temps))
                          then (trace ["allocating fixture for temporary ", Int.toString t];
                                temps := (Ast.SpecialType Ast.Any, Mach.UninitTemp)::(!temps)) 
                          else if t < (List.length (!temps))
                                then (trace ["ignoring fixture, already allocated ", Int.toString t];
                                      temps := (List.take (!temps,((length (!temps))-t-1)))@((ty, Mach.UninitTemp)::(List.drop (!temps,(length (!temps)-t))))) 
                          else (trace ["allocating fixtures for temporaries ", Int.toString (length (!temps)), " to ", Int.toString t];
                                temps := (Ast.SpecialType Ast.Any, Mach.UninitTemp)
                                            ::(((tempPadding (t-(length (!temps))))@(!temps)))))  
                       | _ => error ["allocating non-value temporary"])
                  | Ast.PropName pn => 
                    let 
                        val _ = trace ["allocating fixture for property ", fmtName pn]
                        fun allocProp state p = 
                            if Mach.hasProp props pn
                            then error ["allocating duplicate property name: ", 
                                        fmtName pn]
                            else (trace ["allocating fixture for ", state, " property ", 
                                         fmtName pn]; 
                                  Mach.addProp props pn p)                            
                    in 
                        case f of 
                            Ast.TypeFixture te => 
                            allocProp "type" 
                                      { ty = te,
                                        state = Mach.TypeProp,                                   
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }

                          | Ast.MethodFixture { func, ty, readOnly, ... } => 
                            let
                                val Ast.Func { isNative, ... } = func
                                val p = if isNative
                                        then Mach.NativeFunctionProp (Mach.getNativeFunction pn)
                                        else Mach.MethodProp (newFunClosure methodScope func this)
                            in
                                allocProp "method" 
                                          { ty = ty,
                                            state = p,
                                            attrs = { dontDelete = true,
                                                      dontEnum = true,
                                                      readOnly = readOnly,
                                                      isFixed = true } }
                            end

                          | Ast.ValFixture { ty, readOnly, ... } => 
                            allocProp "value" 
                                      { ty = ty,
                                        state = valAllocState ty,
                                        attrs = { dontDelete = true,
                                                  dontEnum = false,
                                                  readOnly = readOnly,
                                                  isFixed = true } }
                            
                          | Ast.VirtualValFixture { ty, getter, setter, ... } => 
                            let
                                val getFn = case getter of
                                                NONE => NONE
                                              | SOME f => SOME (newFunClosure methodScope (#func f) this)
                                val setFn = case setter of
                                                NONE => NONE
                                              | SOME f => SOME (newFunClosure methodScope (#func f) this)
                            in
                                allocProp "virtual value" 
                                          { ty = ty,
                                            state = Mach.VirtualValProp { getter = getFn,
                                                                          setter = setFn },
                                            attrs = { dontDelete = true,
                                                      dontEnum = false,
                                                      readOnly = true,
                                                      isFixed = true } }
                            end
                            
                          | Ast.ClassFixture cls =>
                            let
                                val Ast.Cls {classFixtures, ...} = cls
                                val _ = trace ["allocating class object for class ", fmtName pn]
                                val classObj = needObj (newClass scope cls)
                                val _ = trace ["allocating class fixtures on class ", fmtName pn]
                                (* FIXME: 'this' binding in class objects might be wrong here. *)
                                val _ = allocObjFixtures regs classObj NONE classFixtures
                            in
                                allocProp "class"
                                          { ty = (Name.typename Name.public_Class),
                                            state = Mach.ValProp (Mach.Object classObj),
                                            attrs = { dontDelete = true,
                                                      dontEnum = true,
                                                      readOnly = true,
                                                      isFixed = true } }
                            end
                            
                          | Ast.NamespaceFixture ns => 
                            allocProp "namespace" 
                                      { ty = (Name.typename Name.public_Namespace),
                                        state = Mach.NamespaceProp ns,
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }
                            
                          | Ast.TypeVarFixture =>
                            allocProp "type variable"
                                      { ty = (Name.typename Name.public_Type),
                                        state = Mach.TypeVarProp,
                                        attrs = { dontDelete = true,
                                                  dontEnum = true,
                                                  readOnly = true,
                                                  isFixed = true } }

                          | Ast.InterfaceFixture =>  (* FIXME *)
                            ()

                          (* | _ => error ["Shouldn't happen: failed to match in Eval.allocFixtures#allocFixture."] *)

                    end
        in                    
            List.app allocFixture f
        end

    
and allocObjFixtures (regs:Mach.REGS) 
                     (obj:Mach.OBJ)
                     (this:Mach.OBJ option)
                     (f:Ast.FIXTURES) 
    : unit = 
    let 
        val (temps:Mach.TEMPS) = ref [] 
    in
        allocFixtures regs obj this temps f;
        if not ((length (!temps)) = 0)
        then error ["allocated temporaries in non-scope object"]
        else ()
    end
    
and allocScopeFixtures (regs:Mach.REGS) 
                       (f:Ast.FIXTURES) 
    : unit = 
    case (#scope regs) of
        Mach.Scope { object, temps, ... } => 
        allocFixtures regs object NONE temps f


and hasOwnValue (obj:Mach.OBJ) 
                (n:Ast.NAME) 
    : bool = 
    case obj of 
        Mach.Obj { props, ... } => 
        Mach.hasProp props n


and hasValue (obj:Mach.OBJ) 
             (n:Ast.NAME) 
    : bool = 
    if hasOwnValue obj n
    then true
    else (case obj of 
              Mach.Obj { proto, ... } => 
              case (!proto) of 
                  Mach.Object p => hasValue p n
                | _ => false)


(* 
 * *Similar to* ES3 8.7.1 GetValue(V), there's 
 * no Reference type in ES4.
 *)
and getValueOrVirtual (obj:Mach.OBJ)
                      (name:Ast.NAME) 
                      (doVirtual:bool)
    : Mach.VAL = 
    let 
        val Mach.Obj { props, ... } = obj
    in
        case Mach.findProp props name of 
            SOME prop => 
            (case (#state prop) of 
                 Mach.TypeProp => 
                 error ["getValue on a type property: ",
                        fmtName name]
                 
               | Mach.TypeVarProp => 
                 error ["getValue on a type variable property: ",
                        fmtName name]
                 
               | Mach.UninitProp => 
                 error ["getValue on an uninitialized property: ",
                        fmtName name]
                 
               | Mach.VirtualValProp { getter, ... } => 
                 if doVirtual
                 then 
                     case getter of 
                         SOME g => 
                         invokeFuncClosure obj g []
                       | NONE => 
                         error ["getValue on a virtual property w/o getter: ",
                                fmtName name]
                 else 
                     (* FIXME: possibly throw here? *)
                     Mach.Undef
                     
               | Mach.NamespaceProp n =>
                 newNamespace n
                 
               | Mach.NativeFunctionProp nf =>
                 newNativeFunction nf
                 
               | Mach.MethodProp closure => 
                 newFunctionFromClosure closure
                 
               | Mach.ValListProp vals =>
                 newArray vals
                 
               | Mach.ValProp v => v)
          | NONE => 
            if Mach.hasProp props Name.meta_get
            then 
                let 
                    (* FIXME: no idea if this is correct behavior for meta::get *)
                    val metaGetFn = needObj (getValueOrVirtual obj Name.meta_get false)
                    val nameObj = (* FIXME: need a builtin Name.es object here. *)
                        newString (#id name)
                in
                    evalCallExpr obj metaGetFn [nameObj]
                end
            else
                Mach.Undef
    end

and getValue (obj:Mach.OBJ)
             (name:Ast.NAME)
    : Mach.VAL = 
    getValueOrVirtual obj name true

and setValueOrVirtual (base:Mach.OBJ) 
                      (name:Ast.NAME) 
                      (v:Mach.VAL) 
                      (doVirtual:bool) 
    : unit =     
    let
        val Mach.Obj { props, ... } = base
    in
        case Mach.findProp props name of
            SOME existingProp => 
            let 
                val existingAttrs = (#attrs existingProp)
                val newProp = { state = Mach.ValProp v,
                                ty = (#ty existingProp), 
                                attrs = existingAttrs }
                fun write _ = 
                    ((* FIXME: insert typecheck here *)
                      Mach.delProp props name;
                      Mach.addProp props name newProp)                    
            in
                case (#state existingProp) of 
                    Mach.UninitProp => 
                    error ["setValue on uninitialized property", 
                           fmtName name]
                    
                  | Mach.TypeVarProp => 
                    error ["setValue on type variable property:", 
                           fmtName name]
                    
                  | Mach.TypeProp => 
                    error ["setValue on type property: ", 
                           fmtName name]

                  | Mach.NamespaceProp _ => 
                    error ["setValue on namespace property: ", 
                           fmtName name]

                  | Mach.NativeFunctionProp _ => 
                    error ["setValue on native function property: ", 
                           fmtName name]

                  | Mach.MethodProp _ => 
                    error ["setValue on method property: ", 
                           fmtName name]

                  | Mach.ValListProp _ => 
                    error ["setValue on value-list property: ", 
                           fmtName name]
                    
                  | Mach.VirtualValProp { setter = SOME s, ... } => 
                    if doVirtual
                    then (invokeFuncClosure base s [v]; ())
                    else write ()
                         
                  | Mach.VirtualValProp { setter = NONE, ... } => 
                    if doVirtual
                    then error ["setValue on virtual property w/o setter: ", 
                                fmtName name]
                    else write ()
                         
                  | Mach.ValProp _ => 
                    if (#readOnly existingAttrs)
                    then ()  (* ignore it *)
                    else write ()
            end
          | NONE =>  
            if doVirtual andalso Mach.hasProp props Name.meta_set
            then 
                let 
                    (* FIXME: no idea if this is correct behavior for meta::set *)
                    val metaSetFn = needObj (getValueOrVirtual base Name.meta_set false)
                    val nameObj = (* FIXME: need a builtin Name.es object here. *)
                        newString (#id name)
                in
                    evalCallExpr base metaSetFn [nameObj, v];
                    ()
                end
            else
                let 
                    val prop = { state = Mach.ValProp v,
                                 ty = Ast.SpecialType Ast.Any,
                                 attrs = { dontDelete = false,
                                           dontEnum = false,
                                           readOnly = false,
                                           isFixed = false } }
                in
                    Mach.addProp props name prop
                end
    end


and setValue (base:Mach.OBJ) 
             (name:Ast.NAME) 
             (v:Mach.VAL) 
    : unit = 
    setValueOrVirtual base name v true

(* A "defValue" call occurs when assigning a property definition's 
 * initial value, as specified by the user. All other assignments
 * to a property go through "setValue". *)

and defValue (base:Mach.OBJ) 
             (name:Ast.NAME) 
             (v:Mach.VAL) 
    : unit =
    case base of 
        Mach.Obj { props, ... } => 
        if not (Mach.hasProp props name)
        then error ["defValue on missing property: ", fmtName name]
        else 
            (* 
             * defProp has relaxed rules: you can write to an 
             * uninitialized property or a read-only property. 
             *)
            let 
                val existingProp = Mach.getProp props name
                val newProp = { state = Mach.ValProp v,
                                ty = (#ty existingProp), 
                                attrs = (#attrs existingProp) }
                fun writeProp _ = 
                    ((* FIXME: insert typecheck here *)
                     Mach.delProp props name;
                     Mach.addProp props name newProp)
            in       
                case (#state existingProp) of                     
                    Mach.TypeVarProp => 
                    error ["defValue on type variable property: ", 
                           fmtName name]
                    
                  | Mach.TypeProp => 
                    error ["defValue on type property: ", 
                           fmtName name]

                  | Mach.NamespaceProp _ => 
                    error ["defValue on namespace property: ", 
                           fmtName name]
                    
                  | Mach.NativeFunctionProp _ => 
                    error ["defValue on native function property: ", 
                           fmtName name]

                  | Mach.MethodProp _ => 
                    error ["defValue on method property: ", 
                           fmtName name]

                  | Mach.ValListProp _ => 
                    error ["defValue on value-list property: ", 
                           fmtName name]

                  | Mach.VirtualValProp { setter = SOME s, ... } => 
                    (invokeFuncClosure base s [v]; ())
                    
                  | Mach.VirtualValProp { setter = NONE, ... } => 
                    error ["defValue on virtual property w/o setter: ", 
                           fmtName name]
                    
                  | Mach.UninitProp => writeProp ()
                  | Mach.ValProp _ => writeProp ()
            end
            
and instantiateGlobalClass (n:Ast.NAME) 
                           (args:Mach.VAL list)
    : Mach.VAL = 
      let
          val _ = trace ["instantiating global class ", fmtName n];
          val (cls:Mach.VAL) = getValue (getGlobalObject ()) n
      in
          case cls of 
              Mach.Object ob => evalNewExpr ob args
            | _ => error ["global class name ", fmtName n, 
                          " did not resolve to object"]
      end

and newObject _ = 
    instantiateGlobalClass Name.public_Object []

and newObj _ = 
    needObj (instantiateGlobalClass Name.public_Object [])

and newRootBuiltin (n:Ast.NAME) (m:Mach.MAGIC) 
    : Mach.VAL = 
    (* 
     * Five of our builtin types require special handling when it comes
     * to constructing them: we wish to run the builtin ctors with no 
     * arguments at all, then clobber the magic slot in the resulting 
     * object. All other builtins we can pass a tagless "ur-Objects" into the
     * builtin ctor and let it modify its own magic slot using magic::setValue.
     * 
     * For these cases (Function, Class, Namespace, Boolean and boolean) we 
     * cannot rely on  the builtin ctor calling magic::setValue, as they need 
     * to exist in order to *execute* a call to magic::setValue (or execute 
     * the tiny amount of surrounding control flow that is used to bottom our 
     * of the conversion functions in Conversion.es).
     *)
    let 
        val obj = needObj (instantiateGlobalClass n [])
        val _ = trace ["finished building root builtin ", fmtName n]
    in
        Mach.Object (Mach.setMagic obj (SOME m))
    end

and newArray (vals:Mach.VAL list)
    : Mach.VAL = 
    let val a = instantiateGlobalClass Name.public_Array [newInt (Int32.fromInt (List.length vals))]
        fun init a _ [] = ()
          | init a k (x::xs) =
            (setValue a (Name.public (Int.toString k)) x ;
             init a (k+1) xs)
    in
        init (needObj a) 0 vals;
        a
    end

and newBuiltin (n:Ast.NAME) (m:Mach.MAGIC option) 
    : Mach.VAL =
    instantiateGlobalClass n [Mach.Object (Mach.setMagic (Mach.newObjNoTag()) m)]

and newDouble (n:Real64.real) 
    : Mach.VAL = 
    newBuiltin Name.public_double (SOME (Mach.Double n))

and newDecimal (n:Decimal.DEC) 
    : Mach.VAL = 
    newBuiltin Name.public_decimal (SOME (Mach.Decimal n))

and newInt (n:Int32.int) 
    : Mach.VAL = 
    newBuiltin Name.public_int (SOME (Mach.Int n))

and newUInt (n:Word32.word) 
    : Mach.VAL = 
    newBuiltin Name.public_uint (SOME (Mach.UInt n))

and newString (s:Ast.USTRING) 
    : Mach.VAL = 
    newBuiltin Name.public_string (SOME (Mach.String s))

and newByteArray (b:Word8Array.array) 
    : Mach.VAL = 
    newBuiltin Name.public_ByteArray (SOME (Mach.ByteArray b))

and newBoolean (b:bool) 
    : Mach.VAL = 
    newBuiltin Name.public_boolean (SOME (Mach.Boolean b))

and newNamespace (n:Ast.NAMESPACE) 
    : Mach.VAL =
    newRootBuiltin Name.public_Namespace (Mach.Namespace n)

and newClsClosure (env:Mach.SCOPE)
                  (cls:Ast.CLS)
    : Mach.CLS_CLOSURE =
    { cls = cls,
      (* FIXME: are all types bound? *)
      allTypesBound = true,
      env = env }
    
and newClass (e:Mach.SCOPE) 
             (cls:Ast.CLS) 
    : Mach.VAL =
    let
        val closure = newClsClosure e cls 
    in
        newRootBuiltin Name.public_Class (Mach.Class closure)
    end

and newFunClosure (e:Mach.SCOPE)
                  (f:Ast.FUNC)
                  (this:Mach.OBJ option)
    : Mach.FUN_CLOSURE = 
    let
        val Ast.Func { fsig, ... } = f
        val allTypesBound = (case fsig of 
                                 Ast.FunctionSignature { typeParams, ... } 
                                 => (length typeParams) = 0)
    in
        { func = f, 
          this = this,
          allTypesBound = allTypesBound,
          env = e }
    end

and newFunctionFromClosure (closure:Mach.FUN_CLOSURE) = 
    let 
        val { func, ... } = closure
        val Ast.Func { fsig, ... } = func
        val tag = Mach.FunctionTag fsig
        val res = newRootBuiltin Name.public_Function (Mach.Function closure)
        val _ = trace ["finding Function.prototype"]
        val globalFuncObj = needObj (getValue (getGlobalObject()) Name.public_Function)
        val globalFuncProto = getValue globalFuncObj Name.public_prototype
        val _ = trace ["building new prototype chained to Function.prototype"]
        val newProto = Mach.Object (Mach.setProto (newObj ()) globalFuncProto)
        val _ = trace ["built new prototype chained to Function.prototype"]
    in
        (* 
         * FIXME: modify the returned object to have the proper tag, a subtype
         * of Function. 
         *)
        setValue (needObj res) Name.public_prototype newProto;
        res
    end

and newFunctionFromFunc (e:Mach.SCOPE) 
                        (f:Ast.FUNC) 
    : Mach.VAL = 
    newFunctionFromClosure (newFunClosure e f NONE)

and newNativeFunction (f:Mach.NATIVE_FUNCTION) = 
    newRootBuiltin Name.public_Function (Mach.NativeFunction f)


(* An approximation of an invocation argument list, for debugging. *)
and callApprox (id:Ast.IDENT) (args:Mach.VAL list) 
    : string = 
    let
        fun approx arg = 
            if Mach.isString arg
            then "\"" ^ (toString arg) ^ "\""
            else toString arg
    in
        id ^ "(" ^ (join ", " (map approx args)) ^ ")"
    end

(* FIXME: this is not the correct toString *)

and magicToString (magic:Mach.MAGIC) 
    : string =
    case magic of 
        Mach.Double n => 
        if Real64.isFinite n andalso Real64.==(Real64.realFloor n, n)
        then LargeInt.toString (Real64.toLargeInt IEEEReal.TO_NEGINF n)
        else (if Real64.isNan n
              then "NaN"
              else (if Real64.==(Real64.posInf, n)
                    then "Infinity"
                    else (if Real64.==(Real64.negInf, n)
                          then "-Infinity"
                          else Real64.toString n)))

      | Mach.Decimal d => Decimal.toString d
      | Mach.Int i => Int32.toString i
      | Mach.UInt u => LargeInt.toString (Word32.toLargeInt u)
      | Mach.String s => s
      | Mach.Boolean true => "true"
      | Mach.Boolean false => "false"
      | Mach.Namespace (Ast.Private _) => "[private namespace]"
      | Mach.Namespace (Ast.Protected _) => "[protected namespace]"
      | Mach.Namespace Ast.Intrinsic => "[intrinsic namespace]"
      | Mach.Namespace Ast.OperatorNamespace => "[operator namespace]"
      | Mach.Namespace (Ast.Public id) => "[public namespace: " ^ id ^ "]"
      | Mach.Namespace (Ast.Internal _) => "[internal namespace]"
      | Mach.Namespace (Ast.UserNamespace id) => "[user-defined namespace " ^ id ^ "]"
      | Mach.Class _ => "[class Class]"
      | Mach.Interface _ => "[interface Interface]"
      | Mach.Function _ => "[function Function]"
      | Mach.Type _ => "[type Function]"
      | Mach.ByteArray _ => "[ByteArray]"
      | Mach.NativeFunction _ => "[function NativeFunction]"
      | _ => error ["Shouldn't happen: failed to match in Eval.magicToString."]

(* 
 * FIXME: want to transfer *some* of these up to Conversions.es, but it's
 * very easy to get into feedback loops if you do so. 
 *)

and toString (v:Mach.VAL) 
    : string = 
    ( trace ["toString"] ;
    case v of 
        Mach.Undef => "undefined"
      | Mach.Null => "null"
      | Mach.Object (Mach.Obj ob) => 
        case !(#magic ob) of 
            NONE => let val r = resolveOnObjAndPrototypes (Mach.Obj ob) 
                                                          { nss=[[Name.internalNS], [Name.publicNS]], id="toString" }
                    in
                        case r of 
                            NONE => "[Object object 0]"
                          | SOME (base, name) => 
                            let val meth = getValue base name
                            in
                                case meth of 
                                    Mach.Object metho => 
                                    let val res = Mach.Undef (* evalCallExpr (SOME (Mach.Obj ob)) metho [] *) (* Awaiting fix to "this" bug *)
                                    in
                                        case res of 
                                            Mach.Object (Mach.Obj ro) =>
                                            (case !(#magic ro) of
                                                 SOME (Mach.String s) => s
                                               | _ => "[Object object 1]")
                                          | _ => "[Object object 2]"
                                    end
                                  | Mach.Null => "[Object object 3.1]"
                                  | Mach.Undef => "[Object object 3.2]"
                            end
                    end
          | SOME magic => 
            magicToString magic)

and toBoolean (v:Mach.VAL) : bool = 
    case v of 
        Mach.Undef => false
      | Mach.Null => false
      | Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of 
             SOME (Mach.Boolean b) => b
           | SOME (Mach.Int x) => not (x = (Int32.fromInt 0))
           | SOME (Mach.UInt x) => not (x = (Word32.fromInt 0))
           | SOME (Mach.Double x) => not (Real64.==(x,(Real64.fromInt 0))
                                          orelse
                                          Real64.isNan x)
           | SOME (Mach.Decimal x) => not (x = Decimal.zero)
           | _ => true)

(* 
 * Arithmetic operations.
 *)

and toNumeric (v:Mach.VAL) 
    : Mach.VAL =          
    let 
        fun NaN _ = newDouble (Real64.posInf / Real64.posInf)
        fun zero _ = newDouble (Real64.fromInt 0)
        fun one _ = newDouble (Real64.fromInt 1)
    in
        case v of 
            Mach.Undef => NaN ()
          | Mach.Null => zero ()
          | Mach.Object (Mach.Obj ob) => 
            (case !(#magic ob) of 
                 SOME (Mach.Double _) => v
               | SOME (Mach.Decimal _) => v
               | SOME (Mach.Int _) => v
               | SOME (Mach.UInt _) => v
               | SOME (Mach.Boolean false) => zero ()
               | SOME (Mach.Boolean true) => one ()
               (* 
                * FIXME: This is not the correct definition of ToNumber applied to string.
                * See ES3 9.3.1. We need to talk it over.
                *) 
               | SOME (Mach.String s) => (case Real64.fromString s of
                                              SOME s' => newDouble s'
                                            | NONE => NaN ())
               (* 
                * FIXME: ES3 9.3 defines ToNumber on objects in terms of primitives. We've
                * reorganized the classification of primitives vs. objects. Revisit this.
                *)
               | _ => zero ())
    end
    

and toDecimal (precision:int) 
              (mode:Decimal.ROUNDING_MODE) 
              (v:Mach.VAL) 
    : Decimal.DEC = 
    case v of 
        Mach.Undef => Decimal.NaN
      | Mach.Null => Decimal.zero
      | Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of 
             SOME (Mach.Double d) => 
             (* NB: Lossy. *)
             (case Decimal.fromString precision mode (Real64.toString d) of
                  SOME d' => d'
                | NONE => Decimal.NaN)
           | SOME (Mach.Decimal d) => d
           | SOME (Mach.Int i) => Decimal.fromLargeInt (Int32.toLarge i)
           | SOME (Mach.UInt u) => Decimal.fromLargeInt (Word32.toLargeInt u)
           | SOME (Mach.Boolean false) => Decimal.zero
           | SOME (Mach.Boolean true) => Decimal.one
           (* 
            * FIXME: This is not the correct definition either. See toNumeric.
            *) 
           | SOME (Mach.String s) => (case Decimal.fromString precision mode s of
                                     SOME s' => s'
                                   | NONE => Decimal.NaN)
           (* 
            * FIXME: Possibly wrong here also. See comment in toNumeric.
            *)
           | _ => Decimal.zero)


and toDouble (v:Mach.VAL) 
    : Real64.real = 
    let 
        fun NaN _ = (Real64.posInf / Real64.posInf)
        fun zero _ = (Real64.fromInt 0)
        fun one _ = (Real64.fromInt 1)
    in            
        case v of 
            Mach.Undef => NaN ()
          | Mach.Null => zero ()
          | Mach.Object (Mach.Obj ob) => 
            (case !(#magic ob) of 
                 SOME (Mach.Double d) => d
               | SOME (Mach.Decimal d) => 
                 (* NB: Lossy. *)
                 (case Real64.fromString (Decimal.toString d) of
                      SOME d' => d'
                    | NONE => NaN ())
                 
               | SOME (Mach.Int i) => Real64.fromLargeInt (Int32.toLarge i)
               | SOME (Mach.UInt u) => Real64.fromLargeInt (Word32.toLargeInt u)
               | SOME (Mach.Boolean false) => zero ()
               | SOME (Mach.Boolean true) => one ()
               (* 
                * FIXME: This is not the correct definition either. See toNumeric.
                *) 
               | SOME (Mach.String s) => (case Real64.fromString s  of
                                              SOME s' => s'
                                            | NONE => NaN())
               (* 
                * FIXME: Possibly wrong here also. See comment in toNumeric.
                *)
               | _ => zero ())
    end


and isPositiveZero (v:Mach.VAL) 
    : bool =
    let 
        fun doubleIsPosZero x = 
            Real64.class x = IEEEReal.ZERO
            andalso not (Real64.signBit x)
    in        
        mathOp v 
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsPosZero)
               NONE NONE false
    end


and isNegativeZero (v:Mach.VAL) 
    : bool =
    let 
        fun doubleIsNegZero x = 
            Real64.class x = IEEEReal.ZERO
            andalso Real64.signBit x
    in        
        mathOp v 
               (SOME Decimal.isPositiveZero) 
               (SOME doubleIsNegZero)
               NONE NONE false
    end


and isPositiveInf (v:Mach.VAL) 
    : bool =
    let 
        fun doubleIsPosInf x = 
            Real64.class x = IEEEReal.INF
            andalso not (Real64.signBit x)
    in        
        mathOp v 
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsPosInf)
               NONE NONE false
    end


and isNegativeInf (v:Mach.VAL) 
    : bool =
    let 
        fun doubleIsNegInf x = 
            Real64.class x = IEEEReal.INF
            andalso Real64.signBit x
    in        
        mathOp v 
               (SOME Decimal.isPositiveZero)
               (SOME doubleIsNegInf)
               NONE NONE false
    end

    
and isNaN (v:Mach.VAL)
    : bool = 
    mathOp v 
           (SOME Decimal.isNaN) 
           (SOME Real64.isNan)
           NONE NONE false


and sign (v:Mach.VAL)
    : int = 
    let 
        (* 
         * FIXME: this implemented 'sign' function returns 1, 0, or -1
         * depending on proximity to 0. Some definitions only return 1 or 0,
         * or only return 1 or -1. Don't know which one the ES3 spec means.
         *)

        (* FIXME: should decimal rounding mode and precision used in sign-determination? *)
        fun decimalSign d = case Decimal.compare Decimal.defaultPrecision 
                                                 Decimal.defaultRoundingMode 
                                                 d Decimal.zero 
                             of 
                                LESS => ~1
                              | EQUAL => 0
                              | GREATER => 1

        fun uint32Sign u = if u = (Word32.fromInt 0)
                           then 0
                           else 1
    in
        mathOp v 
               (SOME decimalSign)
               (SOME Real64.sign)
               (SOME Int32.sign)
               (SOME uint32Sign)
               0
    end


and floor (v:Mach.VAL)
    : LargeInt.int =
    mathOp v 
           (SOME Decimal.floor)
           (SOME (Real64.toLargeInt IEEEReal.TO_NEGINF))
           (SOME Int32.toLarge)
           (SOME Word32.toLargeInt)
           (LargeInt.fromInt 0)
    

and signFloorAbs (v:Mach.VAL)
    : LargeInt.int =
    let
        val sign = Int.toLarge (sign v)
        val floor = floor v
    in
        LargeInt.*(sign, (LargeInt.abs floor))
    end
    

(* ES3 9.4 ToInteger 
 *
 * FIXME: If I understand the compatibility requirements
 * correctly, this should return an integral double. 
 * Not certain though. 
 *)

and toInteger (v:Mach.VAL)
    : Mach.VAL = 
    let
        val v' = toNumeric v
    in
        if isNaN v'
        then newDouble (Real64.fromInt 0)
        else (if (isPositiveInf v' orelse
                  isNegativeInf v' orelse
                  isPositiveZero v' orelse
                  isNegativeZero v')
              then v'
              else newDouble (Real64.fromLargeInt (signFloorAbs v')))
    end

(* ES3 9.5 ToInt32 *)

and toInt32 (v:Mach.VAL) 
    : Int32.int =
    let 
        val v' = toNumeric v
    in
        if (isNaN v' orelse
            isPositiveInf v' orelse
            isNegativeInf v' orelse
            isPositiveZero v' orelse
            isNegativeZero v')
        then Int32.fromInt 0
        else 
            let 
                val l31 = IntInf.pow (2, 31)
                val l32 = IntInf.pow (2, 32)
                val v'' = IntInf.mod (signFloorAbs v', l32)
            in
                Int32.fromLarge (if LargeInt.>= (v'', l31)
                                 then LargeInt.- (v'', l32)
                                 else v'')
            end
    end


(* ES3 9.6 ToUInt32 *)

and toUInt32 (v:Mach.VAL) 
    : Word32.word =
    let 
        val v' = toNumeric v
    in
        if (isNaN v' orelse
            isPositiveInf v' orelse
            isNegativeInf v' orelse
            isPositiveZero v' orelse
            isNegativeZero v')
        then Word32.fromInt 0
        else 
            let 
                val l32 = IntInf.pow (2, 32)
            in
                Word32.fromLargeInt (LargeInt.mod (signFloorAbs v', l32))
            end
    end

(* ES3 9.6 ToUInt16 *)

and toUInt16 (v:Mach.VAL) 
    : Word32.word =
    let 
        val v' = toNumeric v
    in
        if (isNaN v' orelse
            isPositiveInf v' orelse
            isNegativeInf v' orelse
            isPositiveZero v' orelse
            isNegativeZero v')
        then Word32.fromInt 0
        else 
            let 
                val l16 = IntInf.pow (2, 16)
            in
                Word32.fromLargeInt (LargeInt.mod (signFloorAbs v', l16))
            end
    end

    
and evalExpr (regs:Mach.REGS) 
             (expr:Ast.EXPR)
    : Mach.VAL =
    case expr of
        Ast.LiteralExpr lit => 
        evalLiteralExpr regs lit
        
      | Ast.ListExpr es =>
        evalListExpr regs es
        
      | Ast.LexicalRef { ident, pos } =>
        let 
            val _ = LogErr.setPos pos;
            val (obj, name) = evalRefExpr regs expr true
            val _ = LogErr.setPos pos;
        in
            getValue obj name
        end

      | Ast.ObjectRef { base, ident, pos } => 
        let 
            val _ = LogErr.setPos pos
            val (obj, name) = evalRefExpr regs expr false
            val _ = LogErr.setPos pos
        in
            getValue obj name
        end
        
      | Ast.LetExpr {defs, body, head} =>
        evalLetExpr regs (valOf head) body

      | Ast.TernaryExpr (Ast.Cond, aexpr, bexpr, cexpr) => 
        evalCondExpr regs aexpr bexpr cexpr

      | Ast.BinaryExpr (bop, aexpr, bexpr) => 
        evalBinaryOp regs bop aexpr bexpr

      | Ast.UnaryExpr (unop, expr) => 
        evalUnaryOp regs unop expr

      | Ast.ThisExpr => 
        let
            val { this, ... } = regs
        in
            Mach.Object this
        end

      | Ast.SetExpr (aop, pat, expr) => 
        evalSetExpr regs aop pat (evalExpr regs expr)

      | Ast.CallExpr { func, actuals } => 
        (* FIXME: order of evaluation: func must be evaluated first.  (Actuals must be
         * evaluated left-to-right, but map does this so we're fine.)
         *)
        let
            val args = map (evalExpr regs) actuals
        in
            case func of 
                Ast.LexicalRef _ => evalCallMethod regs func args
              | Ast.ObjectRef _ => evalCallMethod regs func args
              | _ => evalCallExpr (#this regs) (needObj (evalExpr regs func)) args
        end

      | Ast.NewExpr { obj, actuals } => 
        let
            val args = map (evalExpr regs) actuals
        in
            evalNewExpr (needObj (evalExpr regs obj)) args
        end

      | Ast.GetTemp n => 
        Mach.getTemp (getScopeTemps (#scope regs)) n

      | Ast.InitExpr (target,temps,inits) =>
        let
            val tempRegs = evalHead regs temps
        in
            evalScopeInits tempRegs target inits;
            Mach.Undef
        end

      | Ast.BinaryTypeExpr (typeOp, expr, tyExpr) => 
        evalBinaryTypeOp regs typeOp expr tyExpr

      | _ => LogErr.unimplError ["unhandled expression type"]


and evalLiteralArrayExpr (regs:Mach.REGS)
                         (exprs:Ast.EXPR list)
                         (ty:Ast.TYPE_EXPR option)
    : Mach.VAL =
    let 
        val vals = map (evalExpr regs) exprs
        val tys = case ty of 
                      NONE => [Ast.SpecialType Ast.Any]
                    | SOME (Ast.ArrayType tys) => tys
                    (* FIXME: hoist this to parsing or defn; don't use
                     * a full TYPE_EXPR in LiteralArray. *)
                    | SOME _ => error ["non-array type on array literal"]
        val tag = Mach.ArrayTag tys
        fun constructArray _ = 
            let 
                val arr = needObj (getValue (getGlobalObject ()) Name.public_Array)
                val Mach.Obj { magic, ... } = arr
            in
                case (!magic) of 
                    SOME (Mach.Class arrayClass) => needObj (constructClassInstance arr arrayClass [])
                  | _ => error ["Error in constructing array literal"]
            end
        val obj = constructArray ()
        val (Mach.Obj {props, ...}) = obj
        fun putVal n [] = n
          | putVal n (v::vs) = 
            let 
                val name = Name.public (Int.toString n)
                (* FIXME: this is probably incorrect wrt. Array typing rules. *)
                val ty = if n < (length tys) 
                         then List.nth (tys, n)
                         else (if (length tys) > 0
                               then List.last tys
                               else Ast.SpecialType Ast.Any)
                val prop = { ty = ty,
                             state = Mach.ValProp v,
                             attrs = { dontDelete = false,
                                       dontEnum = false,
                                       readOnly = false,
                                       isFixed = false } }
            in
                Mach.addProp props name prop;
                putVal (n+1) vs
            end
        val numProps = putVal 0 vals 
    in
        setValue obj Name.public_length (newUInt (Word32.fromInt numProps)) ;
        Mach.Object obj
    end

             
and evalLiteralObjectExpr (regs:Mach.REGS)
                          (fields:Ast.FIELD list)
                          (ty:Ast.TYPE_EXPR option)
    : Mach.VAL =
    let 
        fun searchFieldTypes n [] = Ast.SpecialType Ast.Any
          | searchFieldTypes n ({name,ty}::ts) = 
            if n = name 
            then ty
            else searchFieldTypes n ts
        val tys = case ty of 
                      NONE => []
                    | SOME (Ast.ObjectType tys) => tys
                    (* FIXME: hoist this to parsing or defn; don't use
                     * a full TYPE_EXPR in LiteralObject. *)
                    | SOME _ => error ["non-object type on object literal"]
        val tag = Mach.ObjectTag tys
        (* FIXME: hook up to Object.prototype. *)
        val obj = newObj ()
        val (Mach.Obj {props, ...}) = obj
        fun processField {kind, name, init} = 
            let 
                val const = case kind of 
                                Ast.Const => true
                              | Ast.LetConst => true
                              | _ => false
                val n = Name.public (#id (evalIdentExpr regs name))
                val v = evalExpr regs init
                val ty = searchFieldTypes (#id n) tys
                val prop = { ty = ty,
                             (* FIXME: handle virtuals *)
                             state = Mach.ValProp v,
                             attrs = { dontDelete = false,
                                       dontEnum = false,
                                       readOnly = const,
                                       isFixed = false } }
            in
                Mach.addProp props n prop
            end
    in
        List.app processField fields;
        Mach.Object obj
    end


and evalLiteralExpr (regs:Mach.REGS) 
                    (lit:Ast.LITERAL) 
    : Mach.VAL = 
    case lit of 
        Ast.LiteralNull => Mach.Null
      | Ast.LiteralUndefined => Mach.Undef
      | Ast.LiteralDouble r => newDouble r
      | Ast.LiteralDecimal d => newDecimal d
      | Ast.LiteralInt i => newInt i
      | Ast.LiteralUInt u => newUInt u
      | Ast.LiteralBoolean b => newBoolean b
      | Ast.LiteralString s => newString s
      | Ast.LiteralArray {exprs, ty} => evalLiteralArrayExpr regs exprs ty
      | Ast.LiteralObject {expr, ty} => evalLiteralObjectExpr regs expr ty
      | Ast.LiteralNamespace n => newNamespace n
      | Ast.LiteralFunction f => newFunctionFromFunc (#scope regs) f
      | Ast.LiteralContextualDecimal _ => error ["contextual decimal literal at runtime"]
      | Ast.LiteralContextualDecimalInteger _ => error ["contextual decimal integer literal at runtime"]
      | Ast.LiteralContextualHexInteger _ => error ["contextual hex integer literal at runtime"]

      | Ast.LiteralXML _ => LogErr.unimplError ["unhandled literal XML"]
      | Ast.LiteralRegExp re => LogErr.unimplError ["unhandled literal regexp ",(#str re)]


and evalListExpr (regs:Mach.REGS) 
                 (es:Ast.EXPR list) 
    : Mach.VAL = 
    case es of 
        [] => Mach.Undef
      | [e] => evalExpr regs e
      | (e::ez) => ((evalExpr regs e); (evalListExpr regs ez))

                   
and constructObjectViaFunction (ctorObj:Mach.OBJ) 
                               (ctor:Mach.FUN_CLOSURE) 
                               (args:Mach.VAL list) 
    : Mach.VAL = 
    case ctorObj of 
        Mach.Obj { props, ... } => 
        let
            (* FIXME: the default prototype should be the initial Object prototype, 
             * as per ES3 13.2.2, not the current Object prototype. *)
            val (proto:Mach.VAL) = 
                if Mach.hasProp props Name.public_prototype
                then getValue ctorObj Name.public_prototype
                else 
                    let
                        val globalObjectObj = needObj (getValue (getGlobalObject()) Name.public_Object)
                    in
                        getValue globalObjectObj Name.public_prototype
                    end
            val (newObj:Mach.OBJ) = Mach.setProto (newObj ()) proto 
        in
            case invokeFuncClosure newObj ctor args of 
                Mach.Object ob => Mach.Object ob
              | _ => Mach.Object newObj
        end


and evalNewExpr (obj:Mach.OBJ) 
                (args:Mach.VAL list) 
    : Mach.VAL =
    case obj of 
        Mach.Obj { magic, ... } => 
        case (!magic) of 
            SOME (Mach.Class c) => constructClassInstance obj c args
          | SOME (Mach.Function f) => constructObjectViaFunction obj f args
          | _ => error ["operator 'new' applied to unknown object"]


and evalCallMethod (regs:Mach.REGS) 
                   (func:Ast.EXPR)
                   (args:Mach.VAL list)
    : Mach.VAL = 
    let
        (* 
         * If we have a method or native function *property*, we can just 
         * call it directly without manufacturing a temporary Function 
         * wrapper object. 
         *)
        val _ = trace ["evaluating ref expr for call-method"];
        val (baseOpt, (obj, name)) = evalRefExprFull regs func true
        val thisObj = case baseOpt of 
                       NONE => (#this regs)
                     | SOME base => base
        val _ = trace [">>> call method: ", fmtName name]
        val Mach.Obj { props, ... } = obj
        val res = case (#state (Mach.getProp props name)) of
                      Mach.NativeFunctionProp nf => nf args
                    | Mach.MethodProp f => invokeFuncClosure thisObj f args
                    | _ => evalCallExpr thisObj (needObj (getValue obj name)) args
        val _ = trace ["<<< call method: ", fmtName name]
    in
        res
    end

                                
and evalCallExpr (thisObj:Mach.OBJ) 
                 (fobj:Mach.OBJ) 
                 (args:Mach.VAL list) 
    : Mach.VAL =
    case fobj of
        Mach.Obj { magic, ... } => 
        case !magic of 
            SOME (Mach.NativeFunction f) => 
            (trace ["entering native function"]; 
             f args)
          | SOME (Mach.Function f) => 
            (trace ["entering standard function"]; 
             invokeFuncClosure thisObj f args)
          | _ => 
            if hasValue fobj Name.meta_invoke
            then
                let 
                    val _ = trace ["redirecting through meta::invoke"]
                    val invokeFn = getValue fobj Name.meta_invoke
                in
                    evalCallExpr thisObj (needObj invokeFn) args
                end
            else error ["calling non-callable object"]


and evalSetExpr (regs:Mach.REGS) 
                (aop:Ast.ASSIGNOP) 
                (lhs:Ast.EXPR) 
                (v:Mach.VAL) 
    : Mach.VAL = 
    let
        val _ = trace ["evalSetExpr"]
        val (obj, name) = evalRefExpr regs lhs false
        val v =
            let 
                fun modifyWith bop = 
                    performBinop bop (getValue obj name) v
            in
                case aop of 
                    Ast.Assign => v
                  | Ast.AssignPlus mode => modifyWith (Ast.Plus mode)
                  | Ast.AssignMinus mode => modifyWith (Ast.Minus mode)
                  | Ast.AssignTimes mode => modifyWith (Ast.Times mode)
                  | Ast.AssignDivide mode => modifyWith (Ast.Divide mode)
                  | Ast.AssignRemainder mode => modifyWith (Ast.Remainder mode)
                  | Ast.AssignLeftShift => modifyWith Ast.LeftShift
                  | Ast.AssignRightShift => modifyWith Ast.RightShift
                  | Ast.AssignRightShiftUnsigned => modifyWith Ast.RightShiftUnsigned
                  | Ast.AssignBitwiseAnd => modifyWith Ast.BitwiseAnd
                  | Ast.AssignBitwiseOr => modifyWith Ast.BitwiseOr
                  | Ast.AssignBitwiseXor => modifyWith Ast.BitwiseXor
                  | Ast.AssignLogicalAnd => modifyWith Ast.LogicalAnd
                  | Ast.AssignLogicalOr => modifyWith Ast.LogicalOr
            end
    in
        trace ["setExpr assignment to slot ", fmtName name];
        setValue obj name v;
        v
    end
    

and evalUnaryOp (regs:Mach.REGS) 
                (unop:Ast.UNOP) 
                (expr:Ast.EXPR) 
    : Mach.VAL =
    let
        fun crement (mode:Ast.NUMERIC_MODE) decimalOp doubleOp intOp uintOp isPre = 
            let 
                val _ = trace ["performing crement"]
                val (obj, name) = evalRefExpr regs expr false
                val v = getValue obj name

                fun asDecimal _ =                         
                    let 
                        val vd = toDecimal (#precision mode) (#roundingMode mode) v
                        val one = valOf (Decimal.fromStringDefault "1")
                    in
                        (newDecimal vd, 
                         newDecimal (decimalOp 
                                              (#precision mode) 
                                              (#roundingMode mode) 
                                              vd one))
                    end

                fun asDouble _ = 
                    let
                        val vd = toDouble v
                        val one = Real64.fromInt 1
                    in
                        (newDouble vd, newDouble (doubleOp (vd, one)))
                    end

                fun asInt _ = 
                    let
                        val vi = toInt32 v
                        val one = Int32.fromInt 1
                    in
                        (newInt vi, newInt (intOp (vi, one)))
                    end

                fun asUInt _ = 
                    let
                        val vu = toUInt32 v
                        val one = Word32.fromInt 1
                    in
                        (newUInt vu, newUInt (uintOp (vu, one)))
                    end
                    
                val (n, n') = 
                    case (#numberType mode) of 
                        Ast.Decimal => asDecimal ()         
                      | Ast.Double => asDouble ()
                      | Ast.Int => asInt ()
                      | Ast.UInt => asUInt ()
                      | Ast.Number => 
                        (* 
                         * FIXME: not clear in the spec what conversions to perform here. 
                         * Draft spec language says "converted to a number". Defaulting to
                         * "calling toNumeric but otherwise keeping the value in whatever 
                         * type it is before crement".
                         *)                        
                        (case Mach.needMagic (toNumeric v) of
                             Mach.Decimal _ => asDecimal ()
                           | Mach.Double _ => asDouble ()
                           | Mach.Int _ => asInt ()
                           | Mach.UInt _ => asUInt ()
                           | _ => error ["non-numeric operand to crement operation"])
            in
                setValue obj name n'; 
                if isPre then n' else n
            end
    in
        case unop of 
            Ast.Delete =>             
            (trace ["performing operator delete"];
             case evalRefExpr regs expr false of
                 (Mach.Obj {props, ...}, name) => 
                 (if (#dontDelete (#attrs (Mach.getProp props name)))
                  then newBoolean false
                  else (Mach.delProp props name; newBoolean true)))

          | Ast.PreIncrement mode => crement (valOf mode)
                                             (Decimal.add) 
                                             (Real64.+) 
                                             (Int32.+) 
                                             (Word32.+) 
                                             true

          | Ast.PreDecrement mode => crement (valOf mode)
                                             (Decimal.subtract) 
                                             (Real64.-) 
                                             (Int32.-) 
                                             (Word32.-) 
                                             true

          | Ast.PostIncrement mode => crement (valOf mode)
                                              (Decimal.add) 
                                              (Real64.+) 
                                              (Int32.+) 
                                              (Word32.+) 
                                              false

          | Ast.PostDecrement mode => crement (valOf mode)
                                              (Decimal.subtract) 
                                              (Real64.-) 
                                              (Int32.-) 
                                              (Word32.-) 
                                              false

          | Ast.BitwiseNot => 
            newInt (Int32.fromLarge 
                         (Word32.toLargeInt 
                              (Word32.notb 
                                   (toUInt32 
                                        (evalExpr regs expr)))))

          | Ast.LogicalNot => 
            newBoolean (not (toBoolean (evalExpr regs expr)))

          | Ast.UnaryPlus mode => 
            let 
                val v = evalExpr regs expr
                val mode = valOf mode
            in
                case (#numberType mode) of 
                    Ast.Decimal => newDecimal (toDecimal (#precision mode) 
                                                                   (#roundingMode mode) 
                                                                   v)
                  | Ast.Double => newDouble (toDouble v)
                  | Ast.Int => newInt (toInt32 v)
                  | Ast.UInt => newUInt (toUInt32 v)
                  | Ast.Number => toNumeric v
            end

          | Ast.UnaryMinus mode => 
            let 
                val v = evalExpr regs expr
                val mode = valOf mode
                fun asDecimal _ = newDecimal (Decimal.minus 
                                                       (#precision mode) 
                                                       (#roundingMode mode) 
                                                       (toDecimal (#precision mode)
                                                                       (#roundingMode mode)
                                                                       v))
                fun asDouble _ = newDouble (Real64.~ (toDouble v))
                fun asInt _ = newInt (Int32.~ (toInt32 v))
                fun asUInt _ = newUInt (Word32.~ (toUInt32 v))
            in
                case (#numberType mode) of 
                    Ast.Decimal => asDecimal ()
                  | Ast.Double => asDouble ()
                  | Ast.Int => asInt ()
                  | Ast.UInt => asUInt ()
                  | Ast.Number => 
                    (case Mach.needMagic (toNumeric v) of 
                         Mach.Decimal _ => asDecimal ()
                       | Mach.Double _ => asDouble ()
                       (* 
                        * FIXME: The "numbers" proposal says that when there's no pragma in effect,
                        * unary minus on int and uint stays in the representation if it can be 
                        * performed without loss of precision, else it's converted to double.
                        *
                        * It's not clear to me what "loss of precision" means on unary minus applied
                        * to twos complement numbers, since negation is just "flip bits and add 1". 
                        * Maybe it has to do with overflow? Until this is clarified I'll leave it 
                        * as "preserve the representation" in all cases.
                        *)
                       | Mach.Int _ => asInt ()
                       | Mach.UInt _ => asUInt ()
                       | _ => asDouble ())
            end

          | Ast.Void => Mach.Undef

          | Ast.Type => 
            (* 
             * FIXME: not clear what this operator does; I thought it just 
             * affected parse context. 
             *)
            evalExpr regs expr

          | Ast.Typeof => 
            (* 
             * ES3 1.4.3 backward-compatibility operation.
             *)
            let
                fun typeOfVal (v:Mach.VAL) = 
                    case v of 
                        Mach.Null => "null"
                      | Mach.Undef => "undefined"
                      | Mach.Object (Mach.Obj ob) => 
                        let 
                            val n = Mach.nominalBaseOfTag (#tag ob)
                        in
                            if n = Name.public_int orelse 
                               n = Name.public_uint orelse 
                               n = Name.public_double orelse 
                               n = Name.public_decimal
                            then "number"
                            else 
                                (if n = Name.public_boolean
                                 then "boolean"
                                 else 
                                     (if n = Name.public_Function
                                      then "function"
                                      else 
                                          (if n = Name.public_string
                                           then "string"
                                           else "object")))
                        end
            in
                newString 
                    (case expr of 
                         Ast.LexicalRef { ident, pos } => 
                         let 
                             val _ = LogErr.setPos pos
                             val multiname = evalIdentExpr regs ident
                         in
                             case resolveOnScopeChain (#scope regs) multiname of 
                                 NONE => "undefined"
                               | SOME (obj, name) => typeOfVal (getValue obj name)
                         end
                       | _ => typeOfVal (evalExpr regs expr))
            end
    end


and performBinop (bop:Ast.BINOP) 
                 (a:Mach.VAL) 
                 (b:Mach.VAL) 
    : Mach.VAL = 

    let
        fun stringConcat _ = 
            newString ((toString a) ^ (toString b))

        fun dispatch (mode:Ast.NUMERIC_MODE) decimalOp doubleOp intOp uintOp largeOp =
            case (#numberType mode) of 
                Ast.Decimal => decimalOp (toDecimal 
                                              (#precision mode) 
                                              (#roundingMode mode) a) 
                                         (toDecimal 
                                              (#precision mode) 
                                              (#roundingMode mode) b)
              | Ast.Double => doubleOp (toDouble a) (toDouble b)
              | Ast.Int => intOp (toInt32 a) (toInt32 b)
              | Ast.UInt => uintOp (toUInt32 a) (toUInt32 b)
                            
              | Ast.Number => 
                (* 
                 * Ast.Number implies magic operand-based dispatch. 
                 * FIXME: Refactor this if you can figure out how. 
                 *)
                    
                if Mach.isDecimal a orelse Mach.isDecimal b
                then decimalOp (toDecimal
                                    (#precision mode) 
                                    (#roundingMode mode) a)
                               (toDecimal 
                                    (#precision mode) 
                                    (#roundingMode mode) b)
                else 
                    (if Mach.isDouble a orelse Mach.isDouble b
                     then 
                         (trace ["dynamic dispatch as double op"];
                          doubleOp (toDouble a) (toDouble b))
                     else
                         let
                             fun isIntegral x = Mach.isUInt x orelse Mach.isInt x
                             fun enlarge x = if Mach.isUInt x 
                                             then Word32.toLargeInt (toUInt32 x)
                                             else Int32.toLarge (toInt32 x)
                         in
                             if isIntegral a andalso isIntegral b
                             then 
                                 (trace ["dynamic dispatch as large op"];
                                  largeOp (enlarge a) (enlarge b))
                             else
                                 (trace ["dynamic dispatch as double op"];
                                  doubleOp (toDouble a) (toDouble b))
                         end)
                    
        fun reorder (ord:order) : IEEEReal.real_order =
            case ord of
                EQUAL => IEEEReal.EQUAL
              | LESS => IEEEReal.LESS
              | GREATER => IEEEReal.GREATER

        fun dispatchComparison mode cmp =
            let
                fun decimalOp da db =
                    newBoolean (cmp (reorder (Decimal.compare (#precision mode) 
                                                              (#roundingMode mode) 
                                                              da
                                                              db)))
                fun doubleOp da db = 
                    newBoolean (cmp (Real64.compareReal (da, db)))
                fun intOp ia ib = 
                    newBoolean (cmp (reorder (Int32.compare (ia, ib))))
                fun uintOp ua ub = 
                    newBoolean (cmp (reorder (Word32.compare (ua, ub))))
                fun largeOp la lb = 
                    newBoolean (cmp (reorder (LargeInt.compare (la, lb))))
            in
                if Mach.isNumeric a andalso Mach.isNumeric b
                then (if isNaN a orelse isNaN b
                      then newBoolean (cmp IEEEReal.UNORDERED)
                      else dispatch mode decimalOp doubleOp intOp uintOp largeOp)
                else newBoolean (cmp (reorder (String.compare ((toString a), 
                                                               (toString b)))))
            end
            
        fun dispatchNumeric mode decimalFn doubleFn intFn uintFn largeFn =
            let
                fun decimalOp da db =
                    newDecimal (decimalFn (#precision mode) (#roundingMode mode) da db)
                fun doubleOp da db = 
                    newDouble (doubleFn (da, db))
                fun intOp ia ib = 
                    newInt (intFn (ia, ib))
                fun uintOp ua ub = 
                    newUInt (uintFn (ua, ub))
                fun largeOp la lb = 
                    let 
                        val x = largeFn (la, lb)
                    in
                        if Mach.fitsInInt x
                        then newInt (Int32.fromLarge x)
                        else (if Mach.fitsInUInt x
                              then newUInt (Word32.fromLargeInt x)
                              else (case Real64.fromString (LargeInt.toString x) of 
                                        SOME d => newDouble d
                                      | NONE => (case Decimal.fromStringDefault (LargeInt.toString x) of
                                                     SOME d => newDecimal d
                                                   | NONE => error ["arithmetic overflow"])))
                    end                                 
            in
                dispatch mode decimalOp doubleOp intOp uintOp largeOp
            end            

        fun masku5 (x:Word32.word) : Word.word = 
            Word.fromInt (Word32.toInt (Word32.andb (x, (valOf (Word32.fromString "0x1F")))))

        fun i2u (x:Int32.int) : Word32.word = 
            Word32.fromLargeInt (Int32.toLarge x)

        fun u2i (x:Word32.word) : Int32.int = 
            Int32.fromLarge (Word32.toLargeInt x)

        fun bitwiseWordOp f = 
            newUInt (f ((toUInt32 a),
                             (toUInt32 b)))

        fun pickRepByA (x:Word32.word) = 
            if Mach.isUInt a
            then newUInt x
            else newInt (u2i x)

        fun tripleEquals mode = 
            case (a, b) of 
                (Mach.Null, Mach.Null) => newBoolean true
              | (Mach.Undef, Mach.Undef) => newBoolean true
              | (Mach.Object oa, Mach.Object ob) => 
                if Mach.hasMagic oa andalso 
                   Mach.hasMagic ob 
                then                     
                    dispatchComparison (valOf mode) 
                                       (fn x => x = IEEEReal.EQUAL)
                else
                    newBoolean ((getObjId oa) = (getObjId ob))
              | (_, _) => newBoolean false


        val binOpName = 
            case bop of 
                Ast.Plus _ => "+"
              | Ast.Minus _ => "-"
              | Ast.Times _ => "*"
              | Ast.Divide _ => "/"
              | Ast.Remainder _ => "%"
              | Ast.LeftShift => "<<"
              | Ast.RightShift => ">>"
              | Ast.RightShiftUnsigned => ">>>"
              | Ast.BitwiseAnd => "&"
              | Ast.BitwiseOr => "|"
              | Ast.BitwiseXor => "^"
              | Ast.LogicalAnd => "&&"
              | Ast.LogicalOr => "||"
              | Ast.InstanceOf => "instanceof"
              | Ast.In => "in"
              | Ast.Equals _ => "=="
              | Ast.NotEquals _ => "!="
              | Ast.StrictEquals _ => "==="
              | Ast.StrictNotEquals _ => "!=="
              | Ast.Less _ => "<"
              | Ast.LessOrEqual _ => "<="
              | Ast.Greater _ => ">"
              | Ast.GreaterOrEqual _ => ">="
              | Ast.Comma => ","
                

        val _ = trace ["binop ", toString a, " ", binOpName, " ", toString b];
                          
    in
        case bop of
            Ast.Plus mode => 
            if Mach.isString a orelse
               Mach.isString b
            then stringConcat ()
            else dispatchNumeric ( valOf mode ) 
                                 ( Decimal.add )
                                 ( Real64.+ )
                                 ( Int32.+ )
                                 ( Word32.+ )
                                 ( LargeInt.+ )
                                              
          | Ast.Minus mode => 
            dispatchNumeric ( valOf mode ) 
                            ( Decimal.subtract )
                            ( Real64.- )
                            ( Int32.- )
                            ( Word32.- )
                            ( LargeInt.- )

          | Ast.Times mode => 
            dispatchNumeric (valOf mode) 
                            ( Decimal.multiply )
                            ( Real64.* )
                            ( Int32.* )
                            ( Word32.* )
                            ( LargeInt.* )

          | Ast.Divide mode => 
            dispatchNumeric ( valOf mode ) 
                            ( Decimal.divide )
                            ( Real64./ )
                            ( Int32.div )
                            ( Word32.div )
                            ( LargeInt.div )

          | Ast.Remainder mode => 
            dispatchNumeric ( valOf mode ) 
                            ( Decimal.remainder )
                            ( Real64.rem )
                            ( Int32.mod )
                            ( Word32.mod )
                            ( LargeInt.mod )

          | Ast.LeftShift => 
            pickRepByA (Word32.<< ((i2u (toInt32 a)),
                                   (masku5 (toUInt32 b))))

          | Ast.RightShift => 
            pickRepByA (Word32.>> ((i2u (toInt32 a)),
                                   (masku5 (toUInt32 b))))

          | Ast.RightShiftUnsigned => 
            pickRepByA (Word32.~>> ((toUInt32 a),
                                    (masku5 (toUInt32 b))))

          (* FIXME: should we return int if we do int|int or int&int ? *)
          | Ast.BitwiseAnd => bitwiseWordOp (Word32.andb)
          | Ast.BitwiseOr => bitwiseWordOp (Word32.orb)
          | Ast.BitwiseXor => bitwiseWordOp (Word32.xorb)

          | Ast.Equals mode => 
            dispatchComparison (valOf mode) 
                               (fn x => x = IEEEReal.EQUAL)
            
          | Ast.NotEquals mode => 
            dispatchComparison (valOf mode) 
                               (fn x => not (x = IEEEReal.EQUAL))
            
          | Ast.StrictEquals mode => 
            tripleEquals mode

          | Ast.StrictNotEquals mode => 
            newBoolean (not (toBoolean (tripleEquals mode))) 

          | Ast.Less mode => 
            dispatchComparison (valOf mode) 
                               (fn x => x = IEEEReal.LESS)

          | Ast.LessOrEqual mode => 
            dispatchComparison (valOf mode) 
                               (fn x => (x = IEEEReal.LESS) orelse (x = IEEEReal.EQUAL))

          | Ast.Greater mode => 
            dispatchComparison (valOf mode) 
                               (fn x => x = IEEEReal.GREATER)

          | Ast.GreaterOrEqual mode => 
            dispatchComparison (valOf mode) 
                               (fn x => (x = IEEEReal.GREATER) orelse (x = IEEEReal.EQUAL))

          | _ => error ["unexpected binary operator in performBinOp"]
    end


and doubleEquals (mode:Ast.NUMERIC_MODE option)
                 (a:Mach.VAL) 
                 (b:Mach.VAL)                  
  : bool = 
    toBoolean (performBinop (Ast.Equals mode) a b)


(*
and hasInstance (ob:OBJ)
                (v:VAL)
    : bool = 
    let 
        val Obj { magic, ... } = ob
        fun functionHasInstance _ = 
            case v of 
                Object ob => 
                if hasValue ob Name.public_prototype
                else 
                    let 
                        val proto = getValue ob Name.public_prototype
                    in
                        if Mach.isObject proto
                        then tripleEquals ...
                
            if not isObject v
            then false
            else 
    in
        case !magic of 
            Function => isFunction v orelse isNativeFunction v
          | NativeFunction => isFunction v orelse isNativeFunction v
          | _ => false
    end
*)

and evalBinaryTypeOp (regs:Mach.REGS)
                     (bop:Ast.BINTYPEOP)
                     (expr:Ast.EXPR)
                     (tyExpr:Ast.TYPE_EXPR) 
    : Mach.VAL = 
    case bop of 
        Ast.Cast => error ["unimplemented: operator 'cast'"]
      | Ast.To => error ["unimplemented: operator 'to'"]
      | Ast.Is => 
        (* 
         * FIXME: hook up to verifier when it's ready. At the moment
         * this is a fantastically hackish definition.
         *)
        let 
            val v = evalExpr regs expr
            val _ = trace ["processing 'is' operator"]
        in            
            case tyExpr of 
                Ast.TypeName (Ast.Identifier { ident, ... }) => 
                (case ident of 
                     "double" => newBoolean (Mach.isDouble v)
                   | "decimal" => newBoolean (Mach.isDecimal v)
                   | "int" => newBoolean (Mach.isInt v)
                   | "uint" => newBoolean (Mach.isUInt v)
                   | "String" => newBoolean (Mach.isString v)
                   | "Boolean" => newBoolean (Mach.isBoolean v)
                   | n => 
                     (case v of 
                          Mach.Undef => newBoolean false
                        | Mach.Null => newBoolean true
                        | Mach.Object (Mach.Obj { tag, ... }) => 
                          (case tag of 
                               Mach.ObjectTag _ => newBoolean (n = "Object")
                             | Mach.ArrayTag _ => newBoolean (n = "Array")
                             | Mach.FunctionTag _ => newBoolean (n = "Function")
                             | Mach.ClassTag {id, ...} => (trace ["operator 'is' on object of class ", id]; 
                                                           newBoolean (n = id))
                             | Mach.NoTag => newBoolean false)))
              | _ => error ["operator 'is' on unknown type expression"]
        end
        

and evalBinaryOp (regs:Mach.REGS) 
                 (bop:Ast.BINOP) 
                 (aexpr:Ast.EXPR) 
                 (bexpr:Ast.EXPR) 
    : Mach.VAL =
    case bop of 
        Ast.LogicalAnd => 
        let 
            val a = evalExpr regs aexpr
        in
            if toBoolean a
            then evalExpr regs bexpr
            else a
        end
        
      | Ast.LogicalOr => 
        let 
            val a = evalExpr regs aexpr
        in
            if toBoolean a
            then a
            else evalExpr regs bexpr
        end

      | Ast.Comma => (evalExpr regs aexpr;
                      evalExpr regs bexpr)

      | Ast.InstanceOf => 
        let 
            val a = evalExpr regs aexpr
            val b = evalExpr regs bexpr
        in
            case b of 
                Mach.Object (ob) =>
                newBoolean true (* FIXME: (hasInstance ob b) *)
              | _ => raise ThrowException (newByGlobalName regs Name.public_TypeError)
        end

      | Ast.In =>
        let 
            val a = evalExpr regs aexpr
            val astr = toString a
            val aname = Name.public astr
            val b = evalExpr regs bexpr
        in
            case b of 
                Mach.Object (Mach.Obj {props, ...}) =>
                newBoolean (Mach.hasProp props aname)
              | _ => raise ThrowException (newByGlobalName regs Name.public_TypeError)
                     
        end
        
      | _ => performBinop bop 
                          (evalExpr regs aexpr) 
                          (evalExpr regs bexpr)


and evalCondExpr (regs:Mach.REGS) 
                 (cond:Ast.EXPR) 
                 (thn:Ast.EXPR) 
                 (els:Ast.EXPR) 
    : Mach.VAL = 
    let 
        val v = evalExpr regs cond
        val b = toBoolean v
    in
        if b 
        then evalExpr regs thn
        else evalExpr regs els
    end
    

and evalExprToNamespace (regs:Mach.REGS) 
                        (expr:Ast.EXPR)
    : Ast.NAMESPACE = 
    (* 
     * If we have a namespace property we can evaluate to an Ast.Namespace 
     * directly without manufacturing a temporary Namespace wrapper object.
     *)
    let 
        fun evalRefNamespace _ = 
            let 
                val _ = trace ["evaluating ref to namespace"];
                val (obj, name) = evalRefExpr regs expr true
                val Mach.Obj { props, ... } = obj
            in
                case (#state (Mach.getProp props name)) of
                    Mach.NamespaceProp ns => ns
                  | _ => needNamespace (getValue obj name)
            end
    in
        case expr of 
            Ast.LexicalRef _ => evalRefNamespace ()
          | Ast.ObjectRef _ => evalRefNamespace ()
          | _ => needNamespace (evalExpr regs expr)
    end


and evalIdentExpr (regs:Mach.REGS) 
                  (r:Ast.IDENT_EXPR) 
    : Ast.MULTINAME = 
    case r of 
        Ast.Identifier { ident, openNamespaces } => 
        { nss=openNamespaces, id=ident }
        
      | Ast.QualifiedIdentifier { qual, ident } => 
        { nss = [[evalExprToNamespace regs qual]], id = ident }

      | Ast.QualifiedExpression { qual, expr } => 
        { nss = [[evalExprToNamespace regs qual]], 
          id = toString (evalExpr regs expr) }
        
      | Ast.ExpressionIdentifier { expr, openNamespaces } =>
        { nss = openNamespaces,
          id = toString (evalExpr regs expr) }

      | _ => LogErr.unimplError ["unimplemented identifier expression form"]


and evalRefExpr (regs:Mach.REGS)
                (expr:Ast.EXPR)
                (errIfNotFound:bool)
    : REF = 
    let
        val (_, r) = evalRefExprFull regs expr errIfNotFound
    in
        r
    end

and evalRefExprFull (regs:Mach.REGS)
                    (expr:Ast.EXPR)
                    (errIfNotFound:bool)
    : (Mach.OBJ option * REF) =
    let 
        val (base,ident) =
            case expr of         
                Ast.LexicalRef { ident, pos } => 
                (LogErr.setPos pos; 
                 (NONE,ident))

              | Ast.ObjectRef { base, ident, pos } => 
                (LogErr.setPos pos; 
                 (SOME (evalExpr regs base), ident))

              | _ => error ["need lexical or object-reference expression"]
                     
        val defaultObj = case base of 
                             SOME (Mach.Object ob) => ob
                           | _ => getGlobalObject ()

        val baseObj = case base of
                          SOME (Mach.Object ob) => SOME ob
                        | _ => NONE
                                       
        (* FIXME: ns might be user settable default *)
        fun makeRefNotFound (b:Mach.VAL option) (mname:Ast.MULTINAME) 
            : REF = (defaultObj, (Name.internal (#id mname)))

        val (multiname:Ast.MULTINAME) = evalIdentExpr regs ident

        val refOpt = 
            case base of 
                SOME (Mach.Object ob) => 
                resolveOnObjAndPrototypes ob multiname
              | NONE => 
                resolveOnScopeChain (#scope regs) multiname
              | SOME Mach.Undef => error ["ref expression on undef value with ",fmtMultiname multiname]
              | SOME Mach.Null => error ["ref expression on null value with ",fmtMultiname multiname]
                                                                             
    in
        (baseObj, (case refOpt of 
                       NONE => if errIfNotFound 
                               then error ["unresolved identifier expression",
                                           fmtMultiname multiname]
                               else makeRefNotFound base multiname
                     | SOME r' => r'))
    end

(*
    EXPR = LetExpr
*)
and evalLetExpr (regs:Mach.REGS) 
                (head:Ast.HEAD) 
                (body:Ast.EXPR) 
    : Mach.VAL = 
    let 
        val letRegs = evalHead regs head
    in
        evalExpr letRegs body
    end

and resolveOnScopeChain (scope:Mach.SCOPE) 
                        (mname:Ast.MULTINAME) 
    : REF option =
    let 
        val _ = trace ["resolving multiname on scope chain: ", 
                              fmtMultiname mname]
        fun getScopeParent (Mach.Scope { parent, ... }) = parent
        fun matchFixedScopeBinding (Mach.Scope {object=Mach.Obj {props, ...}, ... }, n, nss)
            = Mach.matchProps true props n nss
    in
        (* 
         * First do a fixed-properties-only lookup along the scope chain alone. 
         *)
        case Multiname.resolve 
                 mname scope 
                 matchFixedScopeBinding
                 getScopeParent
         of
            SOME (Mach.Scope {object, ...}, name) => 
            (trace ["found ",fmtName name]; SOME (object, name))
          | NONE => 
            (* 
             * If that fails, do a sequence of dynamic-property-permitted
             * lookups on every scope object (and along its prototype chain)
             * in the scope chain. 
             *)
            let
                fun tryProtoChain (Mach.Scope {object, parent, ...}) = 
                    case resolveOnObjAndPrototypes object mname of
                        SOME result => SOME result
                      | NONE => case parent of 
                                    NONE => NONE
                                  | SOME p => tryProtoChain p
            in
                tryProtoChain scope
            end
    end


and resolveOnObjAndPrototypes (obj:Mach.OBJ) 
                              (mname:Ast.MULTINAME) 
    : REF option = 
    let 
        fun matchFixedBinding (Mach.Obj {props, ...}, n, nss)
            = Mach.matchProps true props n nss
        fun matchBinding (Mach.Obj {props, ...}, n, nss)
            = Mach.matchProps false props n nss
        fun getObjProto (Mach.Obj {proto, ...}) = 
            case (!proto) of 
                Mach.Object ob => SOME ob
              | _ => NONE
    in
        trace ["resolveOnObjAndPrototypes: ", fmtMultiname mname];
        case Multiname.resolve mname obj matchFixedBinding getObjProto of
            NONE => Multiname.resolve mname obj matchBinding getObjProto
          | refOpt => refOpt
    end

     
and evalTailCallExpr (regs:Mach.REGS) 
                     (e:Ast.EXPR) 
    : Mach.VAL = 
    raise (TailCallException (fn _ => evalExpr regs e))


and evalNonTailCallExpr (regs:Mach.REGS) 
                        (e:Ast.EXPR) 
    : Mach.VAL = 
    evalExpr regs e
    handle TailCallException thunk => thunk ()
         | ReturnException v => v


and labelMatch (stmtLabels:Ast.IDENT list) 
            (exnLabel:Ast.IDENT option) 
    : bool = 
    case (stmtLabels, exnLabel) of
        (sl::[], SOME el) => sl = el
      | (sl::[], NONE) => sl = ""
      | ([], NONE) => false                   (* FIXME: refactor *)
      | ([], SOME el) => false
      | (sl::sls,NONE) => 
            if sl = ""
            then true
            else labelMatch sls exnLabel
      | (sl::sls,SOME el) => 
            if sl = el
            then true
            else labelMatch sls exnLabel


and evalStmts (regs:Mach.REGS)    
              (stmts:Ast.STMT list) 
    : Mach.VAL = 
    case stmts of 
        [s] => evalStmt regs s
      | (s::ss) => (evalStmt regs s; 
                    evalStmts regs ss) 
                (* FIXME: need to keep the last non-empty value and fixup abrupt completions here *)
      | [] => Mach.Undef


and evalStmt (regs:Mach.REGS) 
             (stmt:Ast.STMT) 
    : Mach.VAL = 
    case stmt of 
        Ast.ExprStmt e => evalExpr regs e
      | Ast.IfStmt {cnd,thn,els} => evalIfStmt regs cnd thn els
      | Ast.WhileStmt w => evalWhileStmt regs w
      | Ast.DoWhileStmt w => evalDoWhileStmt regs w
      | Ast.WithStmt { obj, ty, body } => evalWithStmt regs obj ty body
      | Ast.SwitchStmt { mode, cond, cases, labels } => 
        evalSwitchStmt regs mode cond cases labels
      | Ast.ForStmt w => evalForStmt regs w
      | Ast.ReturnStmt r => evalReturnStmt regs r
      | Ast.BreakStmt lbl => evalBreakStmt regs lbl
      | Ast.ContinueStmt lbl => evalContinueStmt regs lbl
      | Ast.ThrowStmt t => evalThrowStmt regs t
      | Ast.LabeledStmt (lab, s) => evalLabelStmt regs lab s
      | Ast.BlockStmt b => evalBlock regs b
      | Ast.ClassBlock c => evalClassBlock regs c
      | Ast.LetStmt b => evalBlock regs b
      | Ast.EmptyStmt => Mach.Undef
      | Ast.TryStmt { block, catches, finally } => 
        evalTryStmt regs block catches finally
      | Ast.SwitchTypeStmt { cond, ty, cases } => 
        LogErr.unimplError ["switch-type statements not handled"]
      | Ast.ForInStmt w => evalForInStmt regs w
      | _ => error ["Shouldn't happen: failed to match in Eval.evalStmt."]


and multinameOf (n:Ast.NAME) = 
    { nss = [[(#ns n)]], id = (#id n) }


and findVal (scope:Mach.SCOPE) 
            (mn:Ast.MULTINAME) 
    : Mach.VAL = 
    case resolveOnScopeChain scope mn of 
        NONE => error ["unable to resolve multiname: ", 
                       fmtMultiname mn ]
      | SOME (obj, name) => getValue obj name 


and checkAllPropertiesInitialized (obj:Mach.OBJ)
    : unit = 
    let 
        fun checkOne (n:Ast.NAME, p:Mach.PROP) = 
            case (#state p) of 
                Mach.UninitProp => error ["uninitialized property: ", 
                                                     fmtName n]
              | _ => ()
    in
        case obj of 
            Mach.Obj { props, ... } => 
            List.app checkOne (!props)
    end


and invokeFuncClosure (callerThis:Mach.OBJ) 
                      (closure:Mach.FUN_CLOSURE) 
                      (args:Mach.VAL list) 
    : Mach.VAL =
    let
        val { func, this, env, allTypesBound } = closure
        val Ast.Func { name, fsig, block, param=(paramFixtures, paramInits), ... } = func
        val this = case this of 
                       SOME t => t
                     | NONE => callerThis
        val regs = { this = this, scope = env }
    in
        if not allTypesBound
        then error ["invoking function with unbound type variables"]
        else
            let 
                val id = (#ident name)
                val strname = case (#kind name) of 
                                  Ast.Ordinary => id 
                                | Ast.Operator => "operator " ^ id
                                | Ast.Get => "get " ^ id
                                | Ast.Set => "set " ^ id
                                | Ast.Call => "call " ^ id
                                | Ast.Has => "has " ^ id

                val _ = push [callApprox strname args]

                val (varObj:Mach.OBJ) = Mach.newObjNoTag ()
                val (varRegs:Mach.REGS) = extendScopeReg regs varObj Mach.ActivationScope
                val (varScope:Mach.SCOPE) = (#scope varRegs)
                val (Mach.Obj {props, ...}) = varObj
                                 
                (* FIXME: self-name binding is surely more complex than this! *)
                val selfName = Name.internal (#ident name)
                fun initSelf _ = Mach.addProp props selfName { ty = Ast.SpecialType Ast.Any,
                                                               state = Mach.MethodProp closure,
                                                               attrs = { dontDelete = true,
                                                                         dontEnum = true,
                                                                         readOnly = true,
                                                                         isFixed = true } }
            in
                trace ["invokeFuncClosure: allocating scope fixtures"];
                allocScopeFixtures varRegs paramFixtures;
                trace ["invokeFuncClosure: binding args"];
                bindArgs regs varScope func args;
                trace ["invokeFuncClosure: evaluating scope inits on scope obj #", 
                       Int.toString (getScopeId varScope)];
                evalScopeInits varRegs Ast.Local paramInits;

                (* NOTE: is this for the binding of a function expression to its optional
                 * identifier? If so, we need to extend the scope chain before extending it
                 * with the activation object, and add the self-name binding to that new 
                 * scope, as in sec 13 ed. 3.
                 * 
                 * Changing defValue to setValue for now.
                 *)

                initSelf ();
                checkAllPropertiesInitialized varObj;
                trace ["invokeFuncClosure: evaluating block"];
                let 
                    val res = ((evalBlock varRegs block; 
                                Mach.Undef)
                               handle ReturnException v => v)
                in
                    pop ();
                    res
                end
            end
    end


and evalTryStmt (regs:Mach.REGS) 
                (block:Ast.BLOCK) 
                (catches:Ast.CATCH_CLAUSE list)
                (finally:Ast.BLOCK option) 
    : Mach.VAL = 
    let
        fun typesCompatible a b = true (* FIXME: do a real type test here! *)

        fun catch (e:Mach.VAL) 
                  (clauses:Ast.CATCH_CLAUSE list) 
            : Mach.VAL option = 
            case clauses of
                [] => NONE
              | {ty, fixtures, block, ...}::cs => 
                if typesCompatible ty e
                then 
                    let 
                        (* FIXME: doesn't this need inits? *)
                        val head = (valOf fixtures, [])
                        val regs = evalHead regs head
                    in
                        SOME (evalBlock regs block)
                    end
                else
                    catch e cs

        fun finishWith (v:Mach.VAL) 
            : Mach.VAL = 
            case finally of 
                NONE => v
              (* 
               * FIXME: Not sure how to read the spec wrt. labels; 
               * are we supposed to return the finally-block's value
               * or the main block's value? 
               *)
              | SOME f => evalBlock regs f
    in
        evalBlock regs block
        handle ThrowException v => 
               case catch v catches of 
                   SOME fix => finishWith fix
                 | NONE => (finishWith v; 
                            raise ThrowException v)
    end
           

(*    
    Here are the structures we have to work with to instantiate objects:

     and CLS =
         Cls of
           { extends: NAME option,
             implements: NAME list,
             classFixtures: FIXTURES,
             instanceFixtures: FIXTURES,
             instanceInits: INITS,
             constructor: CTOR option,
             classType: TYPE_EXPR,
             instanceType: TYPE_EXPR }

     and CTOR =
         Ctor of
           { settings: INITS,
             func: FUNC }

     and FUNC =
         Func of 
           { name: FUNC_NAME,
             fsig: FUNC_SIG,                   
             fixtures: FIXTURES option,
             inits: STMT list,
             body: BLOCK }

    Here's how it works:

    val scope = [globalObj,classObj]
    val thisObj = newObj 
    evalFixtures scope thisObj instanceFixtures
    evalInits scope thisObj instanceInits (* step 4 *)

    val paramsObj = newObj
    val paramsFixtures = (#fixtures (#func (#constructor cls)))
	evalFixtures scope paramsObj paramsFixtures
    val paramsInits = (#inits (#func (#constructor class)))
    evalInits scope paramsObj paramsInits

	val settingsInits = (#settings (#constructor cls))
    evalInits paramsObj::scope thisObj settingsInits
        (* settings calls super constructor jumping to step 4 *)
    
    checkAllPropertiesInitialized obj

	val ctorBody = (#body (#func (#constructor cls)))
    evalBlock paramsObj::(thisObj::scope) thisObj ctorBody

*)

(* 

On the whiteboard today (feb 22):

  - get class closure for RHS of operator "new"
  - make an object
  - allocate instance fixtures
  CTOR(x) - eval inits in class scope [..., class]
          - allocate param obj
          - allocate param fixtures
          - eval param inits in class scope [..., class]
          - eval instance settings [...,class,params]
          - call CTOR(parent), via super() call at end of settings
          - check all allocated fixtures are initialized
          - execute ctor body (if native, run native fn) [..,class,instance,params]

*)

and bindArgs (regs:Mach.REGS)
             (argScope:Mach.SCOPE)
             (func:Ast.FUNC)
             (args:Mach.VAL list)
    : unit =
    let
        val Mach.Scope { object = Mach.Obj { props, ... }, ... } = argScope
        val Ast.Func { defaults, ty, 
                       fsig = Ast.FunctionSignature { hasRest, ... }, 
                       ... } = func
            
        (* If we have:
         * 
         * P formal parameters
         * D parameter defaults (at the end)
         * A args 
         *
         * Then we must have A + D >= P, and we let
         * I = (A+D) - P, the number of ignored defaults.
         *
         * We assign the args to temps numbered [0, A),
         * and assign the last D-I defaults to temps numbered [A, P-A).
         *
         * If the function has a ...rest arg and A <= (P-1), we do as above. 
         * 
         * If the function has a ...rest arg and A > (P-1), we do the above
         * for the (P-1) args and then take the A-(P-1) args and put them 
         * in a new array, bound to the ...rest name.
         *)

        val p = length (#params ty)
        val d = length defaults
        val a = length args
        val i = Int.min (d, Int.max(0, (a+d) - p));
        val _ = trace ["bindArgs",
                       ": p=", Int.toString p,
                       ", d=", Int.toString d,
                       ", a=", Int.toString a,
                       ", i=", Int.toString i,
                       ", hasRest=", Bool.toString hasRest]
                
        val argTemps = getScopeTemps argScope
        fun bindArg _ [] = ()                           
          | bindArg (n:int) ((arg:Mach.VAL)::args) = 
            (trace ["defining temp ", Int.toString n];
             Mach.defTemp argTemps n arg; 
             bindArg (n+1) args)

        fun bind (finalArgs:Mach.VAL list) = 
            (* 
             * FIXME: this is a random guess at the appropriate form
             * of 'arguments'. 
             *)
            (Mach.addProp props Name.arguments { state = Mach.ValListProp finalArgs,
                                                 ty = Name.typename Name.public_Array,
                                                 attrs = { dontDelete = true,
                                                           dontEnum = true,
                                                           readOnly = true, 
                                                           isFixed = true } };
             bindArg 0 finalArgs)

    (* 
     * FIXME: should handle a flag in func that indicates whether 
     * to insist on precise number of args. 
     *)
    in
        if hasRest andalso a > (p-1)
        then 
            let
                val _ = trace ["dropping ", Int.toString i, " defaults"]
                val defExprs = List.drop (defaults, i)
                val defVals = List.map (evalExpr regs) defExprs
                val restArgs = List.drop (args, (p-1))
                val _ = trace ["dropping ", Int.toString (p-1), 
                               " args, binding ", Int.toString (List.length restArgs), 
                               " rest args"];
                val allArgs = (List.take (args, (p-1))) @ defVals @ [newArray restArgs]
            in
                bind allArgs
            end
        else 
            let
                val padding = if a < (p-d) 
                              then (List.tabulate (((p-d) - a), (fn _ => Mach.Undef)))
                              else []
                val defExprs = List.drop (defaults, i)
                val defVals = List.map (evalExpr regs) defExprs
                val allArgs = args @ padding @ defVals
            in
                bind (List.take (allArgs, p))
            end
    end


and evalInits (regs:Mach.REGS)
              (obj:Mach.OBJ)
              (temps:Mach.TEMPS)
              (inits:Ast.INITS)
    : unit = 
    evalInitsMaybePrototype regs obj temps inits false


and evalInitsMaybePrototype (regs:Mach.REGS)
              (obj:Mach.OBJ)   
              (temps:Mach.TEMPS)
              (inits:Ast.INITS)
              (isPrototypeInit:bool)
    : unit =
    let 
        fun evalInit (n,e) =
            let
                val id = case n of 
                             Ast.PropName { id, ... } => id
                           | Ast.TempName t => ("#" ^ Int.toString t)
                val _ = push ["init ", id]
                val v = evalExpr regs e
            in
                case n of 
                    Ast.PropName pn => 
                    (trace ["evalInit assigning to prop ", fmtName pn, 
                           " on object #", (Int.toString (getObjId obj))];
(*                     if isPrototypeInit then
                         LogErr.log ["Propname: ", fmtName pn] 
                     else
                         () ; *)
                     if isPrototypeInit 
                     then setValue obj pn v
                     else defValue obj pn v)
                  | Ast.TempName tn => 
                    (trace ["evalInit assigning to temp ", (Int.toString tn),
                            " on object #", (Int.toString (getObjId obj))];
                     Mach.defTemp temps tn v);
                pop ()
            end
    in 
        List.app evalInit inits
    end

(*
    Evaluate INITs targeting an obj, in scope of temporaries
*)

and evalObjInits (regs:Mach.REGS)                   
                 (instanceObj:Mach.OBJ)
                 (head:Ast.HEAD)
    : unit = 
        let
            val (fixtures,inits) = head
            val tempRegs = evalHead regs (fixtures,[])
            val temps = getScopeTemps (#scope regs)
        in
            evalInits tempRegs instanceObj temps inits
        end


and evalScopeInits (regs:Mach.REGS)
                   (target:Ast.INIT_TARGET)
                   (inits:Ast.INITS)
    : unit =
        let
            fun findTargetObj scope = 
                let
                    val Mach.Scope { object, kind, parent, ...} = scope
                    val Mach.Obj {props,...} = object
                in
                    trace ["considering init target as object #", 
                           Int.toString (getScopeId scope)];
                    case target of
                        Ast.Local => 
                        if (length (!props)) > 0 orelse parent = NONE
                        then object
                        else findTargetObj (valOf parent) 
                      (* if there are no props then it is at temp scope *)
                             
                      | Ast.Hoisted => 
                        if kind = Mach.InstanceScope orelse
                           kind = Mach.ActivationScope orelse 
                           parent = NONE
                        then object
                        else findTargetObj (valOf parent)
                             
                      | Ast.Prototype => 
                        if kind = Mach.InstanceScope
                        then (needObj (getValue object Name.public_prototype))
                        else findTargetObj (valOf parent)
                end 

            val obj = findTargetObj (#scope regs)
            val Mach.Obj { ident, ... } = obj
            val _ = trace ["resolved init target to object id #", Int.toString ident]
        in 
            case (#scope regs) of
                Mach.Scope { temps, ...} =>
                evalInitsMaybePrototype regs obj temps inits (target=Ast.Prototype)
        end


and initializeAndConstruct (classClosure:Mach.CLS_CLOSURE)
                           (classObj:Mach.OBJ)
                           (classRegs:Mach.REGS)                           
                           (args:Mach.VAL list) 
                           (instanceObj:Mach.OBJ)
    : unit =
    case classClosure of 
        { cls, env, allTypesBound } => 
        if not allTypesBound 
        then error ["constructing instance of class with unbound type variables"]
        else
            let
                val Ast.Cls { name, 
                              extends,
                              instanceInits,
                              constructor, 
                              ... } = cls
                fun initializeAndConstructSuper (superArgs:Mach.VAL list) = 
                    case extends of 
                        NONE => 
                        (trace ["checking all properties initialized at root class", 
                                       fmtName name];
                         checkAllPropertiesInitialized instanceObj)
                      | SOME superName => 
                        let 
                            val _ = trace ["initializing and constructing superclass ", fmtName superName ]
                            val (superObj:Mach.OBJ) = needObj (findVal env (multinameOf superName))
                            val (superClsClosure:Mach.CLS_CLOSURE) = 
                                case Mach.getObjMagic superObj of
                                    SOME (Mach.Class cc) => cc
                                  | _ => error ["Superclass object ", 
                                                           fmtName superName, 
                                                           "is not a class closure"]
                            val (superEnv:Mach.REGS) = {scope=(#env superClsClosure), 
                                                        this=(#this classRegs)}
                        in
                            initializeAndConstruct 
                                superClsClosure superObj superEnv superArgs instanceObj
                        end
            in
                trace ["evaluating instance initializers for ", fmtName name];
                evalObjInits classRegs instanceObj instanceInits;   
                case constructor of 
                    NONE => initializeAndConstructSuper []
                  | SOME (Ast.Ctor { settings, superArgs, func }) => 
                    let 
                        val _ = push ["ctor ", callApprox (#id name) args]
                        val Ast.Func { block, param=(paramFixtures,paramInits), ... } = func
                        val (varObj:Mach.OBJ) = Mach.newObjNoTag ()
                        val (varRegs:Mach.REGS) = extendScopeReg classRegs
                                                                  varObj 
                                                                  Mach.ActivationScope
                        val varScope = (#scope varRegs)
                        val (instanceRegs:Mach.REGS) = extendScopeReg classRegs
                                                                      instanceObj 
                                                                      Mach.InstanceScope
                        val (ctorRegs:Mach.REGS) = extendScopeReg instanceRegs 
                                                                  varObj 
                                                                  Mach.ActivationScope
                    in
                        trace ["allocating scope fixtures for constructor of ", fmtName name];
                        allocScopeFixtures varRegs paramFixtures;
                        trace ["binding constructor args of ", fmtName name];
                        bindArgs classRegs varScope func args;
                        trace ["evaluating inits of ", fmtName name,
                               " in scope #", Int.toString (getScopeId varScope)];
                        evalScopeInits varRegs Ast.Local paramInits;
                        trace ["evaluating settings"];
                        evalObjInits varRegs instanceObj settings;
                        trace ["initializing and constructing superclass of ", fmtName name];
                        initializeAndConstructSuper (map (evalExpr varRegs) superArgs);  
                        trace ["entering constructor for ", fmtName name];
                        evalBlock ctorRegs block
                        handle ReturnException v => v;
                        pop ();
                        ()
                    end
            end

and runAnySpecialConstructor (id:Mach.OBJ_IDENT) 
                             (args:Mach.VAL list) 
                             (instanceObj:Mach.OBJ) 
    : unit =
    (trace ["checking for special case constructors with value ", Int.toString id];
     if id = !FunctionClassIdentity andalso (not (args = []))
     then
         (*
          * We synthesize a token stream here that feeds back into the parser.
          *)
         let 
             val nargs = length args
             val argArgs = List.take (args, nargs-1)
             val source = toString (List.last args) 
             fun ident v = Token.Identifier (toString v)
             val argIdents = map ident argArgs
             val nloc = {file="<no filename>", span=(1,1), sm=StreamPos.mkSourcemap (), post_newline=false}
             val argList = case argIdents of
                               [] => []
                             | x::xs => ((x, nloc) :: 
                                         (List.concat 
                                              (map (fn i => [(Token.Comma, nloc), (i, nloc)]) xs)))
             val lines = [source] (* FIXME: split lines *)
             val lineTokens = List.filter (fn t => case t of 
                                                       (Token.Eof, _) => false 
                                                     | _ => true)
                                          (Parser.lexLines lines)
             val funcTokens = [(Token.Function, nloc), 
                               (Token.LeftParen, nloc)]
                              @ argList
                              @ [(Token.RightParen, nloc)]
                              @ [(Token.LeftBrace, nloc)]
                              @ lineTokens
                              @ [(Token.RightBrace, nloc)]
                              @ [(Token.Eof, nloc)]

             val (_,funcExpr) = Parser.functionExpression (funcTokens, 
                                                           Parser.NOLIST, 
                                                           Parser.ALLOWIN)

             val funcExpr = Defn.defExpr (Defn.topEnv()) funcExpr
             val funcVal = evalExpr (getInitialRegs())  funcExpr
         in
             case funcVal of 
                 Mach.Object tmp =>
                 let 
                     val Mach.Obj { magic, ...} = instanceObj
                     val sname = Name.public_source
                     val sval = newString source
                 in
                     magic := Mach.getObjMagic tmp;
                     setValue instanceObj sname sval
                 end
               | _ => error ["function did not compile to object"]
         end        
     else 
         if id = !ArrayClassIdentity
         then 
             let
                 fun bindVal _ [] = ()
                   | bindVal n (x::xs) = 
                     (setValue instanceObj (Name.public (Int.toString n)) x;
                      bindVal (n+1) xs)
             in
                 case args of
                     [] => setValue instanceObj (Name.public "length") (newUInt 0w0)
                   | [k] => if Mach.isNumeric k then
                                setValue instanceObj (Name.public "length") k
                            else
                                bindVal 0 args
                   | _ => bindVal 0 args
             end
         else 
             ())
    

and constructClassInstance (classObj:Mach.OBJ)
                           (classClosure:Mach.CLS_CLOSURE) 
                           (args:Mach.VAL list) 
    : Mach.VAL =
    let
        val {cls = Ast.Cls { name, instanceFixtures, ...}, env, ...} = classClosure
        val _ = push ["new ", callApprox (#id name) args]
        val (tag:Mach.VAL_TAG) = Mach.ClassTag name
        val (proto:Mach.VAL) = if hasOwnValue classObj Name.public_prototype
                               then getValue classObj Name.public_prototype
                               else Mach.Null
                                    
        (* FIXME: might have 'this' binding wrong in class scope here. *)
        val (classScope:Mach.SCOPE) = extendScope env 
                                                  classObj 
                                                  Mach.InstanceScope
        val (instanceObj:Mach.OBJ) = Mach.newObj tag proto NONE
        val Mach.Obj { ident, ... } = classObj
        val classRegs = { scope = classScope, 
                          this = instanceObj }
    in
        trace ["allocating ", 
               Int.toString (length instanceFixtures), 
               " instance fixtures for new ", fmtName name];
        allocObjFixtures classRegs instanceObj (SOME instanceObj) instanceFixtures;
        trace ["entering most derived constructor for ", fmtName name];
        initializeAndConstruct classClosure classObj classRegs args instanceObj;
        runAnySpecialConstructor ident args instanceObj;
        trace ["finished constructing new ", fmtName name];
        pop ();
        Mach.Object instanceObj
    end


and newByGlobalName (regs:Mach.REGS) 
                    (n:Ast.NAME) 
    : Mach.VAL = 
    let
        val (cls:Mach.VAL) = getValue (getGlobalObject ()) n
    in
        case cls of 
            Mach.Object ob => evalNewExpr ob []
          | _ => error ["trying to 'new' non-object global value"]
    end


(* 
 * ES3 9.9 ToObject 
 * 
 * FIXME: no idea if this makes the most sense given 
 * the ES3 meaning of the operation. 
 *)

and toObject (regs:Mach.REGS)
             (v:Mach.VAL) 
    : Mach.OBJ = 
    case v of 
        Mach.Undef => raise ThrowException (newByGlobalName regs Name.public_TypeError)
      | Mach.Null => raise ThrowException (newByGlobalName regs Name.public_TypeError)
      | Mach.Object ob => ob


(* 
 * ES3 8.6.2.1 [[Get]](P)
 * 
 * FIXME: no idea if this makes the most sense given 
 * the ES3 meaning of the operation. 
 *)
and get (obj:Mach.OBJ) 
        (n:Ast.NAME) 
    : Mach.VAL =
    let
        fun tryObj ob = 
            if hasOwnValue ob n
            then getValue ob n
            else 
                case obj of 
                    Mach.Obj { proto, ... } => 
                    (case (!proto) of 
                         Mach.Object p => tryObj p
                       | _ => Mach.Undef)
    in
        tryObj obj
    end


(* 
 * ES3 8.6.2.6 [[DefaultValue]](hint)
 * 
 * FIXME: no idea if this makes the most sense given 
 * the ES3 meaning of the operation. 
 *)
and defaultValue (regs:Mach.REGS)
                 (obj:Mach.OBJ) 
                 (hint:string option)
    : Mach.VAL = 
    let 
        fun tryProps [] = raise ThrowException 
                                    (newByGlobalName regs
                                         Name.public_TypeError)
          | tryProps (n::ns) =
            let 
                val f = get obj n
            in 
                if Mach.isObject f
                then 
                    let 
                        val v = evalCallExpr obj (needObj f) []
                    in
                        if isPrimitive v
                        then v
                        else tryProps ns
                    end
                else
                    tryProps ns
            end
    in
        (* FIXME: Date objects are supposed to default to "String" hint. *)
        if hint = (SOME "String")
        then tryProps [Name.public_toString, Name.public_valueOf]
        else tryProps [Name.public_valueOf, Name.public_toString]
    end


and isPrimitive (v:Mach.VAL) 
    : bool = 
    case v of 
        Mach.Null => true
      | Mach.Undef => true
      | Mach.Object (Mach.Obj ob) => 
        (case !(#magic ob) of
             SOME (Mach.UInt _) => true
           | SOME (Mach.Int _) => true
           | SOME (Mach.Double _) => true
           | SOME (Mach.Decimal _) => true
           | SOME (Mach.String _) => true
           | SOME (Mach.Boolean _) => true
           | _ => false)

(* 
 * ES3 1.9 ToPrimitive 
 * 
 * FIXME: no idea if this makes the most sense given 
 * the ES3 meaning of the operation. 
 *)
and toPrimitive (regs:Mach.REGS)
                (v:Mach.VAL) 
                (preferredType:string option)
    : Mach.VAL = 
    if isPrimitive v 
    then v
    else defaultValue regs (needObj v) preferredType
         
(*
    HEAD
*)

and evalHead (regs:Mach.REGS)
             (head:Ast.HEAD)
    : Mach.REGS =
        let
            val (fixtures,inits) = head
            val obj = Mach.newObjNoTag ()
            val regs = extendScopeReg regs obj Mach.TempScope
            val {scope,...} = regs
            val _ = trace ["built temp scope #", 
                           Int.toString (getScopeId (#scope regs)), 
                           " for head"]
        in
            allocScopeFixtures regs fixtures;
            evalInits regs obj (getScopeTemps scope) inits;
            regs
        end
            
(*
    BLOCK
*)

and evalBlock (regs:Mach.REGS) 
              (block:Ast.BLOCK) 
    : Mach.VAL = 
    let 
        val Ast.Block {head, body, pos, ...} = block
        val _ = LogErr.setPos pos
        val blockRegs = evalHead regs (valOf head)
        val _ = LogErr.setPos pos
        val res = evalStmts blockRegs body
        val _ = LogErr.setPos pos
    in
        res
    end



(*
    Initialise Class Prototype

    Initialise a class object. A class object has been allocated and stored
    in a global property. All that remains is to initialise it by executing
    the class sttatement.

    The class statement sets static and prototype properties while executing
    its body.
*)

and initClassPrototype (regs:Mach.REGS)
                       (classObj:Mach.OBJ) 
    : unit =
    let 
        val Mach.Obj { props, magic, ... } = classObj
        val SOME (Mach.Class {cls=Ast.Cls {extends,...},...}) = !magic
        val baseProtoVal = 
            case extends of 
                NONE => Mach.Null
              | SOME baseClassName => 
                let
                in
                    case findVal (#scope regs) (multinameOf baseClassName) of 
                        Mach.Object ob => 
                        if hasOwnValue ob Name.public_prototype
                        then getValue ob Name.public_prototype
                        else Mach.Null
                      | _ => error ["base class resolved to non-object: ", 
                                    fmtName baseClassName]
                end
                
        val _ = trace ["constructing prototype"]
        val newPrototype = Mach.setProto (newObj ()) baseProtoVal
    in
        defValue classObj Name.public_prototype (Mach.Object newPrototype);
        trace ["finished initialising class prototype"]
    end

(*
    ClassBlock classBlock

*)

and evalClassBlock (regs:Mach.REGS) 
                   (classBlock)
    : Mach.VAL =

    (* 
        The property that holds the class object was allocated when the 
        fixtures of the outer scope were allocated. Still to do is
        initialising the class object, including creating its prototype
    *)

    let 
        val {name, block, ...} = classBlock
        val {scope, ...} = regs

        val _ = trace ["evaluating class stmt for ", fmtName (valOf name)]
        
        val classObj = needObj (findVal scope (multinameOf (valOf name)))

        val _ = initClassPrototype regs classObj

        (* FIXME: might have 'this' binding wrong in class scope *)
        val _ = trace ["extending scope for class block of ", fmtName (valOf name)]
        val classRegs = extendScopeReg regs classObj Mach.InstanceScope
    in
        trace ["evaluating class block of ", fmtName (valOf name)];
        evalBlock classRegs block
    end

(*

*)

and evalIfStmt (regs:Mach.REGS) 
               (cnd:Ast.EXPR) 
               (thn:Ast.STMT)
               (els:Ast.STMT) 
    : Mach.VAL = 
    let 
        val v = evalExpr regs cnd 
        val b = toBoolean v
    in
        if b 
        then evalStmt regs thn
        else evalStmt regs els
    end


and evalLabelStmt (regs:Mach.REGS) 
                  (lab:Ast.IDENT) 
                  (s:Ast.STMT) 
    : Mach.VAL =
    evalStmt regs s
        handle BreakException exnLabel => 
            if labelMatch [lab] exnLabel
                then Mach.Undef
                else raise BreakException exnLabel


and evalWhileStmt (regs:Mach.REGS)
                  (whileStmt:Ast.WHILE_STMT)
    : Mach.VAL =
    case whileStmt of
        { cond, body, fixtures, labels } =>
        let
            val accum = ref Mach.Undef
            fun loop _ =
                if toBoolean (evalExpr regs cond)
                then 
                    (accum := evalStmt regs body;   
                                    (* FIXME: doesn't this need to happen in evalStmts? *)
                     loop ())
                    handle ContinueException exnLabel => 
                           if labelMatch labels exnLabel
                           then loop ()
                           else raise ContinueException exnLabel
                                      
                         | BreakException exnLabel => 
                           if labelMatch labels exnLabel
                           then (!accum)
                           else raise BreakException exnLabel
                else 
                    (!accum)
        in
            loop ()
        end


and evalDoWhileStmt (regs:Mach.REGS)
                    (whileStmt:Ast.WHILE_STMT)
    : Mach.VAL =
    case whileStmt of
        { cond, body, fixtures, labels } =>
        let
            val accum = ref Mach.Undef
            fun loop _ =
                let 
                    fun bottom _ = 
                        if toBoolean (evalExpr regs cond)
                        then loop ()
                        else (!accum)
                in                
                    (accum := evalStmt regs body;
                     bottom ())
                    handle ContinueException exnLabel => 
                           if labelMatch labels exnLabel
                           then bottom ()
                           else raise ContinueException exnLabel
                                      
                         | BreakException exnLabel => 
                           if labelMatch labels exnLabel
                           then (!accum)
                           else raise BreakException exnLabel
                end
        in
            loop ()
        end


and evalWithStmt (regs:Mach.REGS) 
                 (obj:Ast.EXPR) 
                 (ty:Ast.TYPE_EXPR) 
                 (body:Ast.STMT) 
    : Mach.VAL = 
    let 
        val v = evalExpr regs obj
        val ob = (toObject regs v)
        val s = extendScope (#scope regs) ob Mach.WithScope
        val regs = {this=ob, scope=s}
    in
        evalStmt regs body
    end


and evalSwitchStmt (regs:Mach.REGS)
                   (mode:Ast.NUMERIC_MODE option)
                   (cond:Ast.EXPR)
                   (cases:Ast.CASE list)
                   (labels:Ast.IDENT list) 
    : Mach.VAL = 
    let
        fun tryCases (v:Mach.VAL) [] = Mach.Undef
          | tryCases (v:Mach.VAL) ({label, inits, body}::cs) =
            if (case label of 
                    NONE => true
                  | SOME e => doubleEquals mode v (evalExpr regs e))
            then 
                (* FIXME: This will change when switch stmt bodies are
                 * reorganized and get a proper head. *)
                let
                    val head = ([], case inits of NONE => [] 
                                                | SOME i => i)
                    val caseRegs = evalHead regs head
                    val first = evalBlock caseRegs body
                    fun evalRest accum [] = accum
                      | evalRest accum ({label, inits, body}::xs) = 
                        let
                            val a = evalBlock caseRegs body
                        in
                            evalRest a xs
                        end
                in
                    evalRest first cs
                end
            else
                tryCases v cs
    in
        tryCases (evalExpr regs cond) cases
                    handle BreakException exnLabel => 
                           if labelMatch labels exnLabel
                           then Mach.Undef
                           else raise BreakException exnLabel
    end


(*
    FOR_STMT
        structural subtype test in evalIterable
        TODO isEach
*)

and evalIterable (regs:Mach.REGS)
                 (obj:Ast.EXPR)
    : Mach.OBJ =
    let
        val v = evalExpr regs obj
    in
        (*
         * Implement the IE JScript quirk where for (i in null) and
         * for (i in undefined) do not loop at all by returning an
         * empty object for Undef and Null.
         *)
        case v of
            Mach.Object ob => ob
          | (Mach.Undef | Mach.Null) => newObj ()
    end

and callIteratorGet (regs:Mach.REGS)
                    (iterable:Mach.OBJ)
    : Mach.OBJ =
    let
        val iterValue = newBuiltin Name.public_Array NONE
        val iterator  = (case iterValue of
                             Mach.Object ob => ob
                           | _ => raise InternalError)
    in
        case iterable of
            Mach.Obj { props, ... } =>
            let
                val cursorName = Name.public "cursor"
                val cursorInit = newInt 0
                val length = ref 0
                fun enumerate ((name, prop):(Ast.NAME * Mach.PROP)) : bool =
                    if #dontEnum (#attrs prop)
                    then
                        true
                    else
                        let
                            val lengthValue = getValue iterator Name.public_length
                            val _ = length := toInt32 lengthValue
                            val elemIndex   = Name.public (Int32.toString (!length))
                            val elemValue   = newString (#id name)
                        in
                            setValue iterator elemIndex elemValue;
                            true
                        end
            in
                setValue iterator cursorName cursorInit;
                List.all enumerate (!props);
                setValue iterator Name.public_length (newInt (!length+1));
                iterator
            end
    end

and callIteratorNext (regs:Mach.REGS)
                     (iterator:Mach.OBJ)
    : Mach.VAL =
    let
        val lengthValue = getValue iterator Name.public_length
        val length      = toInt32 lengthValue
        val cursorName  = Name.public "cursor"
        val cursorValue = getValue iterator cursorName
        val cursor      = toInt32 cursorValue
    in
        if cursor < length
        then
            let
                val nextName       = Name.public (Int32.toString cursor)
                val newCursorValue = newInt (cursor + 1)
            in
                setValue iterator cursorName newCursorValue;
                getValue iterator nextName
            end
        else
            raise Mach.StopIterationException
    end

and evalForInStmt (regs:Mach.REGS)
                  (forInStmt:Ast.FOR_ENUM_STMT)
    : Mach.VAL =
    case forInStmt of
        { isEach, defn, obj, fixtures, next, labels, body, ... } =>
        let
            val iterable = evalIterable regs obj
            val iterator = callIteratorGet regs iterable
            val forInRegs = evalHead regs (valOf fixtures, [])

            (*
                The following code is ugly but it needs to handle the cases
                when there is a binding on the lhs of 'in' and not. If there
                is no binding on the lhs, then the parser uses a LetExpr to
                allocate a temporary fixture for the iteration value and
                possibly some for desugaring. The associated inits (i) are
                separated out as 'nextInits' for execution after the iteration
                value is set each time around the loop.

                FIXME: maybe unify 'next' as a new kind of expr. This would
                trade redundancy for clarity. Not sure it's worth it.
            *)

            val (nextTarget, nextHead, nextInits, nextExpr) =
                case next of
                    Ast.ExprStmt (Ast.InitExpr (target, head, inits)) =>
                        (target, head, inits, [])
                  | Ast.ExprStmt (Ast.LetExpr {defs, body, head = SOME (f, i)}) =>
                        (Ast.Hoisted, (f, []), i, body::[])
                  | _ => LogErr.internalError ["evalForInStmt: invalid structure"]

            (*
                A binding init on the lhs of 'in' will be included in nextHead
                and evaluated once when the tempRegs is created here
            *)

            val tempRegs = evalHead forInRegs nextHead
            val temps = getScopeTemps (#scope tempRegs)

            fun loop (accum:Mach.VAL option) =
                let
                    val v = (SOME (callIteratorNext forInRegs iterator)
                             handle Mach.StopIterationException =>
                                    NONE)
                    val b = (case v of
                                 NONE => false
                               | x => true)
                in
                    if b
                    then
                        let
                            val _ = Mach.defTemp temps 0 (valOf v)   (* def the iteration value *)
                            val _ = evalScopeInits tempRegs nextTarget nextInits
                            val _ = map (evalExpr tempRegs) nextExpr
                            val curr = (SOME (evalStmt forInRegs body)
                                        handle ContinueException exnLabel =>
                                               if labelMatch labels exnLabel
                                               then NONE
                                               else raise ContinueException exnLabel)
                            val nextVal = (case curr of
                                               NONE => accum
                                             | x => x)
                        in
                            loop nextVal handle BreakException exnLabel =>
                                          if labelMatch labels exnLabel
                                          then accum
                                          else raise BreakException exnLabel
                        end
                    else
                        accum
                end
        in
            case
                loop NONE handle BreakException exnLabel =>
                              if labelMatch labels exnLabel
                              then NONE
                              else raise BreakException exnLabel
            of
                NONE => Mach.Undef
              | SOME v => v
        end

and evalForStmt (regs:Mach.REGS)
                (forStmt:Ast.FOR_STMT)
    : Mach.VAL =
    case forStmt of
        { fixtures, init, cond, update, labels, body, ... } =>
        let
            val forRegs = evalHead regs (valOf fixtures, [])

            fun loop (accum:Mach.VAL option) =
                let
                    val b = case cond of 
                                Ast.ListExpr [] => true
                              | _ => toBoolean (evalExpr forRegs cond)
                in
                    if b
                    then
                        let
                            val curr = (SOME (evalStmt forRegs body)
                                        handle ContinueException exnLabel =>
                                               if labelMatch labels exnLabel
                                               then NONE
                                               else raise ContinueException exnLabel)
                            val next = (case curr of
                                            NONE => accum
                                          | x => x)
                        in
                            evalExpr forRegs update;
                            loop next handle BreakException exnLabel =>
                                          if labelMatch labels exnLabel
                                          then accum
                                          else raise BreakException exnLabel
                        end
                    else
                        accum
                end
        in
            evalStmts forRegs init;
            case
                loop NONE handle BreakException exnLabel =>
                              if labelMatch labels exnLabel
                              then NONE
                              else raise BreakException exnLabel
            of
                NONE => Mach.Undef
              | SOME v => v
        end


and evalReturnStmt (regs:Mach.REGS) 
                   (e:Ast.EXPR) 
    : Mach.VAL =
    raise (ReturnException (evalExpr regs e))


and evalThrowStmt (regs:Mach.REGS) 
                  (e:Ast.EXPR) 
    : Mach.VAL =
    raise (ThrowException (evalExpr regs e))


and evalBreakStmt (regs:Mach.REGS) 
                  (lbl:Ast.IDENT option) 
    : Mach.VAL =
    (trace ["raising BreakException ",case lbl of NONE => "empty" | SOME id => id];
    raise (BreakException lbl))


and evalContinueStmt (regs:Mach.REGS) 
                     (lbl:Ast.IDENT option) 
    : Mach.VAL =
    raise (ContinueException lbl)


and evalPackage (regs:Mach.REGS) 
                (package:Ast.PACKAGE) 
    : Mach.VAL = 
    evalBlock regs (#block package)

and evalProgram (prog:Ast.PROGRAM) 
    : Mach.VAL = 
    let
        val regs = getInitialRegs ()
    in
        LogErr.setPos NONE;
        allocScopeFixtures regs (valOf (#fixtures prog));
        map (evalPackage regs) (#packages prog);
        evalBlock regs (#block prog)
    end

fun resetGlobal (ob:Mach.OBJ) 
    : unit = 
    (globalObject := SOME ob;
     globalScope := SOME (Mach.Scope { object = ob,
                                       parent = NONE,
                                       temps = ref [],
                                       kind = Mach.GlobalScope });
     setValue ob Name.public_global (Mach.Object ob))


fun bindSpecialIdentities _ = 
    let 
        fun bindIdent (n, r) = 
            let 
                val Mach.Obj obj = needObj (getValue (getGlobalObject()) n)
                val id = (#ident obj)
            in
                trace ["noting identity ", Int.toString id, " of class ", fmtName n];
                r := id
            end
    in
        List.app bindIdent
                 [ 
                  (Name.public_Array, ArrayClassIdentity),
                  (Name.public_Function, FunctionClassIdentity)
                 ]
    end

end
