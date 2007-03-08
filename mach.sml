(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[mach] " :: ss) else ()

(* Local type aliases *)

type TYPE = Ast.TYPE_EXPR
type STR = Ast.USTRING
type ID = Ast.IDENT
type NS = Ast.NAMESPACE
type NAME = Ast.NAME
type MULTINAME = Ast.MULTINAME
type BINDINGS = Ast.BINDINGS
type FIXTURES = Ast.FIXTURES
type CLS = Ast.CLS

datatype VAL = Object of OBJ
             | Null
             | Undef

     and OBJ = 
         Obj of { tag: VAL_TAG,                           
                  props: PROP_BINDINGS,
                  proto: VAL ref,
                  magic: (MAGIC option) ref }

     and VAL_TAG =
         ObjectTag of Ast.FIELD_TYPE list
       | ArrayTag of TYPE list
       | FunctionTag of Ast.FUNC_SIG
       | ClassTag of NAME

(* 
 * Magic is visible only to the interpreter; 
 * it is not visible to users.
 *)
               
     and MAGIC = Number of real (* someday to be more complicated *)
               | String of STR  (* someday to be unicode *)
               | Bool of bool
               | Namespace of NS
               | Class of CLS_CLOSURE
               | Interface of IFACE_CLOSURE
               | Function of FUN_CLOSURE
               | Type of TYPE
               | HostFunction of (VAL list -> VAL)
                    
     and IFACE = 
         Iface of { ty: TYPE,
                    bases: IFACE list,
                    definition: Ast.INTERFACE_DEFN,                        
                    isInitialized: bool ref }
                   
     and SCOPE = 
         Scope of { object: OBJ,
                    parent: SCOPE option,
                    temps: TEMPS,
                    isVarObject: bool }

     and TEMP_STATE = UninitTemp
                    | ValTemp of VAL
                        
     and PROP_STATE = TypeVarProp
                    | TypeProp
                    | UninitProp
                    | ValProp of VAL
                    | VirtualValProp of 
                      { getter: FUN_CLOSURE option,
                        setter: FUN_CLOSURE option }
                
withtype FUN_CLOSURE = 
         { func: Ast.FUNC,
           allTypesBound: bool,
           env: SCOPE }

     and CLS_CLOSURE = 
         { cls: CLS, 
           allTypesBound: bool,
           env: SCOPE }
         
     and IFACE_CLOSURE = 
         { iface: IFACE, 
           allTypesBound: bool,
           env: SCOPE }


(* Important to model "fixedness" separately from 
 * "dontDelete-ness" because fixedness affects 
 * which phase of name lookup the name is found during.
 *)

     and ATTRS = { dontDelete: bool,
                   dontEnum: bool,
                   readOnly: bool,
                   isFixed: bool}

     and TEMPS = (TYPE * TEMP_STATE) list ref

     and PROP = { ty: TYPE,
                  state: PROP_STATE,                  
                  attrs: ATTRS }

     and PROP_BINDINGS = ((NAME * PROP) list) ref

(* Binding operations. *)

fun newPropBindings _ : PROP_BINDINGS = 
    let 
        val b:PROP_BINDINGS = ref []
    in
        b
    end


fun addProp (b:PROP_BINDINGS) 
            (n:NAME) 
            (x:PROP) 
    : unit = 
    b := ((n,x) :: (!b))


fun delProp (b:PROP_BINDINGS) 
            (n:NAME) 
    : unit = 
    let 
        fun strip [] = LogErr.hostError ["deleting nonexistent property binding: ", 
                                         (#id n)]
          | strip ((k,v)::bs) = 
            if k = n 
            then bs
            else (k,v)::(strip bs)
    in
        b := strip (!b)
    end    


fun getProp (b:PROP_BINDINGS) 
            (n:NAME) 
    : PROP = 
    let 
        fun search [] = LogErr.hostError ["property binding not found: ", 
                                          (#id n)]
          | search ((k,v)::bs) = 
            if k = n 
            then v
            else search bs
    in
        search (!b)
    end

fun getFixedProp (b:PROP_BINDINGS) 
                 (n:NAME) 
    : PROP = 
    let 
        fun search [] = LogErr.hostError ["property binding not found: ", 
                                          (#id n)]
          | search ((k,(v:PROP))::bs) = 
            if k = n andalso (#isFixed (#attrs v))
            then v
            else search bs
    in
        search (!b)
    end

fun hasFixedProp (b:PROP_BINDINGS) 
                 (n:NAME) 
    : bool = 
    let 
        fun search [] = false
          | search ((k,(v:PROP))::bs) = 
            if k = n andalso (#isFixed (#attrs v))
            then true
            else search bs
    in
        search (!b)
    end


fun hasProp (b:PROP_BINDINGS) 
            (n:NAME) 
    : bool = 
    let 
        fun search [] = false
          | search ((k,v)::bs) = 
            if k = n 
            then true
            else search bs
    in
        search (!b)
    end


(* Standard runtime objects and functions. *)

val publicPrototypeName:NAME = { ns = (Ast.Public ""), id = "prototype" }
val internalConstructorName:NAME = { ns = (Ast.Internal ""), id = "constructor" }
val internalObjectName:NAME = { ns = (Ast.Internal ""), id = "Object" }
                                            
val intrinsicObjectName:NAME = { ns = Ast.Intrinsic, id = "Object" }
val intrinsicArrayName:NAME = { ns = Ast.Intrinsic, id = "Array" }
val intrinsicFunctionName:NAME = { ns = Ast.Intrinsic, id = "Function" }
val intrinsicBooleanName:NAME = { ns = Ast.Intrinsic, id = "Boolean" }
val intrinsicNumberName:NAME = { ns = Ast.Intrinsic, id = "Number" }
val intrinsicStringName:NAME = { ns = Ast.Intrinsic, id = "String" }
val intrinsicNamespaceName:NAME = { ns = Ast.Intrinsic, id = "Namespace" }
val intrinsicClassName:NAME = { ns = Ast.Intrinsic, id = "Class" }
val intrinsicInterfaceName:NAME = { ns = Ast.Intrinsic, id = "Interface" }
val intrinsicTypeName:NAME = { ns = Ast.Intrinsic, id = "Type" }

val intrinsicApplyName:NAME = { ns = Ast.Intrinsic, id = "apply" }
val intrinsicInvokeName:NAME = { ns = Ast.Intrinsic, id = "invoke" }
val intrinsicConstructName:NAME = { ns = Ast.Intrinsic, id = "construct" }

val intrinsicObjectBaseTag:VAL_TAG = ClassTag (intrinsicObjectName)
val intrinsicArrayBaseTag:VAL_TAG = ClassTag (intrinsicArrayName)
val intrinsicFunctionBaseTag:VAL_TAG = ClassTag (intrinsicFunctionName)
val intrinsicBooleanBaseTag:VAL_TAG = ClassTag (intrinsicBooleanName)
val intrinsicNumberBaseTag:VAL_TAG = ClassTag (intrinsicNumberName)
val intrinsicStringBaseTag:VAL_TAG = ClassTag (intrinsicStringName)
val intrinsicNamespaceBaseTag:VAL_TAG = ClassTag (intrinsicNamespaceName)
val intrinsicClassBaseTag:VAL_TAG = ClassTag (intrinsicClassName)
val intrinsicInterfaceBaseTag:VAL_TAG = ClassTag (intrinsicInterfaceName)
val intrinsicTypeBaseTag:VAL_TAG = ClassTag (intrinsicTypeName)

(* To reference something in the intrinsic namespace, you need a complicated expression. *)
val intrinsicNsExpr = Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Intrinsic))
fun intrinsicName id = Ast.QualifiedIdentifier { qual = intrinsicNsExpr, ident = id }

(* Define some global intrinsic nominal types. *)

val typeType = Ast.TypeName (intrinsicName "Type")
val namespaceType = Ast.TypeName (intrinsicName "Namespace")
val classType = Ast.TypeName (intrinsicName "Class")


fun newObj (t:VAL_TAG) 
           (p:VAL) 
           (m:MAGIC option) 
    : OBJ = 
    Obj { tag = t,
          props = newPropBindings (),
          proto = ref p,
          magic = ref m }


fun newSimpleObj (m:MAGIC option) 
    : OBJ = 
    newObj intrinsicObjectBaseTag Null m


fun newObject (t:VAL_TAG) 
              (p:VAL) 
              (m:MAGIC option) 
    : VAL = 
    Object (newObj t p m)


fun newSimpleObject (m:MAGIC option) 
    : VAL = 
    Object (newSimpleObj m)


fun newNumber (n:real) 
    : VAL = 
    newObject intrinsicNumberBaseTag Null (SOME (Number n))


fun newString (s:STR) 
    : VAL = 
    newObject intrinsicStringBaseTag Null (SOME (String s))


fun newBoolean (b:bool) 
    : VAL = 
    newObject intrinsicBooleanBaseTag Null (SOME (Bool b))

fun newType (t:TYPE) 
    : VAL = 
    newObject intrinsicTypeBaseTag Null (SOME (Type t))

fun newNamespace (n:NS) 
    : VAL = 
    newObject intrinsicNamespaceBaseTag Null (SOME (Namespace n))

fun newClass (e:SCOPE) 
             (cls:CLS) 
    : VAL =
    let
        val closure = { cls = cls,
                        (* FIXME: are all types bound? *)
                        allTypesBound = true,
                        env = e }
        val obj = newObject intrinsicClassBaseTag Null (SOME (Class closure))
    in
        obj
    end

fun newIface (e:SCOPE) 
             (iface:IFACE) 
    : VAL =
    let
        val closure = { iface = iface,
                        (* FIXME: are all types bound? *)
                        allTypesBound = true,
                        env = e }
        val obj = newObject intrinsicInterfaceBaseTag Null (SOME (Interface closure))
    in
        obj
    end
        

fun newFunc (e:SCOPE) 
            (f:Ast.FUNC) 
    : VAL = 
    let 
        val fsig = case f of Ast.Func { fsig, ... } => fsig
        val tag = FunctionTag fsig
        val allTypesBound = (case fsig of 
                                 Ast.FunctionSignature { typeParams, ... } 
                                 => (length typeParams) = 0)
                            
        val closure = { func = f, 
                        allTypesBound = allTypesBound,
                        env = e }
    in
        newObject tag Null (SOME (Function closure))
    end
    
fun newHostFunction (f:(VAL list -> VAL)) = 
    Object (Obj { tag = intrinsicFunctionBaseTag,
                  props = newPropBindings (),
                  proto = ref Null,
                  magic = ref (SOME (HostFunction f)) })
    
val (objectType:TYPE) = Ast.ObjectType []

val (emptyBlock:Ast.BLOCK) = Ast.Block { pragmas = [],
                                         defns = [],
                                         body = [],
                                         head= NONE }

val (globalObject:OBJ) = newObj intrinsicObjectBaseTag Null NONE

val (globalScope:SCOPE) = 
    Scope { object = globalObject,
            parent = NONE,
            temps = ref [],
            isVarObject = true }


val nan = Real.posInf / Real.posInf


fun hasOwnValue (obj:OBJ) 
                (n:NAME) 
    : bool = 
    case obj of 
        Obj { props, ... } => hasProp props n


fun hasValue (obj:OBJ) 
             (n:NAME) 
    : bool = 
    if hasOwnValue obj n
    then true
    else (case obj of 
              Obj { proto, ... } => 
              case (!proto) of 
                  Object p => hasValue p n
                | _ => false)


fun getValue (obj:OBJ, 
              name:NAME) 
    : VAL = 
    case obj of 
        Obj {props, ...} => 
        let 
            val prop = getProp props name
        in
            case (#state prop) of 
                TypeProp => LogErr.machError ["getValue on a type property"]
              | TypeVarProp => LogErr.machError ["getValue on a type variable property"]
              | UninitProp => LogErr.machError ["getValue on an uninitialized property"]
              | VirtualValProp _ => LogErr.machError ["getValue on an virtual property"]
              | ValProp v => v
        end


(* A "defValue" call occurs when assigning a property definition's 
 * initial value, as specified by the user. All other assignments
 * to a property go through "setValue". *)

fun defValue (base:OBJ) 
             (name:NAME) 
             (v:VAL) 
    : unit =
    case base of 
        Obj { props, ... } => 
        if not (hasProp props name)
        then LogErr.machError ["defValue on missing property: ", LogErr.name name]
        else (* Here we have relaxed rules: you can write to an 
              * uninitialized property or a read-only property. *)
            let 
                val existingProp = getProp props name
                                   
                val _ = case (#state existingProp) of 
                            
                            TypeVarProp => 
                            LogErr.machError ["defValue on type variable property: ", 
                                              LogErr.name name]
                            
                          | TypeProp => 
                            LogErr.machError ["defValue on type property: ", 
                                              LogErr.name name]

                          | VirtualValProp _ => 
                            LogErr.machError ["defValue on virtual property: ", 
                                              LogErr.name name]
                            
                          | UninitProp => ()
                          | ValProp _ => ()
                val newProp = { state = ValProp v,
                                ty = (#ty existingProp), 
                                attrs = (#attrs existingProp) }
            in
                (* FIXME: insert typecheck here *)
                delProp props name;
                addProp props name newProp
            end

fun getTemp (temps:TEMPS)
            (n:int)
    : VAL =
    case List.nth ((!temps), n) of
        (_, UninitTemp) => LogErr.machError ["getting uninitialized temporary"]
      | (_, ValTemp v) => v

fun defTemp (temps:TEMPS)
            (n:int)
            (v:VAL) 
    : unit = 
    let
        fun replaceNth k [] = LogErr.machError ["temporary-definition error"]
          | replaceNth k (x::xs) =
            if k = 0 
            then (case x of 
                      (t, UninitTemp) => 
                      ((* FIXME: put typecheck here *)
                       (t, ValTemp v) :: xs)
                    | (_, _) => LogErr.machError ["re-defining temporary"])
            else x :: (replaceNth (k-1) xs)
    in
        if n >= (length (!temps))
        then LogErr.machError ["defining out-of-bounds temporary"]
        else temps := replaceNth n (!temps)
    end
             
fun setValue (base:OBJ) 
             (name:NAME) 
             (v:VAL) 
    : unit = 
    case base of 
        Obj {props, ...} => 
        if hasProp props name
        then 
            let 
                val existingProp = getProp props name
                                   
                val _ = case (#state existingProp) of 
                            UninitProp => 
                            LogErr.machError ["setValue on uninitialized property", 
                                              LogErr.name name]

                          | TypeVarProp => 
                            LogErr.machError ["setValue on type variable property:", 
                                              LogErr.name name]

                          | TypeProp => 
                            LogErr.machError ["setValue on type property: ", 
                                              LogErr.name name]

                          | VirtualValProp _ => 
                            LogErr.machError ["setValue on virtual property:", 
                                              LogErr.name name]

                          | ValProp _ => ()

                val existingAttrs = (#attrs existingProp)
                val newProp = { state = ValProp v,
                                ty = (#ty existingProp), 
                                attrs = existingAttrs }
            in
                if (#readOnly existingAttrs)
                then LogErr.machError ["setValue on read-only property"]
                else ((* FIXME: insert typecheck here *)
                      delProp props name;
                      addProp props name newProp)
            end
        else
            let 
                val prop = { state = ValProp v,
                             ty = Ast.SpecialType Ast.Any,
                             attrs = { dontDelete = false,
                                       dontEnum = false,
                                       readOnly = false,
                                       isFixed = false } }
            in
                addProp props name prop
            end

(*
 * To get from any object to its CLS, you work out the
 * "nominal base" of the object's tag. You can then find
 * a fixed prop in the global object that has a "Class"
 * magic value pointing to the CLS.
 *)

fun nominalBaseOfTag (t:VAL_TAG) 
    : NAME = 
    case t of 
        ObjectTag _ => intrinsicObjectName
      | ArrayTag _ => intrinsicArrayName
      | FunctionTag _ => intrinsicFunctionName
      | ClassTag c => c

fun getObjMagic (ob:OBJ) 
    : (MAGIC option) = 
    case ob of 
        Obj ob' => !(#magic ob')
                   
fun getMagic (v:VAL) 
    : (MAGIC option) = 
    case v of 
        Object (Obj ob) => !(#magic ob)
      | _ => NONE


fun getGlobalVal (n:NAME) 
    : VAL = 
    getValue (globalObject, n)


fun valToCls (v:VAL) 
    : (CLS option) = 
    case v of 
        Object (Obj ob) => 
        (case getMagic (getGlobalVal (nominalBaseOfTag (#tag ob))) of
             SOME (Class {cls,...}) => SOME cls
           | _ => NONE)
      | _ => NONE

(* FIXME: this is not the correct toString *)

fun toString (v:VAL) : string = 
    case v of 
        Undef => "undefined"
      | Null => "null"
      | Object (Obj ob) => 
        (case !(#magic ob) of 
             NONE => "[object Object]"
           | SOME magic => 
             (case magic of 
                  Number n => if Real.== (n, (Real.realFloor n))
                              then Int.toString (Real.floor n)
                              else Real.toString n
                | String s => s
                | Bool true => "true"
                | Bool false => "false"
                | Namespace (Ast.Private _) => "[private namespace]"
                | Namespace (Ast.Protected _) => "[protected namespace]"
                | Namespace Ast.Intrinsic => "[intrinsic namespace]"
                | Namespace Ast.OperatorNamespace => "[operator namespace]"
                | Namespace (Ast.Public id) => "[public namespace: " ^ id ^ "]"
                | Namespace (Ast.Internal _) => "[internal namespace]"
                | Namespace (Ast.UserNamespace id) => "[user-defined namespace " ^ id ^ "]"
                | Class _ => "[class Class]"
                | Interface _ => "[interface Interface]"
                | Function _ => "[function Function]"
                | Type _ => "[type Function]"
                | HostFunction _ => "[function HostFunction]"))

fun toNum (v:VAL) : real = 
    case v of 
        Undef => nan
      | Null => 0.0
      | Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Number n) => n
           | SOME (Bool true) => 1.0
           | SOME (Bool false) => 0.0
           | SOME (String s) => (case Real.fromString s of 
                                     SOME n => n
                                   | NONE => nan)
           | _ => nan)

fun arrayToList (arr:OBJ) 
    : VAL list = 
    let 
        val ns = Ast.Internal ""
        val len = Real.floor (toNum (getValue (arr, {id="length", ns=ns})))
        fun build i vs = 
            if i < 0
            then vs
            else 
                let
                    val n = {id=(Int.toString i), ns=ns}
                    val curr = if hasValue arr n
                               then getValue (arr, n)
                               else Undef
                in
                    build (i-1) (curr::vs)
                end
    in
        build (len-1) []
    end

fun toBoolean (v:VAL) : bool = 
    case v of 
        Undef => false
      | Null => false
      | Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Bool b) => b
           | _ => true)

                  
fun equals (va:VAL) (vb:VAL) : bool = 
    case (va,vb) of 
        (Object (Obj oa), Object (Obj ob)) => 
        (case (!(#magic oa), !(#magic ob)) of 
             (SOME ma, SOME mb) => 
             (case (ma, mb) of
                  (Number na, String _) => Real.== (na, (toNum vb))
                | (String _, Number nb) => Real.== ((toNum va), nb)
                | (Number a, Number b) => Real.==(a, b)
                | _ => (toString va) = (toString vb))
           | (_, _) => (toString va) = (toString vb))
      | _ => (toString va) = (toString vb)


fun less (va:VAL) (vb:VAL) : bool = 
    case (va,vb) of 
        (Object (Obj oa), Object (Obj ob)) =>
        (case (!(#magic oa), !(#magic ob)) of 
             (SOME ma, SOME mb) => 
             (case (ma, mb) of 
                  (Number na, String _) => na < (toNum vb)
                | (String _, Number nb) => (toNum va) < nb
                | (String sa, String sb) => sa < sb
                | _ => (toNum va) < (toNum vb))
           | _ => (toNum va) < (toNum vb))
      | _ => (toNum va) < (toNum vb)


fun hostPrintFunction (vals:VAL list) : VAL = 
    let
        fun printOne v = print (toString v) 
    in
        (List.app printOne vals; Undef)
    end

fun hostAssertFunction (vals:VAL list) : VAL = 
    case vals of 
        [] => LogErr.hostError ["intrinsic::assert() called with zero args"]
      | [a] => 
        (case a of 
             Null => LogErr.hostError ["intrinsic::assert() called with Null"]
           | Undef => LogErr.hostError ["intrinsic::assert() called with Undef"]
           | Object (Obj {magic, ...}) => 
             (case !magic of 
                  (SOME (Bool true)) => Undef
                | (SOME (Bool false)) => LogErr.hostError ["intrinsic::assert() failed"]
                | _ => LogErr.hostError ["intrinsic::assert() called with non-boolean"]))
      | _ => LogErr.hostError ["intrinsic::assert() called with multiple args"]

fun populateIntrinsics globalObj = 
    case globalObj of 
        Obj { props, ... } => 
        let 
            fun bindFunc (n, f) = 
                let 
                    val name = { id = n, ns = Ast.Intrinsic }
                    val prop = { ty = Ast.SpecialType Ast.Any,
                                 state = ValProp (newHostFunction f), 
                                 attrs = { dontDelete = true,
                                           dontEnum = false,
                                           readOnly = true,
                                           isFixed = true } }
                in
                    addProp props name prop
                end
        in
            List.app bindFunc 
            [ ("print", hostPrintFunction),
              ("assert", hostAssertFunction) ]
        end        

fun resetGlobalObject _ = 
    case globalObject of
        (Obj { props, magic, proto, ... }) => 
        (props := [];
         magic := NONE;
         proto := Null;
         populateIntrinsics globalObject)

end


         
         
