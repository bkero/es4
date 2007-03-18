(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[mach] " :: ss) else ()
fun error ss = LogErr.machError ss

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
       | ArrayTag of Ast.TYPE_EXPR list
       | FunctionTag of Ast.FUNC_SIG
       | ClassTag of Ast.NAME

(* 
 * Magic is visible only to the interpreter; 
 * it is not visible to users.
 *)
               
     and MAGIC = 
         UInt of Word32.word
       | Int of Int32.int
       | Double of Real64.real
       | Decimal of Decimal.DEC
       | ByteArray of Word8Array.array
       | String of Ast.USTRING  (* someday to be unicode *)
       | Bool of bool
       | Namespace of Ast.NAMESPACE
       | Class of CLS_CLOSURE
       | Interface of IFACE_CLOSURE
       | Function of FUN_CLOSURE
       | Type of Ast.TYPE_EXPR
       | NativeFunction of NATIVE_FUNCTION
                    
     and IFACE = 
         Iface of { ty: Ast.TYPE_EXPR,
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
         { cls: Ast.CLS, 
           allTypesBound: bool,
           env: SCOPE }
         
     and IFACE_CLOSURE = 
         { iface: IFACE, 
           allTypesBound: bool,
           env: SCOPE }

     and NATIVE_FUNCTION = 
         (VAL list -> VAL)


(* Important to model "fixedness" separately from 
 * "dontDelete-ness" because fixedness affects 
 * which phase of name lookup the name is found during.
 *)

     and ATTRS = { dontDelete: bool,
                   dontEnum: bool,
                   readOnly: bool,
                   isFixed: bool}

     and TEMPS = (Ast.TYPE_EXPR * TEMP_STATE) list ref

     and PROP = { ty: Ast.TYPE_EXPR,
                  state: PROP_STATE,                  
                  attrs: ATTRS }

     and PROP_BINDINGS = ((Ast.NAME * PROP) list) ref

(* Binding operations. *)

fun newPropBindings _ : PROP_BINDINGS = 
    let 
        val b:PROP_BINDINGS = ref []
    in
        b
    end


fun addProp (b:PROP_BINDINGS) 
            (n:Ast.NAME) 
            (x:PROP) 
    : unit = 
    b := ((n,x) :: (!b))


fun delProp (b:PROP_BINDINGS) 
            (n:Ast.NAME) 
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
            (n:Ast.NAME) 
    : PROP = 
    let 
        (*
            If not found, then cons up a temporary property
            with value undefined. Any property not found
            errors would have been caught by evalRefExpr
        *)
        fun search [] = {ty=Ast.SpecialType Ast.Undefined,
                              state=ValProp Undef,
                              attrs={dontDelete=false,  (* unused attrs *)
                                     dontEnum=false,
                                     readOnly=false,
                                     isFixed=false}}
          | search ((k,v)::bs) = 
            if k = n 
            then v
            else search bs
    in
        search (!b)
    end

fun getFixedProp (b:PROP_BINDINGS) 
                 (n:Ast.NAME) 
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
                 (n:Ast.NAME) 
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
            (n:Ast.NAME) 
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

val publicPrototypeName:Ast.NAME = { ns = (Ast.Public ""), id = "prototype" }
val internalConstructorName:Ast.NAME = { ns = (Ast.Internal ""), id = "constructor" }
val internalObjectName:Ast.NAME = { ns = (Ast.Internal ""), id = "Object" }
                                            
val intrinsicObjectName:Ast.NAME = { ns = Ast.Intrinsic, id = "Object" }
val intrinsicArrayName:Ast.NAME = { ns = Ast.Intrinsic, id = "Array" }
val intrinsicFunctionName:Ast.NAME = { ns = Ast.Intrinsic, id = "Function" }
val intrinsicBooleanName:Ast.NAME = { ns = Ast.Intrinsic, id = "Boolean" }
val intrinsicNumberName:Ast.NAME = { ns = Ast.Intrinsic, id = "Number" }
val intrinsicDoubleName:Ast.NAME = { ns = Ast.Intrinsic, id = "double" }
val intrinsicDecimalName:Ast.NAME = { ns = Ast.Intrinsic, id = "decimal" }
val intrinsicIntName:Ast.NAME = { ns = Ast.Intrinsic, id = "int" }
val intrinsicUIntName:Ast.NAME = { ns = Ast.Intrinsic, id = "uint" }
val intrinsicStringName:Ast.NAME = { ns = Ast.Intrinsic, id = "String" }
val intrinsicByteArrayName:Ast.NAME = { ns = Ast.Intrinsic, id = "ByteArray" }
val intrinsicNamespaceName:Ast.NAME = { ns = Ast.Intrinsic, id = "Namespace" }
val intrinsicClassName:Ast.NAME = { ns = Ast.Intrinsic, id = "Class" }
val intrinsicInterfaceName:Ast.NAME = { ns = Ast.Intrinsic, id = "Interface" }
val intrinsicTypeName:Ast.NAME = { ns = Ast.Intrinsic, id = "Type" }

val intrinsicApplyName:Ast.NAME = { ns = Ast.Intrinsic, id = "apply" }
val coreInvokeName:Ast.NAME = { ns = Ast.UserNamespace "core", id = "invoke" }
val coreConstructName:Ast.NAME = { ns = Ast.UserNamespace "core", id = "construct" }

val intrinsicObjectBaseTag:VAL_TAG = ClassTag (intrinsicObjectName)
val intrinsicArrayBaseTag:VAL_TAG = ClassTag (intrinsicArrayName)
val intrinsicFunctionBaseTag:VAL_TAG = ClassTag (intrinsicFunctionName)
val intrinsicBooleanBaseTag:VAL_TAG = ClassTag (intrinsicBooleanName)
val intrinsicNumberBaseTag:VAL_TAG = ClassTag (intrinsicNumberName)
val intrinsicDoubleBaseTag:VAL_TAG = ClassTag (intrinsicDoubleName)
val intrinsicDecimalBaseTag:VAL_TAG = ClassTag (intrinsicDecimalName)
val intrinsicIntBaseTag:VAL_TAG = ClassTag (intrinsicIntName)
val intrinsicUIntBaseTag:VAL_TAG = ClassTag (intrinsicUIntName)
val intrinsicStringBaseTag:VAL_TAG = ClassTag (intrinsicStringName)
val intrinsicByteArrayBaseTag:VAL_TAG = ClassTag (intrinsicByteArrayName)
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


fun newDouble (n:Real64.real) 
    : VAL = 
    newObject intrinsicDoubleBaseTag Null (SOME (Double n))

fun newDecimal (n:Decimal.DEC) 
    : VAL = 
    newObject intrinsicDecimalBaseTag Null (SOME (Decimal n))

fun newInt (n:Int32.int) 
    : VAL = 
    newObject intrinsicIntBaseTag Null (SOME (Int n))

fun newUInt (n:Word32.word) 
    : VAL = 
    newObject intrinsicUIntBaseTag Null (SOME (UInt n))

fun newString (s:Ast.USTRING) 
    : VAL = 
    newObject intrinsicStringBaseTag Null (SOME (String s))

fun newByteArray (b:Word8Array.array) 
    : VAL = 
    newObject intrinsicByteArrayBaseTag Null (SOME (ByteArray b))

fun newBoolean (b:bool) 
    : VAL = 
    newObject intrinsicBooleanBaseTag Null (SOME (Bool b))

fun newType (t:Ast.TYPE_EXPR) 
    : VAL = 
    newObject intrinsicTypeBaseTag Null (SOME (Type t))

fun newNamespace (n:Ast.NAMESPACE) 
    : VAL = 
    newObject intrinsicNamespaceBaseTag Null (SOME (Namespace n))

fun newClass (e:SCOPE) 
             (cls:Ast.CLS) 
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
        

fun newFunClosure (e:SCOPE)
                  (f:Ast.FUNC)
    : FUN_CLOSURE = 
    let
        val fsig = case f of Ast.Func { fsig, ... } => fsig
        val allTypesBound = (case fsig of 
                                 Ast.FunctionSignature { typeParams, ... } 
                                 => (length typeParams) = 0)
    in
        { func = f, 
          allTypesBound = allTypesBound,
          env = e }
    end

fun newFunc (e:SCOPE) 
            (f:Ast.FUNC) 
    : VAL = 
    let 
        val fsig = case f of Ast.Func { fsig, ... } => fsig
        val tag = FunctionTag fsig
        val closure = newFunClosure e f
    in
        newObject tag Null (SOME (Function closure))
    end
    
fun newNativeFunction (f:NATIVE_FUNCTION) = 
    Object (Obj { tag = intrinsicFunctionBaseTag,
                  props = newPropBindings (),
                  proto = ref Null,
                  magic = ref (SOME (NativeFunction f)) })
    
val (objectType:Ast.TYPE_EXPR) = Ast.ObjectType []

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
             
(*
 * To get from any object to its CLS, you work out the
 * "nominal base" of the object's tag. You can then find
 * a fixed prop in the global object that has a "Class"
 * magic value pointing to the CLS.
 *)

fun nominalBaseOfTag (t:VAL_TAG) 
    : Ast.NAME = 
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

fun needMagic (v:VAL) 
    : (MAGIC) = 
    case v of 
        Object (Obj ob) => valOf (!(#magic ob))
      | _ => error ["require object with magic"]

(* FIXME: this is not the correct toString *)

fun magicToString (magic:MAGIC) 
    : string =
    case magic of 
        Double n => if Real64.== (n, (Real64.realFloor n))
                    then Int.toString (Real64.floor n)
                    else Real64.toString n
      | Decimal d => Decimal.toString d
      | Int i => Int32.toString i
      | UInt u => Word32.toString u
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
      | ByteArray _ => "[ByteArray]"
      | NativeFunction _ => "[function NativeFunction]"

fun toString (v:VAL) : string = 
    case v of 
        Undef => "undefined"
      | Null => "null"
      | Object (Obj ob) => 
        (case !(#magic ob) of 
             NONE => "[object Object]"
           | SOME magic => 
             magicToString magic)
        

fun toBoolean (v:VAL) : bool = 
    case v of 
        Undef => false
      | Null => false
      | Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Bool b) => b
           | _ => true)

        
fun coerceToDecimal (magic:MAGIC) 
    : Decimal.DEC = 
    case magic of 
        Decimal d => d
      | Double d => valOf (Decimal.fromStringDefault (Real64.toString d))
      | Int i => valOf (Decimal.fromStringDefault (Int32.toString i))
      | UInt u => valOf (Decimal.fromStringDefault (Word32.toString u))
      | _ => error ["unexpected magic in coercion to decimal"]

fun coerceToDouble (magic:MAGIC) 
    : Real64.real = 
    case magic of 
        Double d => d
      | Decimal d => valOf (Real64.fromString (Decimal.toString d))
      | Int i => Real64.fromLargeInt (Int32.toLarge i)
      | UInt u => Real64.fromLargeInt (Word32.toLargeInt u)
      | _ => error ["unexpected magic in coercion to double"]

fun coerceToUInt (magic:MAGIC) 
    : Word32.word =
    case magic of 
        UInt u => u
      | Int i => 
        (* FIXME: is < 0 really an error? Or do we want to permit it? *)
        if i >= 0 
        then Word32.fromInt (Int32.toInt i)
        else error ["negative integer to unsigned integer conversion"]
      | Decimal d => valOf (Word32.fromString (Decimal.toString d))
      (* FIXME: might want to involve the rounding mode and IEEEReal here. *)
      | Double d => Word32.fromInt (Real64.trunc d)
      | _ => error ["unexpected magic in coercion to uint"]

fun coerceToInt (magic:MAGIC) 
    : Int32.int =
    case magic of
        Int i => i
      | UInt u => 
        let 
            val lu = Word32.toLargeInt u
            val lmax = Int32.toLarge (valOf (Int32.maxInt))
        in
            if LargeInt.<=(lu, lmax)
            then Int32.fromLarge lu
            (* FIXME: is > maxint really an error? Or do we want to permit it? *)
            else error ["unsigned integer out of range of signed integer"]
        end
      | Decimal d => valOf (Int32.fromString (Decimal.toString d))
      (* FIXME: might want to involve the rounding mode and IEEEReal here. *)
      | Double d => Int32.fromInt (Real64.trunc d)
      | _ => error ["unexpected magic in coercion to int"]


val nativeFunctions:(Ast.NAME * NATIVE_FUNCTION) list ref = ref [] 
                                                        
fun registerNativeFunction (name:Ast.NAME)
                           (func:NATIVE_FUNCTION)
    : unit =
    (trace ["registering native function: ", LogErr.name name];
     nativeFunctions := (name, func) :: (!nativeFunctions))
    
fun getNativeFunction (name:Ast.NAME) 
    : NATIVE_FUNCTION = 
    let 
        fun search [] = LogErr.hostError ["native function not found: ",
					                      LogErr.name name]
          | search ((n,f)::bs) = 
            if n = name
            then f
            else search bs
    in
        search (!nativeFunctions)
    end

fun resetGlobalObject _ = 
    case globalObject of
        (Obj { props, magic, proto, ... }) => 
        (props := [];
         magic := NONE;
         proto := Null)

end


         
         
