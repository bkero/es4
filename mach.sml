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
         Obj of { ident: OBJ_IDENT,
                  tag: VAL_TAG,                           
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

     and OBJ_IDENT = 
         LargeInt.int

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

(* Exceptions for control transfer. *)

exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> VAL)
exception ThrowException of VAL
exception ReturnException of VAL

fun isObject (v:VAL) : bool = 
    case v of 
        Object _ => true
      | _ => false


fun isUInt (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (UInt _) => true
           | _ => false)
      | _ => false


fun isInt (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Int _) => true
           | _ => false)
      | _ => false


fun isDouble (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Double _) => true
           | _ => false)
      | _ => false


fun isDecimal (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Decimal _) => true
           | _ => false)
      | _ => false


fun isString (v:VAL) 
    : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (String _) => true
           | _ => false)
      | _ => false


fun isBoolean (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Bool _) => true
           | _ => false)
      | _ => false


fun isNamespace (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Namespace _) => true
           | _ => false)
      | _ => false


fun isClass (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Class _) => true
           | _ => false)
      | _ => false


fun isInterface (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Interface _) => true
           | _ => false)
      | _ => false


fun isFunction (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Function _) => true
           | _ => false)
      | _ => false


fun isType (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Type _) => true
           | _ => false)
      | _ => false


fun isNativeFunction (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (NativeFunction _) => true
           | _ => false)
      | _ => false


fun isNumeric (v:VAL) : bool = 
    case v of 
        Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Double _) => true
           | SOME (Decimal _) => true
           | SOME (Int _) => true
           | SOME (UInt _) => true
           | _ => false)
      | _ => false


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

val publicToStringName:Ast.NAME = { ns = (Ast.Public ""), id = "toString" }
val publicValueOfName:Ast.NAME = { ns = (Ast.Public ""), id = "valueOf" }

val publicPrototypeName:Ast.NAME = { ns = (Ast.Public ""), id = "prototype" }
val internalConstructorName:Ast.NAME = { ns = (Ast.Internal ""), id = "constructor" }
val internalObjectName:Ast.NAME = { ns = (Ast.Internal ""), id = "Object" }

val internalErrorName:Ast.NAME = { ns = (Ast.Internal ""), id = "Error" }
val internalTypeErrorName:Ast.NAME = { ns = (Ast.Internal ""), id = "TypeError" }
                                            
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


val currIdent = ref (LargeInt.fromInt 0)
fun nextIdent _ =
    (currIdent := (!currIdent) + (LargeInt.fromInt 1);
     !currIdent)

fun newObj (t:VAL_TAG) 
           (p:VAL) 
           (m:MAGIC option) 
    : OBJ = 
    Obj { ident = nextIdent (), 
          tag = t,
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
    newObject intrinsicFunctionBaseTag 
              Null 
              (SOME (NativeFunction f))
    
val (objectType:Ast.TYPE_EXPR) = 
    Ast.ObjectType []

val (emptyBlock:Ast.BLOCK) = 
    Ast.Block { pragmas = [],
                defns = [],
                body = [],
                head= NONE }
    
val (globalObject:OBJ) = 
    newObj intrinsicObjectBaseTag Null NONE
    
val (globalScope:SCOPE) = 
    Scope { object = globalObject,
            parent = NONE,
            temps = ref [],
            isVarObject = true }    
    
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
      | UInt u => LargeInt.toString (Word32.toLargeInt u)
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


fun toString (v:VAL) 
    : string = 
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


fun toNumeric (v:VAL) 
    : VAL =          
    let 
        fun NaN _ = newDouble (Real64.posInf / Real64.posInf)
        fun zero _ = newDouble (Real64.fromInt 0)
        fun one _ = newDouble (Real64.fromInt 1)
    in
        case v of 
            Undef => NaN ()
          | Null => zero ()
          | Object (Obj ob) => 
            (case !(#magic ob) of 
                 SOME (Double _) => v
               | SOME (Decimal _) => v
               | SOME (Int _) => v
               | SOME (UInt _) => v
               | SOME (Bool false) => zero ()
               | SOME (Bool true) => one ()
               (* 
                * FIXME: This is not the correct definition of ToNumber applied to string.
                * See ES3 9.3.1. We need to talk it over.
                *) 
               | SOME (String s) => (case Real64.fromString s of
                                         SOME s' => newDouble s'
                                       | NONE => NaN ())
               (* 
                * FIXME: ES3 9.3 defines ToNumber on objects in terms of primitives. We've
                * reorganized the classification of primitives vs. objects. Revisit this.
                *)
               | _ => zero ())
    end
    

fun toDecimal (precision:int) 
              (mode:Decimal.ROUNDING_MODE) 
              (v:VAL) 
    : Decimal.DEC = 
    case v of 
        Undef => Decimal.NaN
      | Null => Decimal.zero
      | Object (Obj ob) => 
        (case !(#magic ob) of 
             SOME (Double d) => 
             (* NB: Lossy. *)
             (case Decimal.fromString precision mode (Real64.toString d) of
                  SOME d' => d'
                | NONE => Decimal.NaN)
           | SOME (Decimal d) => d
           | SOME (Int i) => Decimal.fromLargeInt (Int32.toLarge i)
           | SOME (UInt u) => Decimal.fromLargeInt (Word32.toLargeInt u)
           | SOME (Bool false) => Decimal.zero
           | SOME (Bool true) => Decimal.one
           (* 
            * FIXME: This is not the correct definition either. See toNumeric.
            *) 
           | SOME (String s) => (case Decimal.fromString precision mode s of
                                     SOME s' => s'
                                   | NONE => Decimal.NaN)
           (* 
            * FIXME: Possibly wrong here also. See comment in toNumeric.
            *)
           | _ => Decimal.zero)


fun toDouble (v:VAL) 
    : Real64.real = 
    let 
        fun NaN _ = (Real64.posInf / Real64.posInf)
        fun zero _ = (Real64.fromInt 0)
        fun one _ = (Real64.fromInt 1)
    in            
        case v of 
            Undef => NaN ()
          | Null => zero ()
          | Object (Obj ob) => 
            (case !(#magic ob) of 
                 SOME (Double d) => d
               | SOME (Decimal d) => 
                 (* NB: Lossy. *)
                 (case Real64.fromString (Decimal.toString d) of
                      SOME d' => d'
                    | NONE => NaN ())
                 
               | SOME (Int i) => Real64.fromLargeInt (Int32.toLarge i)
               | SOME (UInt u) => Real64.fromLargeInt (Word32.toLargeInt u)
               | SOME (Bool false) => zero ()
               | SOME (Bool true) => one ()
               (* 
                * FIXME: This is not the correct definition either. See toNumeric.
                *) 
               | SOME (String s) => (case Real64.fromString s  of
                                         SOME s' => s'
                                       | NONE => NaN())
               (* 
                * FIXME: Possibly wrong here also. See comment in toNumeric.
                *)
               | _ => zero ())
    end


fun mathOp (v:VAL) 
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
            Object (Obj ob) => 
            (case !(#magic ob) of 
                 SOME (Decimal d) => fnOrDefault decimalFn d
               | SOME (Double d) => fnOrDefault doubleFn d
               | SOME (Int i) => fnOrDefault intFn i
               | SOME (UInt u) => fnOrDefault uintFn u
               | _ => default)
          | _ => default
    end


fun isPositiveZero (v:VAL) 
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


fun isNegativeZero (v:VAL) 
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


fun isPositiveInf (v:VAL) 
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


fun isNegativeInf (v:VAL) 
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

    
fun isNaN (v:VAL)
    : bool = 
    mathOp v 
           (SOME Decimal.isNaN) 
           (SOME Real64.isNan)
           NONE NONE false


fun sign (v:VAL)
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


fun floor (v:VAL)
    : LargeInt.int =
    mathOp v 
           (SOME Decimal.floor)
           (SOME (Real64.toLargeInt IEEEReal.TO_NEGINF))
           (SOME Int32.toLarge)
           (SOME Word32.toLargeInt)
           (LargeInt.fromInt 0)
    

fun signFloorAbs (v:VAL)
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

fun toInteger (v:VAL)
    : VAL = 
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

fun toInt32 (v:VAL) 
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

fun toUInt32 (v:VAL) 
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

fun toUInt16 (v:VAL) 
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


fun fitsInUInt (x:LargeInt.int) 
    : bool = 
    let
        val uintMax = IntInf.pow(2, 32) - 1
        val uintMin = IntInf.fromInt 0
    in
        uintMin <= x andalso x <= uintMax
    end


fun fitsInInt (x:LargeInt.int) 
    : bool = 
    let
        val intMax = IntInf.pow(2, 31) - 1
        val intMin = ~ (IntInf.pow(2, 31))
    in
        intMin <= x andalso x <= intMax
    end


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




