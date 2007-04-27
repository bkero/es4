(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local tracing machinery *)

val doTrace = ref false
fun log ss = LogErr.log ("[mach] " :: ss) 
fun trace ss = if (!doTrace) then log ss else ()
fun error ss = LogErr.machError ss

fun nameEq (a:Ast.NAME) (b:Ast.NAME) = ((#id a) = (#id b) andalso (#ns a) = (#ns b))

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
       | NoTag (* 
                * NoTag objects are made for scopes and 
                * temporaries passed as arguments during 
                * builtin construction. 
                *)

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
       | String of Ustring.STRING
       | Boolean of bool
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
                    kind: SCOPE_KIND }
                  
     and SCOPE_KIND = 
         WithScope
       | GlobalScope
       | InstanceScope
       | ActivationScope
       | TempScope

     and TEMP_STATE = UninitTemp
                    | ValTemp of VAL
                        
     and PROP_STATE = TypeVarProp
                    | TypeProp
                    | UninitProp
                    | ValProp of VAL

                    (* One might imagine that namespaces, methods and 
                     * the 'arguments' object in a function can all be stored
                     * as instances of the classes Namespace, Function and 
                     * Array, respectively. This works in some contexts, but leads
                     * to feedback loops when constructing the classes Function,
                     * Namespace and Array themselves. So instead of eagerly instantiating
                     * such values, we allocate them as the following 3 "lazy" 
                     * property states. If anyone performs a "get" on these property
                     * states, a Namespace, Function or Array object (respectively) is 
                     * constructed. We then ensure that the Namespace, Function and Array
                     * constructors, settings and initializers (in the builtins) 
                     * do *not* cause any "get" operations on properties in these states.
                     *
                     * FIXME: The 'arguments' object can't be an array.
                     *)
                    | NamespaceProp of Ast.NAMESPACE
                    | MethodProp of FUN_CLOSURE
                    | ValListProp of VAL list

                    | NativeFunctionProp of NATIVE_FUNCTION
                    | VirtualValProp of 
                      { getter: FUN_CLOSURE option,
                        setter: FUN_CLOSURE option }
                
withtype FUN_CLOSURE = 
         { func: Ast.FUNC,
           this: OBJ option,
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
         int

(* Important to model "fixedness" separately from 
 * "dontDelete-ness" because fixedness affects 
 * which phase of name lookup the name is found during.
 *)

     and ATTRS = { dontDelete: bool,
                   dontEnum: bool,
                   readOnly: bool,
                   isFixed: bool }

     and TEMPS = (Ast.TYPE_EXPR * TEMP_STATE) list ref

     and PROP = { ty: Ast.TYPE_EXPR,
                  state: PROP_STATE,                  
                  attrs: ATTRS }

     and PROP_BINDINGS = ((Ast.NAME * PROP) list) ref

     and REGS = { scope: SCOPE, 
                  this: OBJ }

(* Exceptions for control transfer. *)

exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> VAL)
exception ThrowException of VAL
exception ReturnException of VAL
exception StopIterationException

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
             SOME (Boolean _) => true
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
                                         Ustring.toAscii (#id n)]
          | strip (((k:Ast.NAME),v)::bs) = 
            if (#ns k) = (#ns n) andalso 
               (#id k) = (#id n)
            then bs
            else (k,v)::(strip bs)
    in
        b := strip (!b)
    end    


fun findProp (b:PROP_BINDINGS) 
             (n:Ast.NAME) 
    : PROP option = 
    let 
        fun search [] = NONE
          | search (((k:Ast.NAME),v)::bs) = 
            if (#id k) = (#id n) andalso 
               (#ns k) = (#ns n)
            then SOME v
            else search bs
    in
        search (!b)
    end


fun matchProps (fixedProps:bool)
               (b:PROP_BINDINGS)
               (searchId:Ast.IDENT)
               (nss:Ast.NAMESPACE list)
    : Ast.NAME list =
    let 
        fun matchProp (n:Ast.NAME,p:PROP) : Ast.NAME option = 
            if not (searchId = (#id n))
            then NONE
            else 
                let
                    fun matchNS candidateNS = 
                        case candidateNS of
                            Ast.LimitedNamespace (ident,limNS) =>
                            if searchId = ident
                            then (#ns n) = limNS
                            else false
                          | _ => (#ns n) = candidateNS
                in
                    if ((fixedProps andalso (#isFixed (#attrs p))) orelse
                        (not fixedProps andalso not (#isFixed (#attrs p))))
                    then 
                        if List.exists matchNS nss
                        then SOME n
                        else NONE
                    else
                        NONE
                end
    in
        List.mapPartial matchProp (!b)
    end
      

fun getProp (b:PROP_BINDINGS) 
            (n:Ast.NAME) 
    : PROP = 
    case findProp b n of
        SOME p => p
      | NONE => 
        (*
         * If not found, then cons up a temporary property
         * with value undefined. Any property not found
         * errors would have been caught by evalRefExpr
         *)
        {ty=Ast.SpecialType Ast.Undefined,
         state=ValProp Undef,
         attrs={dontDelete=false,  (* unused attrs *)
                dontEnum=false,
                readOnly=false,
                isFixed=false}}


fun hasProp (b:PROP_BINDINGS) 
            (n:Ast.NAME) 
    : bool = 
    case findProp b n of 
        NONE => false
      | SOME _ => true


fun hasMagic (ob:OBJ) = 
    case ob of 
        Obj { magic, ... } => 
        case !magic of 
            SOME _ => true
          | NONE => false

fun setPropDontEnum (props:PROP_BINDINGS)
                    (n:Ast.NAME)
                    (dontEnum:bool) 
    : unit = 
    case findProp props n of
        SOME prop => 
        let 
            val attrs = (#attrs prop)
            val newProp = { ty = (#ty prop),
                            state = (#state prop),
                            attrs = { dontDelete = (#dontDelete attrs),
                                      dontEnum = dontEnum,
                                      readOnly = (#readOnly attrs),
                                      isFixed = (#isFixed attrs) } }
        in
            delProp props n;
            addProp props n newProp
        end
      | NONE => ()
    

(* Safe: will overflow when it runs out of identities. *)
val currIdent = ref 0
fun nextIdent _ =
    (currIdent := (!currIdent) + 1;
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

fun newObjNoTag _ 
    : OBJ = 
    newObj NoTag Null NONE

fun setProto (ob:OBJ) (p:VAL) 
    : OBJ =
    let 
         val Obj {proto, ...} = ob
    in
        proto := p;
        ob
    end

fun setMagic (ob:OBJ) (m:MAGIC option) 
    : OBJ =
    let 
         val Obj {magic, ...} = ob
    in
        magic := m;
        ob
    end

fun newObject (t:VAL_TAG) 
              (p:VAL) 
              (m:MAGIC option) 
    : VAL = 
    Object (newObj t p m)

    
val (objectType:Ast.TYPE_EXPR) = 
    Ast.ObjectType []

val (emptyBlock:Ast.BLOCK) = 
    Ast.Block { pragmas = [],
                defns = [],
                body = [],
                head= NONE,
                pos=NONE }
                             
fun getTemp (temps:TEMPS)
            (n:int)
    : VAL =
    case List.nth ((!temps), n) of
        (_, UninitTemp) => LogErr.machError ["getting uninitialized temporary ",Int.toString n]
      | (_, ValTemp v) => v

fun defTemp (temps:TEMPS)
            (n:int)
            (v:VAL) 
    : unit = 
    let
        val _ = trace ["defTemp ",Int.toString n]
        fun replaceNth k [] = LogErr.machError ["temporary-definition error"]
          | replaceNth k (x::xs) =
            if k = 0 
            then (case x of 
                      (t, UninitTemp) => 
                      ((* FIXME: put typecheck here *)
                       (t, ValTemp v) :: xs)
                    | (t, _) => 
                       (t, ValTemp v) :: xs)
                (* ISSUE: we allow redef of temps: LogErr.machError ["re-defining temporary"]) *)
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
        ObjectTag _ => Name.public_Object
      | ArrayTag _ => Name.public_Array
      | FunctionTag _ => Name.public_Function
      | ClassTag c => c
      | ScopeTag => error ["searching for nominal base of scope object"]

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

fun needClass (v:VAL) 
    : (CLS_CLOSURE) = 
    case needMagic v of 
        Class cls => cls
      | _ => error ["require class object"]

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

end




