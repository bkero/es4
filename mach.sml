(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local type aliases *)

type TYPE = Ast.TYPE_EXPR
type STR = Ast.USTRING
type ID = Ast.IDENT
type NS = Ast.NAMESPACE
type NAME = Ast.NAME
type MULTINAME = Ast.MULTINAME
type BINDINGS = Ast.BINDINGS
type FIXTURES = Ast.FIXTURES

datatype SCOPE_TAG = 
	 VarGlobal       (* Variable object created before execution starts *)
       | VarClass        (* Variable object for class objects               *)
       | VarInstance     (* Variable object for class instances             *)
       | VarInitializer  (* Variable object created on entry an initializer *)
       | VarActivation   (* Variable object created on entry to a function  *)
       | With            (* Created by 'with' bindings                      *)
       | Let             (* Created by 'catch', 'let', etc.                 *)

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
		    
     and CLS = 
	 Cls of { ty: TYPE,
		  (* FIXME: revive these slots as needed; disabled for now *)
    (*
		  isSealed: bool,
		  scope: SCOPE,
   		  base: CLS option,
		  interfaces: IFACE list,
		  
		  call: FUN_CLOSURE option,
     *)
		  definition: Ast.CLASS_DEFN
    (*
		  constructor: FUN_CLOSURE option,
		  
		  instanceTy: TYPE,
		  instancePrototype: VAL,
		  
		  initialized: bool ref
     *)
		}

     and IFACE = 
	 Iface of { ty: TYPE,
		    bases: IFACE list,
		    definition: Ast.INTERFACE_DEFN,			
		    isInitialized: bool ref }
		   
     and SCOPE = 
	 Scope of { tag: SCOPE_TAG, 
		    object: OBJ,
		    parent: SCOPE option }
			
     and PROP_STATE = TypeVarProp
		    | TypeProp
		    | UninitProp
		    | ValProp of VAL
		
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

fun addProp (b:PROP_BINDINGS) (n:NAME) (x:PROP) = 
    b := ((n,x) :: (!b))

fun delProp (b:PROP_BINDINGS) (n:NAME) = 
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

fun getProp (b:PROP_BINDINGS) (n:NAME) : PROP = 
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


fun getFixture (b:Ast.FIXTURE_BINDINGS) (n:NAME) : Ast.FIXTURE = 
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


fun hasProp (b:PROP_BINDINGS) (n:NAME) : bool = 
    let 
	fun search [] = false
	  | search ((k,v)::bs) = 
	    if k = n 
	    then true
	    else search bs
    in
	search (!b)
    end

fun hasFixture (b:Ast.FIXTURE_BINDINGS) (n:NAME) : bool = 
    let 
	fun search [] = false
	  | search ((k,v)::bs) = 
	    if k = n 
	    then true
	    else search bs
    in
	search b
    end

(* Standard runtime objects and functions. *)

val intrinsicObjectName:NAME = { ns = Ast.Intrinsic, id = "Object" }
val intrinsicArrayName:NAME = { ns = Ast.Intrinsic, id = "Array" }
val intrinsicFunctionName:NAME = { ns = Ast.Intrinsic, id = "Function" }
val intrinsicBooleanName:NAME = { ns = Ast.Intrinsic, id = "Boolean" }
val intrinsicNumberName:NAME = { ns = Ast.Intrinsic, id = "Number" }
val intrinsicStringName:NAME = { ns = Ast.Intrinsic, id = "String" }
val intrinsicNamespaceName:NAME = { ns = Ast.Intrinsic, id = "Namespace" }
val intrinsicClassName:NAME = { ns = Ast.Intrinsic, id = "Class" }
val intrinsicInterfaceName:NAME = { ns = Ast.Intrinsic, id = "Interface" }

val intrinsicObjectBaseTag:VAL_TAG = ClassTag (intrinsicObjectName)
val intrinsicArrayBaseTag:VAL_TAG = ClassTag (intrinsicArrayName)
val intrinsicFunctionBaseTag:VAL_TAG = ClassTag (intrinsicFunctionName)
val intrinsicBooleanBaseTag:VAL_TAG = ClassTag (intrinsicBooleanName)
val intrinsicNumberBaseTag:VAL_TAG = ClassTag (intrinsicNumberName)
val intrinsicStringBaseTag:VAL_TAG = ClassTag (intrinsicStringName)
val intrinsicNamespaceBaseTag:VAL_TAG = ClassTag (intrinsicNamespaceName)
val intrinsicClassBaseTag:VAL_TAG = ClassTag (intrinsicClassName)
val intrinsicInterfaceBaseTag:VAL_TAG = ClassTag (intrinsicInterfaceName)

(* To reference something in the intrinsic namespace, you need a complicated expression. *)
val intrinsicNsExpr = Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Intrinsic))
fun intrinsicName id = Ast.QualifiedIdentifier { qual = intrinsicNsExpr, ident = id }

(* Define some global intrinsic nominal types. *)

val typeType = Ast.NominalType { ident = (intrinsicName "Type"), nullable = NONE }
val namespaceType = Ast.NominalType { ident = (intrinsicName "Namespace"), nullable = NONE }
val classType = Ast.NominalType { ident = intrinsicName "Class", nullable = NONE }

fun newObj (t:VAL_TAG) (p:VAL) (m:MAGIC option) : OBJ = 
    Obj { tag = t,
	  props = newPropBindings (),
	  proto = ref p,
	  magic = ref m }
			  
fun newSimpleObj (m:MAGIC option) : OBJ = 
    newObj intrinsicObjectBaseTag Null m

fun newObject (t:VAL_TAG) (p:VAL) (m:MAGIC option) : VAL = 
    Object (newObj t p m)

fun newSimpleObject (m:MAGIC option) : VAL = 
    Object (newSimpleObj m)

fun newNumber (n:real) : VAL = 
    newObject intrinsicNumberBaseTag Null (SOME (Number n))

fun newString (s:STR) : VAL = 
    newObject intrinsicStringBaseTag Null (SOME (String s))

fun newBoolean (b:bool) : VAL = 
    newObject intrinsicBooleanBaseTag Null (SOME (Bool b))

fun newNamespace (n:NS) : VAL = 
    newObject intrinsicNamespaceBaseTag Null (SOME (Namespace n))

fun newClass (e:SCOPE) (c:Ast.CLASS_DEFN) : VAL =
    let
	val cls = Cls { ty = classType,
			definition = c }
	val closure = { cls = cls,
			allTypesBound = ((length (#params c)) = 0),
			env = e }
    in
	newObject intrinsicClassBaseTag Null (SOME (Class closure))
    end


fun newFunc (e:SCOPE) (f:Ast.FUNC) : VAL = 
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
	    
val (objectType:TYPE) = Ast.ObjectType []

val (defaultAttrs:Ast.ATTRIBUTES) = 
    Ast.Attributes { ns = Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Public "")),
		     override = false,
		     static = false,
		     final = false,
		     dynamic = true,
		     prototype = false,
		     native = false,
		     rest = false }

val (emptyBlock:Ast.BLOCK) = Ast.Block { pragmas = [],
					 defns = [],
					 stmts = [],
					 fixtures = NONE }

val (globalObject:OBJ) = newObj intrinsicObjectBaseTag Null NONE

val (globalScope:SCOPE) = 
    Scope { tag = VarGlobal,
	    object = globalObject,
	    parent = NONE }

val nan = Real.posInf / Real.posInf


fun getValue (obj:OBJ) (name:NAME) : VAL = 
    case obj of 
	Obj {props, ...} => 
	let 
	    val prop = getProp props name
	in
	    case (#state prop) of 
		TypeProp => LogErr.machError ["getValue on a type property"]
	      | TypeVarProp => LogErr.machError ["getValue on a type variable property"]
	      | UninitProp => LogErr.machError ["getValue on an uninitialized property"]
	      | ValProp v => v
	end

(* A "defValue" call occurs when assigning a property definition's 
 * initial value, as specified by the user. All other assignments
 * to a property go through "setValue". *)

fun defValue (base:OBJ) (name:NAME) (v:VAL) : unit =
    case base of 
	Obj { props, ... } => 
	if not (hasProp props name)
	then LogErr.machError ["defValue on missing property"]
	else (* Here we have relaxed rules: you can write to an 
	      * uninitialized property or a read-only property. *)
	    let 
		val existingProp = getProp props name
				   
		val _ = case (#state existingProp) of 
			    
			    TypeVarProp => 
			    LogErr.machError ["defValue on type variable property:", 
					      LogErr.name name]
			    
			  | TypeProp => 
			    LogErr.machError ["defValue on type property: ", 
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

fun setValue (base:OBJ) (name:NAME) (v:VAL) : unit = 
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

fun nominalBaseOfTag (t:VAL_TAG) = 
    case t of 
	ObjectTag _ => intrinsicObjectName
      | ArrayTag _ => intrinsicArrayName
      | FunctionTag _ => intrinsicFunctionName
      | ClassTag c => c

fun getMagic (v:VAL) : (MAGIC option) = 
    case v of 
	Object (Obj ob) => !(#magic ob)
      | _ => NONE

fun getGlobalVal (n:NAME) : VAL = 
    getValue globalObject n

fun valToCls (v:VAL) : (CLS option) = 
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
		  Number n => Real.toString n
		| String s => s
		| Bool true => "true"
		| Bool false => "false"
		| Namespace Ast.Private => "[private namespace]"
		| Namespace Ast.Protected => "[protected namespace]"
		| Namespace Ast.Intrinsic => "[intrinsic namespace]"
		| Namespace (Ast.Public id) => "[public namespace: " ^ id ^ "]"
		| Namespace (Ast.Internal _) => "[internal namespace]"
		| Namespace (Ast.UserDefined id) => "[user-defined namespace " ^ id ^ "]"
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


fun populateIntrinsics globalObj = 
    case globalObj of 
	Obj { props, ... } => 
	let 
	    fun newHostFunctionObj f = 
		Object (Obj { tag = intrinsicFunctionBaseTag,
			      props = newPropBindings (),
			      proto = ref Null,
			      magic = ref (SOME (HostFunction f)) })
	    fun bindFunc (n, f) = 
		let 
		    val name = { id = n, ns = Ast.Intrinsic }
		    val prop = { ty = Ast.SpecialType Ast.Any,
				 state = ValProp (newHostFunctionObj f), 
				 attrs = { dontDelete = true,
					   dontEnum = false,
					   readOnly = true,
					   isFixed = true } }
		in
		    addProp props name prop
		end
	in
	    List.app bindFunc 
	    [ ("print", hostPrintFunction) ]
	end	
end
