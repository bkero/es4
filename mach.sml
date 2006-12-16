(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local type aliases *)

type TYPE = Ast.TYPE_EXPR
type STR = Ast.USTRING
type ID = Ast.IDENT
type NS = Ast.NAMESPACE

datatype SCOPE_TAG = 
	 VarGlobal       (* Variable object created before execution starts *)
       | VarClass        (* Variable object for class objects               *)
       | VarInstance     (* Variable object for class instances             *)
       | VarActivation   (* Variable object created on entry to a function  *)
       | With            (* Created by 'with' bindings                      *)
       | Let             (* Created by 'catch', 'let', etc.                 *)

datatype VAL = Object of { tag: VAL_TAG,			   
			   props: BINDINGS,
			   proto: (VAL option) ref,
			   magic: (MAGIC option) ref }
	     | Null
	     | Undef
	       
     and VAL_TAG =
	 ObjectTag of Ast.FIELD_TYPE list
       | ArrayTag of TYPE list
       | FunctionTag of Ast.FUNC_SIG
       | ClassTag of NAME

(* 
 * Magic is visible only to the interpreter; 
 * it is not visible to users.
 *)
	       
     and MAGIC = Number of real    (* someday to be more complicated *)
	       | String of STR (* someday to be unicode *)
	       | Bool of bool
	       | Namespace of NS
	       | Class of CLS_CLOSURE
	       | Interface of IFACE_CLOSURE
	       | Function of FUNC_CLOSURE
	       | HostFunction of (VAL list -> VAL)
		    
     and CLS = 
	 Cls of { ty: TYPE,
		  isSealed: bool,
		  scope: SCOPE,
   		  base: CLS option,
		  interfaces: IFACE list,
		  
		  call: FUNC_CLOSURE option,
		  definition: Ast.CLASS_DEFN,
		  constructor: FUNC_CLOSURE option,
		  
		  instanceTy: TYPE,
		  instanceLayout: LAYOUT,
		  instancePrototype: VAL,
		  
		  initialized: bool ref }

     and IFACE = 
	 Iface of { ty: TYPE,
		    bases: IFACE list,
		    definition: Ast.INTERFACE_DEFN,			
		    isInitialized: bool ref }
		   
     and SCOPE = 
	 Scope of { tag: SCOPE_TAG, 
		    object: VAL,
		    parent: SCOPE option }

     and LAYOUT_TAG = 
	 LayoutClass  
       | LayoutInterface
       | LayoutFunction

     and LAYOUT = 
	 Layout of { parent: LAYOUT option,		     
		     tag: LAYOUT_TAG,
		     items: ITEM list,
		     isExtensible: bool }

     and ITEM = 
	 Item of { name: NAME,
		   ty: TYPE,   
		   attrs: ATTRS }

     and PROP_KIND = TypeProp 
		   | ValProp
		
withtype NAME = { ns: NS, 
		  id: ID }
		
     and MULTINAME = { nss: NS list, 
		       id: ID }

     and FUNC_CLOSURE = 
	 { fsig: Ast.FUNC_SIG,
	   func: Ast.FUNC, 
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

     and PROP = { kind: PROP_KIND,
		  ty: TYPE,
		  value: VAL,	   
		  attrs: ATTRS }
	 
     and BINDINGS = ((NAME * PROP) list) ref

(* Exceptions for "abstract machine failures". *)

exception ReferenceException of NAME
exception ConversionException of STR
exception MultiReferenceException of MULTINAME
exception UnimplementedException of STR
exception MachineException of STR

(* Values *)

fun newBindings _ = 
    let 
	val b:BINDINGS = ref []
    in
	b
    end

fun addBinding (b:BINDINGS) (n:NAME) (p:PROP) = 
    b := ((n,p) :: (!b))

fun delBinding (b:BINDINGS) (n:NAME) = 
    let 
	fun strip [] = raise ReferenceException n
	  | strip ((k,v)::bs) = 
	    if k = n 
	    then bs
	    else (k,v)::(strip bs)
    in
	b := strip (!b)
    end

fun getBinding (b:BINDINGS) (n:NAME) = 
    let 
	fun search [] = raise ReferenceException n			      
	  | search ((k,v)::bs) = 
	    if k = n 
	    then v
	    else search bs
    in
	search (!b)
    end

fun hasBinding (b:BINDINGS) (n:NAME) = 
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

val intrinsicObjectName:NAME = { ns = Ast.Intrinsic, id = "Object" }
val intrinsicArrayName:NAME = { ns = Ast.Intrinsic, id = "Array" }
val intrinsicFunctionName:NAME = { ns = Ast.Intrinsic, id = "Function" }

val intrinsicObjectBaseTag:VAL_TAG = ClassTag (intrinsicObjectName)
val intrinsicArrayBaseTag:VAL_TAG = ClassTag (intrinsicArrayName)
val intrinsicFunctionBaseTag:VAL_TAG = ClassTag (intrinsicFunctionName)

fun newObject (t:VAL_TAG) : VAL = 
    Object { tag = t,
	     props = newBindings (),
	     proto = ref NONE,
	     magic = ref NONE }

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
					 stmts = [] }

val (emptyClassDefn:Ast.CLASS_DEFN) = 
    { name = "",
      nonnullable = true,
      attrs = defaultAttrs,
      params = [],
      extends = NONE,
      implements = [],
      body = emptyBlock,
      instanceVars = [],
      instanceMethods = [],
      vars = [],
      methods = [],
      constructor = NONE,
      initializer = [] }
    
val (emptyClassLayout:LAYOUT) = 
    Layout { parent = NONE,
	     tag = LayoutClass,
	     items = [],	     
	     isExtensible = true }

val (globalObject:VAL) = newObject intrinsicObjectBaseTag
    
val (globalScope:SCOPE) = 
    Scope { tag = VarGlobal,
	    object = globalObject,
	    parent = NONE }
    
val (globalClass:CLS) = 
    Cls { ty = objectType,
	  scope = globalScope,
	  base = NONE,
	  interfaces = [],
	  call = NONE,
	  definition = emptyClassDefn,
	  constructor = NONE,
	  instanceTy = objectType,
	  instanceLayout = emptyClassLayout,
	  instancePrototype = globalObject,
	  isSealed = false,
	  initialized = ref true }

val nan = Real.posInf / Real.posInf

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
	Object ob => !(#magic ob)
      | _ => NONE

fun getGlobalVal (n:NAME) = 
    case globalObject of 
	Object ob => 
	let 
	    val prop = getBinding (#props ob) n
	in 
	    if (#kind prop) = ValProp
	    then (#value prop)
	    else Undef
	end
      | _ => Undef

fun valToCls (v:VAL) : (CLS option) = 
    case v of 
	Object ob => (case getMagic (getGlobalVal (nominalBaseOfTag (#tag ob))) of
			  SOME (Class {cls,...}) => SOME cls
			| _ => NONE)
      | _ => NONE

(* FIXME: this is not the correct toString *)

fun toString (v:VAL) : string = 
    case v of 
	Undef => "undefined"
      | Null => "null"
      | Object ob => 
	(case !(#magic ob) of 
	     NONE => "[object Object]"
	   | SOME magic => 
	     (case magic of 
		  Number n => Real.toString n
		| Bool true => "true"
		| Bool false => "false"
		| String s => s
		| Namespace Ast.Private => "[private namespace]"
		| Namespace Ast.Protected => "[protected namespace]"
		| Namespace Ast.Intrinsic => "[intrinsic namespace]"
		| Namespace (Ast.Public id) => "[public namespace: " ^ id ^ "]"
		| Namespace (Ast.Internal _) => "[internal namespace]"
		| Namespace (Ast.UserDefined id) => "[user-defined namespace " ^ id ^ "]"
		| Function _ => "[function Function]"
		| HostFunction _ => "[function HostFunction]"
		| Class _ => "[class Class]"
		| Interface _ => "[interface Interface]"))

fun toNum (v:VAL) : real = 
    case v of 
	Undef => nan
      | Null => 0.0
      | Object ob => 
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
      | Object ob => 
	(case !(#magic ob) of 
	     SOME (Bool b) => b
	   | _ => true)

		  
fun equals (va:VAL) (vb:VAL) : bool = 
    case (va,vb) of 
	(Object oa, Object ob) => 
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
	(Object oa, Object ob) =>
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

and populateIntrinsics globalObj = 
    case globalObj of 
	Object { props, ... } => 
	let 
	    fun newHostFunctionObj f = 
		Object { tag = intrinsicFunctionBaseTag,
			 props = newBindings (),
			 proto = ref NONE,
			 magic = ref (SOME (HostFunction f)) }
	    fun bindFunc (n, f) = 
		let 
		    val name = { id = n, ns = Ast.Intrinsic }
		    val prop = { kind = ValProp, 
				 ty = Ast.SpecialType Ast.Any,
				 value = newHostFunctionObj f,
				 attrs = { dontDelete = true,
					   dontEnum = false,
					   readOnly = true,
					   isFixed = false } }
		in
		    addBinding props name prop
		end
	in
	    List.app bindFunc 
	    [ ("print", hostPrintFunction) ]
	end	
      | _ => raise MachineException "Undefined or Null Global Object"
end
