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

datatype VAL = Object of OBJ
	     | Null
	     | Undef

     and OBJ = 
	 Obj of { tag: VAL_TAG,			   
		  props: PROP BINDINGS,
		  proto: (VAL option) ref,
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
		  instanceFixtures: FIXTURES,
		  instancePrototype: VAL,
		  
		  initialized: bool ref }

     and IFACE = 
	 Iface of { ty: TYPE,
		    bases: IFACE list,
		    definition: Ast.INTERFACE_DEFN,			
		    isInitialized: bool ref }
		   
     and SCOPE = 
	 Scope of { tag: SCOPE_TAG, 
		    object: OBJ,
		    parent: SCOPE option }

     and FIXTURES_TAG = 
	 ClassFixtures  
       | InterfaceFixtures
       | FunctionFixtures
       | GlobalFixtures

     and FIXTURES = 
	 Fixtures of { tag: FIXTURES_TAG,
		       parent: FIXTURES option,	     		       
		       fixtures: FIXTURE BINDINGS,
		       isExtensible: bool }

     and FIXTURE = 
	 Fixture of { ty: TYPE,
		      readOnly: bool,
		      isOverride: bool }

     and PROP_KIND = TypeProp 
		   | ValProp
		
withtype NAME = { ns: NS, 
		  id: ID }
		
     and MULTINAME = { nss: NS list, 
		       id: ID }

     and FUNC_CLOSURE = 
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

     and PROP = { kind: PROP_KIND,
		  ty: TYPE,
		  value: VAL,	   
		  attrs: ATTRS }

     and 'a BINDINGS = ((NAME * 'a) list) ref


(* Exceptions for "abstract machine failures". *)

exception ReferenceException of NAME
exception ConversionException of STR
exception MultiReferenceException of MULTINAME
exception UnimplementedException of STR
exception MachineException of STR

(* Values *)

fun newPropBindings _ : PROP BINDINGS = 
    let 
	val b:PROP BINDINGS = ref []
    in
	b
    end

fun newFixtureBindings _ : FIXTURE BINDINGS = 
    let 
	val b:FIXTURE BINDINGS = ref []
    in
	b
    end

fun addBinding (b:'a BINDINGS) (n:NAME) (x:'a) = 
    b := ((n,x) :: (!b))

fun delBinding (b:'a BINDINGS) (n:NAME) = 
    let 
	fun strip [] = raise ReferenceException n
	  | strip ((k,v)::bs) = 
	    if k = n 
	    then bs
	    else (k,v)::(strip bs)
    in
	b := strip (!b)
    end

fun getBinding (b:'a BINDINGS) (n:NAME) : 'a = 
    let 
	fun search [] = raise ReferenceException n			      
	  | search ((k,v)::bs) = 
	    if k = n 
	    then v
	    else search bs
    in
	search (!b)
    end

fun hasBinding (b:'a BINDINGS) (n:NAME) : bool = 
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
val intrinsicBooleanName:NAME = { ns = Ast.Intrinsic, id = "Boolean" }
val intrinsicNumberName:NAME = { ns = Ast.Intrinsic, id = "Number" }
val intrinsicStringName:NAME = { ns = Ast.Intrinsic, id = "String" }
val intrinsicNamespaceName:NAME = { ns = Ast.Intrinsic, id = "Namespace" }

val intrinsicObjectBaseTag:VAL_TAG = ClassTag (intrinsicObjectName)
val intrinsicArrayBaseTag:VAL_TAG = ClassTag (intrinsicArrayName)
val intrinsicFunctionBaseTag:VAL_TAG = ClassTag (intrinsicFunctionName)
val intrinsicBooleanBaseTag:VAL_TAG = ClassTag (intrinsicBooleanName)
val intrinsicNumberBaseTag:VAL_TAG = ClassTag (intrinsicNumberName)
val intrinsicStringBaseTag:VAL_TAG = ClassTag (intrinsicStringName)
val intrinsicNamespaceBaseTag:VAL_TAG = ClassTag (intrinsicNamespaceName)

fun newObj (t:VAL_TAG) (p:VAL option) (m:MAGIC option) : OBJ = 
    Obj { tag = t,
	  props = newPropBindings (),
	  proto = ref p,
	  magic = ref m }
			  
fun newObject (t:VAL_TAG) (p:VAL option) (m:MAGIC option) : VAL = 
    Object (newObj t p m)

fun newNumber (n:real) : VAL = 
    newObject intrinsicNumberBaseTag NONE (SOME (Number n))

fun newString (s:STR) : VAL = 
    newObject intrinsicStringBaseTag NONE (SOME (String s))

fun newBoolean (b:bool) : VAL = 
    newObject intrinsicBooleanBaseTag NONE (SOME (Bool b))

fun newNamespace (n:NS) : VAL = 
    newObject intrinsicNamespaceBaseTag NONE (SOME (Namespace n))

fun newFunc (e:SCOPE)  (f:Ast.FUNC) : VAL = 
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
	newObject tag NONE (SOME (Function closure))
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
					 stmts = [] }

val (globalFixtures:FIXTURES) = 
    Fixtures { parent = NONE,
	       tag = GlobalFixtures,
	       fixtures = newFixtureBindings (),
	       isExtensible = false }

val (globalObject:OBJ) = newObj intrinsicObjectBaseTag NONE NONE

val (globalScope:SCOPE) = 
    Scope { tag = VarGlobal,
	    object = globalObject,
	    parent = NONE }

val nan = Real.posInf / Real.posInf

fun addFixturesToObject (fixs:FIXTURES) (obj:OBJ) : unit = 
    case fixs of
	Fixtures { fixtures, ... } => ()

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

fun getGlobalVal (n:NAME) = 
    case globalObject of 
	Obj ob => 
	let 
	    val prop = getBinding (#props ob) n
	in 
	    if (#kind prop) = ValProp
	    then (#value prop)
	    else Undef
	end

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

and populateIntrinsics globalObj = 
    case globalObj of 
	Obj { props, ... } => 
	let 
	    fun newHostFunctionObj f = 
		Object (Obj { tag = intrinsicFunctionBaseTag,
			      props = newPropBindings (),
			      proto = ref NONE,
			      magic = ref (SOME (HostFunction f)) })
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
end
