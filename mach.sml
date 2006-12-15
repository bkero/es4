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

datatype VAL = 
         Null
       | Undef
       | Num of real
       | Bool of bool
       | Str of STR
       | Object of OBJ
       | Function of FUN
       | Namespace of NS
       | Type of TYPE

       (* 
	* The remaining VAL types are not "first class" -- they cannot be stored
	* in new variables or copied around -- but can still be bound to names 
	* within scopes, or the transient result of certain expressions.
	*) 

       | Class of CLS
       | Interface of IFACE
       | Reference of REF
		
     and OBJ = 
	 Obj of { class: CLS option,
		  bindings: BINDINGS,
		  prototype: (OBJ option) ref }

     and FUN = 
	 Fun of { ty: TYPE, 
		  layout: LAYOUT,
		  code: (OBJ -> (VAL list) -> VAL),
		  definition: Ast.FUNC_DEFN option }
		    
     and CLS = 
	 Cls of { ty: TYPE,
		  scope: SCOPE,
   		  base: CLS option,
		  interfaces: IFACE list,
		  
		  call: FUN,
		  definition: Ast.CLASS_DEFN,
		  constructor: FUN option,
		  
		  instanceTy: TYPE,
		  instanceLayout: LAYOUT,
		  instancePrototype: OBJ,
		  
		  initialized: bool ref }

     and IFACE = 
	 IFACE of { ty: TYPE,
		    bases: IFACE list,
		    definition: Ast.INTERFACE_DEFN,			
		    isInitialized: bool ref }

     and REF = 
	 Ref of { base: OBJ,
		  name: NAME }      
		   
     and SCOPE = 
	 Scope of { tag: SCOPE_TAG, 
		    obj: OBJ,
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

     and MACH = 
	 Mach of { scope: SCOPE,
		   result: VAL,
		   thisObject: OBJ,
		   numberType: Ast.NUMBER_TYPE,
		   roundingMode: Ast.ROUNDING_MODE,
		   openNamespaces: NS list }

		
withtype NAME = { ns: NS, 
		  id: ID }
		
     and MULTINAME = { nss: NS list, 
		       id: ID }

     and ATTRS = { dontDelete: bool,
		   dontEnum: bool,
		   readOnly: bool }

     and PROP = { ty: TYPE,
		  value: VAL,	   
		  attrs: ATTRS }
	 
     and BINDINGS = ((NAME * PROP) list) ref

(* Exceptions for "abstract machine failures". *)

exception ReferenceException of NAME
exception ConversionException of STR
exception MultiReferenceException of MULTINAME
exception UnimplementedException of STR

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

(* There's a circularity: 
 * 
 * SCOPE -> OBJ -> CLS -> SCOPE.
 * 
 * We need to break this so that it's possible to instantiate
 * the roots of the builtin objects, so we make OBJ have only a CLS
 * option, not a mandatory CLS. When you access the CLS of an OBJ
 * you should use an accessor function that "closes the loop" and
 * returns the global "object" CLS when you fetch the CLS of an
 * OBJ like "Obj {class = NONE, ...}".
 *)			  

val (objectType:TYPE) = Ast.RecordType []

val (defaultAttrs:Ast.ATTRIBUTES) = 
    Ast.Attributes { ns = Ast.LiteralExpr (Ast.LiteralNamespace (Ast.Public "")),
		     override = false,
		     static = false,
		     final = false,
		     dynamic = true,
		     prototype = false,
		     native = false,
			 rest = false }

val (noOpFunctionSignature:Ast.FUNC_SIGN) = 
    Ast.FunctionSignature { typeparams = [],
			    params = [],
			    resulttype = Ast.SpecialType Ast.Any }

val (emptyBlock:Ast.BLOCK) = Ast.Block { pragmas = [],
					 defns = [],
					 stmts = [] }

val (noOpFunc:Ast.FUNC) = 
    Ast.Func { name = {kind = Ast.Ordinary, ident = ""},
	       sign = noOpFunctionSignature,
	       body = emptyBlock }
    
val (noOpFunctionDefn:Ast.FUNC_DEFN) = 
    { attrs = defaultAttrs,
      kind = Ast.Var,
      func = noOpFunc }

val (emptyFuncLayout:LAYOUT) = 
    Layout { parent = NONE,
	     tag = LayoutFunction,
	     items = [],	     
	     isExtensible = true }

val (noOpFunction:FUN) = 
    Fun { ty = Ast.FunctionType { paramTypes = [],
				  returnType = Ast.SpecialType Ast.Any,
				  boundThisType = NONE,
				  hasRest = false },
	  layout = emptyFuncLayout,
	  code = (fn _ => fn _ => Undef),
	  definition = SOME noOpFunctionDefn }

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
    
val (globalObject:OBJ) = 
    Obj { class = NONE, 
	  bindings = newBindings (), 
	  prototype = ref NONE } 
    
val (globalScope:SCOPE) = 
    Scope { tag = VarGlobal,
	    obj = globalObject,
	    parent = NONE }
    
val (globalClass:CLS) = 
    Cls { ty = objectType,
	  scope = globalScope,
	  base = NONE,
	  interfaces = [],
	  call = noOpFunction,
	  definition = emptyClassDefn,
	  constructor = NONE,
	  instanceTy = objectType,
	  instanceLayout = emptyClassLayout,
	  instancePrototype = globalObject,
	  initialized = ref true }

val nan = Real.posInf / Real.posInf

fun newObject (c:CLS option) = 
    Obj { class = c,
	  bindings = newBindings (),
	  prototype = ref NONE }

fun deref (Ref {base=Obj {bindings,...}, name}) = 
    (#value (getBinding bindings name))

(* FIXME: this is not the correct toString *)
fun toString (Str s) = s
  | toString (Num n) = Real.toString n
  | toString (Bool true) = "true"
  | toString (Bool false) = "false"
  | toString (Object _) = "[object Object]"
  | toString (Function _) = "[function Function]"
  | toString (Reference r) = toString (deref r)
  | toString (Namespace _) = "[namespace]"
  | toString (Class _) = "[class]"
  | toString (Interface _) = "[interface]"
  | toString Undef = "undefined"
  | toString Null = "null"


fun toNum (Str s) = (case Real.fromString s of
			SOME n => n
		      | NONE => nan)
  | toNum (Num n) = n
  | toNum (Bool true) = 1.0
  | toNum (Bool false) = 0.0
  | toNum (Object _) = nan
  | toNum (Function _) = nan
  | toNum (Reference r) = toNum (deref r)
  | toNum (Namespace _) = 0.0
  | toNum (Class _) = 0.0
  | toNum (Interface _) = 0.0
  | toNum Undef = nan
  | toNum Null = 0.0
		    
fun toBoolean (Bool b) = b
  | toBoolean (Str _) = true
  | toBoolean (Num _) = true
  | toBoolean (Object _) = true
  | toBoolean (Function _) = true
  | toBoolean (Reference r) = toBoolean (deref r)
  | toBoolean (Namespace _) = true
  | toBoolean (Class _) = true
  | toBoolean (Interface _) = true
  | toBoolean Undef = false
  | toBoolean Null = false

fun equals (Bool a) (Bool b) = (a = b)
  | equals (Str a) (Str b) = (a = b)
  | equals (Num a) (Num b) = (Real.== (a,b))
  | equals (Object (Obj a)) (Object (Obj b)) = ((#bindings a) = (#bindings b))
  | equals (Namespace a) (Namespace b) = (a = b)
  | equals (Class (Cls a)) (Class (Cls b)) = ((#definition a) = (#definition b))
  | equals (Interface a) (Interface b) = (a = b)
  | equals Undef Undef = true
  | equals Null Null = true
  | equals Undef Null = true
  | equals Null Undef = true
  | equals (Str a) (Num b) = (a = (toString (Num b)))
  | equals (Num a) (Str b) = ((toString (Num a)) = b)
  | equals (Reference a) b = equals (deref a) b
  | equals a (Reference b) = equals a (deref b)
  | equals _ _ = false

fun less (Bool true) (Bool true) = false
  | less (Bool false) (Bool true) = true
  | less (Bool false) (Bool false) = false
  | less (Bool true) (Bool false) = false
  | less (Str a) (Str b) = (a < b)
  | less (Num a) (Num b) = (a < b)
  | less (Str a) (Num b) = (a < (toString (Num b)))
  | less (Num a) (Str b) = ((toString (Num a)) < b)
  | less (Reference a) b = less (deref a) b
  | less a (Reference b) = less a (deref b)
  | less _ _ = false


fun hostPrintFunction (obj:OBJ) (vals:VAL list) = 
    let
	fun printOne v = print (toString v) 
    in
	(map printOne vals; Undef)
    end

and populateIntrinsics obj = 
    case obj of 
	Obj { bindings, ... } => 
	let 
	    fun bindFunc (n, f) = 
		let 
		    val name = { id = n, ns = Ast.Intrinsic }
		    val prop = { ty = Ast.SpecialType Ast.Any,
				 value = Function (Fun { code=f, 
							 definition=NONE, 
							 layout=emptyFuncLayout, 
							 ty=Ast.SpecialType Ast.Any} ),	   
				 attrs = { dontDelete = true,
					   dontEnum = false,
					   readOnly = true } }
		in
		    addBinding bindings name prop
		end
	in
	    map bindFunc 
	    [ ("print", hostPrintFunction) ]
	end	
end
