(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local type aliases *)

type TYPE = Ast.TYPE_EXPR
type STR = Ast.USTRING
type ID = Ast.IDENT
type NS = Ast.NAMESPACE

datatype SCOPE_TAG = 
	 VarGlobal       (* Variable object created before execution starts *)
       | VarClass        (* Variable object for class instances             *)
       | VarInstance     (* Variable object for class objects               *)
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
       | Reference of REF
       | Namespace of NS
		
     and FUN = 
	 Fun of (OBJ -> (VAL list) -> VAL) 
		    
     and REF = 
	 Ref of { base: OBJ,
		  name: NAME }      
		   
     and SCOPE = 
	 Scope of { tag: SCOPE_TAG, 
		    obj: OBJ,
		    parent: SCOPE option }
		  
     and OBJ = 
	 Obj of { class: CLASS option,
		  bindings: BINDINGS,
		  prototype: (OBJ option) ref }
		
     and CLASS = 
	 Class of { ty: TYPE,
		    scope: SCOPE,
   		    base: CLASS option,
		    interfaces: INTERFACE list,
		    
		    call: Ast.FUNC,
		    definition: Ast.CLASS_DEFN,
		    constructor: Ast.FUNC,
		    
		    instanceTy: TYPE,
		    instanceLayout: LAYOUT,
		    instancePrototype: OBJ,
		    
		    initialized: bool ref }

     and LAYOUT_TAG = 
	 LayoutClass  
       | LayoutInterface

     and LAYOUT = 
	 Layout of { ty: TYPE,
		     name: STR,
		     parent: LAYOUT option,		     
		     tag: LAYOUT_TAG,
		     isExtensible: bool }

     and ROW = 
	 Row of { ty: TYPE,
		  dontDelete: bool,
		  dontEnum: bool,
		  readOnly: bool }
		
     and INTERFACE = 
	 Interface of { ty: TYPE,
			bases: INTERFACE list,
			definition: Ast.INTERFACE_DEFN,			
			isInitialized: bool ref }


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
     and PROP = 
	 { ty: TYPE,
	   value: VAL,	   
	   dontDelete: bool,
	   dontEnum: bool,
	   readOnly: bool }
	 
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
 * SCOPE -> OBJ -> CLASS -> SCOPE.
 * 
 * We need to break this so that it's possible to instantiate
 * the roots of the builtin objects, so we make OBJ have only a CLASS
 * option, not a mandatory CLASS. When you access the CLASS of an OBJ
 * you should use an accessor function that "closes the loop" and
 * returns the global "object" CLASS when you fetch the CLASS of an
 * OBJ like "Obj {class = NONE, ...}".
 *)			  

val (objectType:TYPE) = Ast.RecordType []

val (defaultAttrs:Ast.ATTRIBUTES) = 
    Ast.Attributes { ns = Ast.Public "",
		     override = false,
		     static = false,
		     final = false,
		     dynamic = true,
		     prototype = false,
			 rest = false,
		     nullable = false }

val (noOpFunction:Ast.FUNC) = 
    Ast.Func { name = "",
	       attrs = defaultAttrs,
	       formals = [],
	       ty = NONE,
	       body = Ast.Block { pragmas = [],
				  defns = [],
				  stmts = [] } }

val (emptyClass:Ast.CLASS_DEFN) = 
    { name = "",
      attrs = defaultAttrs,
      params = [],
      extends = [],
      implements = [],
      instanceVars = [],
      vars = [],
      constructor = noOpFunction,
      methods = [],
      initializer = [] }
    
val (emptyLayout:LAYOUT) = 
    Layout { ty = objectType,
	     name = "",
	     parent = NONE,
	     tag = LayoutClass,
	     isExtensible = true }
    
val (globalObject:OBJ) = 
    Obj { class = NONE, 
	  bindings = newBindings (), 
	  prototype = ref NONE } 
    
val (globalScope:SCOPE) = 
    Scope { tag = VarGlobal,
	    obj = globalObject,
	    parent = NONE }
    
val (globalClass:CLASS) = 
    Class { ty = objectType,
	    scope = globalScope,
	    base = NONE,
	    interfaces = [],
	    call = noOpFunction,
	    definition = emptyClass,
	    constructor = noOpFunction,
	    instanceTy = objectType,
	    instanceLayout = emptyLayout,
	    instancePrototype = globalObject,
	    initialized = ref true }

val nan = Real.posInf / Real.posInf

fun newObject (c:CLASS option) = 
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
  | toString (Namespace n) = "[namespace]"
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
  | toNum (Namespace n) = 0.0
  | toNum Undef = nan
  | toNum Null = 0.0
		    
fun toBoolean (Bool b) = b
  | toBoolean (Str _) = true
  | toBoolean (Num _) = true
  | toBoolean (Object _) = true
  | toBoolean (Function _) = true
  | toBoolean (Reference r) = toBoolean (deref r)
  | toBoolean (Namespace n) = true
  | toBoolean Undef = false
  | toBoolean Null = false

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
		    val name = {id = n, ns = Ast.Intrinsic }
		    val prop = { ty = Ast.SpecialType Ast.Any,
				 value = Function (Fun f),	   
				 dontDelete = true,
				 dontEnum = false,
				 readOnly = true }
		in
		    addBinding bindings name prop
		end
	in
	    map bindFunc 
	    [ ("print", hostPrintFunction) ]
	end	
end
