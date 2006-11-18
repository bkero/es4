(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local type aliases *)

type TYPE = Ast.TYPE_EXPR
type STR = Ast.USTRING
type ID = Ast.IDENT
type NS = Ast.NAMESPACE

datatype SCOPE_TAG = 
	 VarGlobal       (* Variable object created before execution starts *)
       | VarClass        (* Variable object created on entry to a function  *)
       | VarInstance     (* Variable object for class objects               *)
       | VarActivation   (* Variable object for class instances             *)
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
		
     and FUN = 
	 Fun of (VAL -> VAL) 
		    
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

		
withtype NAME = { ns: NS option, 
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
		     nullable = false }

val (noOpFunction:Ast.FUNC) = 
    Ast.Func { name = "",
	       attrs = defaultAttrs,
	       formals = [],
	       ty = NONE,
	       body = Ast.Block { directives = [],
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

fun newObject (c:CLASS option) = 
    Obj { class = c,
	  bindings = newBindings (),
	  prototype = ref NONE }
		   
and toBoolean (Bool b) = b
  | toBoolean (Str _) = true
  | toBoolean (Num _) = true
  | toBoolean (Object _) = true
  | toBoolean (Function _) = true
  | toBoolean Undef = false
  | toBoolean Null = false

end
