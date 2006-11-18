(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local type aliases *)

type TYPE = Ast.TY_EXPR
type STR = Ast.USTRING
type ID = Ast.IDENT
type NS = Ast.NAMESPACE

datatype ENV_TAG = 
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
		   
     and ENV = 
	 Env of { tag: envtag, 
		  parent: ENV option,
		  bindings: BINDINGS }

     and OBJ = 
	 Obj of { class: CLASS,
		  slots: BINDINGS,
		  prototype: (OBJ option) ref }
		
     and CLASS = 
	 Class of { ty: TYPE,
		    env: ENV,
   		    base: CLASS option,
		    interfaces: INTERFACE list,
		    
		    call: Ast.funcDefn,
		    definition: Ast.classDefn,
		    constructor: Ast.funcDefn,
		    
		    instanceTy: TYPE,
		    instanceLayout: LAYOUT,
		    instancePrototype: OBJ,
		    
		    initialized: bool ref }

     and LAYOUT = 
	 Layout of { ty: TYPE,
		     name: STR,
		     parent: LAYOUT option,		     
		     isClass: bool,
		     isInterface: bool,
		     isExtensible: bool }

     and ROW = 
	 Row of { ty: TY,
		  dontDelete: bool,
		  dontEnum: bool,
		  readOnly: bool }
		
     and INTERFACE = 
	 Interface of { ty: TYPE,
			bases: INTERFACE list,
			definition: Ast.interfaceDefn,			
			isInitialized: bool ref }


     and MACH = 
	 Mach of { env: ENV,
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
	 { ty: TY,
	   value: VAL,	   
	   dontDelete: bool,
	   dontEnum: bool,
	   readOnly: bool }
	 
     and BINDINGS = ((NAME * PROP) list) ref

(* Exceptions for "abstract machine failures". *)

exception ReferenceException of NAME
exception UnimplementedException of STR

(* Values *)

fun addProp (b:BINDINGS) (n:NAME) (p:PROP) = 
    b := ((n,p) :: (!b))

fun delProp (b:BINDINGS) (n:NAME) = 
    let 
	fun strip [] = raise ReferenceException n
	  | strip ((k,v)::bs) = 
	    if k = n 
	    then bs
	    else (k,v)::(strip bs)
    in
	b := strip (!b)
    end

fun getProp (b:BINDINGS) (n:NAME) = 
    let 
	fun search [] = raise ReferenceException n			      
	  | search ((k,v)::bs) = 
	    if k = n 
	    then v
	    else search bs
    in
	search (!b)
    end
    

fun makeObject _ = Obj { ty = ref (Ast.SpecialType Ast.ANY),
			 slots = ref [],
			 prototype = ref NONE }
		   
fun toBoolean (Bool b) = b
  | toBoolean (Str _) = true
  | toBoolean (Num _) = true
  | toBoolean (Object _) = true
  | toBoolean (Fun _) = true
  | toBoolean Undef = false
  | toBoolean Null = false

val globalEnv : ENV = 
    Env { tag = VarGlobal,
	  parent = NONE,
	  bindings = ref [] }

end
