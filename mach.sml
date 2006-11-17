(* "Virtual machine" for executing ES4 code. *)

structure Mach = struct 

(* Local type aliases *)
type STR = Ast.ustring
type TY = Ast.tyExpr
type NS = Ast.namespace

exception UnimplementedException of STR


datatype envtag = 
	 VAR_GLOBAL      (* Variable object created before execution starts *)
       | VAR_CLASS       (* Variable object created on entry to a function  *)
       | VAR_INSTANCE    (* Variable object for class objects               *)
       | VAR_ACTIVATION  (* Variable object for class instances             *)
       | WITH            (* Created by 'with' bindings                      *)
       | LET             (* Created by 'catch', 'let', etc.                 *)

datatype VAL = 
	 Undef
       | Null
       | Bool of bool
       | Str of STR
       | Num of real
       | Object of OBJ
       | Fun of (VAL -> VAL)
       | Ref of { base: OBJ,
		  name: NAME }
		
     and TRAITS = 
	 Traits of { name: STR,
		     parent: TRAITS option,
		     ty: TY,
		     isClass: bool,
		     isInterface: bool,
		     isExtensible: bool }
		   
     and ENV = 
	 Env of { tag: envtag, 
		  parent: ENV option,
		  bindings: BINDINGS }
		
     and CLASS = 
	 Class of { ty: TY,
		    env: ENV,
   		    base: CLASS option,
		    interfaces: INTERFACE list,
		    
		    definition: Ast.classDefn,
		    constructor: Ast.funcDefn,
		    call: Ast.funcDefn,
		    
		    instanceTy: TY,
		    instanceTraits: TRAITS,
		    instancePrototype: OBJ,
		    
		    initialized: bool ref }

     and INTERFACE = 
	 Interface of { definition: Ast.interfaceDefn,
			ty: TY,
			bases: INTERFACE list,
			isInitialized: bool ref }

     and MACH = 
	 Mach of { env: ENV,
		   result: VAL,
		   thisObject: OBJ,
		   numberType: Ast.numberType,
		   roundingMode: Ast.roundingMode,
		   openNamespaces: STR list }

     and OBJ = 
	 Obj of { ty: TY ref,
		  traits: TRAITS,
		  slots: BINDINGS,
		  prototype: (OBJ option) ref }
		
withtype NAME = { ns: NS option, id: STR }
		
     and PROP = 
	 { value: VAL,
	   ty: TY,
	   dontDelete: bool,
	   dontEnum: bool,
	   readOnly: bool }
	 
     and BINDINGS = ((NAME * PROP) list) ref
		    

(* Values *)

fun setProp (b:BINDINGS) (n:NAME) (p:PROP) = 
    b := ((n,p) :: (!b))

fun getProp (b:BINDINGS) (n:NAME) = 
    let 
	fun lookup ((k,v)::ps) = 
	    if k = n 
	    then v
	    else lookup ps
    in
	lookup (!b)
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
    Env { tag = VAR_GLOBAL,
	  parent = NONE,
	  bindings = ref [] }

end
