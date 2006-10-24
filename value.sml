structure Value = struct

type str = Ast.ustring

type T = Ast.tyExpr

datatype object = 
	 Object of { ty: T ref,
		     slots: ((T*V) NameMap.map) ref,
		     proto: (object option) ref }

     and V = Undef
           | Null
	   | Bool of bool
           | Str of str
           | Num of real
	   | Obj of object
	   | Fun of (V -> V)

fun makeObject _ = Object { ty = ref (Ast.SpecialType Ast.ANY),
			    slots = ref NameMap.empty,
			    proto = ref NONE }

fun toBoolean (Bool b) = b
  | toBoolean (Str _) = true
  | toBoolean (Num _) = true
  | toBoolean (Obj _) = true
  | toBoolean (Fun _) = true
  | toBoolean Undef = false
  | toBoolean Null = false

end
