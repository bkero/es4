(* A sketch of the ES4 expression evaluator in SML *)

structure Expr = struct

type value = Value.V

type multiname = { namespaces: (Ast.ustring list), prop: Ast.ustring }

exception ReferenceException of multiname

exception UnimplementedException of string

exception TailCallException of (unit -> value)
			       
exception ReturnException of value

fun makeName p (Ast.Attributes {vis=(Ast.BuiltinNamespace n), ...}) =
    {ns=n, prop=p}

  | makeName p (Ast.Attributes {vis=(Ast.UserNamespace n), ...}) =
    {ns=n, prop=p}
			     
fun setPropertyValue (Value.Object obj) name v =
    let
	val slots = #slots obj
    in
	slots := NameMap.insert ((!slots), name, v)
    end

fun extendEnvironment env bindingobj = 
    bindingobj :: env


fun evalExpr env (Ast.QualIdent { lhs=NONE, rhs=(Ast.Ident id), openNamespaces=nss }) = 
    evalVarExpr env {prop=id, namespaces=nss}

  | evalExpr env (Ast.LetExpr {defs, body}) = 
    evalLetExpr env defs body

  | evalExpr env _ = 
    raise (UnimplementedException "unhandled expression type")

and evalLetExpr env ds body = 
    let 
        val bindings = Value.makeObject ()
        val newEnv = extendEnvironment env bindings 
    in
        List.app (createBinding bindings env) ds;
        evalExpr newEnv body
    end
    
and createBinding bindings env 
		  (Ast.SimpleDefn { attrs, init, name, ty, ...}) = 
    let 
	val n = makeName name attrs
	val t = case ty of
		    NONE => Ast.SpecialType Ast.ANY
		  | SOME e => e
	val v = case init of 
		    NONE => Value.Undef
		  | SOME e => evalExpr env e
    in
	setPropertyValue bindings n (t, v)
    end

  | createBinding bindings env 
		  (Ast.DestructuringDefn { init, postInit, names, ...}) = 
    let 
        val v = case init of 
		    NONE => Value.Undef
		  | SOME e => evalExpr env e
    in 
        destructureAndBind bindings postInit names v
    end

    
and destructureAndBind bindings postInit names v = 
    raise (UnimplementedException "destructureAndBind")

and evalVarExpr env mname = 
    case lookupInEnv env mname of 
	NONE => raise ReferenceException mname
      | SOME (t,v) => v
		  
and lookupInEnv [] (mname:multiname) = 
    NONE

  | lookupInEnv (b::bs) (mname:multiname) = 
    (case getValueProtocol b mname of 
	 NONE => lookupInEnv bs mname
       | result => result)
    
and getValueProtocol (Value.Object obj) (mname:multiname) = 
    case getPropertyValue obj mname of 
	NONE => (case !(#proto obj) of 
		     NONE => NONE
                   | SOME proto => getValueProtocol proto mname)
      | result => result

and getPropertyValue obj (mname:multiname) = 
    let 	
	val slots = !(#slots obj)
	val p = #prop mname
	fun search [] = NONE
	  | search (x::xs) = case NameMap.find (slots, {ns=x, prop=p}) of
				 NONE => search xs
			       | res => res
    in
	search (#namespaces mname)
    end

and evalTailCallExpr env e = 
    raise (TailCallException (fn _ => evalExpr env e))

and evalNonTailCallExpr env e = 
    evalExpr env e
    handle TailCallException thunk => thunk ()
         | ReturnException v => v

end
