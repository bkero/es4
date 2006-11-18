structure Eval = struct 

(* Exceptions for object-language control transfer. *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL

(* 
		   
fun newName (i:Ast.IDENT) (attrs:Ast.ATTRIBUTES) =
    case attrs of 
	Ast.Attributes {ns=n, ...} => {ns=n, id=i}

                 
fun setPropertyValue (obj:Mach.OBJ) (n:Mach.NAME) (p:Mach.PROP) =
    case obj of 
	Mach.Obj ob => Mach.addBinding (#bindings ob) n p


fun extendScope (p:Mach.SCOPE) (t:Mach.SCOPE_TAG) (ob:Mach.OBJ) = 
    Mach.Scope { parent=(SOME p), tag=t, obj=ob }


fun evalExpr (scope:Mach.SCOPE) (expr:Ast.EXPR) =
    case expr of
	Ast.Identifier { id=ident, nss=openNamespaces } =>
	evalVarExpr scope {nss=(!nss), id=id}
	
      | Ast.LetExpr {defs, body} => 
	evalLetExpr scope defs body

      | _ => 
	raise (Mach.UnimplementedException "unhandled expression type")

and evalLetExpr (scope:Mach.SCOPE) (defs:Ast.VAR_DEFN list) (body:Ast.EXPR) = 
    let 
	val obj = Mach.newObject NONE
        val newScope = extendScope scope Mach.Let obj
    in
        List.app (addBinding obj scope) defs;
        evalExpr newScope body
    end

and addBinding (obj:Mach.OBJ) (scope:Mach.SCOPE) (defn:Ast.VAR_DEFN) =
    case defn of 
	(Ast.SimpleDefn { attrs, init, name, ty, ...}) =>
	let 
	    val p = name
	    val n = newName p attrs
	    val t = case ty of
			NONE => Ast.SpecialType Ast.Any
		      | SOME e => e
	    val v = case init of 
			NONE => Mach.Undef
		      | SOME e => evalExpr scope e
	    val prop = { ty = t, 
			 value = v, 
			 dontDelete = true,
			 dontEnum = true,
			 readOnly = false } 
	in
	    setPropertyValue obj n prop
	end
	
      | (Ast.DestructuringDefn { init, postInit, names, ...}) =>
	let 
            val v = case init of 
			NONE => Mach.Undef
		      | SOME e => evalExpr scope e
	in 
            destructureAndBind obj postInit names v
	end
	
	
and destructureAndBind obj postInit names v = 
    raise (Mach.UnimplementedException "destructureAndBind")

and evalVarExpr (scope:Mach.SCOPE) (mname:Mach.MULTINAME) = 
    case lookupInScope scope mname of 
	NONE => raise Mach.MultiReferenceException mname
      | SOME (t,v) => v
          
and lookupInScope (scope:Mach.SCOPE) (mname:Mach.MULTINAME) =     
    case scope of 
	Mach.Scope { parent, obj, ... } => 
	case getValueProtocol obj mname of
	    NONE => lookupInScope parent mname
	  | result => result
    
and getValueProtocol (obj:Mach.OBJ) (mname:Mach.MULTINAME) = 
    case obj of 
	Mach.Obj ob => 
	case getPropertyValue obj mname of 
	    NONE => 
	    let 
		val proto = !(#proto ob)
	    in
		case proto of 
		    NONE => NONE
                  | SOME p => getValueProtocol p mname
	    end
	  | result => result

and getPropertyValue (obj:Mach.OBJ) (mname:Mach.MULTINAME) =    
    case obj of 
	Mach.Obj ob => 
	let     
	    val bindings = !(#bindings ob)
	    val id = (#id mname)
	    fun tryName [] = NONE
	      | tryName (x::xs) = 
		let 
		    val n = {ns=x, id=id} 
		in
		    if Mach.hasBinding bindings n
		    then SOME (Mach.getBinding bindings n)
		    else tryName xs
		end
	in
	    tryName (#nss mname)
	end

and evalTailCallExpr scope e = 
    raise (TailCallException (fn _ => evalExpr scope e))

and evalNonTailCallExpr scope e = 
    evalExpr scope e
    handle TailCallException thunk => thunk ()
         | ReturnException v => v

fun labelEq stmtLabel exnLabel = 
    case (stmtLabel, exnLabel) of
	(SOME sl, SOME el) => sl = el
      | (NONE, SOME _) => false
      | (_, NONE) => true
		     
fun evalStmts scope (s::ss) = (evalStmt scope s; evalStmts scope ss)
  | evalStmts scope [] = Undef
		       
and evalStmt scope (Ast.ExprStmt e) = evalExpr scope e
  | evalStmt scope (Ast.IfStmt i) = evalIfStmt scope i
  | evalStmt scope (Ast.WhileStmt w) = evalWhileStmt scope w
  | evalStmt scope (Ast.ReturnStmt r) = evalReturnStmt scope r
  | evalStmt scope (Ast.BreakStmt lbl) = evalBreakStmt scope lbl
  | evalStmt scope (Ast.ContinueStmt lbl) = evalContinueStmt scope lbl
  | evalStmt scope (Ast.ThrowStmt t) = evalThrowStmt scope t
  | evalStmt scope (Ast.LabeledStmt (lab, s)) = evalLabelStmt scope lab s
  | evalStmt scope (Ast.BlockStmt b) = evalBlock scope b
  | evalStmt _ _ = raise Mach.UnimplementedException "Unimplemented statement type"
			 
and evalBlock scope (Ast.Block{stmts=s,... }) = 
    evalStmts scope s

and evalIfStmt scope { cond, consequent, alternative } = 
    let 
        val v = evalExpr scope cond 
        val b = Mach.toBoolean v
    in
        if b 
        then evalStmt scope consequent
        else evalStmt scope alternative
    end
    
and evalLabelStmt scope lab s = 
    evalStmt scope s
    handle BreakException exnLabel 
	   => if labelEq (SOME lab) exnLabel
              then Undef 
              else raise BreakException exnLabel
			 
and evalWhileStmt scope { cond, body, contLabel } = 
    let
        fun loop (accum:VAL option)
          = let 
              val v = evalExpr scope cond
              val b = Mach.toBoolean v
          in
              if not b 
              then 
                  let
                      val curr = (SOME (evalStmt scope body)
                                  handle ContinueException exnLabel => 
					 if labelEq contLabel exnLabel
					 then NONE
					 else raise ContinueException exnLabel)
                      val next = (case curr 
                                   of NONE => accum
                                    | x => x)
                  in
                      loop next
                  end
              else
                  accum
          end
    in
        case loop NONE of
            NONE => Undef
	  | SOME v => v
    end
    
and evalReturnStmt scope e
  = raise (ReturnException (evalExpr scope e))
	  
and evalThrowStmt scope e
  = raise (ThrowException (evalExpr scope e))
	  
and evalBreakStmt scope lbl
  = raise (BreakException lbl)
	  
and evalContinueStmt scope lbl
  = raise (ContinueException lbl)
*)
	  
end
