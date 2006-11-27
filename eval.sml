structure Eval = struct 

(* Exceptions for object-language control transfer. *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL
exception SemanticException of Ast.USTRING
		   
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
	Ast.Ref { base, ident } => 
	(case base of 
	     SOME bexpr => evalRefExpr scope (SOME (evalExpr scope bexpr)) ident
	   | NONE => evalRefExpr scope NONE ident)
	
      | Ast.LetExpr {defs, body} => 
	evalLetExpr scope defs body

      | _ => 
	raise (Mach.UnimplementedException "unhandled expression type")

and evalRefExpr (scope:Mach.SCOPE) (b:Mach.VAL option) (r:Ast.IDENT_EXPR) =
    case r of 
	Ast.Identifier { ident, openNamespaces } =>
	let 	    
	    val multiname : Mach.MULTINAME = { nss=(!openNamespaces), id=ident }
	    val refOpt = (case b of 
			      SOME (Mach.Object ob) => resolveOnObjAndPrototypes ob multiname
			    | NONE => resolveOnScopeChain scope multiname
			    | _ => raise (SemanticException "ref expression on non-object value"))
	in
	    case refOpt of 
		NONE => raise Mach.MultiReferenceException multiname
	      | SOME r => r
	end
      | _ => raise (Mach.UnimplementedException "unimplemented identifier expression form")
		   
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
	(Ast.VariableDefinition { tag, init, attrs, pattern, ty }) =>
	case pattern of 
	    Ast.IdentifierPattern name => 
	    let 
		val n = newName name attrs
		val ro = case tag of 
			     Ast.Const => true
			   | Ast.LetConst => true
			   | _ => false
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
			     readOnly = ro } 
	    in
		setPropertyValue obj n prop
	    end
	  | _ => raise (Mach.UnimplementedException "unhandled binding form")
          
and resolveOnScopeChain (scope:Mach.SCOPE) (mname:Mach.MULTINAME) =     
    case scope of 
	Mach.Scope { parent, obj, ... } => 
	case resolveOnObjAndPrototypes obj mname of
	    NONE => (case parent of 
			 SOME p => resolveOnScopeChain p mname
		       | NONE => NONE)
	  | result => result
    
and resolveOnObjAndPrototypes (obj:Mach.OBJ) (mname:Mach.MULTINAME) = 
    case obj of 
	Mach.Obj ob => 
	case resolveOnObj obj mname of 
	    NONE => 
	    let 
		val proto = !(#prototype ob)
	    in
		case proto of 
		    NONE => NONE
                  | SOME p => resolveOnObjAndPrototypes p mname
	    end
	  | result => result

and resolveOnObj (obj:Mach.OBJ) (mname:Mach.MULTINAME) =    
    case obj of 
	Mach.Obj ob => 
	let     
	    val id = (#id mname)
	    fun tryName [] = NONE
	      | tryName (x::xs) = 
		let 
		    val n = {ns=x, id=id} 
		in
		    if Mach.hasBinding (#bindings ob) n
		    then SOME (Mach.Reference (Mach.Ref { base = obj, name = n }))
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
  | evalStmts scope [] = Mach.Undef
		       
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
              then Mach.Undef 
              else raise BreakException exnLabel
			 
and evalWhileStmt scope { cond, body, contLabel } = 
    let
        fun loop (accum:Mach.VAL option)
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
            NONE => Mach.Undef
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
	  
end
