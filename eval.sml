structure Eval = struct 

(* Exceptions for object-language control transfer  *)
exception ContinueException of (Ast.IDENT option)
exception BreakException of (Ast.IDENT option)
exception TailCallException of (unit -> Mach.VAL)
exception ThrowException of Mach.VAL
exception ReturnException of Mach.VAL

		   
fun makeName prop:IDENT (Ast.Attributes {ns=n,...}) =
    Name{ns=n, id=p}

(*  | makeName p (Ast.Attributes {vis=(Ast.UserNamespace n), ...}) =
    Name{ns=Ast.UserNamespace(n), id=p}
*)
                 
fun setPropertyValue (obj:Mach.OBJ) (name:Mach.NAME) v =
    Mach.setProp (#slots obj) name

fun extendEnvironment env bindingobj = 
    bindingobj :: env


fun evalExpr env (Ast.QualIdent { qual=NONE, ident=id, opennss=nss }) = 
    evalVarExpr env (Multiname {id=id, namespaces=nss})

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
    val p = name
    val n = makeName p attrs
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
    
and getValueProtocol (Mach.Object obj) (mname:multiname) = 
    case getPropertyValue obj mname of 
    NONE => (case !(#proto obj) of 
             NONE => NONE
                   | SOME proto => getValueProtocol proto mname)
      | result => result

and getPropertyValue obj (mname:multiname) =
    
    let     
    val slots = !(#slots obj)
    val Multiname {namespaces=nss,id=p} = mname
    fun search [] = NONE
      | search (x::xs) = case Mach.getProp (slots,{ns="", prop=p}) of
                 NONE => search xs
               | res => res
    in
    search(nss)
    end

and evalTailCallExpr env e = 
    raise (TailCallException (fn _ => evalExpr env e))

and evalNonTailCallExpr env e = 
    evalExpr env e
    handle TailCallException thunk => thunk ()
         | ReturnException v => v

fun labelEq (SOME (stmtLabel:Ast.ident)) 
            (SOME (exnLabel:Ast.ident)) = (stmtLabel = exnLabel)
  | labelEq NONE (SOME exnLabel) = false
  | labelEq _ NONE = true
		     
fun evalStmts env (s::ss) = (evalStmt env s; evalStmts env ss)
  | evalStmts env [] = Undef
		       
and evalStmt env (Ast.ExprStmt e) = evalExpr env e
  | evalStmt env (Ast.IfStmt i) = evalIfStmt env i
  | evalStmt env (Ast.WhileStmt w) = evalWhileStmt env w
  | evalStmt env (Ast.ReturnStmt r) = evalReturnStmt env r
  | evalStmt env (Ast.BreakStmt lbl) = evalBreakStmt env lbl
  | evalStmt env (Ast.ContinueStmt lbl) = evalContinueStmt env lbl
  | evalStmt env (Ast.ThrowStmt t) = evalThrowStmt env t
  | evalStmt env (Ast.LabeledStmt (lab, s)) = evalLabelStmt env lab s
  | evalStmt env (Ast.BlockStmt b) = evalBlock env b
  | evalStmt _ _ = raise UnimplementedException "Unimplemented statement type"
			 
and evalBlock env (Ast.Block{stmts=s,... }) = 
    evalStmts env s

and evalIfStmt env { cond, consequent, alternative } = 
    let 
        val v = evalExpr env cond 
        val b = toBoolean v
    in
        if b 
        then evalStmt env consequent
        else evalStmt env alternative
    end
    
and evalLabelStmt env lab s = 
    evalStmt env s
    handle BreakException exnLabel 
	   => if labelEq (SOME lab) exnLabel
              then Undef 
              else raise BreakException exnLabel
			 
and evalWhileStmt env { cond, body, contLabel } = 
    let
        fun loop (accum:VAL option)
          = let 
              val v = evalExpr env cond
              val b = toBoolean v
          in
              if not b 
              then 
                  let
                      val curr = (SOME (evalStmt env body)
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
    
and evalReturnStmt env e
  = raise (ReturnException (evalExpr env e))
	  
and evalThrowStmt env e
  = raise (ThrowException (evalExpr env e))
	  
and evalBreakStmt env lbl
  = raise (BreakException lbl)
	  
and evalContinueStmt env lbl
  = raise (ContinueException lbl)
	  
end
