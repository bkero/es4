structure Stmt = struct

exception ContinueException of (Ast.ident option)

exception BreakException of (Ast.ident option)

exception ThrowException of Value.V

exception ReturnException of Value.V

fun labelEq (SOME (stmtLabel:Ast.ident)) 
        (SOME (exnLabel:Ast.ident)) = (stmtLabel = exnLabel)
  | labelEq NONE (SOME exnLabel) = false
  | labelEq _ NONE = true

fun evalStmts env (s::ss) = (evalStmt env s; evalStmts env ss)
  | evalStmts env [] = Value.Undef

and evalStmt env (Ast.ExprStmt e) = Expr.evalExpr env e
  | evalStmt env (Ast.IfStmt i) = evalIfStmt env i
  | evalStmt env (Ast.WhileStmt w) = evalWhileStmt env w
  | evalStmt env (Ast.ReturnStmt r) = evalReturnStmt env r
  | evalStmt env (Ast.BreakStmt lbl) = evalBreakStmt env lbl
  | evalStmt env (Ast.ContinueStmt lbl) = evalContinueStmt env lbl
  | evalStmt env (Ast.ThrowStmt t) = evalThrowStmt env t
  | evalStmt env (Ast.LabeledStmt (lab, s)) = evalLabelStmt env lab s
  | evalStmt env (Ast.BlockStmt b) = evalBlock env b
  | evalStmt _ _ = raise Expr.UnimplementedException "Unimplemented statement type"

and evalBlock env (Ast.Block{stmts=s,... }) = 
    evalStmts env s

and evalIfStmt env { cond, consequent, alternative } = 
    let 
        val v = Expr.evalExpr env cond 
        val b = Value.toBoolean v
    in
        if b 
        then evalStmt env consequent
        else evalStmt env alternative
    end
    
and evalLabelStmt env lab s = 
    evalStmt env s
    handle BreakException exnLabel 
       => if labelEq (SOME lab) exnLabel
          then Value.Undef 
          else raise BreakException exnLabel
             
and evalWhileStmt env { cond, body, contLabel } = 
    let
        fun loop (accum:Value.V option)
          = let 
                val v = Expr.evalExpr env cond
                val b = Value.toBoolean v
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
        NONE => Value.Undef
      | SOME v => v
    end

and evalReturnStmt env e
  = raise (ReturnException (Expr.evalExpr env e))

and evalThrowStmt env e
  = raise (ThrowException (Expr.evalExpr env e))

and evalBreakStmt env lbl
  = raise (BreakException lbl)

and evalContinueStmt env lbl
  = raise (ContinueException lbl)

end
