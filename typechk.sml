structure TypeChk = struct

exception IllTypedException of (Ast.ident option * string)

val boolType = Ast.PrimaryType { name="boolean",  annotation=Ast.NAMED }

fun tcProgram { packages, body } = ()

fun tcStmts env (s::ss) = (tcStmt env s; tcStmts env ss)
  | tcStmts env [] = ()

and tcStmt env (Ast.ExprStmt e) = tcExpr env e
  | tcStmt env (Ast.IfStmt {cond,consequent,alternative}    ) = 
    (
    checkConvertible (tcExpr env cond) boolType;
    tcStmt env consequent;
    tcStmt env alternative
    )
(*
  | tcStmt env (Ast.WhileStmt w) = tcWhileStmt env w
  | tcStmt env (Ast.ReturnStmt r) = tcReturnStmt env r
  | tcStmt env (Ast.BreakStmt lbl) = tcBreakStmt env lbl
  | tcStmt env (Ast.ContinueStmt lbl) = tcContinueStmt env lbl
  | tcStmt env (Ast.ThrowStmt t) = tcThrowStmt env t
  | tcStmt env (Ast.LabeledStmt (lab, s)) = tcLabelStmt env lab s
  | tcStmt env (Ast.BlockStmt b) = tcBlock env b
*)
  | tcStmt _ _ = raise Expr.UnimplementedException "Unimplemented statement type"

and tcExpr env e = 
    boolType

and checkConvertible t1 t2 =
    true



end
