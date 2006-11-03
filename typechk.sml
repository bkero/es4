structure TypeChk = struct

exception IllTypedException of string

val boolType = Ast.PrimaryType { name="boolean",  annotation=Ast.NAMED }
val exceptionType = Ast.PrimaryType { name="exception",  annotation=Ast.NAMED }

fun tcProgram { packages, body } = ()

fun tcStmts env lbls retTy (s::ss) = (
	tcStmt env lbls retTy s; 
	tcStmts env lbls retTy ss
    )
  | tcStmts env _ _ [] = ()

and tcStmt env lbls retTy (Ast.ExprStmt e) = 
    (
	tcExpr env e;
	()
    )

  | tcStmt env lbls retTy (Ast.IfStmt {cond,consequent,alternative}) = 
    (
	checkConvertible (tcExpr env cond) boolType;
	tcStmt env lbls retTy consequent;
	tcStmt env lbls retTy alternative
    )

  | tcStmt env lbls retTy (Ast.WhileStmt {cond,body,contLabel}) = 
    (
	checkConvertible (tcExpr env cond) boolType;
	tcStmt env (contLabel::lbls) retTy body
    )

  | tcStmt env lbls retTy (Ast.ReturnStmt e) = 
	checkConvertible (tcExpr env e) retTy

  | tcStmt env lbls retTy (Ast.BreakStmt NONE) =
 (* | tcStmt env lbls retTy (Ast.ContinueStmt NONE) =  *)
    (
	case lbls of
	  [] => raise IllTypedException "Not in a loop"
	| _ => ()
    )

  | tcStmt env lbls retTy (Ast.BreakStmt (SOME lbl)) = 
(*  | tcStmt env lbls retTy (Ast.ContinueStmt (SOME lbl)) =  *)
    (
	if List.exists (fn x => x=(SOME lbl)) lbls	
	then ()
	else raise IllTypedException "No such label"
    )

  | tcStmt env lbls retTy (Ast.BlockStmt b) = tcBlock env lbls retTy b

  | tcStmt env lbls retTy (Ast.LabeledStmt (lab, s)) = 
	tcStmt env ((SOME lab)::lbls) retTy s
	(*TODO - check on this *)

  | tcStmt env lbls retTy (Ast.ThrowStmt t) = 
	checkConvertible (tcExpr env t) exceptionType

  | tcStmt _ _ _ _ = raise Expr.UnimplementedException "Unimplemented statement type"

and tcBlock env lbls retTy {directives,defns,stmts} = ()

and tcExpr env e = 
	boolType

and checkConvertible t1 t2 = ()

and mergeTypes t1 t2 =
	t1



end
