structure TypeChk = struct

exception IllTypedException of string

open Ast

val boolType = PrimaryType { name="boolean",  annotation=Ast.NAMED }
val exceptionType = Ast.PrimaryType { name="exception",  annotation=Ast.NAMED }

fun tcProgram { packages, body } = ()

fun tcStmts env lbls retTy (s::ss) = (
	tcStmt env lbls retTy s; 
	tcStmts env lbls retTy ss
    )
  | tcStmts env _ _ [] = ()

and tcStmt env lbls retTy stmt =
  (case stmt of
    Ast.EmptyStmt => ()
  | Ast.ExprStmt e => (
        tcExpr env e; ()
    )

  | Ast.IfStmt {cond,consequent,alternative} => (
	checkConvertible (tcExpr env cond) boolType;
	tcStmt env lbls retTy consequent;
	tcStmt env lbls retTy alternative
    )

  | (Ast.DoWhileStmt {cond,body,contLabel} | Ast.WhileStmt {cond,body,contLabel}) => (
	checkConvertible (tcExpr env cond) boolType;
	tcStmt env (contLabel::lbls) retTy body
    )

  | Ast.ReturnStmt e => 
	checkConvertible (tcExpr env e) retTy

  | (Ast.BreakStmt NONE | Ast.ContinueStmt NONE) =>  
    (
	case lbls of
	  [] => raise IllTypedException "Not in a loop"
	| _ => ()
    )

  | (Ast.BreakStmt (SOME lbl) | Ast.ContinueStmt (SOME lbl)) => 
    (
	if List.exists (fn x => x=(SOME lbl)) lbls	
	then ()
	else raise IllTypedException "No such label"
    )

  | Ast.BlockStmt b => tcBlock env lbls retTy b

  | Ast.LabeledStmt (lab, s) => 
	tcStmt env ((SOME lab)::lbls) retTy s
	(*TODO - check on this *)
 
  | Ast.ThrowStmt t => 
	checkConvertible (tcExpr env t) exceptionType

(*
       | DefineStmt of varDefn
       | ForEachStmt of forEnumStmt
       | ForInStmt of forEnumStmt
       | LetStmt of ((varDefn list) * block)
       | SuperStmt of expr list

       | ForStmt of { isVar: bool,
                      defns: varDefn list,
                      init: expr,
                      cond: expr,
                      update: expr,
                      contLabel: ident option,
                      body: stmt }

       | WithStmt of { obj: expr,
                       body: stmt }

       | TryStmt of { body: block,
                      catches: (formal * block) list,
                      finally: block }

       | SwitchStmt of { cond: expr,
                         cases: (expr * (stmt list)) list,
                         default: stmt list }
*)
(*  | tcStmt _ _ _ _ => raise Expr.UnimplementedException "Unimplemented statement type" *)

)
and tcBlock env lbls retTy (Ast.Block {directives=directives,defns=defns,stmts=stmts}) = ()

and tcExpr env e = 
	boolType

(*
     and expr =
         TrinaryExpr of (triOp * expr * expr * expr)
       | BinaryExpr of (binOp * expr * expr)
       | UnaryExpr of (unOp * expr)
       | NullaryExpr of nulOp
       | YieldExpr of expr
       | SuperExpr of expr list
       | LiteralExpr of literal

       | CallExpr of {func: expr,
                      actuals: expr list}

       | Property of { indirect: bool,
                       obj: expr,
                       field: expr }

       | QualIdent of { lhs: identOrExpr option,
                        rhs: identOrExpr,
                        openNamespaces: ident list }

       | AttrQualIdent of { indirect: bool,
                            operand: identOrExpr }

       | LetExpr of { defs: varDefn list,
                      body: expr }

       | NewExpr of { obj: expr,
                      actuals: expr list }
*)
and checkConvertible t1 t2 = ()

and mergeTypes t1 t2 =
	t1



end
