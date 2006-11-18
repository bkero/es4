structure TypeChk = struct

exception IllTypedException of string

open Ast

val boolType = PrimaryType { name="boolean",  annotation=Named }
val exceptionType = PrimaryType { name="exception",  annotation=Named }

type TYPE_ENV = (IDENT * TY_EXPR) list

fun extendEnv ((name, ty), env) = (name, ty)::env

type CONTEXT = {env: TYPE_ENV, lbls: IDENT option list, retTy: TY_EXPR}

fun withEnv ({env=_, lbls=lbls, retTy=retTy}, env) = {env=env, lbls=lbls, retTy=retTy}

fun withLbls ({env=env, lbls=_, retTy=retTy}, lbls) = {env=env, lbls=lbls, retTy=retTy}

fun withRetTy ({env=env, lbls=lbls, retTy=_}, retTy) = {env=env, lbls=lbls, retTy=retTy}

fun checkConvertible t1 t2 = ()

fun checkForDuplicates' [] = ()
  | checkForDuplicates' (x::xs) =
        if List.exists (fn y => x = y) xs
	then raise IllTypedException "concurrent definition"
        else checkForDuplicates' xs

fun checkForDuplicates extensions =
    let val (names, _) = ListPair.unzip extensions
    in
        checkForDuplicates' names
    end

fun mergeTypes t1 t2 =
	t1

fun tcProgram { packages, body } = ()

fun tcStmts ctxt (s::ss) = (
	tcStmt ctxt s; 
	tcStmts ctxt ss
    )
  | tcStmts _ [] = ()

and tcStmt ((ctxt as {env,lbls,retTy}):CONTEXT) stmt =
  (case stmt of
    EmptyStmt => ()
  | ExprStmt e => (
        tcExpr ctxt e; ()
    )

  | IfStmt {cond,consequent,alternative} => (
	checkConvertible (tcExpr ctxt cond) boolType;
	tcStmt ctxt consequent;
	tcStmt ctxt alternative
    )

  | (DoWhileStmt {cond,body,contLabel} | WhileStmt {cond,body,contLabel}) => (
	checkConvertible (tcExpr ctxt cond) boolType;
	tcStmt (withLbls (ctxt, contLabel::lbls)) body
    )

  | ReturnStmt e => 
	checkConvertible (tcExpr ctxt e) retTy

  | (BreakStmt NONE | ContinueStmt NONE) =>  
    (
	case lbls of
	  [] => raise IllTypedException "Not in a loop"
	| _ => ()
    )

  | (BreakStmt (SOME lbl) | ContinueStmt (SOME lbl)) => 
    (
	if List.exists (fn x => x=(SOME lbl)) lbls	
	then ()
	else raise IllTypedException "No such label"
    )

  | BlockStmt b => tcBlock ctxt b

  | LabeledStmt (lab, s) => 
	tcStmt (withLbls (ctxt, ((SOME lab)::lbls))) s
 
  | ThrowStmt t => 
	checkConvertible (tcExpr ctxt t) exceptionType

  | LetStmt (defns, body) =>
    (
        let val extensions = List.concat (List.map (fn d => tcVarDefn ctxt d) defns)
        in
	    checkForDuplicates extensions;
	    tcBlock (withEnv (ctxt, foldl extendEnv env extensions)) body
	end
    )
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
and tcBlock ctxt (Block {directives=directives,defns=defns,stmts=stmts}) = ()

and tcExpr ctxt e = 
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

and tcVarDefn ctxt (SimpleDefn {tag,init,attrs,name,ty}) = []


end
