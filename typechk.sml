structure TypeChk = struct

exception IllTypedException of string

open Ast

val boolType = PrimaryType { ident=Ast.Identifier "boolean",  annotation=Named }
val exceptionType = PrimaryType { ident=Ast.Identifier "exception",  annotation=Named }

fun assert b s = if b then () else (raise Fail s)

type TYPE_ENV = (IDENT * TYPE_EXPR) list

fun extendEnv ((name, ty), env) = (name, ty)::env

type CONTEXT = {env: TYPE_ENV, lbls: IDENT option list, retTy: TYPE_EXPR}

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

fun tcStmts ctxt ss = List.app (fn s => tcStmt ctxt s) ss

and tcStmt ((ctxt as {env,lbls,retTy}):CONTEXT) stmt =
  (case stmt of
    EmptyStmt => ()
  | ExprStmt e => (tcExpr ctxt e; ())
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
  | DefineStmt _ =>
        raise Fail "should have been hoisted"
(*
       | ForEachStmt of FOR_ENUM_STMT
       | ForInStmt of FOR_ENUM_STMT
       | SuperStmt of EXPR list

       | ForStmt of { isVar: bool,
                      defns: VAR_DEFN list,
                      init: EXPR,
                      cond: EXPR,
                      update: EXPR,
                      contLabel: IDENT option,
                      body: STMT }


       | WithStmt of { obj: EXPR,
                       body: STMT }

       | TryStmt of { body: BLOCK,
                      catches: (FORMAL * BLOCK) list,
                      finally: BLOCK }

       | SwitchStmt of { cond: EXPR,
                         cases: (EXPR * (STMT list)) list,
                         default: STMT list }
*)
(*  | tcStmt _ _ _ _ => raise Expr.UnimplementedException "Unimplemented statement type" *)

)
and tcDefn ctxt d =
    (case d of
        VariableDefn vd => (tcVarDefn ctxt vd, [])
    )

and tcDefns ctxt [] = ([], [])
  | tcDefns ctxt (d::ds) =
        let val (extensions1, classes1) = tcDefn ctxt d
            val (extensions2, classes2) = tcDefns ctxt ds
        in
            (extensions1 @ extensions2, classes1 @ classes2)
        end

and tcBlock (ctxt as {env,...}) (Block {pragmas=pragmas,defns=defns,stmts=stmts}) =
    let val (extensions, classes) = tcDefns ctxt defns
        val ctxt' = withEnv (ctxt, foldl extendEnv env extensions)
    in
        assert (classes = []) "class definition inside block";
        tcStmts ctxt stmts
    end

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

and tcVarDefn ctxt (VariableDefinition {tag,init,attrs,pattern=Ast.SimplePattern(name),ty}) = []


end
