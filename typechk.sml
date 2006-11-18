structure TypeChk = struct

exception IllTypedException of string

open Ast

val boolType      = PrimaryType { name="boolean",   annotation=Named }
val numberType    = PrimaryType { name="number",    annotation=Named }
val decimalType   = PrimaryType { name="decimal",   annotation=Named }
val intType       = PrimaryType { name="int",       annotation=Named }
val uintType      = PrimaryType { name="uint",      annotation=Named }
val stringType    = PrimaryType { name="string",    annotation=Named }
val regexpType    = PrimaryType { name="regexp",    annotation=Named }
val exceptionType = PrimaryType { name="exception", annotation=Named }
val undefinedType = SpecialType Undefined
val nullType      = SpecialType Null

fun assert b s = if b then () else (raise Fail s)

type TYPE_ENV = (IDENT * TYPE_EXPR) list

fun extendEnv ((name, ty), env) = (name, ty)::env

type CONTEXT = {env: TYPE_ENV, lbls: IDENT option list, retTy: TYPE_EXPR option}

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

(******************** Expressions **************************************************)
	
fun tcExpr ((ctxt as {env,...}):CONTEXT) (e:EXPR) :TYPE_EXPR = 
	let
	in 
     	TextIO.print "type checking expr ... \n";
	case e of
	  LiteralExpr LiteralNull => nullType
        | LiteralExpr (LiteralNumber _) => intType
        | LiteralExpr (LiteralBoolean _) => boolType
        | LiteralExpr (LiteralString _) => stringType
        | LiteralExpr (LiteralRegExp _) => regexpType
        | LiteralExpr LiteralUndefined => undefinedType 
	| ListExpr l => List.last (List.map (tcExpr ctxt) l)
	| LetExpr {defs, body} => 
          let val extensions = List.concat (List.map (fn d => tcVarDefn ctxt d) defs)
          in
	    checkForDuplicates extensions;
	    tcExpr (withEnv (ctxt, foldl extendEnv env extensions)) body
	  end
	end


(*
     and LITERAL =
       | LiteralArray of EXPR list
       | LiteralXML of EXPR list
       | LiteralNamespace of NAMESPACE

       | LiteralObject of
         { name: EXPR,
           init: EXPR } list

     and EXPR =
         TrinaryExpr of (TRIOP * EXPR * EXPR * EXPR)
       | BinaryExpr of (BINOP * EXPR * EXPR)
       | BinaryTypeExpr of (BINOP * EXPR * TYPE_EXPR)
       | UnaryExpr of (UNOP * EXPR)
       | TypeExpr of TYPE_EXPR
       | NullaryExpr of NULOP
       | YieldExpr of EXPR option
       | SuperExpr of EXPR option

       | CallExpr of {func: EXPR,
                      actuals: EXPR list}

       | Ref of { base: EXPR option,
                  ident: IDENT_EXPR }
       | NewExpr of { obj: EXPR,
                      actuals: EXPR list }

       | FunExpr of { ident: IDENT option,
                      sign: FUNC_SIGN,
                      body: BLOCK }

     and IDENT_EXPR =
         QualifiedIdentifier of { qual : EXPR,
                                  ident : USTRING }
       | QualifiedExpression of { qual : EXPR,
                                  expr : EXPR }
       | AttributeIdentifier of IDENT_EXPR
       | Identifier of IDENT
       | Expression of EXPR   (* for bracket exprs: o[x] and @[x] *)

*)

and tcVarDefn (ctxt:CONTEXT) 
     (VariableDefinition {tag,init,attrs,pattern=Ast.SimplePattern(name),ty}) = 
	[]



(**************************************************************)

fun tcStmts ctxt ss = List.app (fn s => tcStmt ctxt s) ss

and tcStmt ((ctxt as {env,lbls,retTy}):CONTEXT) stmt =
  (TextIO.print "type checking stmt ... \n";
   case stmt of
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

  | ReturnStmt e => (
	case retTy of
	  NONE => raise IllTypedException "return not allowed here"
        | SOME retTy => checkConvertible (tcExpr ctxt e) retTy
    )

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

fun tcProgram { packages, body } = 
   (tcBlock {env=[], lbls=[], retTy=NONE} body; true)
   handle IllTypedException msg => (
     TextIO.print "Ill typed exception: "; 
     TextIO.print msg; 
     TextIO.print "\n"; 
     false)

   


end
