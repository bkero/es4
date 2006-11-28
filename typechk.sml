(*
 * INVARIANTS:
 *   - all typed libraries in host environment must be DontDelete
 *   - all typed libraries in host environment must carry compatible runtime type constraints
 *)

structure TypeChk = struct

exception IllTypedException of string

open Ast

(* TODO: what is the proper way to resolve these built-ins? *)
fun simpleIdent s = Identifier { ident=s, openNamespaces=ref [] }

val boolType      = PrimaryType { ident=simpleIdent "boolean",   kind=Named }
val numberType    = PrimaryType { ident=simpleIdent "number",    kind=Named }
val decimalType   = PrimaryType { ident=simpleIdent "decimal",   kind=Named }
val intType       = PrimaryType { ident=simpleIdent "int",       kind=Named }
val uintType      = PrimaryType { ident=simpleIdent "uint",      kind=Named }
val stringType    = PrimaryType { ident=simpleIdent "string",    kind=Named }
val regexpType    = PrimaryType { ident=simpleIdent "regexp",    kind=Named }
val exceptionType = PrimaryType { ident=simpleIdent "exception", kind=Named }
val undefinedType = SpecialType Undefined
val nullType      = SpecialType Null
val anyType       = SpecialType Any

fun assert b s = if b then () else (raise Fail s)

type TYPE_ENV = (IDENT * TYPE_EXPR) list

fun extendEnv ((name, ty), env) = (name, ty)::env

type CONTEXT = {this: TYPE_EXPR, env: TYPE_ENV, lbls: IDENT option list, retTy: TYPE_EXPR option}

fun withThis ({this=_, env=env, lbls=lbls, retTy=retTy}, this) = {this=this, env=env, lbls=lbls, retTy=retTy}

fun withEnv ({this=this, env=_, lbls=lbls, retTy=retTy}, env) = {this=this, env=env, lbls=lbls, retTy=retTy}

fun withLbls ({this=this, env=env, lbls=_, retTy=retTy}, lbls) = {this=this, env=env, lbls=lbls, retTy=retTy}

fun withRetTy ({this=this, env=env, lbls=lbls, retTy=_}, retTy) = {this=this, env=env, lbls=lbls, retTy=retTy}

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

fun unOptionTy NONE = Any
  | unOptionTy (SOME t) = t

(******************** Expressions **************************************************)
	
fun tcExpr ((ctxt as {env,this,...}):CONTEXT) (e:EXPR) :TYPE_EXPR = 
	let
	in 
     	TextIO.print "type checking expr: ";
        Pretty.ppExpr e;
        TextIO.print "\n";
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
       | NullaryExpr This => this
       | NullaryExpr Empty => (TextIO.print "what is Empty?\n"; raise Match)
       | UnaryExpr (unop, arg) => tcUnaryExpr ctxt (unop, arg)
       | _ => (TextIO.print "tcExpr incomplete: "; Pretty.ppExpr e; raise Match)
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
       | TypeExpr of TYPE_EXPR
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

(* TODO: tcPattern returns a pair of env extension and (inferred) type?
         or takes a type (checked) and returns just extension? *)
(*
and tcPattern (ctxt:CONTEXT) (Ast.IdentifierPattern name) = (
  | tcPattern ctxt (Ast.ObjectPattern props) =
  | tcPattern ctxt (Ast.ArrayPattern elts) =
  | tcPattern ctxt (Ast.SimplePattern expr) = ??
*)

(* TODO: this needs to return some type structure as well *)
and tcVarDefn (ctxt:CONTEXT) 
     (VariableDefinition {tag,init,attrs,pattern,ty}) =
        (* TODO: what are simple patterns? *)
	[]

and tcUnaryExpr (ctxt:CONTEXT) (unop:UNOP, arg:EXPR) =
    (case unop of
(*
          Delete => (case arg of
                          Ref {base=NONE,ident=???} =>
                        | Ref {base=SOME baseExpr,ident=???} =>
                        | _ => raise IllTypedException "can only delete ref expressions")
*)
          Void => (tcExpr ctxt arg; undefinedType)
        | Typeof => (tcExpr ctxt arg; stringType)
(*
        | PreIncrement
        | PreDecrement
        | PostIncrement
        | PostDecrement
        | UnaryPlus
        | UnaryMinus
        | BitwiseNot
        | LogicalNot
        | MakeNamespace
        | Type
*)
        | _ => (TextIO.print "tcUnaryExpr incomplete: "; Pretty.ppExpr (UnaryExpr (unop,arg)); raise Match)
    )

(**************************************************************)

fun tcStmts ctxt ss = List.app (fn s => tcStmt ctxt s) ss

and tcStmt ((ctxt as {this,env,lbls,retTy}):CONTEXT) stmt =
   let
   in
   TextIO.print "type checking stmt ... \n";
        Pretty.ppStmt stmt;
        TextIO.print "\n";
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
  | _ => (TextIO.print "tcStmt incomplete: "; Pretty.ppStmt stmt; raise Match)

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

   end

and tcDefn ctxt d =
    (case d of
        VariableDefn vd => (tcVarDefn ctxt vd, [])
       | d => (TextIO.print "tcDefn incomplete: "; Pretty.ppDefinition d; raise Match)
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
   (tcBlock {this=anyType, env=[], lbls=[], retTy=NONE} body; true)
   handle IllTypedException msg => (
     TextIO.print "Ill typed exception: "; 
     TextIO.print msg; 
     TextIO.print "\n"; 
     false)

   


end
