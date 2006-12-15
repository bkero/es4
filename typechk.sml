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

val boolType      = NominalType { ident=simpleIdent "boolean",   nullable=NONE }
val numberType    = NominalType { ident=simpleIdent "number",    nullable=NONE }
val decimalType   = NominalType { ident=simpleIdent "decimal",   nullable=NONE }
val intType       = NominalType { ident=simpleIdent "int",       nullable=NONE }
val uintType      = NominalType { ident=simpleIdent "uint",      nullable=NONE }
val stringType    = NominalType { ident=simpleIdent "string",    nullable=NONE }
val regexpType    = NominalType { ident=simpleIdent "regexp",    nullable=NONE }
val exceptionType = NominalType { ident=simpleIdent "exception", nullable=NONE }
val namespaceType = NominalType { ident=simpleIdent "Namespace", nullable=NONE }
val undefinedType = SpecialType Undefined
val nullType      = SpecialType Null
val anyType       = SpecialType Any

fun assert b s = if b then () else (raise Fail s)

type TYPE_ENV = (IDENT * TYPE_EXPR) list

fun extendEnv ((name, ty), env) = (name, ty)::env

type CONTEXT = {this: TYPE_EXPR, env: TYPE_ENV, lbls: IDENT option list, retTy: TYPE_EXPR option}

fun withThis  ({this=_,    env=env, lbls=lbls, retTy=retTy}, this) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withEnv   ({this=this, env=_,   lbls=lbls, retTy=retTy},  env) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withLbls  ({this=this, env=env, lbls=_,    retTy=retTy}, lbls) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withRetTy ({this=this, env=env, lbls=lbls, retTy=_},    retTy) = {this=this, env=env, lbls=lbls, retTy=retTy}

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

fun unOptionDefault NONE def = def
  | unOptionDefault (SOME v) _ = v
	
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
        | LiteralExpr (LiteralArray { exprs, ty }) =>
          (* EXAMPLES:
               [a, b, c] : [int, Boolean, String]
               [a, b, c] : Array
               [a, b, c] : *
               [a, b, c] : Object
           *)
          let val annotatedTy = unOptionDefault ty anyType
              val inferredTy = ArrayType (map (fn elt => tcExpr ctxt elt) exprs)
          in
              checkConvertible inferredTy annotatedTy;
              annotatedTy
          end
        | LiteralExpr (LiteralObject { expr, ty }) =>
          let val annotatedTy = unOptionDefault ty anyType
              val inferredTy = inferObjectType ctxt expr
          in
              checkConvertible inferredTy annotatedTy;
              annotatedTy
          end
	   | ListExpr l => List.last (List.map (tcExpr ctxt) l)
	| LetExpr {defs, body} => 
          let val extensions = List.concat (List.map (fn d => tcVarDefn ctxt d) defs)
          in
	    checkForDuplicates extensions;
	    tcExprList (withEnv (ctxt, foldl extendEnv env extensions)) body
	  end
       | NullaryExpr This => this
       | NullaryExpr Empty => (TextIO.print "what is Empty?\n"; raise Match)
       | UnaryExpr (unop, arg) => tcUnaryExpr ctxt (unop, arg)
(*
       | FunExpr {ident, sign as (FunctionSignature {typeParams,params,returnType,...}), body} =>
    (* What to do with typeparams - no place in context for type variables
    *  also need to check well-formedness of resulttype
    *  No place in FUNC_TY for type parameters
    *)
          let val extensions = List.concat (List.map (fn d => tcVarDefn ctxt d) params);
	      val ctxt1 = withEnv (ctxt, foldl extendEnv env extensions);
	      val ctxt2 = withRetTy (ctxt1, SOME returnType)
          in
	    checkForDuplicates extensions;
	    tcBlock ctxt2 body;
	    FunctionType { paramTypes= (List.map (fn (Binding {kind=_,init=_,attrs=_,pattern=_,ty=tyo}) => tyo) params),
			   returnType=returnType,
			   boundThisType=NONE,  (*FIXME*)
			   hasRest=false  (*FIXME*)
			 }
	  end
*)
       | _ => (TextIO.print "tcExpr incomplete: "; Pretty.ppExpr e; raise Match)
	end

and tcExprList ((ctxt as {env,this,...}):CONTEXT) (l:EXPR list) :TYPE_EXPR = 
	let
	in 	case l of
		_  => List.last (List.map (tcExpr ctxt) l)
	end

(*
(sign as (FunctionSignature {typeparams, params, resulttype}))

       | FunExpr of { ident: IDENT option,
                      sign: FUNC_SIGN,
                      body: BLOCK }

     and FUNC_TY =
         { paramTypes: TYPE_EXPR option list,
           returnType: TYPE_EXPR,
           boundThisType: TYPE_EXPR option,
           hasRest: bool }

     and FUNC_SIGN =
         FunctionSignature of { typeparams: IDENT list,
                                params: VAR_BINDING list,
                                resulttype: TYPE_EXPR }


     and LITERAL =
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

and inferObjectType ctxt fields =
    (* TODO: get a (name, type) option for every field *)
    raise (Fail "blah")

(* TODO: this needs to return some type structure as well *)
and tcVarDefn (ctxt:CONTEXT) 
     (Binding {kind,init,attrs,pattern,ty}) =
        (* TODO: what are simple patterns? *)
	[]

and tcIdentExpr (ctxt:CONTEXT) (id:IDENT_EXPR) =
    (case id of
          QualifiedIdentifier { qual, ident=_ } => (checkConvertible (tcExpr ctxt qual) namespaceType; ())
        | QualifiedExpression { qual, expr } => (checkConvertible (tcExpr ctxt qual) namespaceType;
                                                 checkConvertible (tcExpr ctxt expr) stringType;
                                                 ())
        | Identifier _ => ()
        | ExpressionIdentifier expr => (checkConvertible (tcExpr ctxt expr) stringType; ()))
(*
       | AttributeIdentifier of IDENT_EXPR
       | TypeIdentifier of { ident : IDENT_EXPR, 
			     typeParams : TYPE_EXPR list }
*)

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

and tcStmts ctxt ss = List.app (fn s => tcStmt ctxt s) ss

and tcStmt ((ctxt as {this,env,lbls,retTy}):CONTEXT) (stmt:STMT) =
   let
   in
   TextIO.print "type checking stmt ... \n";
        Pretty.ppStmt stmt;
        TextIO.print "\n";
   case stmt of
    EmptyStmt => ()
  | ExprStmt e => (tcExprList ctxt e; ())
  | IfStmt {cnd,thn,els} => (
	checkConvertible (tcExpr ctxt cnd) boolType;
	tcStmt ctxt thn;
	tcStmt ctxt els
    )

  | (DoWhileStmt {cond,body,contLabel} | WhileStmt {cond,body,contLabel}) => (
	checkConvertible (tcExpr ctxt cond) boolType;
	tcStmt (withLbls (ctxt, contLabel::lbls)) body
    )

  | ReturnStmt e => (
	case retTy of
	  NONE => raise IllTypedException "return not allowed here"
        | SOME retTy => checkConvertible (tcExprList ctxt e) retTy
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
	checkConvertible (tcExprList ctxt t) exceptionType

  | LetStmt (defns, body) =>
    (
        let val extensions = List.concat (List.map (fn d => tcVarDefn ctxt d) defns)
        in
	    checkForDuplicates extensions;
(* FIXME: not sure the following change is correct	    
		tcBlock (withEnv (ctxt, foldl extendEnv env extensions)) body  
*)
	    tcStmt ctxt body

	end
    )
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
        VariableDefn vd => (tcVarDefn ctxt (hd vd), [])
(*		VariableDefn vd => (List.map (fn d => tcVarDefn ctxt d) vd) *)
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
