(*
 * INVARIANTS:
 *   - all typed libraries in host environment must be DontDelete
 *   - all typed libraries in host environment must carry compatible runtime type constraints
 *)

(* TODO: rename to Verify *)
structure TypeChk = struct

(* TODO: rename to VerifyError *)
exception IllTypedException of string
exception BrokenInvariant of string
exception CalledEval
    
open Ast

(* TODO: what is the proper way to resolve these built-ins? *)
fun simpleIdent (s:string) : IDENT_EXPR 
  = Identifier { ident=s, openNamespaces=ref [] }
    
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

(* type env has program variables, with types, and type variables, with no types *)
(* TODO: do we need to consider namespaces here ??? *)
type TYPE_ENV = (IDENT * TYPE_EXPR option) list

fun extendEnv ((name, ty), env:TYPE_ENV) :TYPE_ENV = (name, ty)::env

fun lookupProgramVariable (env:TYPE_ENV) (name:IDENT) : TYPE_EXPR =
    case List.find (fn (n,_) => n=name) env of
	NONE => raise IllTypedException ("Unbound variable: " ^ name)
      | SOME (_,NONE) => raise IllTypedException "Refered to type variable as a program variable"
      | SOME (_,SOME t) => t

type CONTEXT = {this: TYPE_EXPR, env: TYPE_ENV, lbls: IDENT option list, retTy: TYPE_EXPR option}

fun withThis  ({this=_,    env=env, lbls=lbls, retTy=retTy}, this) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withEnv   ({this=this, env=_,   lbls=lbls, retTy=retTy},  env) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withLbls  ({this=this, env=env, lbls=_,    retTy=retTy}, lbls) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withRetTy ({this=this, env=env, lbls=lbls, retTy=_},    retTy) = {this=this, env=env, lbls=lbls, retTy=retTy}

fun checkConvertible (t1:TYPE_EXPR) (t2:TYPE_EXPR) = ()

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

fun normalizeType (t:TYPE_EXPR):TYPE_EXPR =
    case t of
	AppType {base, args } => base (*TODO*)
      | _ => t

(*
 * TODO: when type checking a function body, handle CalledEval
 * TODO: during type checking, if you see naked invocations of "eval" (that
 *       exact name), raise CalledEval
 *)

(******************** Expressions **************************************************)

fun tcExpr ((ctxt as {env,this,...}):CONTEXT) (e:EXPR) :TYPE_EXPR = 
    let
    in 
      TextIO.print ("type checking expr: env len " ^ (Int.toString (List.length env)) ^"\n");
      Pretty.ppExpr e;
      TextIO.print "\n";
      case e of
	LiteralExpr LiteralNull => nullType
      | LiteralExpr (LiteralNumber _) => intType
      | LiteralExpr (LiteralBoolean _) => boolType
      | LiteralExpr (LiteralString _) => stringType
      | LiteralExpr (LiteralRegExp _) => regexpType
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
          let val extensions = List.concat (List.map (fn d => tcBinding ctxt d) defs)
          in
	    checkForDuplicates extensions;
	    tcExprList (withEnv (ctxt, foldl extendEnv env extensions)) body
	  end
       | NullaryExpr This => this
       | NullaryExpr Empty => (TextIO.print "what is Empty?\n"; raise Match)
       | UnaryExpr (unop, arg) => tcUnaryExpr ctxt (unop, arg)
       | LexicalRef {ident=Identifier { ident, openNamespaces }} =>
	 lookupProgramVariable env ident

       | FunExpr { ident, 
		   fsig as (FunctionSignature {typeParams,params,returnType,thisType,inits,...}), 
		   body} 
	 =>
         let (* Add the type parameters to the environment. *)
             val extensions1 = List.map (fn id => (id,NONE)) typeParams;
	     val ctxt1 = withEnv (ctxt,foldl extendEnv env extensions1);
	     (* Add the function arguments to the environment. *)
             val extensions2 = List.concat (List.map (fn d => tcBinding ctxt1 d) params); 
	     val ctxt2 = withEnv (ctxt,foldl extendEnv env extensions2);
	     (* Add the return type to the context. *)
             val ctxt3 = withRetTy (ctxt2, SOME returnType)
	 in
	     checkForDuplicates (extensions1 @ extensions2);
	     tcType ctxt1 returnType;
	     tcBlock ctxt3 body;
	     FunctionType fsig
         end
       | CallExpr { func, actuals } =>
         let val functy = tcExpr ctxt func;
	     val actualsTy = (map (fn a => tcExpr ctxt a) actuals)
	 in case normalizeType functy of
		SpecialType Any =>
		(* not much to do *)
		anyType
	      | FunctionType 
		    (FunctionSignature { typeParams, params, returnType, inits, thisType, hasBoundThis (*deprecated*),
					 hasRest })
		=> 
		let 
		in
		    if not (null typeParams)
		    then raise IllTypedException "Attempt to apply polymorphic function to values"
		    else ();
		    (* check this has compatible type *)
		    case thisType of
			NONE => ()  (* this lexically bound, nothing to check *)
		      | SOME t => checkConvertible this t;
		    (* check compatible parameters *)
		    let val (normalParams, restParam) =
			    if hasRest
			    then (List.rev (List.tl (List.rev params)), SOME (List.last params))
			    else (params, NONE)
			(* handleArgs normalParams actualTys *)
			fun handleArgs [] [] = ()
			  | handleArgs (p::pr) (a::ar) =				
			    let val Binding {kind,init,attrs,pattern,ty} = p in
				checkConvertible a (unOptionDefault ty anyType);
				handleArgs pr ar
			    end
			  | handleArgs [] (_::_) = 
			    if hasRest 
			    then (* all ok, no type checking to do on rest arg *) ()
			    else raise IllTypedException "Too many args to function"
			  | handleArgs (_::_) [] =
			    raise IllTypedException "Not enough args to function"
		    in
			handleArgs params actualsTy;
			returnType
		    end
		end
	      | _ => raise IllTypedException "Function expression does not have a function type"
	 end

       | ApplyTypeExpr {expr, actuals} =>
	 (* Can only instantiate Functions, classes, and interfaces *)
	 let val exprTy = tcExpr ctxt expr;
	     val typeParams = 
		 case exprTy of
		     FunctionType (FunctionSignature {typeParams, ...}) => typeParams
                   (* TODO: class and interface types *)
		   | _ => raise IllTypedException "Cannot instantiate a non-polymorphic type"
	 in
	     List.app (fn t => tcType ctxt t) actuals;
	     if (List.length typeParams) = (List.length actuals)
	     then ()
	     else raise IllTypedException "Wrong number of type arguments";
	     normalizeType (AppType { base=exprTy, args=actuals })
	 end

       | _ => (TextIO.print "tcExpr incomplete: "; Pretty.ppExpr e; raise Match)
    end
    
(*
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

and tcExprList ((ctxt as {env,this,...}):CONTEXT) (l:EXPR list) :TYPE_EXPR = 
	let
	in 	case l of
		_  => List.last (List.map (tcExpr ctxt) l)
	end

and inferObjectType ctxt fields =
    (* TODO: get a (name, type) option for every field *)
    raise (Fail "blah")

(* TODO: this needs to return some type structure as well *)
and tcBinding (ctxt:CONTEXT) (Binding {kind,init,attrs,pattern,ty}) : TYPE_ENV =
    let val ty = unOptionDefault ty anyType in
	case init of
	    SOME expr => checkConvertible (tcExpr ctxt expr) ty
	  | NONE => ();
	case pattern of
	    IdentifierPattern (Identifier {ident,openNamespaces}) => [(ident,SOME ty)]
    end
(*
 and VAR_BINDING =
         Binding of { kind: VAR_DEFN_TAG,
                      init: EXPR option,
                      attrs: ATTRIBUTES,
                      pattern: PATTERN,
                      ty: TYPE_EXPR option }

*)

and tcVarBinding (ctxt:CONTEXT) (v:VAR_BINDING) : TYPE_ENV =
    case v of
	Binding {kind,init, attrs, pattern, ty} =>
	case pattern of
	    IdentifierPattern (Identifier {ident, openNamespaces}) =>
	    [(ident,SOME (unOptionDefault ty anyType))]
    (*TODO*)
    

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

and tcType ctxt ty = 
    (*TODO*)
    ()

(**************************************************************)

and tcStmts ctxt ss = List.app (fn s => tcStmt ctxt s) ss

and tcStmt ((ctxt as {this,env,lbls,retTy}):CONTEXT) (stmt:STMT) =
   let
   in
       TextIO.print ("type checking stmt: env len " ^ (Int.toString (List.length env)) ^"\n");
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
    let val extensions = List.concat (List.map (fn d => tcBinding ctxt d) defns)
    in
        checkForDuplicates extensions;
        tcStmt (withEnv (ctxt, foldl extendEnv env extensions)) body  
    end
    

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

and tcDefn (ctxt:CONTEXT) (d:DEFN) : (TYPE_ENV * int list) =
    (case d of
	 VariableDefn vd => (List.concat (List.map (fn d => tcVarBinding ctxt d) vd), []) 
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
    handle IllTypedException msg => 
	   let in
     	       TextIO.print "Ill typed exception: "; 
     	       TextIO.print msg; 
     	       TextIO.print "\n"; 
     	       false
	   end

   
				    

end
