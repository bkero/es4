(*
 * INVARIANTS:
 *   - all typed libraries in host environment must be DontDelete
 *   - all typed libraries in host environment must carry compatible runtime type constraints
 *)

structure Verify = struct

exception VerifyError of string
exception BrokenInvariant of string
exception CalledEval
    
open Ast LogErr

(* TODO: what is the proper way to resolve these built-ins? *)
fun simpleIdent (s:string) : IDENT_EXPR 
  = Identifier { ident=s, openNamespaces=[] }
    
val boolType      = NominalType { ident=simpleIdent "boolean" }
val numberType    = NominalType { ident=simpleIdent "number" }
val decimalType   = NominalType { ident=simpleIdent "decimal" }
val intType       = NominalType { ident=simpleIdent "int" }
val uintType      = NominalType { ident=simpleIdent "uint" }
val stringType    = NominalType { ident=simpleIdent "string" }
val regexpType    = NominalType { ident=simpleIdent "regexp" }
val exceptionType = NominalType { ident=simpleIdent "exception" }
val namespaceType = NominalType { ident=simpleIdent "Namespace" }
val undefinedType = SpecialType Undefined
val nullType      = SpecialType Null
val anyType       = SpecialType Any

fun assert b s = if b then () else (raise Fail s)

(* type env has program variables, with types, axnd type variables, with no types *)
(* TODO: do we need to consider namespaces here ??? *)
type TYPE_ENV = (IDENT * TYPE_EXPR option) list

fun checkForDuplicates [] = ()
  | checkForDuplicates (x::xs) =
    if List.exists (fn y => x = y) xs
    then raise VerifyError "concurrent definition"
    else checkForDuplicates xs
      
fun checkForDuplicateExtension extensions =
    let val (names, _) = ListPair.unzip extensions
    in
      checkForDuplicates names
end

fun extendEnv ((name,ty),(env:TYPE_ENV)) : TYPE_ENV = (name, ty)::env
fun extendEnvWithTypeVars (params:IDENT list) (env:TYPE_ENV):TYPE_ENV =
    let in
	checkForDuplicates params;
	foldl (fn (p,env) => (extendEnv ((p,NONE),env))) env params
    end

fun lookupProgramVariable (env:TYPE_ENV) (name:IDENT) : TYPE_EXPR =
    case List.find (fn (n,_) => n=name) env of
	NONE => raise VerifyError ("Unbound variable: " ^ name)
      | SOME (_,NONE) => raise VerifyError "Refered to type variable as a program variable"
      | SOME (_,SOME t) => t

type CONTEXT = {this: TYPE_EXPR, env: TYPE_ENV, lbls: IDENT option list, retTy: TYPE_EXPR option}

fun withThis  ({this=_,    env=env, lbls=lbls, retTy=retTy}, this) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withEnv   ({this=this, env=_,   lbls=lbls, retTy=retTy},  env) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withLbls  ({this=this, env=env, lbls=_,    retTy=retTy}, lbls) = {this=this, env=env, lbls=lbls, retTy=retTy}
fun withRetTy ({this=this, env=env, lbls=lbls, retTy=_},    retTy) = {this=this, env=env, lbls=lbls, retTy=retTy}



fun unOptionDefault NONE def = def
  | unOptionDefault (SOME v) _ = v

(************************* Substitution on Types *********************************)

val gensymCounter : int ref = ref 0
fun gensym (s:IDENT):IDENT = 
    let
    in 
	gensymCounter := 1+(!gensymCounter);
	s^"$"^(Int.toString (!gensymCounter))
    end

fun substTypeExpr (s:(IDENT*TYPE_EXPR) list) (t:TYPE_EXPR):TYPE_EXPR = 
    let in
	case t of
	    UnionType ts => 
	    UnionType (map (substTypeExpr s) ts)
	  | ArrayType ts => 
	    ArrayType (map (substTypeExpr s) ts)
	  | AppType {base, args} => 
	    AppType {base=substTypeExpr s base, args=map (substTypeExpr s) args}
	  | NullableType {expr, nullable}
	    => NullableType {expr=substTypeExpr s expr, nullable=nullable}

	  | NominalType { ident=(Identifier {ident=ident, openNamespaces=_})} =>
	    let in
		case List.find 
			 (fn (id,ty) => id=ident)
			 s 
		 of
		    (* TODO: we're dropping the nullable here, is that right? *)
		    SOME (_,ty) => ty 
		  | NONE => t
	    end
	  | NominalType { ident=_ } => t

	  | ObjectType fields =>
	    ObjectType (map (fn {name,ty} => 
				{name=name,ty=substTypeExpr s ty})
			fields)
	  | SpecialType st => SpecialType st

	  | FunctionType (FunctionSignature {typeParams,params,inits,
					     returnType,thisType,hasRest}) =>
	    (* Need to uniquify typeParams to avoid capture *)
	    let val oldNew = map (fn id => (id, gensym id)) typeParams 
		val nuSub = 
		    map 
			(fn (oldId,newId) => 
			     (oldId, 
			      NominalType { ident=Identifier {ident=newId,openNamespaces=[]}}))
			oldNew
		val bothSubs = fn t => substTypeExpr s (substTypeExpr nuSub t)
		val nuTypeParams = map (fn (oldId,newId) => newId) oldNew
		val nuParams = map (fn t => substVarBinding s (substVarBinding nuSub t)) params
		in
		FunctionType (FunctionSignature {typeParams=nuTypeParams,
						 params=nuParams,
						 inits=inits, (* ignored in types *)
						 returnType = bothSubs returnType,
						 thisType = Option.map bothSubs thisType,
						 hasRest=hasRest})
		end
    end 

and substVarBinding (s:(IDENT*TYPE_EXPR) list) (Binding {init,pattern,ty}:VAR_BINDING):VAR_BINDING = 
    Binding {init=init,pattern=pattern, ty=Option.map (substTypeExpr s) ty}

(************************* Compatibility *********************************)

fun l [] = 0
  | l (h::t) = 1

fun checkCompatible (t1:TYPE_EXPR) 
		    (t2:TYPE_EXPR) 
    : unit = 
    if isCompatible t1 t2
    then ()
    else let in
	     TextIO.print ("Types are not compatible\n");
	     Pretty.ppType t1;
	     Pretty.ppType t2;
	     raise VerifyError "Types are not compatible"
	 end

and isCompatible (t1:TYPE_EXPR) 
		 (t2:TYPE_EXPR) 
    : bool = 
    let 
    in
	TextIO.print ("Checking compatible\nFirst type: ");
	Pretty.ppType t1;
	TextIO.print("\nSecond type: ");
	Pretty.ppType t2; 
	TextIO.print("\n");
	(t1=t2) orelse
	(t1=anyType) orelse
	(t2=anyType) orelse
	case (t1,t2) of
	    (UnionType types1,_) => 
	    List.all (fn t => isCompatible t t2) types1
	  | (_, UnionType types2) =>
	    (* t1 must exist in types2 *)
	    List.exists (fn t => isCompatible t1 t) types2 
	  | (ArrayType types1, ArrayType types2) => 
	    (* arrays are invariant, every entry should be compatible in both directions *)
	    let fun check (h1::t1) (h2::t2) =
		    (isCompatible h1 h2)
		    andalso
		    (isCompatible h2 h1)
		    andalso
		    (case (t1,t2) of
			 ([],[]) => true
		       | ([],_::_) => check [h1] t2
		       | (_::_,[]) => check t1 [h2]
		       | (_::_,_::_) => check t1 t2)
	    in
		check types1 types2
	    end

	  | (ArrayType _, 
	     NominalType {ident=Identifier {ident="Array", openNamespaces=[]}}) 
	    => true

	  | (ArrayType _, 
	     NominalType {ident=Identifier {ident="Object", openNamespaces=[]}}) 
	    => true

	  | (FunctionType _, 
	     NominalType {ident=Identifier {ident="Function", openNamespaces=[]}}) 
	    => true

	  | (FunctionType _, 
	     NominalType {ident=Identifier {ident="Object", openNamespaces=[]}})
	    => true

	  | (AppType {base=base1,args=args1},AppType {base=base2,args=args2}) => 
	    (* We keep types normalized wrt beta-reduction, 
	     * so base1 and base2 must be class or interface types.
	     * Type arguments are covariant, and so must be intra-compatible - CHECK 
	     *)
	    false
	    
	  | (ObjectType fields1,ObjectType fields2) =>
	    false

	  | (FunctionType 
		 (FunctionSignature 
		      {typeParams=typeParams1,params=params1, 
		       inits=inits1,returnType=returnType1,
		       thisType=thisType1,hasRest=hasRest1}),
	     FunctionType 
		 (FunctionSignature 
		      {typeParams=typeParams2,params=params2, 
		       inits=inits2,returnType=returnType2,
		       thisType=thisType2,hasRest=hasRest2})) =>
	    let
	    in
		(* TODO: Assume for now that functions are not polymorphic *)
		assert (typeParams1 = [] andalso typeParams2=[]) "cannot handle polymorphic fns";
		assert (not hasRest1 andalso not hasRest2) "cannot handle rest args";

		let fun checkArgs params1 params2 =
			case (params1,params2) of
			    ([],[]) => true
			  | (Binding {init=_,pattern=_,ty=ty1}::t1,
			     Binding {init=_,pattern=_,ty=ty2}::t2) => 
			    isCompatible (unOptionDefault ty2 anyType)
					  (unOptionDefault ty1 anyType)
			    andalso checkArgs t1 t2
			  | _ =>
			    (* different args length *)
			    false
		in
		    checkArgs params1 params2
		    andalso
		    isCompatible returnType1 returnType2
		end
		
	    (* TODO: Gets pretty complicated here! *)
	    end
	    
	  (* catch all *)
	  | _ => unimplError ["isCompatible"]
    end
    
(*
    and TYPE_EXPR =
         SpecialType of SPECIAL_TY
       | NominalType of { ident : IDENT_EXPR }
       | NullableType of {expr:TYPE_EXPR,nullable:bool}
*)

fun checkBicompatible (ty1:TYPE_EXPR) 
		      (ty2:TYPE_EXPR)
    : unit = 
    let in
	checkCompatible ty1 ty2;
	checkCompatible ty2 ty1
    end
    
fun checkConvertible (ty1:TYPE_EXPR) (ty2:TYPE_EXPR)
    : unit =
    (* TODO: int to float, etc, and to() methods *)
    checkCompatible ty1 ty2

(************************* Handling Types *********************************)

fun mergeTypes t1 t2 =
	t1

fun normalizeType (t:TYPE_EXPR)
    : TYPE_EXPR =
    let in
	case t of
	    AppType {base=FunctionType (FunctionSignature {typeParams,params,inits,returnType,
							   thisType,hasRest}), 
		     args } =>
	    let val _ =	assert (length args = length typeParams);
		val sub = ListPair.zip (typeParams,args)
		fun applySub t = substTypeExpr sub t
	    in
		FunctionType (FunctionSignature { typeParams=[],
						  params=map (substVarBinding sub) params,
						  inits=inits,
						  returnType=substTypeExpr sub returnType,
						  thisType= Option.map (substTypeExpr sub) thisType,
						  hasRest=hasRest })
	    end
	  | _ => t
    end

(*
 * TODO: when type checking a function body, handle CalledEval
 * TODO: during type checking, if you see naked invocations of "eval" (that
 *       exact name), raise CalledEval
 *)

(******************** Checking types are well-formed **************************************************)

fun verifyTypeExpr (ctxt as {env,this,...}:CONTEXT)
		   (ty:TYPE_EXPR) 
    : unit = 
    let
    in 
	case ty of
	    SpecialType _ => ()
	  | UnionType tys => verifyTypeExprs ctxt tys
	  | ArrayType tys => verifyTypeExprs ctxt tys
	  | NullableType {expr=ty,nullable} => verifyTypeExpr ctxt ty
	  | AppType {base,args} =>
	    let in
		verifyTypeExpr ctxt base;
		verifyTypeExprs ctxt args
	    (*TODO: check # args is correct *)
	    end
	  | FunctionType fsig =>
	    let in
		verifyFunctionSignature ctxt fsig;
		()
	    end
	  | ObjectType fields => 
	    let val names: IDENT list =
		    map (fn {name,ty} =>
			     (verifyTypeExpr ctxt ty;
			     name))
			fields
	    in
		checkForDuplicates names
	    end
	
    (*TODO:
          | NominalType of { ident : IDENT_EXPR } 
      *)	    
    end

and verifyTypeExprs  (ctxt as {env,this,...}:CONTEXT) 
		     (tys:TYPE_EXPR list) 
    : unit = 
    (List.app (verifyTypeExpr ctxt) tys)

and verifyFunctionSignature  (ctxt as {env,this,...}:CONTEXT)
			     (FunctionSignature {typeParams, params, inits, returnType, 
						 thisType, hasRest})
    : CONTEXT =
    let (* Add the type parameters to the environment. *)
	val extensions1 = List.map (fn id => (id,NONE)) typeParams;
	val ctxt1 = withEnv (ctxt,foldl extendEnv env extensions1);
	(* Add the function arguments to the environment. *)
	val extensions2 = List.concat (List.map (fn d => verifyBinding ctxt1 d) params); 
	val ctxt2 = withEnv (ctxt,foldl extendEnv env extensions2);
	(* Add the return type to the context. *)
	val ctxt3 = withRetTy (ctxt2, SOME returnType)
    in
	checkForDuplicateExtension (extensions1 @ extensions2);
	verifyTypeExpr ctxt1 returnType;
	(case thisType of
	     NONE => ()
	   | SOME t => verifyTypeExpr ctxt t);
	ctxt3
	(* TODO: check inits, hasRest *)
    end

(******************** Expressions **************************************************)

and verifyExpr (ctxt as {env,this,...}:CONTEXT) 
	       (e:EXPR) 
    : TYPE_EXPR = 
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
            val inferredTy = ArrayType (map (fn elt => verifyExpr ctxt elt) exprs)
        in
          checkCompatible inferredTy annotatedTy;
          annotatedTy
        end
      | LiteralExpr (LiteralObject { expr=fields, ty }) =>
        let val annotatedTy = unOptionDefault ty anyType
            val inferredTy = ObjectType (map (verifyField ctxt) fields)
        in
          checkCompatible inferredTy annotatedTy;
          annotatedTy
        end
      | ListExpr l => List.last (List.map (verifyExpr ctxt) l)
      | LetExpr {defs, body, fixtures } => 
          let val extensions = List.concat (List.map (fn d => verifyBinding ctxt d) defs)
          in
	    checkForDuplicateExtension extensions;
	    verifyExpr (withEnv (ctxt, foldl extendEnv env extensions)) body
	  end
       | ThisExpr => this
(* jd: deleted       | NullaryExpr Empty => (TextIO.print "what is Empty?\n"; raise Match)   *)
       | UnaryExpr (unop, arg) => verifyUnaryExpr ctxt unop arg
       | BinaryExpr (binop, lhs, rhs ) => verifyBinaryExpr ctxt (binop, lhs, rhs)
       | BinaryTypeExpr (binop, lhs, rhs ) => verifyBinaryTypeExpr ctxt (binop, lhs, rhs)
       | TrinaryExpr (triop, a,b,c ) => verifyTrinaryExpr ctxt (triop, a,b,c)

       | LexicalRef {ident=Identifier { ident, openNamespaces }} =>
	 lookupProgramVariable env ident

       | FunExpr (Func {fsig, body, ...})
	 =>
	 let
		val ctxt3 = verifyFunctionSignature ctxt fsig
	 in
	     verifyBlock ctxt3 body;
	     FunctionType fsig
         end

       | CallExpr { func, actuals } => verifyCallExpr ctxt func actuals
       | ApplyTypeExpr {expr, actuals} =>
	 (* Can only instantiate Functions, classes, and interfaces *)
	 let val exprTy = verifyExpr ctxt expr;
	     val typeParams = 
		 case exprTy of
		     FunctionType (FunctionSignature {typeParams, ...}) => typeParams
                   (* TODO: class and interface types *)
		   | _ => raise VerifyError "Cannot instantiate a non-polymorphic type"
	 in
	     List.app (fn t => verifyTypeExpr ctxt t) actuals;
	     if (List.length typeParams) = (List.length actuals)
	     then ()
	     else raise VerifyError "Wrong number of type arguments";
	     normalizeType (AppType { base=exprTy, args=actuals })
	 end

       | TypeExpr ty => 
	 let 
	 in
	     verifyTypeExpr ctxt ty;
	     ty
	 end

       | _ => (TextIO.print "verifyExpr incomplete: "; Pretty.ppExpr e; raise Match)
    end
    
(*
     and LITERAL =
       | LiteralXML of EXPR list
       | LiteralNamespace of NAMESPACE

       | LiteralObject of
         { name: EXPR,
           init: EXPR } list

     and EXPR =
        | YieldExpr of EXPR option
       | SuperExpr of EXPR option

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


and verifyCallExpr  (ctxt as {env,this,...}:CONTEXT) 
		    func actuals
    : TYPE_EXPR =
    let val functy = verifyExpr ctxt func;
	val actualsTy = (map (fn a => verifyExpr ctxt a) actuals)
    in case normalizeType functy of
	   SpecialType Any =>
	   (* not much to do *)
	   anyType
	 | FunctionType 
	       (FunctionSignature { typeParams, params, returnType, inits, thisType, hasRest })
	   => 
	   let 
	   in
	       if not (null typeParams)
	       then raise VerifyError "Attempt to apply polymorphic function to values"
	       else ();
	       (* check this has compatible type *)
	       case thisType of
		   NONE => ()  (* this lexically bound, nothing to check *)
		 | SOME t => checkCompatible this t;
	       (* check compatible parameters *)
	       let val (normalParams, restParam) =
		       if hasRest
		       then (List.rev (List.tl (List.rev params)), SOME (List.last params))
		       else (params, NONE)
		   (* handleArgs normalParams actualTys *)
		   fun handleArgs [] [] = ()
		     | handleArgs (p::pr) (a::ar) =				
		       let val Binding {init,pattern,ty} = p in
			   checkCompatible a (unOptionDefault ty anyType);
			   handleArgs pr ar
		       end
		     | handleArgs [] (_::_) = 
		       if hasRest 
		       then (* all ok, no type checking to do on rest arg *) ()
		       else raise VerifyError "Too many args to function"
		     | handleArgs (_::_) [] =
		       raise VerifyError "Not enough args to function"
	       in
		   handleArgs params actualsTy;
		   returnType
	       end
	   end
	 | _ => raise VerifyError "Function expression does not have a function type"
    end
    

(* TODO: verifyPattern returns a pair of env extension and (inferred) type?
         or takes a type (checked) and returns just extension? *)
(*
and verifyPattern (ctxt:CONTEXT) (Ast.IdentifierPattern name) = (
  | verifyPattern ctxt (Ast.ObjectPattern props) =
  | verifyPattern ctxt (Ast.ArrayPattern elts) =
  | verifyPattern ctxt (Ast.SimplePattern expr) = ??
*)

and verifyExprList (ctxt as {env,this,...}:CONTEXT)
		   (l:EXPR list) 
    : TYPE_EXPR = 
    let
    in 	case l of
	    _  => List.last (List.map (verifyExpr ctxt) l)
    end
    
and verifyField (ctxt:CONTEXT) 
		({kind,name,init}:FIELD) 
    : FIELD_TYPE =
	let
		val Ast.Identifier {ident,...} = name
	in
	    {name=ident, ty = verifyExpr ctxt init }
	end

(* This type checksTODO: this needs to return some type structure as well *)
and verifyBinding (ctxt:CONTEXT) 
		  (Binding {init,pattern,ty}) 
    : TYPE_ENV =
    let val ty = unOptionDefault ty anyType in
	case init of
	    SOME expr => checkCompatible (verifyExpr ctxt expr) ty
	  | NONE => ();
	case pattern of
	    IdentifierPattern ident => [(ident,SOME ty)]
    end
(*
 and VAR_BINDING =
         Binding of { init: EXPR option,
                      pattern: PATTERN,
                      ty: TYPE_EXPR option }

*)

and verifyVarBinding (ctxt:CONTEXT) 
		     (Binding {init, pattern, ty}:VAR_BINDING) 
    : TYPE_ENV =
    let in
	case pattern of
	    IdentifierPattern ident =>
	    [(ident,SOME (unOptionDefault ty anyType))]
	    end
    
and verifyVarBindings (ctxt:CONTEXT) 
		      (vs:VAR_BINDING list) 
    : TYPE_ENV =
    case vs of
	[] => []
      | h::t => (verifyVarBinding ctxt h) @ (verifyVarBindings ctxt t)

and verifyIdentExpr (ctxt:CONTEXT) 
		    (id:IDENT_EXPR)
    : unit =
    (case id of
          QualifiedIdentifier { qual, ident=_ } => 
	  let in
	      checkCompatible (verifyExpr ctxt qual) namespaceType;
	      ()
	  end

        | QualifiedExpression { qual, expr } => 
	  let in
	      checkCompatible (verifyExpr ctxt qual) namespaceType;
              checkCompatible (verifyExpr ctxt expr) stringType;
              ()
	  end

        | Identifier _ => 
	  ()

        | ExpressionIdentifier expr => 
	  let in
	      checkCompatible (verifyExpr ctxt expr) stringType; 
	      ()
	  end)
(*
       | AttributeIdentifier of IDENT_EXPR
       | TypeIdentifier of { ident : IDENT_EXPR, 
			     typeParams : TYPE_EXPR list }
*)

and verifyUnaryExpr (ctxt:CONTEXT) 
		    (unop:UNOP)
		    (arg:EXPR) 
    : Ast.TYPE_EXPR =
    let val argType = verifyExpr ctxt arg
	fun checkNumeric () = 
	    let in
		checkBicompatible boolType argType;
		argType
	    end
    in
	case unop of
	    Void => (verifyExpr ctxt arg; undefinedType)
          | Typeof => (verifyExpr ctxt arg; stringType)

	  (* Assume arg is an l-value *)
          | PreIncrement  => checkNumeric ()
          | PostIncrement => checkNumeric ()
          | PreDecrement  => checkNumeric ()
          | UnaryPlus     => checkNumeric ()
          | UnaryMinus    => checkNumeric ()
          | BitwiseNot    => 
	    let in checkConvertible argType uintType; uintType end
          | LogicalNot    => 
	    let in checkConvertible argType boolType; boolType end
	    
  (*
             Delete => (case arg of
                            Ref {base=NONE,ident=???} =>
                          | Ref {base=SOME baseExpr,ident=???} =>
                          | _ => raise VerifyError "can only delete ref expressions")
  *)
 
(*    
          | MakeNamespace
          | Type
*)
          | _ => 
	    let in
		TextIO.print "verifyUnaryExpr incomplete: "; 
		Pretty.ppExpr (UnaryExpr (unop,arg)); 
		raise Match
	    end
    end

and verifyBinaryExpr (ctxt:CONTEXT) (bop:BINOP, lhs:EXPR, rhs:EXPR) =
    let
    in
	case bop of
	    _ => 
	    let in
		TextIO.print "verifyBinaryExpr incomplete: "; 
		Pretty.ppExpr (BinaryExpr (bop,lhs,rhs)); 
		raise Match
	    end
    end

and verifyBinaryTypeExpr (ctxt:CONTEXT) 
			 (bop:BINTYPEOP, arg:EXPR, t:TYPE_EXPR) =
    let val argType = verifyExpr ctxt arg	
    in
	verifyTypeExpr ctxt t;
	case bop of
	    Cast =>
	    let in
		(* TODO: check *)
		(* checkConvertible argType t; *)
		t
	    end
	  | To =>
	    let in
		(* TODO: check *)
		checkConvertible argType t;
		t
	    end
	  | Is =>
	    let in
		(* TODO: check *)
		checkConvertible argType t;
		boolType
	    end
    end


and verifyTrinaryExpr (ctxt:CONTEXT) (triop:TRIOP, a:EXPR, b:EXPR, c:EXPR) =
    case triop of
	Cond =>
	let val aty = verifyExpr ctxt a
	    val bty = verifyExpr ctxt b
	    val cty = verifyExpr ctxt c
	in
	    checkConvertible aty boolType;
	    (*FIXME*)
	    checkConvertible bty cty;
	    cty
	end

(****************************** Verifying Statements ********************************)

and verifyStmts ctxt ss = List.app (fn s => verifyStmt ctxt s) ss

and verifyStmt (ctxt as {this,env,lbls,retTy}:CONTEXT) (stmt:STMT) =
   let
   in
       TextIO.print ("type checking stmt: env len " ^ (Int.toString (List.length env)) ^"\n");
       Pretty.ppStmt stmt;
       TextIO.print "\n";
   case stmt of
    EmptyStmt => ()
  | ExprStmt e => (verifyExpr ctxt e; ())
  | IfStmt {cnd,thn,els} => 
    let in
	checkCompatible (verifyExpr ctxt cnd) boolType;
	verifyStmt ctxt thn;
	verifyStmt ctxt els
    end

  | (DoWhileStmt {cond,body,contLabel} | WhileStmt {cond,body,contLabel}) => 
    let in
	checkCompatible (verifyExpr ctxt cond) boolType;
	verifyStmt (withLbls (ctxt, contLabel::lbls)) body
    end

  | ReturnStmt e => 
    let in
	case retTy of
	    NONE => raise VerifyError "return not allowed here"
          | SOME retTy => checkCompatible (verifyExpr ctxt e) retTy
    end

  | (BreakStmt NONE | ContinueStmt NONE) =>  
    let in
	case lbls of
	    [] => raise VerifyError "Not in a loop"
	  | _ => ()
    end

  | (BreakStmt (SOME lbl) | ContinueStmt (SOME lbl)) => 
    let in
	if List.exists (fn x => x=(SOME lbl)) lbls	
	then ()
	else raise VerifyError "No such label"
    end

  | BlockStmt b => verifyBlock ctxt b

  | LabeledStmt (lab, s) => 
	verifyStmt (withLbls (ctxt, ((SOME lab)::lbls))) s
 
  | ThrowStmt t => 
	checkCompatible (verifyExpr ctxt t) exceptionType

  | LetStmt (defns, body) =>
    let val extensions = List.concat (List.map (fn d => verifyBinding ctxt d) defns)
    in
        checkForDuplicateExtension extensions;
        verifyStmt (withEnv (ctxt, foldl extendEnv env extensions)) body  
    end

  | ForStmt { defns, init, cond, update, contLabel, body, fixtures } =>
    let val extensions = verifyVarBindings ctxt defns
        val ctxt' = withEnv (ctxt, foldl extendEnv env extensions)
(* NOT USED 
	fun verifyExprs exprs = 
	    let in
		if List.length exprs = 0
		then boolType
		else List.last (List.map (fn e => verifyExpr ctxt' e) exprs)
	    end
*)
    in
  	verifyExpr ctxt' init;
	checkCompatible (verifyExpr ctxt' cond) boolType;
	verifyExpr ctxt' update;
	verifyStmt (withLbls (ctxt', contLabel::lbls)) body
    end

(* Wait on refactoring AST
  | SwitchStmt { cond, cases } =>
    let val ty = verifyExpr ctxt cond
    in
	List.app
	    (fn (expr,stmts) =>
		let in
		    verifyStmts ctxt stmts;
		    checkBicompatable ty (verifyExpr ctxt expr)
		end)
	    cases
    end
*)

    
  | _ => (TextIO.print "verifyStmt incomplete: "; Pretty.ppStmt stmt; raise Match)

(*
       | ForEachStmt of FOR_ENUM_STMT
       | ForInStmt of FOR_ENUM_STMT
       | SuperStmt of EXPR list

       | WithStmt of { obj: EXPR,
                       body: STMT }

       | TryStmt of { body: BLOCK,
                      catches: (FORMAL * BLOCK) list,
                      finally: BLOCK }
*)

   end

(***************************** Verifying Definitions ****************************************)

and verifyDefn (ctxt as {this,env,lbls,retTy}:CONTEXT) (d:DEFN) : (TYPE_ENV * int list) =
    let
    in
	case d of
	    VariableDefn {bindings,...} => 
	    (List.concat (List.map (verifyVarBinding ctxt) bindings), []) 
	  | FunctionDefn { kind, func,... } =>
	    let val Func {name,fsig,body,fixtures,...} = func
		val FunctionSignature { typeParams, params, inits, 
					returnType, thisType, hasRest } 
		  = fsig
		val { kind, ident } = name
		val ctxt3 = verifyFunctionSignature ctxt fsig
	    in
		verifyBlock ctxt3 body;
		([(ident, SOME (FunctionType fsig))],[])
            end
	    
	  | InterfaceDefn {ident,ns,nonnullable,params,extends,body} =>
	    let val nuEnv = extendEnvWithTypeVars params env
	    in
		verifyBlock (withEnv (ctxt,nuEnv)) body;
		List.app
		(fn (superi:IDENT_EXPR) =>
		    (* need to check that this interface implements _all_ methods of superinterface ident *)
		    ()
		)
		extends;
		([],[])
	    end
		


(*
and INTERFACE_DEFN =
         { ident: IDENT,
           ns: EXPR,
           nonnullable: bool,
           params: IDENT list,
           extends: IDENT_EXPR list,
           body: BLOCK }
     *)
	    
	  | d => (TextIO.print "verifyDefn incomplete: "; Pretty.ppDefinition d; raise Match)
    end


and verifyDefns ctxt ([]:DEFN list) : (TYPE_ENV * int list) = ([], [])
  | verifyDefns ctxt ((d::ds):DEFN list) : (TYPE_ENV * int list) =
        let val (extensions1, classes1) = verifyDefn ctxt d
            val (extensions2, classes2) = verifyDefns ctxt ds
        in
            (extensions1 @ extensions2, classes1 @ classes2)
        end

and verifyBlock (ctxt as {env,...}) (Block {pragmas=pragmas,defns=defns,stmts=stmts,fixtures,...}) =
    let val (extensions, classes) = verifyDefns ctxt defns
        val ctxt' = withEnv (ctxt, foldl extendEnv env extensions)
    in
        assert (classes = []) "class definition inside block";
		verifyStmts ctxt stmts
    end

fun verifyProgram { packages, fixtures, body } = 
    (verifyBlock {this=anyType, env=[], lbls=[], retTy=NONE} body; true)

(* CF: Let this propagate to top-level to see full trace
    handle VerifyError msg => 
	   let in
     	       TextIO.print "Ill typed exception: "; 
     	       TextIO.print msg; 
     	       TextIO.print "\n"; 
     	       false
	   end

*)   
				    

end
