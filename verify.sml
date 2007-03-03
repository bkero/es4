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

(****************************** misc auxiliary functions *************************)

fun assert b s = if b then () else (raise Fail s)

fun checkForDuplicates [] = ()
  | checkForDuplicates (x::xs) =
    if List.exists (fn y => x = y) xs
    then raise VerifyError "concurrent definition"
    else checkForDuplicates xs
      
fun unOptionDefault NONE def = def
  | unOptionDefault (SOME v) _ = v

fun flattenOptionList NONE = []
  | flattenOptionList (SOME l) = l

(****************************** standard types *************************)

(* TODO: what is the proper way to resolve these built-ins? *)
fun simpleIdent (s:string) : IDENT_EXPR 
  = Identifier { ident=s, openNamespaces=[[Public ""]] }
    
val boolType      = TypeName (simpleIdent "boolean")
val numberType    = TypeName (simpleIdent "number")
val decimalType   = TypeName (simpleIdent "decimal")
val intType       = TypeName (simpleIdent "int")
val uintType      = TypeName (simpleIdent "uint")
val stringType    = TypeName (simpleIdent "string")
val regexpType    = TypeName (simpleIdent "regexp")
val exceptionType = TypeName (simpleIdent "exception")
val namespaceType = TypeName (simpleIdent "Namespace")
val typeType      = TypeName (Identifier { ident="Type", openNamespaces=[[Intrinsic]]})
val undefinedType = SpecialType Undefined
val nullType      = SpecialType Null
val anyType       = SpecialType Any

(****************************** type environments *************************)

(* type env has program variables, with types, axnd type variables, with no types *)
(* TODO: do we need to consider namespaces here ??? *)

type ID = FIXTURE_NAME

datatype KIND =
	 TypeVar
       | ProgVar of TYPE_EXPR * bool
		    
type TYPE_ENV = (ID * KIND) list
		
fun checkForDuplicateExtension extensions =
    let val (names, _) = ListPair.unzip extensions
    in
      checkForDuplicates names
end

fun extendEnv (env:TYPE_ENV) (id:ID) (k:KIND) : TYPE_ENV = (id,k)::env
fun extendEnvWithTypeVars (params:ID list) (env:TYPE_ENV):TYPE_ENV =
    let in
	checkForDuplicates params;
	foldl (fn (p,env) => extendEnv env p TypeVar) env params
    end
fun extendEnvs (env:TYPE_ENV) (ext:TYPE_ENV) =
    let in
	ext @ env
    end
fun fixtureNameToString (TempName i) = "TempName" ^ (Int.toString(i))
  | fixtureNameToString (PropName { ns, id }) = id

fun lookupIdNamespace (env:TYPE_ENV) 
		      (id:IDENT) 
		      (ns:NAMESPACE)
    : KIND option =
    case List.find (fn (i,_) => i=PropName {id=id,ns=ns}) env of
	NONE => NONE
      | SOME (_,k) => SOME k

fun lookupIdNamespaces (env:TYPE_ENV) 
		       (id:IDENT) 
		       (nss : NAMESPACE list)
    : KIND option =
    let val theMatches
	  = List.mapPartial (lookupIdNamespace env id) nss
    in
	case theMatches of
	    [] => NONE
	  | [k] => SOME k
	  | _ =>
	    raise VerifyError ("Ambiguous reference " ^ id)
    end

fun lookupIdNamespacess (env:TYPE_ENV) 
			(id:IDENT) 
			(nsss : NAMESPACE list list)
    : KIND =
    let in
	case nsss of
	    [] =>
	    raise VerifyError ("Unbound variable " ^ id)
	  | nss::nsss =>
	    case lookupIdNamespaces env id nss of
		SOME k => k
	      | NONE => lookupIdNamespacess env id nsss
    end

(******************************** Contexts *********************************)

type CONTEXT = {this: TYPE_EXPR, env: TYPE_ENV, lbls: IDENT option list, retTy: TYPE_EXPR option}

fun withThis  {this=_,    env=env, lbls=lbls, retTy=retTy} this 
    = {this=this, env=env, lbls=lbls, retTy=retTy}

fun withEnv   {this=this, env=_,   lbls=lbls, retTy=retTy} env
    = {this=this, env=env, lbls=lbls, retTy=retTy}

fun withLbls  {this=this, env=env, lbls=_,    retTy=retTy} lbls 
    = {this=this, env=env, lbls=lbls, retTy=retTy}

fun withRetTy {this=this, env=env, lbls=lbls, retTy=_} retTy 
    = {this=this, env=env, lbls=lbls, retTy=retTy}

fun withEnvExtn {this, env, lbls, retTy} extn 
    = {this=this, env=extendEnvs env extn, lbls=lbls, retTy=retTy}

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

	  | TypeName ((Identifier {ident=ident, openNamespaces=_})) =>
	    let in
		case List.find 
			 (fn (id,ty) => id=ident)
			 s 
		 of
		    (* TODO: we're dropping the nullable here, is that right? *)
		    SOME (_,ty) => ty 
		  | NONE => t
	    end
	  | TypeName _ => t

	  | ObjectType fields =>
	    ObjectType (map (fn {name,ty} => 
				{name=name,ty=substTypeExpr s ty})
			fields)
	  | SpecialType st => SpecialType st

	  | FunctionType { typeParams, params, result, thisType, hasRest, minArgs } =>
	    (* Need to uniquify typeParams to avoid capture *)
	    let val oldNew = map (fn id => (id, gensym id)) typeParams 
		val nuSub = 
		    map 
			(fn (oldId,newId) => 
			     (oldId, 
			      TypeName (Identifier {ident=newId,openNamespaces=[]})))
			oldNew
		val bothSubs = fn t => substTypeExpr s (substTypeExpr nuSub t)
		val nuTypeParams = map (fn (oldId,newId) => newId) oldNew
		(* val nuParams = map (fn t => substVarBinding s (substVarBinding nuSub t)) params *)
		in
		FunctionType {typeParams=nuTypeParams,
			      params = List.map bothSubs params,
			      result = bothSubs result,      
			      thisType = Option.map bothSubs thisType,
			      hasRest=hasRest, minArgs = minArgs }
		end
    end 

(* deprecated by fixtures 
and substVarBinding (s:(IDENT*TYPE_EXPR) list) (Binding {init,pattern,ty}:VAR_BINDING):VAR_BINDING = 
    Binding {init=init,pattern=pattern, ty=Option.map (substTypeExpr s) ty}
*)

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
	     TypeName (Identifier {ident="Array", openNamespaces=[]})) 
	    => true

	  | (ArrayType _, 
	     TypeName (Identifier {ident="Object", openNamespaces=[]})) 
	    => true

	  | (FunctionType _, 
	     TypeName (Identifier {ident="Function", openNamespaces=[]})) 
	    => true

	  | (FunctionType _, 
	     TypeName (Identifier {ident="Object", openNamespaces=[]}))
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
		 {typeParams=typeParams1,
		  params  =params1, 
		  result  =result1,
		  thisType=thisType1,
		  hasRest =hasRest1,
	      minArgs=minArgs},
	     FunctionType 
		 {typeParams=typeParams2,
		  params=params2, 
		  result=result2, 
		  thisType=thisType2,
		  hasRest=hasRest2,
		  minArgs=minArgs2}) =>
	    let
	    in
		(* TODO: Assume for now that functions are not polymorphic *)
		assert (typeParams1 = [] andalso typeParams2=[]) "cannot handle polymorphic fns";
		assert (not hasRest1 andalso not hasRest2) "cannot handle rest args";

		ListPair.all (fn (t1,t2) => isCompatible t1 t2) (params1,params2)
		andalso
		isCompatible result1 result2
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
	    AppType {base=FunctionType {typeParams,params,result,thisType,hasRest,minArgs}, 
		     args } =>
	    let val _ =	assert (length args = length typeParams);
		val sub = ListPair.zip (typeParams,args)
		fun applySub t = substTypeExpr sub t
	    in
		FunctionType { typeParams=[],
			       params=map (substTypeExpr sub) params,
			       result=substTypeExpr sub result,
			       thisType= Option.map (substTypeExpr sub) thisType,
			       hasRest=hasRest, minArgs=minArgs }
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
      TextIO.print ("type checking a type");
      Pretty.ppType ty;
      TextIO.print "\n";
   
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
	  | FunctionType ftype =>
	    let in
		verifyFunctionType ctxt ftype;
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
(*
          | NominalType { ident } =>
			   (* TODO *)
			   ()
*)   	    
    end

and verifyFunctionType  (ctxt:CONTEXT)
			 {typeParams, params, result, thisType, hasRest, minArgs} 
    : CONTEXT = ctxt

(*
    let (* val FunctionType= t;
	(* Add the type parameters to the environment. *)
	val ctxt1 : CONTEXT = withEnvExtn ctxt (List.map (fn id => (id,TypeVar)) typeParams);
	(* Add the return type to the context. *)
	val ctxt2 : CONTEXT = withRetTy ctxt1 (SOME result)
    *)in (*
	List.app (verifyTypeExpr ctxt1) params;
	verifyTypeExpr ctxt1 result;
	(case thisType of
	     NONE => ()
	   | SOME t => verifyTypeExpr ctxt1 t);
	*) ctxt
    end
*)

and verifyTypeExprs  (ctxt as {env,this,...}:CONTEXT) 
		     (tys:TYPE_EXPR list) 
    : unit = 
    (List.app (verifyTypeExpr ctxt) tys)

(******************** Expressions **************************************************)

and verifyIdentExpr (ctxt as {env,this,...}:CONTEXT) 
		    (ide:IDENT_EXPR) 
    : TYPE_EXPR = 
    let
    in 
	case ide of
	    QualifiedIdentifier { qual, ident } =>
	    let in
		checkCompatible (verifyExpr ctxt qual) namespaceType;
		anyType
	    end	    
	  | QualifiedExpression { qual, expr } =>
	    let in
		checkCompatible (verifyExpr ctxt qual) namespaceType;
		checkCompatible (verifyExpr ctxt qual) stringType;
		anyType
	    end
	  | AttributeIdentifier idexpr =>
	    let in
		verifyIdentExpr ctxt idexpr;
		anyType
	    end
	  | Identifier { ident, openNamespaces } =>
	    let val k : KIND = lookupIdNamespacess env ident openNamespaces
	    in
		case k of
		    TypeVar => raise VerifyError ("Attempt to refer to type variable "
						      ^ ident
						  ^ " as a program variable")
		  | ProgVar (ty,read_only) => ty
	    end
    end

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
      | LiteralExpr (LiteralFunction (Func { param=(fixtures,inits), block, ty, ... }))
	=>
	 let
	     val ctxt1 = verifyFunctionType ctxt ty 
	     val extensions = verifyFixtures ctxt1 fixtures
	     val ctxt2 = withEnvExtn ctxt1 extensions
	 in
	     checkForDuplicateExtension extensions;
(** FIXME: inits are now settings and are BINDINGS 
	     verifyStmts ctxt2 inits;
*)
	     verifyBlock ctxt2 block;
	     Ast.FunctionType ty
         end
      | LexicalRef { ident } =>
	verifyIdentExpr ctxt ident
      | ListExpr l => List.last (List.map (verifyExpr ctxt) l)
      | LetExpr {defs=_, body, head=SOME (fixtures,inits) } =>  (* FIXME: inits added *)
          let val extensions = verifyFixtures ctxt fixtures
          in
	    checkForDuplicateExtension extensions;
	    verifyExpr (withEnvExtn ctxt extensions) body
	  end
       | ThisExpr => this
       | UnaryExpr (unop, arg) => verifyUnaryExpr ctxt unop arg
       | BinaryExpr (binop, lhs, rhs ) => verifyBinaryExpr ctxt (binop, lhs, rhs)
       | BinaryTypeExpr (binop, lhs, rhs ) => verifyBinaryTypeExpr ctxt (binop, lhs, rhs)
       | TrinaryExpr (triop, a,b,c ) => verifyTrinaryExpr ctxt (triop, a,b,c)

       | CallExpr { func, actuals } => verifyCallExpr ctxt func actuals
       | ApplyTypeExpr {expr, actuals} =>
	 (* Can only instantiate Functions, classes, and interfaces *)
	 let val exprTy = verifyExpr ctxt expr;
	     val typeParams = 
		 case exprTy of
		     FunctionType {typeParams, ...} => typeParams
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
	 let in
	     verifyTypeExpr ctxt ty;
	     typeType
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
       | NewExpr of { obj: EXPR,x
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
	 | FunctionType { typeParams, params, result, thisType, hasRest, minArgs }
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
		       let in
			   checkCompatible a p;
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
		   result
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
    in 	
	List.last (List.map (verifyExpr ctxt) l)
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

(* deprecated due to fixtures
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

  | (DoWhileStmt {cond,body,contLabel,fixtures} | WhileStmt {cond,body,contLabel,fixtures}) => 
    let in
	checkCompatible (verifyExpr ctxt cond) boolType;
	verifyStmt (withLbls ctxt (contLabel::lbls)) body
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
	verifyStmt (withLbls ctxt ((SOME lab)::lbls)) s
 
  | ThrowStmt t => 
	checkCompatible (verifyExpr ctxt t) exceptionType

(* deprecated due to fixtures 
  | LetStmt (defns, body) =>
    let val extensions = List.concat (List.map (fn d => verifyBinding ctxt d) defns)
    in
        checkForDuplicateExtension extensions;
        verifyStmt (withEnv (ctxt, foldl extendEnv env extensions)) body  
    end
*)

  | ForStmt { defn=_, fixtures, init, cond, update, contLabel, body } =>
    let val extensions = verifyFixturesOption ctxt fixtures
        val ctxt' = withEnvExtn ctxt extensions
(* NOT USED 
	fun verifyExprs exprs = 
	    let in
		if List.length exprs = 0
		then boolType
		else List.last (List.map (fn e => verifyExpr ctxt' e) exprs)
	    end
*)
    in
  	verifyStmts ctxt' init;
	checkCompatible (verifyExpr ctxt' cond) boolType;
	verifyExpr ctxt' update;
	verifyStmt (withLbls ctxt' (contLabel::lbls)) body
    end

  | SwitchStmt { cond, cases } =>  
    let val ty = verifyExpr ctxt cond
    in
	List.app
	    (fn {label,body,inits} =>	(* FIXME: verify inits *)
		let in
		    (Option.app 
			 (fn e => checkBicompatible ty (verifyExpr ctxt e))
			 label);
		    verifyBlock ctxt body
		end)
	    cases
    end

  | TryStmt { block, catches, finally } =>
    let 
    in
	verifyBlock ctxt block;
	case finally of
	    NONE => ()
	  | SOME block => verifyBlock ctxt block;
	List.app
	(fn {bindings, ty, fixtures, block} =>
	    ())
	catches
    end

(* TODO: InitStmt for function defn test *)
    
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


(*
and verifyDefn (ctxt as {this,env,lbls,retTy}:CONTEXT) (d:DEFN) : (TYPE_ENV * int list) =
    let
    in
	case d of
(* FIXME
	    VariableDefn {bindings,...} => 
	    (List.concat (List.map (verifyVarBinding ctxt) bindings), []) 
	  | 
*)
	    FunctionDefn { kind, func,... } =>
	    let val Func {name, fsig, fixtures, inits, body } = func
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
*)

(******************** Fixtures **************************************************)

(* fixtures at the block level *)
and verifyFixture (ctxt as {env,this,...}:CONTEXT) 
		  (n:FIXTURE_NAME,f:FIXTURE) 
    : TYPE_ENV
  = let in
	case f of
	    NamespaceFixture _ => []
	  | ValFixture { ty, readOnly, ... } => 
	    [(n,ProgVar (ty,readOnly))]
    end	    

and verifyFixtures (ctxt:CONTEXT) 
		   (fs:FIXTURES)  
    : TYPE_ENV 
  = List.concat (List.map (verifyFixture ctxt) fs)

and verifyFixturesOption (ctxt:CONTEXT) 
		   (fs:FIXTURES option)  
    : TYPE_ENV
  = verifyFixtures ctxt (flattenOptionList fs)


(******************** Blocks **************************************************)

and verifyBlock (ctxt as {env,...}) (Block {pragmas,defns=_,body,head=SOME (fixtures,inits)}) =
    let val extensions = verifyFixtures ctxt fixtures
        val ctxt' = withEnvExtn ctxt extensions
    in
	verifyStmts ctxt' body
    end

fun verifyProgram { packages, fixtures, block } = 
    (verifyBlock {this=anyType, env=[], lbls=[], retTy=NONE} block; true)

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
