(* -*- mode: sml; mode: font-lock; tab-width: 4; insert-tabs-mode: nil; indent-tabs-mode: nil -*- *)
structure Verify = struct

open LogErr

type CONTEXT = Ast.FIXTURES

type ENV = { returnType: Ast.TYPE_EXPR option,
             context: CONTEXT }

fun withReturnType { returnType=_, context } returnType =
    { returnType=returnType, context=context }

fun withContext { returnType, context=_ } context =
    { returnType=returnType, context=context }

(* Local tracing machinery *)

val doTrace = ref false
fun trace ss = if (!doTrace) then LogErr.log ("[verify] " :: ss) else ()
fun error ss = LogErr.verifyError ss

(****************************** standard types *************************)

(* TODO: what is the proper way to resolve these built-ins? *)
(* FIXME: change Ast to have a variant of TypeName(?) that should be looked up in the global class table *)
fun simpleIdent (s:string) : Ast.IDENT_EXPR 
  = Ast.Identifier { ident=s, openNamespaces=[[Name.internalNS, Name.publicNS]] }

val boolType      = Ast.TypeName (simpleIdent "boolean")
val numberType    = Ast.TypeName (simpleIdent "number")
val doubleType    = Ast.TypeName (simpleIdent "double")
val decimalType   = Ast.TypeName (simpleIdent "decimal")
val intType       = Ast.TypeName (simpleIdent "int")
val uintType      = Ast.TypeName (simpleIdent "uint")
val stringType    = Ast.TypeName (simpleIdent "string")
val regexpType    = Ast.TypeName (simpleIdent "regexp")
val exceptionType = Ast.TypeName (simpleIdent "exception")
val namespaceType = Ast.TypeName (simpleIdent "Namespace")
val typeType      = Ast.TypeName (Ast.Identifier { ident="Type", openNamespaces=[[Name.intrinsicNS]]})
val undefinedType = Ast.SpecialType Ast.Undefined
val nullType      = Ast.SpecialType Ast.Null
val anyType       = Ast.SpecialType Ast.Any

(****************************** misc auxiliary functions *************************)

fun assert b s = if b then () else (raise Fail s)

fun checkForDuplicates [] = ()
  | checkForDuplicates (x::xs) =
    if List.exists (fn y => x = y) xs
    then error ["concurrent definition"]
    else checkForDuplicates xs
      
fun unOptionDefault NONE def = def
  | unOptionDefault (SOME v) _ = v

fun flattenOptionList NONE = []
  | flattenOptionList (SOME l) = l

val gensymCounter : int ref = ref 0
fun gensym (s) = 
    let
    in 
	gensymCounter := 1+(!gensymCounter);
	s^"$"^(Int.toString (!gensymCounter))
    end


(************************* Compatibility *********************************)

fun checkCompatible (t1:Ast.TYPE_EXPR) 
		    (t2:Ast.TYPE_EXPR) 
    : unit = 
    if isCompatible t1 t2
    then ()
    else let in
	     TextIO.print ("Types are not compatible\n");
	     Pretty.ppType t1;
	     Pretty.ppType t2;
	     verifyError ["Types are not compatible"]
	 end

and isCompatible (t1:Ast.TYPE_EXPR) 
		 (t2:Ast.TYPE_EXPR) 
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
	    (Ast.UnionType types1,_) => 
	    List.all (fn t => isCompatible t t2) types1
	  | (_, Ast.UnionType types2) =>
	    (* t1 must exist in types2 *)
	    List.exists (fn t => isCompatible t1 t) types2 
	  | (Ast.ArrayType types1, Ast.ArrayType types2) => 
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

	  | (Ast.ArrayType _, 
	     Ast.TypeName (Ast.Identifier {ident="Array", openNamespaces=[]})) 
	    => true

	  | (Ast.ArrayType _, 
	     Ast.TypeName (Ast.Identifier {ident="Object", openNamespaces=[]})) 
	    => true

	  | (Ast.FunctionType _, 
	     Ast.TypeName (Ast.Identifier {ident="Function", openNamespaces=[]})) 
	    => true

	  | (Ast.FunctionType _, 
	     Ast.TypeName (Ast.Identifier {ident="Object", openNamespaces=[]}))
	    => true

	  | (Ast.AppType {base=base1,args=args1}, Ast.AppType {base=base2,args=args2}) => 
	    (* We keep types normalized wrt beta-reduction, 
	     * so base1 and base2 must be class or interface types.
	     * Type arguments are covariant, and so must be intra-compatible - CHECK 
	     *)
	    false
	    
	  | (Ast.ObjectType fields1, Ast.ObjectType fields2) =>
	    false

	  | (Ast.FunctionType 
		 {typeParams=typeParams1,
		  params  =params1, 
		  result  =result1,
		  thisType=thisType1,
		  hasRest =hasRest1,
	      minArgs=minArgs},
	     Ast.FunctionType 
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

	  | (Ast.TypeName (Ast.Identifier {ident=i1, openNamespaces=_}),
	     Ast.TypeName (Ast.Identifier {ident=i2, openNamespaces=_})) => false
(*
	    let val fix1 = Defn.resolveMultinameToFixture 
	    (i1=i2)
*)	    
	  (* catch all *)
	  | _ => unimplError ["isCompatible"]
    end
    
(*
    and Ast.TYPE_EXPR =
         Ast.SpecialType of SPECIAL_TY
       | Ast.NominalType of { ident : Ast.IDENT_EXPR }
       | Ast.NullableType of {expr:Ast.TYPE_EXPR,nullable:bool}
*)

fun checkBicompatible (ty1:Ast.TYPE_EXPR) 
		      (ty2:Ast.TYPE_EXPR)
    : unit = 
    let in
	checkCompatible ty1 ty2;
	checkCompatible ty2 ty1
    end
    
fun checkConvertible (ty1:Ast.TYPE_EXPR) (ty2:Ast.TYPE_EXPR)
    : unit =
    (* TODO: int to float, etc, and to() methods *)
    checkCompatible ty1 ty2


(************************* Substitution on Types *********************************)

fun substTypeExpr (s:(Ast.IDENT*Ast.TYPE_EXPR) list) (t:Ast.TYPE_EXPR):Ast.TYPE_EXPR = 
    let in
	case t of
	    Ast.UnionType ts => 
	    Ast.UnionType (map (substTypeExpr s) ts)
	  | Ast.ArrayType ts => 
	    Ast.ArrayType (map (substTypeExpr s) ts)
	  | Ast.AppType {base, args} => 
	    Ast.AppType {base=substTypeExpr s base, args=map (substTypeExpr s) args}
	  | Ast.NullableType {expr, nullable}
	    => Ast.NullableType {expr=substTypeExpr s expr, nullable=nullable}

	  | Ast.TypeName ((Ast.Identifier {ident=ident, openNamespaces=_})) =>
	    let in
		case List.find 
			 (fn (id,ty) => id=ident)
			 s 
		 of
		    (* TODO: we're dropping the nullable here, is that right? *)
		    SOME (_,ty) => ty 
		  | NONE => t
	    end
	  | Ast.TypeName _ => t

	  | Ast.ObjectType fields =>
	    Ast.ObjectType (map (fn {name,ty} => 
				{name=name,ty=substTypeExpr s ty})
			fields)
	  | Ast.SpecialType st => Ast.SpecialType st

	  | Ast.FunctionType { typeParams, params, result, thisType, hasRest, minArgs } =>
	    (* Need to uniquify typeParams to avoid capture *)
	    let val oldNew = map (fn id => (id, gensym id)) typeParams 
		val nuSub = 
		    map 
			(fn (oldId,newId) => 
			     (oldId, 
			      Ast.TypeName (Ast.Identifier {ident=newId,openNamespaces=[]})))
			oldNew
		val bothSubs = fn t => substTypeExpr s (substTypeExpr nuSub t)
		val nuTypeParams = map (fn (oldId,newId) => newId) oldNew
		(* val nuParams = map (fn t => substVarBinding s (substVarBinding nuSub t)) params *)
		in
		Ast.FunctionType {typeParams=nuTypeParams,
			      params = List.map bothSubs params,
			      result = bothSubs result,      
			      thisType = Option.map bothSubs thisType,
			      hasRest=hasRest, minArgs = minArgs }
		end
    end 

(************************* Handling Types *********************************)

fun mergeTypes t1 t2 =
	t1

fun normalizeType (t:Ast.TYPE_EXPR)
    : Ast.TYPE_EXPR =
    let in
	case t of
	    Ast.AppType {base=Ast.FunctionType {typeParams,params,result,thisType,hasRest,minArgs}, 
		     args } =>
	    let val _ =	assert (length args = length typeParams);
		val sub = ListPair.zip (typeParams,args)
		fun applySub t = substTypeExpr sub t
	    in
		Ast.FunctionType { typeParams=[],
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

(*
fun verifyTypeExpr (ctxt as {env,this,...}:CONTEXT)
		   (ty:Ast.TYPE_EXPR) 
    : unit = 
    let
    in 
      TextIO.print ("type checking a type");
      Pretty.ppType ty;
      TextIO.print "\n";
   
	case ty of
	    Ast.SpecialType _ => ()
	  | Ast.UnionType tys => verifyTypeExprs ctxt tys
	  | Ast.ArrayType tys => verifyTypeExprs ctxt tys
	  | Ast.NullableType {expr=ty,nullable} => verifyTypeExpr ctxt ty
	  | Ast.AppType {base,args} =>
	    let in
		verifyTypeExpr ctxt base;
		verifyTypeExprs ctxt args
	    (*TODO: check # args is correct *)
	    end
	  | Ast.FunctionType ftype =>
	    let in
		verifyFunctionType ctxt ftype;
		()
	    end
	  | Ast.ObjectType fields => 
	    let val names: Ast.IDENT list =
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
    let (* val Ast.FunctionType= t;
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
		     (tys:Ast.TYPE_EXPR list) 
    : unit = 
    (List.app (verifyTypeExpr ctxt) tys)
*)

(******************** Verification **************************************************)

(* 
    Verify a program
 *)

(*
    HEAD
*)

and verifyHead (env:ENV) (h:Ast.HEAD)
    : Ast.HEAD = h

(*
    EXPR
*)

and verifyExpr (env:ENV) 
               (expr:Ast.EXPR) 
    : (Ast.EXPR * Ast.TYPE_EXPR) = 
    let
        fun verifySub e = 
            let
                val (e,t) = verifyExpr env e
            in
                e
            end
        val dummyType = Ast.SpecialType Ast.Any
    in
        case expr of 
            Ast.TernaryExpr (t, e1, e2, e3) => 
            (Ast.TernaryExpr (t, e1, e2, e3), dummyType)
            
          | Ast.BinaryExpr (b, e1, e2) => 
            (Ast.BinaryExpr (b, e1, e2), dummyType)
            
          | Ast.BinaryTypeExpr (b, e, te) =>
            (Ast.BinaryTypeExpr (b, e, te), dummyType)

          | Ast.UnaryExpr (u, e) =>
            (Ast.UnaryExpr (u, e), dummyType)
                         
          | Ast.TypeExpr t => 
            (Ast.TypeExpr t, dummyType)

          | Ast.ThisExpr => 
            (Ast.ThisExpr, dummyType)

          | Ast.YieldExpr eo => 
            (Ast.YieldExpr eo, dummyType)

          | Ast.SuperExpr eo => 
            (Ast.SuperExpr eo, dummyType)
            
          | Ast.LiteralExpr le => 
            (Ast.LiteralExpr le, dummyType)
            
          | Ast.CallExpr {func, actuals} => 
            (Ast.CallExpr {func = verifySub func,
                          actuals = map verifySub actuals}, dummyType)

          | Ast.ApplyTypeExpr { expr, actuals } =>
            (Ast.ApplyTypeExpr { expr = expr,
                                actuals = actuals }, dummyType)

          | Ast.LetExpr le => 
            (Ast.LetExpr le, dummyType)

          | Ast.NewExpr { obj, actuals } => 
            (Ast.NewExpr { obj = verifySub obj,
                          actuals = map verifySub actuals }, dummyType)

          | Ast.ObjectRef { base, ident, pos } =>
            (Ast.ObjectRef { base=base, ident=ident, pos=pos }, dummyType)

          | Ast.LexicalRef { ident, pos } => 
            (Ast.LexicalRef { ident=ident, pos=pos }, dummyType)

          | Ast.SetExpr (a, le, re) => 
            (Ast.SetExpr (a, le, re), dummyType)

          | Ast.GetTemp n => 
            (Ast.GetTemp n, dummyType)

          | Ast.GetParam n => 
            (Ast.GetParam n, dummyType)

          | Ast.ListExpr es => 
            (Ast.ListExpr (map verifySub es), dummyType)

          | Ast.SliceExpr (a, b, c) => 
            (Ast.SliceExpr (verifySub a, verifySub b, verifySub c), dummyType)

          | Ast.InitExpr ie =>
            (Ast.InitExpr ie, dummyType)
    end
    
    
and verifyExprs (env:ENV) 
             (exprs:Ast.EXPR list) 
    : Ast.EXPR list * Ast.TYPE_EXPR list = 
    let
        val es = ListPair.unzip (map (verifyExpr env) exprs)
    in
        es
    end





(*
    STMT
*)

and verifyStmt (env:ENV) 
            (stmt:Ast.STMT) 
    : Ast.STMT = 
    let
    in
        case stmt of
            Ast.EmptyStmt => 
            Ast.EmptyStmt
         
          | Ast.ExprStmt e => 
            let
                val (expr,ty) = verifyExpr env e
            in
                Ast.ExprStmt expr
            end

          | Ast.ForInStmt fe => 
            Ast.ForInStmt fe
            
          | Ast.ThrowStmt es =>
            Ast.ThrowStmt es
            
          | Ast.ReturnStmt es =>
            Ast.ReturnStmt es
            
          | Ast.BreakStmt i => 
            Ast.BreakStmt i

          | Ast.ContinueStmt i =>
            Ast.ContinueStmt i

          | Ast.BlockStmt b =>
            Ast.BlockStmt b 
            
          | Ast.ClassBlock cb =>
            Ast.ClassBlock cb
            
          | Ast.LabeledStmt (id, s) =>
            Ast.LabeledStmt (id, s)
            
          | Ast.LetStmt b =>
            Ast.LetStmt b
            
          | Ast.WhileStmt w => 
            Ast.WhileStmt w
            
          | Ast.DoWhileStmt w => 
            Ast.DoWhileStmt w
            
          | Ast.ForStmt f => 
            Ast.ForStmt f

          | Ast.IfStmt {cnd, els, thn} => 
            Ast.IfStmt {cnd=cnd, els=els, thn=thn}
            
          | Ast.WithStmt {obj, ty, body} =>
            Ast.WithStmt {obj=obj, ty=ty, body=body}
        
          | Ast.TryStmt {block, catches, finally} =>
            Ast.TryStmt {block=block, catches=catches, finally=finally}
        
          | Ast.SwitchStmt {cond, cases, mode, labels} => 
            Ast.SwitchStmt {cond=cond, cases=cases, mode=mode, labels=labels}
        
          | Ast.SwitchTypeStmt {cond, ty, cases} =>
            Ast.SwitchTypeStmt {cond=cond, ty=ty, cases=cases}
            
          | Ast.Dxns x => 
            Ast.Dxns x

          | _ => error ["Shouldn't happen: failed to match in Verify.verifyStmt"]

    end

and verifyStmts (env) (stmts:Ast.STMT list)
    : Ast.STMT list =
    case stmts of
        (stmt::stmts) =>
            let 
                val s = verifyStmt env stmt
                val ss = verifyStmts env stmts
            in
                s::ss
            end
      | [] => []

(*
    BLOCK
*)

and verifyBlock (env:ENV) 
             (b:Ast.BLOCK) 
    : Ast.BLOCK =
    let
    in case b of
        Ast.Block { head, body, pos, pragmas=[], defns=[] } =>
            let
                val _ = LogErr.setPos pos
                val head = case head of SOME h => verifyHead env h 
                                      | _ => ([],[])
                val body = verifyStmts env body
            in
                Ast.Block { pragmas = [],
                            defns = [],
                            body = body,
                            head = SOME head,
                            pos = pos }
            end
      | _ => internalError ["defn did not remove pragmas and definitions"]
    end


(*
    PROGRAM
*)

and topEnv () = { context = !Defn.topFixtures,
                  returnType = NONE }

and verifyPackage (p:Ast.PACKAGE)
    : Ast.PACKAGE =
    raise UnimplError

and verifyProgram (p:Ast.PROGRAM) 
    : Ast.PROGRAM =
    let 
    in case p of
        { packages, fixtures, block } =>
            let
                val _ = LogErr.setPos NONE
                val e = topEnv ()
                val block = verifyBlock e block
                val result = { packages = packages,
                               block = block,
                               fixtures = fixtures }
            in
                trace ["verification complete"];
                (if !doTrace 
                 then Pretty.ppProgram result
                 else ());
                result
            end
    end
end



(********************************** OLD **************************************

(*
 * INVARIANTS:
 *   - all typed libraries in host environment must be DontDelete
 *   - all typed libraries in host environment must carry compatible runtime type constraints
 *)

(****************************** type environments *************************)

type ID = FIXTURE_NAME
		
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
		      (id:Ast.IDENT) 
		      (ns:NAMESPACE)
    : KIND option =
    case List.find (fn (i,_) => i=PropName {id=id,ns=ns}) env of
	NONE => NONE
      | SOME (_,k) => SOME k

fun lookupIdNamespaces (env:TYPE_ENV) 
		       (id:Ast.IDENT) 
		       (nss : NAMESPACE list)
    : KIND option =
    let val theMatches
	  = List.mapPartial (lookupIdNamespace env id) nss
    in
	case theMatches of
	    [] => NONE
	  | [k] => SOME k



(******************** Expressions **************************************************)

and verifyIdentExpr (ctxt as {env,this,...}:CONTEXT) 
		    (ide:Ast.IDENT_EXPR) 
    : Ast.TYPE_EXPR = 
    let
    in 
	case ide of
	    Ast.QualifiedIdentifier { qual, ident } =>
	    let in
		checkCompatible (verifyExpr ctxt qual) namespaceType;
		anyType
	    end	    
	  | Ast.QualifiedExpression { qual, expr } =>
	    let in
		checkCompatible (verifyExpr ctxt qual) namespaceType;
		checkCompatible (verifyExpr ctxt qual) stringType;
		anyType
	    end
	  | Ast.AttributeIdentifier idexpr =>
	    let in
		verifyIdentExpr ctxt idexpr;
		anyType
	    end
	  | Ast.Identifier { ident, openNamespaces } =>
	    let val k : KIND = lookupIdNamespacess env ident openNamespaces
	    in
		case k of
		    TypeVar => verifyError ["Attempt to refer to type variable ",
                                    ident,
                                    " as a program variable"]
		  | ProgVar (ty,read_only) => ty
	    end
    end

and verifyExpr (ctxt as {env,this,...}:CONTEXT) 
	       (e:EXPR) 
    : Ast.TYPE_EXPR = 
    let
    in 
      TextIO.print ("type checking expr: env len " ^ (Int.toString (List.length env)) ^"\n");
      Pretty.ppExpr e;
      TextIO.print "\n";
      case e of
	LiteralExpr LiteralNull => nullType
      | LiteralExpr (LiteralInt _) => intType
      | LiteralExpr (LiteralUInt _) => uintType
      | LiteralExpr (LiteralDecimal _) => decimalType
      | LiteralExpr (LiteralDouble _) => doubleType
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
      | LexicalRef { ident, pos } =>
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
		     Ast.FunctionType {typeParams, ...} => typeParams
                   (* TODO: class and interface types *)
		   | _ => verifyError ["Cannot instantiate a non-polymorphic type"]
	 in
	     List.app (fn t => verifyTypeExpr ctxt t) actuals;
	     if (List.length typeParams) = (List.length actuals)
	     then ()
	     else verifyError ["Wrong number of type arguments"];
	     normalizeType (Ast.AppType { base=exprTy, args=actuals })
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
                  ident: Ast.IDENT_EXPR }
       | NewExpr of { obj: EXPR,x
                      actuals: EXPR list }


     and Ast.IDENT_EXPR =
         Ast.QualifiedIdentifier of { qual : EXPR,
                                  ident : USTRING }
       | Ast.QualifiedExpression of { qual : EXPR,
                                  expr : EXPR }
       | Ast.AttributeIdentifier of Ast.IDENT_EXPR
       | Ast.Identifier of Ast.IDENT
       | Ast.Expression of EXPR   (* for bracket exprs: o[x] and @[x] *)

*)


and verifyCallExpr  (ctxt as {env,this,...}:CONTEXT) 
		    func actuals
    : Ast.TYPE_EXPR =
    let val functy = verifyExpr ctxt func;
	val actualsTy = (map (fn a => verifyExpr ctxt a) actuals)
    in case normalizeType functy of
	   Ast.SpecialType Ast.Any =>
	   (* not much to do *)
	   anyType
	 | Ast.FunctionType { typeParams, params, result, thisType, hasRest, minArgs }
	   => 
	   let 
	   in
	       if not (null typeParams)
	       then verifyError ["Attempt to apply polymorphic function to values"]
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
		       else verifyError ["Too many args to function"]
		     | handleArgs (_::_) [] =
		       verifyError ["Not enough args to function"]
	       in
		   handleArgs params actualsTy;
		   result
	       end
	   end
	 | _ => verifyError ["Function expression does not have a function type"]
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
    : Ast.TYPE_EXPR = 
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
                      ty: Ast.TYPE_EXPR option }

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
          | PreIncrement _  => checkNumeric ()
          | PostIncrement _ => checkNumeric ()
          | PreDecrement _  => checkNumeric ()
          | UnaryPlus _     => checkNumeric ()
          | UnaryMinus _    => checkNumeric ()
          | BitwiseNot      => 
	    let in checkConvertible argType uintType; uintType end
          | LogicalNot    => 
	    let in checkConvertible argType boolType; boolType end
	    
  (*
             Delete => (case arg of
                            Ref {base=NONE,ident=???} =>
                          | Ref {base=SOME baseExpr,ident=???} =>
                          | _ => verifyError ["can only delete ref expressions"])
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
			 (bop:BINTYPEOP, arg:EXPR, t:Ast.TYPE_EXPR) =
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

and verifyStmt (ctxt as {this,env,lbls:(Ast.IDENT list),retTy}:CONTEXT) (stmt:STMT) =
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

  | (DoWhileStmt {cond,body,labels,fixtures} | WhileStmt {cond,body,labels,fixtures}) => 
    let in
	checkCompatible (verifyExpr ctxt cond) boolType;
	verifyStmt (withLbls ctxt (labels@lbls)) body
    end

  | ReturnStmt e => 
    let in
	case retTy of
	    NONE => verifyError ["return not allowed here"]
          | SOME retTy => checkCompatible (verifyExpr ctxt e) retTy
    end

  | (BreakStmt NONE | ContinueStmt NONE) =>  
    let in
	case lbls of
	    [] => verifyError ["Not in a loop"]
	  | _ => ()
    end

  | (BreakStmt (SOME lbl) | ContinueStmt (SOME lbl)) => 
    let in
	if List.exists (fn x => x=(lbl)) lbls
	then ()
	else verifyError ["No such label"]
    end

  | BlockStmt b => verifyBlock ctxt b

  | LabeledStmt (lab, s) => 
	verifyStmt (withLbls ctxt ((lab)::lbls)) s
 
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

  | ForStmt { defn=_, fixtures, init, cond, update, labels, body } =>
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
	verifyStmt (withLbls ctxt' (labels@lbls)) body
    end

  | SwitchStmt { cond, cases, ... } =>  
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
		([(ident, SOME (Ast.FunctionType fsig))],[])
            end
	    
	  | InterfaceDefn {ident,ns,nonnullable,params,extends,body} =>
	    let val nuEnv = extendEnvWithTypeVars params env
	    in
		verifyBlock (withEnv (ctxt,nuEnv)) body;
		List.app
		(fn (superi:Ast.IDENT_EXPR) =>
		    (* need to check that this interface implements _all_ methods of superinterface ident *)
		    ()
		)
		extends;
		([],[])
	    end
		

(*
and INTERFACE_DEFN =
         { ident: Ast.IDENT,
           ns: EXPR,
           nonnullable: bool,
           params: Ast.IDENT list,
           extends: Ast.IDENT_EXPR list,
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




and verifyBlock (ctxt as {env,...}) 
		(Block {pragmas,defns=_,body,head,pos}) =
    let val SOME (fixtures,inits) = head 
	val extensions = verifyFixtures ctxt fixtures
        val ctxt' = withEnvExtn ctxt extensions
    in
	verifyStmts ctxt' body
    end

fun verifyProgram (prog as { packages, fixtures, block }) =    
    let
    in 
      TextIO.print ("type checking program\n");
      Pretty.ppFixtures (!Defn.topFixtures);
      TextIO.print "\n";
      Pretty.ppProgram prog;
      TextIO.print "\n";
      verifyBlock {this=anyType, env=[], lbls=[], retTy=NONE} block; 
      true
    end

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

************************************)
